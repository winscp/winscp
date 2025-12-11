using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Threading;
using System.Xml;
#if !NETSTANDARD
using Microsoft.Win32;
#endif
using System.Diagnostics;
#if !NETSTANDARD
using System.Security;
#endif
using System.Text.RegularExpressions;
using System.Linq;
using System.Text;

namespace WinSCP
{
    [Guid("38649D44-B839-4F2C-A9DC-5D45EEA4B5E9")]
    [ComVisible(true)]
    public enum SynchronizationMode
    {
        Local = 0,
        Remote = 1,
        Both = 2,
    }

    [Guid("3F770EC1-35F5-4A7B-A000-46A2F7A213D8")]
    [ComVisible(true)]
    [Flags]
    public enum SynchronizationCriteria
    {
        None = 0x00,
        Time = 0x01,
        Size = 0x02,
        Checksum = 0x04,
        [Obsolete("Use Time | Size")]
        Either = Time | Size,
    }

    [Guid("6C441F60-26AA-44FC-9B93-08884768507B")]
    [ComVisible(true)]
    [Flags]
    public enum EnumerationOptions
    {
        None = 0x00,
        AllDirectories = 0x01,
        MatchDirectories = 0x02,
        EnumerateDirectories = 0x04,
    }

    [Guid("16B6D8F6-C0B4-487D-9546-A25BBF582ED6")]
    [ComVisible(true)]
    public enum ProgressSide
    {
        Local = 0,
        Remote = 1,
    }

    public delegate void OutputDataReceivedEventHandler(object sender, OutputDataReceivedEventArgs e);
    public delegate void FileTransferredEventHandler(object sender, TransferEventArgs e);
    public delegate void FileTransferProgressEventHandler(object sender, FileTransferProgressEventArgs e);
    public delegate void FailedEventHandler(object sender, FailedEventArgs e);
    public delegate void QueryReceivedEventHandler(object sender, QueryReceivedEventArgs e);

    [Guid("56FFC5CE-3867-4EF0-A3B5-CFFBEB99EA35")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComSourceInterfaces(typeof(ISessionEvents))]
    public sealed class Session : ISession, IReflect
    {
        public string ExecutablePath { get { return GetExecutablePath(); } set { CheckNotOpened(); _executablePath = value; } }
#if !NETSTANDARD
        public string ExecutableProcessUserName { get { return _executableProcessUserName; } set { CheckNotOpened(); _executableProcessUserName = value; } }
        public SecureString ExecutableProcessPassword { get { return _executableProcessPassword; } set { CheckNotOpened(); _executableProcessPassword = value; } }
#endif
        public string AdditionalExecutableArguments { get { return _additionalExecutableArguments; } set { CheckNotOpened(); _additionalExecutableArguments = value; } }
        [Obsolete("Use AddRawConfiguration")]
        public bool DefaultConfiguration { get { return _defaultConfiguration; } set { CheckNotOpened(); _defaultConfiguration = value; } }
        public bool DisableVersionCheck { get { return _disableVersionCheck; } set { CheckNotOpened(); _disableVersionCheck = value; } }
        [Obsolete("Use AddRawConfiguration")]
        public string IniFilePath { get { return _iniFilePath; } set { CheckNotOpened(); _iniFilePath = value; } }
        public TimeSpan ReconnectTime { get { return _reconnectTime; } set { CheckNotOpened(); _reconnectTime = value; } }
        public int ReconnectTimeInMilliseconds { get { return Tools.TimeSpanToMilliseconds(ReconnectTime); } set { ReconnectTime = Tools.MillisecondsToTimeSpan(value); } }
        public string DebugLogPath { get { CheckNotDisposed(); return Logger.LogPath; } set { CheckNotDisposed(); Logger.LogPath = value; } }
        public int DebugLogLevel { get { CheckNotDisposed(); return Logger.LogLevel; } set { CheckNotDisposed(); Logger.LogLevel = value; } }
        public string SessionLogPath { get => _sessionLogPath; set => SetSessionLogPath(value); }
        public string XmlLogPath { get { return _xmlLogPath; } set { CheckNotOpened(); _xmlLogPath = value; } }
        public bool XmlLogPreserve { get; set; }
        #if DEBUG
        public bool GuardProcessWithJob { get { return GuardProcessWithJobInternal; } set { GuardProcessWithJobInternal = value; } }
        public bool TestHandlesClosed { get { return TestHandlesClosedInternal; } set { TestHandlesClosedInternal = value; } }
        #endif
        public string HomePath { get { CheckOpened(); return _homePath; } }

        public TimeSpan Timeout { get; set; }

        public StringCollection Output { get; private set; }
        public bool Opened { get { CheckNotDisposed(); return (_process != null); } }

        public event FileTransferredEventHandler FileTransferred;
        public event FailedEventHandler Failed;
        public event OutputDataReceivedEventHandler OutputDataReceived;

        public event FileTransferProgressEventHandler FileTransferProgress
        {
            add
            {
                using (CreateCallstackAndLock())
                {
                    CheckNotOpened();
                    _fileTransferProgress += value;
                }
            }

            remove
            {
                using (CreateCallstackAndLock())
                {
                    CheckNotOpened();
                    _fileTransferProgress -= value;
                }
            }
        }

        private CallstackAndLock CreateCallstackAndLock()
        {
            var result = Logger.CreateCallstackAndLock();
            if ((_process != null) && (_process.StdOut != null))
            {
                result.Dispose();
                throw Logger.WriteException(new InvalidOperationException("Finish reading the stream from Session.GetFile first."));
            }
            return result;
        }

        public event QueryReceivedEventHandler QueryReceived { add { AddQueryReceived(value); } remove { RemoveQueryReceived(value); } }

        private void AddQueryReceived(QueryReceivedEventHandler value)
        {
            using (CreateCallstackAndLock())
            {
                bool send = (_queryReceived == null);
                _queryReceived += value;
                if (Opened && send)
                {
                    SendOptionBatchCommand();
                    WaitForGroup();
                }
            }
        }

        private void RemoveQueryReceived(QueryReceivedEventHandler value)
        {
            using (CreateCallstackAndLock())
            {
                if (_queryReceived != null)
                {
                    _queryReceived -= value;
                    if (Opened && (_queryReceived == null))
                    {
                        SendOptionBatchCommand();
                        WaitForGroup();
                    }
                }
            }
        }

        public Session()
        {
            Logger = new Logger();

            using (CreateCallstackAndLock())
            {
                Timeout = new TimeSpan(0, 1, 0);
                _reconnectTime = new TimeSpan(0, 2, 0); // keep in sync with TScript::OptionImpl
                ResetOutput();
                _operationResults = new List<OperationResultBase>();
                _events = new List<Action>();
                _eventsEvent = new AutoResetEvent(false);
                _choiceEvent = new ManualResetEvent(false);
                _disposed = false;
                _defaultConfiguration = true;
                _logUnique = 0;
                _guardProcessWithJob = true;
                RawConfiguration = new Dictionary<string, string>();
            }
        }

        private void ResetOutput()
        {
            Output = new StringCollection();
            _error = new StringCollection();
        }

        public void Dispose()
        {
            using (CreateCallstackAndLock())
            {
                _disposed = true;

                Cleanup();
                Logger.Dispose();

                if (_eventsEvent != null)
                {
                    _eventsEvent.Close();
                    _eventsEvent = null;
                }

                if (_choiceEvent != null)
                {
                    _choiceEvent.Close();
                    _choiceEvent = null;
                }

                GC.SuppressFinalize(this);
            }
        }

        public void Abort()
        {
            using (Logger.CreateCallstack())
            {
                CheckOpened();

                _aborted = true;

                // double-check
                _process?.Abort();
            }
        }

        public void Open(SessionOptions sessionOptions)
        {
            using (CreateCallstackAndLock())
            {
                CheckNotOpened();

                if (sessionOptions == null)
                {
                    throw Logger.WriteException(new ArgumentNullException(nameof(sessionOptions)));
                }

                try
                {
                    SetupTempPath();
                    ResetOutput();

                    _process = ExeSessionProcess.CreateForSession(this);

                    _process.OutputDataReceived += ProcessOutputDataReceived;

                    _process.Start();

                    GotOutput();

                    // setup batch mode
                    SendOptionBatchCommand();
                    WriteCommand("option confirm off");

                    object reconnectTimeValue;
                    if (ReconnectTime != TimeSpan.MaxValue)
                    {
                        reconnectTimeValue = (int)ReconnectTime.TotalSeconds;
                    }
                    else
                    {
                        reconnectTimeValue = "off";
                    }
                    string reconnectTimeCommand =
                        string.Format(CultureInfo.InvariantCulture, "option reconnecttime {0}", reconnectTimeValue);
                    WriteCommand(reconnectTimeCommand);

                    SessionOptionsToUrlAndSwitches(sessionOptions, false, out string command, out string log);
                    const string openCommand = "open ";
                    command = openCommand + command;
                    log = openCommand + log;
                    WriteCommand(command, log);

                    Logger.WriteLine("Waiting for XML log file");

                    // Wait until the log file gets created or WinSCP terminates (in case of fatal error)
                    do
                    {
                        string logExplanation;
                        lock (Output)
                        {
                            if (_error.Count > 0)
                            {
                                logExplanation = GetErrorOutputMessage();
                            }
                            else if (Output.Count > 0)
                            {
                                logExplanation =
                                    string.Format(
                                        CultureInfo.CurrentCulture, "Output was \"{0}\". ", ListToString(Output));
                            }
                            else
                            {
                                logExplanation = "There was no output. ";
                            }
                        }
                        logExplanation +=
                            string.Format(CultureInfo.CurrentCulture,
                                "Response log file {0} was not created. This could indicate lack of write permissions to the log folder or problems starting WinSCP itself.",
                                XmlLogPath);

                        if (_process.HasExited && !File.Exists(XmlLogPath))
                        {
#if !NETSTANDARD
                            Logger.WriteCounters();
#endif
                            Logger.WriteProcesses();
                            _process.WriteStatus();
                            string exitCode = string.Format(CultureInfo.CurrentCulture, "{0}", _process.ExitCode);
                            if (_process.ExitCode < 0)
                            {
                                exitCode = string.Format(CultureInfo.CurrentCulture, "{0} ({1:X})", exitCode, _process.ExitCode);
                            }
                            throw Logger.WriteException(
                                new SessionLocalException(this,
                                    string.Format(CultureInfo.CurrentCulture, "WinSCP process terminated with exit code {0}. ", exitCode) +
                                    logExplanation));
                        }

                        Thread.Sleep(50);

                        CheckForTimeout(
                            "WinSCP has not responded in time. " +
                            logExplanation);

                    } while (!File.Exists(XmlLogPath));

                    Logger.WriteLine("XML log file created");

                    _logReader = new SessionLogReader(this);

                    _logReader.WaitForNonEmptyElement("session", LogReadFlags.ThrowFailures);

                    // special variant of ElementLogReader that throws when closing element (</session>) is encountered
                    _reader = new SessionElementLogReader(_logReader);

                    // Skip "open" command <group>
                    WaitForGroup();

                    WriteCommand("pwd");

                    using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                    using (ElementLogReader cwdReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("cwd", LogReadFlags.ThrowFailures))
                    {
                        while (cwdReader.Read(0))
                        {
                            if (cwdReader.GetEmptyElementValue("cwd", out string value))
                            {
                                _homePath = value;
                            }
                        }

                        groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                    }

                    _sessionTimeout = sessionOptions.Timeout;
                }
                catch (Exception e)
                {
                    Logger.WriteLine("Exception: {0}", e);
                    Cleanup();
                    throw;
                }
            }
        }

        private void SendOptionBatchCommand()
        {
            string command = string.Format(CultureInfo.InvariantCulture, "option batch {0}", (_queryReceived != null ? "off" : "on"));
            WriteCommand(command);
        }

        private void WaitForGroup()
        {
            using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
            {
                ReadElement(groupReader, LogReadFlags.ThrowFailures);
            }
        }

        internal string GetErrorOutputMessage()
        {
            string result = null;
            if (_error.Count > 0)
            {
                result = string.Format(CultureInfo.CurrentCulture, "Error output was \"{0}\". ", ListToString(_error));
            }
            return result;
        }

        private static string ListToString(StringCollection list)
        {
            string[] stringArray = new string[list.Count];
            list.CopyTo(stringArray, 0);
            string s = string.Join(Environment.NewLine, stringArray);
            return s;
        }

        public string ScanFingerprint(SessionOptions sessionOptions, string algorithm)
        {
            using (CreateCallstackAndLock())
            {
                string normalizeAlgorithm = NormalizeIdent(algorithm);
                if (string.IsNullOrEmpty(normalizeAlgorithm))
                {
                    throw Logger.WriteException(new ArgumentException("Algorithm cannot be empty", nameof(algorithm)));
                }

                string result;

                CheckNotOpened();

                try
                {
                    ResetOutput();

                    SessionOptionsToUrlAndSwitches(sessionOptions, true, out string command, out string log);

                    string additionalArguments = "/fingerprintscan " + command;

                    _process = ExeSessionProcess.CreateForConsole(this, additionalArguments);

                    _process.OutputDataReceived += ProcessOutputDataReceived;

                    _process.Start();

                    GotOutput();

                    while (!_process.HasExited)
                    {
                        Thread.Sleep(50);

                        CheckForTimeout();
                    }

                    if (_process.ExitCode == 0)
                    {
                        result = null;
                        foreach (string s in Output)
                        {
                            int p = s.IndexOf(":", StringComparison.Ordinal);
                            if (p < 0)
                            {
                                throw Logger.WriteException(new SessionLocalException(this, string.Format(CultureInfo.CurrentCulture, "Unexpected fingerprint scan result line '{0}'", s)));
                            }
                            string a = NormalizeIdent(s.Substring(0, p).Trim());
                            if (normalizeAlgorithm.Equals(a, StringComparison.OrdinalIgnoreCase))
                            {
                                result = s.Substring(p + 1).Trim();
                                break;
                            }
                        }

                        if (result == null)
                        {
                            throw Logger.WriteException(new SessionLocalException(this, string.Format(CultureInfo.CurrentCulture, "Fingerprint for algorithm {0} not supported", algorithm)));
                        }
                    }
                    else
                    {
                        throw Logger.WriteException(new SessionRemoteException(this, ListToString(Output)));
                    }
                }
                catch (Exception e)
                {
                    Logger.WriteLine("Exception: {0}", e);
                    throw;
                }
                finally
                {
                    Cleanup();
                }

                return result;
            }
        }

        private static string NormalizeIdent(string algorithm)
        {
            return algorithm.Replace("-", string.Empty);
        }

        public void Close()
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                Cleanup();
            }
        }

        public RemoteDirectoryInfo ListDirectory(string path)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "ls -- \"{0}\"", Tools.ArgumentEscape(IncludeTrailingSlash(path))));

                RemoteDirectoryInfo result = new RemoteDirectoryInfo();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader lsReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("ls", LogReadFlags.ThrowFailures))
                {
                    string destination = null;
                    if (lsReader.TryWaitForEmptyElement("destination", 0))
                    {
                        lsReader.GetEmptyElementValue("destination", out destination);
                    }
                    if ((destination != null) && lsReader.TryWaitForNonEmptyElement("files", 0))
                    {
                        destination = IncludeTrailingSlash(destination);

                        using (ElementLogReader filesReader = lsReader.CreateLogReader())
                        {
                            while (filesReader.TryWaitForNonEmptyElement("file", 0))
                            {
                                RemoteFileInfo fileInfo = new RemoteFileInfo();

                                using (ElementLogReader fileReader = filesReader.CreateLogReader())
                                {
                                    while (fileReader.Read(0))
                                    {
                                        if (fileReader.GetEmptyElementValue("filename", out string value))
                                        {
                                            fileInfo.Name = value;
                                            fileInfo.FullName = destination + value;
                                        }
                                        else
                                        {
                                            ReadFile(fileInfo, fileReader);
                                        }
                                    }

                                    result.AddFile(fileInfo);
                                }
                            }
                        }

                        lsReader.ReadToEnd(LogReadFlags.ThrowFailures);
                        groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                    }
                    else
                    {
                        // "files" not found, keep reading, we expect "failure"
                        // This happens only in case of fatal errors,
                        // in case of normal error (non existing folder),
                        // the "failure" is caught in "group" already, before the "ls".
                        groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                        // only if not "failure", throw "files" not found
                        throw Logger.WriteException(SessionLocalException.CreateElementNotFound(this, "files"));
                    }
                }

                return result;
            }
        }

        private IEnumerable<RemoteFileInfo> DoEnumerateRemoteFiles(string path, Regex regex, EnumerationOptions options, bool throwReadErrors)
        {
            Logger.WriteLine("Starting enumeration of {0} ...", path);

            bool allDirectories = ((options & EnumerationOptions.AllDirectories) == EnumerationOptions.AllDirectories);
            bool matchDirectories = ((options & EnumerationOptions.MatchDirectories) == EnumerationOptions.MatchDirectories);
            bool enumerateDirectories = ((options & EnumerationOptions.EnumerateDirectories) == EnumerationOptions.EnumerateDirectories);

            if (enumerateDirectories && !allDirectories)
            {
                throw Logger.WriteException(new ArgumentException("Cannot use enumeration option EnumerateDirectories without AllDirectories"));
            }

            if (enumerateDirectories && matchDirectories)
            {
                throw Logger.WriteException(new ArgumentException("Cannot combine enumeration option EnumerateDirectories with MatchDirectories"));
            }

            RemoteDirectoryInfo directoryInfo;

            try
            {
                // Need to use guarded method for the listing, see a comment in EnumerateRemoteFiles
                directoryInfo = ListDirectory(path);
            }
            catch (SessionRemoteException)
            {
                if (throwReadErrors)
                {
                    throw;
                }
                else
                {
                    directoryInfo = null;
                }
            }

            if (directoryInfo != null)
            {
                foreach (RemoteFileInfo fileInfo in directoryInfo.Files)
                {
                    if (!fileInfo.IsThisDirectory && !fileInfo.IsParentDirectory)
                    {
                        bool matches = regex.IsMatch(fileInfo.Name);

                        bool enumerate;
                        if (!fileInfo.IsDirectory)
                        {
                            enumerate = matches;
                        }
                        else
                        {
                            if (enumerateDirectories)
                            {
                                enumerate = true;
                            }
                            else if (matchDirectories)
                            {
                                enumerate = matches;
                            }
                            else
                            {
                                enumerate = false;
                            }
                        }

                        if (enumerate)
                        {
                            Logger.WriteLine("Enumerating {0}", fileInfo.FullName);
                            yield return fileInfo;
                        }


                        if (fileInfo.IsDirectory && allDirectories)
                        {
                            foreach (RemoteFileInfo fileInfo2 in DoEnumerateRemoteFiles(RemotePath.Combine(path, fileInfo.Name), regex, options, false))
                            {
                                yield return fileInfo2;
                            }
                        }
                    }
                }
            }

            Logger.WriteLine("Ended enumeration of {0}", path);
        }

        public IEnumerable<RemoteFileInfo> EnumerateRemoteFiles(string path, string mask, EnumerationOptions options)
        {
            // Note that this method exits as soon as DoEnumerateRemoteFiles is entered,
            // so the Session object is not guarded during the whole enumeration.
            // Though it should not matter as it uses only guarded methods (ListDirectory)
            // for the actual work on the session
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                Regex regex = MaskToRegex(mask);

                return new ImplicitEnumerable<RemoteFileInfo>(DoEnumerateRemoteFiles(path, regex, options, true));
            }
        }

        internal static Regex MaskToRegex(string mask)
        {
            if (string.IsNullOrEmpty(mask) ||
                // *.* has to match even filename without dot
                (mask == "*.*"))
            {
                mask = "*";
            }

            string r = "^";
            foreach (var c in mask)
            {
                string p;
                switch (c)
                {
                    case '.': p = "[.]"; break;
                    case '*': p = ".*"; break;
                    case '?': p = "."; break;
                    default: p = Regex.Escape(new string(c, 1)); break;
                }
                r += p;
            }
            r += "$";

            return new Regex(r, RegexOptions.IgnoreCase);
        }

        public TransferOperationResult PutFiles(string localPath, string remotePath, bool remove = false, TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                return DoPutFiles(localPath, remotePath, remove, options);
            }
        }

        public TransferOperationResult DoPutFiles(string localPath, string remotePath, bool remove, TransferOptions options)
        {
            using (Logger.CreateCallstack())
            {
                if (options == null)
                {
                    options = new TransferOptions();
                }

                CheckOpened();

                WriteCommand(
                    string.Format(CultureInfo.InvariantCulture,
                        "put {0} {1} -- \"{2}\" \"{3}\"",
                        BooleanSwitch(remove, "delete"), options.ToSwitches(),
                        Tools.ArgumentEscape(localPath), Tools.ArgumentEscape(remotePath)));

                TransferOperationResult result = new TransferOperationResult();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (RegisterOperationResult(result))
                using (CreateProgressHandler())
                {
                    TransferEventArgs args = null;
                    bool mkdir = false;

                    while (groupReader.Read(0))
                    {
                        if (groupReader.IsNonEmptyElement(TransferEventArgs.UploadTag))
                        {
                            AddTransfer(result, args);
                            args = TransferEventArgs.Read(ProgressSide.Local, groupReader);
                            mkdir = false;
                        }
                        else if (groupReader.IsNonEmptyElement(TransferEventArgs.MkDirTag))
                        {
                            AddTransfer(result, args);
                            args = null;
                            mkdir = true;
                            // For now, silently ignoring results (even errors)
                            // of mkdir operation, including future chmod/touch
                        }
                        else if (groupReader.IsNonEmptyElement(ChmodEventArgs.Tag))
                        {
                            if (!mkdir)
                            {
                                if (args == null)
                                {
                                    throw Logger.WriteException(new InvalidOperationException("Tag chmod before tag upload"));
                                }
                                args.Chmod = ChmodEventArgs.Read(groupReader);
                            }
                        }
                        else if (groupReader.IsNonEmptyElement(TouchEventArgs.Tag))
                        {
                            if (!mkdir)
                            {
                                if (args == null)
                                {
                                    throw Logger.WriteException(new InvalidOperationException("Tag touch before tag upload"));
                                }
                                args.Touch = TouchEventArgs.Read(groupReader);
                            }
                        }
                    }

                    AddTransfer(result, args);
                }

                return result;
            }
        }

        public TransferOperationResult PutFilesToDirectory(
            string localDirectory, string remoteDirectory, string filemask = null, bool remove = false, TransferOptions options = null)
        {
            // Not locking, locked in PutFiles
            using (Logger.CreateCallstack())
            {
                if (localDirectory == null)
                {
                    throw Logger.WriteException(new ArgumentNullException(nameof(localDirectory)));
                }
                if (remoteDirectory == null)
                {
                    throw Logger.WriteException(new ArgumentNullException(nameof(remoteDirectory)));
                }
                if (string.IsNullOrEmpty(filemask))
                {
                    filemask = "*";
                }
                string localPath = Path.Combine(localDirectory, filemask);
                string remotePath = RemotePath.AddDirectorySeparator(remoteDirectory);
                return PutFiles(localPath, remotePath, remove, options);
            }
        }

        public TransferEventArgs PutFileToDirectory(string localFilePath, string remoteDirectory, bool remove = false, TransferOptions options = null)
        {
            // Not locking, locked in PutFiles (within PutFilesToDirectory)
            using (Logger.CreateCallstack())
            {
                if (!File.Exists(localFilePath))
                {
                    throw Logger.WriteException(new FileNotFoundException($"File {localFilePath} does not exist", localFilePath));
                }

                TransferOperationResult operationResult = PutEntryToDirectory(localFilePath, remoteDirectory, remove, options);
                return GetOnlyFileOperation(operationResult.Transfers);
            }
        }

        public void PutFile(Stream stream, string remoteFilePath, TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                if (remoteFilePath == null)
                {
                    throw Logger.WriteException(new ArgumentNullException(nameof(remoteFilePath)));
                }
                if (_process.StdIn != null)
                {
                    throw Logger.WriteException(new InvalidOperationException("Already uploading from a stream"));
                }
                _process.StdIn = stream ?? throw Logger.WriteException(new ArgumentNullException(nameof(stream)));
                try
                {
                    remoteFilePath = RemotePath.EscapeOperationMask(remoteFilePath);
                    TransferOperationResult operationResult = DoPutFiles("-", remoteFilePath, false, options);
                    operationResult.Check();
                    // Assert that any transfer took place at all
                    GetOnlyFileOperation(operationResult.Transfers);
                }
                finally
                {
                    _process.StdIn = null;
                }
            }
        }

        internal TransferOperationResult PutEntryToDirectory(string localFilePath, string remoteDirectory, bool remove = false, TransferOptions options = null)
        {
            using (Logger.CreateCallstack())
            {
                if (string.IsNullOrEmpty(localFilePath))
                {
                    throw Logger.WriteException(new ArgumentException("File to path cannot be empty", nameof(localFilePath)));
                }

                string localDirectory = Path.GetDirectoryName(localFilePath);
                string filemask = Path.GetFileName(localFilePath);

                TransferOperationResult operationResult = PutFilesToDirectory(localDirectory, remoteDirectory, filemask, remove, options);
                operationResult.Check();
                return operationResult;
            }
        }

        private void AddTransfer(TransferOperationResult result, TransferEventArgs args)
        {
            if (args != null)
            {
                result.AddTransfer(args);
                RaiseFileTransferredEvent(args);
            }
        }

        public TransferOperationResult GetFiles(string remotePath, string localPath, bool remove = false, TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                return DoGetFiles(remotePath, localPath, remove, options, string.Empty);
            }
        }

        private TransferOperationResult DoGetFiles(
            string remotePath, string localPath, bool remove, TransferOptions options, string additionalParams)
        {
            using (Logger.CreateCallstack())
            {
                StartGetCommand(remotePath, localPath, remove, options, additionalParams);

                TransferOperationResult result = new TransferOperationResult();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (RegisterOperationResult(result))
                using (CreateProgressHandler())
                {
                    TransferEventArgs args = null;

                    while (groupReader.Read(0))
                    {
                        if (groupReader.IsNonEmptyElement(TransferEventArgs.DownloadTag))
                        {
                            AddTransfer(result, args);
                            args = TransferEventArgs.Read(ProgressSide.Remote, groupReader);
                        }
                        else if (groupReader.IsNonEmptyElement(RemovalEventArgs.Tag))
                        {
                            // When "downloading and deleting" a folder,
                            // we get "rm" tag without preceeding "download" tag.
                            // So we use only the first "rm" tag after preceeding "download" tag,
                            // silently ignoring the others
                            if ((args != null) && (args.Removal == null))
                            {
                                args.Removal = RemovalEventArgs.Read(groupReader);
                            }
                        }
                    }

                    AddTransfer(result, args);
                }

                return result;
            }
        }

        private void StartGetCommand(string remotePath, string localPath, bool remove, TransferOptions options, string additionalParams)
        {
            if (options == null)
            {
                options = new TransferOptions();
            }

            CheckOpened();

            WriteCommand(
                string.Format(CultureInfo.InvariantCulture, "get {0} {1} {2} -- \"{3}\" \"{4}\"",
                    BooleanSwitch(remove, "delete"), options.ToSwitches(), additionalParams,
                    Tools.ArgumentEscape(remotePath), Tools.ArgumentEscape(localPath)));
        }

        public TransferOperationResult GetFilesToDirectory(
            string remoteDirectory, string localDirectory, string filemask = null, bool remove = false, TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                return DoGetFilesToDirectory(remoteDirectory, localDirectory, filemask, remove, options, null);
            }
        }

        private TransferOperationResult DoGetFilesToDirectory(
            string remoteDirectory, string localDirectory, string filemask, bool remove, TransferOptions options, string additionalParams)
        {
            using (Logger.CreateCallstack())
            {
                if (remoteDirectory == null)
                {
                    throw Logger.WriteException(new ArgumentNullException(nameof(remoteDirectory)));
                }
                if (localDirectory == null)
                {
                    throw Logger.WriteException(new ArgumentNullException(nameof(localDirectory)));
                }
                if (string.IsNullOrEmpty(filemask))
                {
                    filemask = "*";
                }
                string remotePath = RemotePath.Combine(remoteDirectory, filemask);
                if (!Directory.Exists(localDirectory))
                {
                    throw Logger.WriteException(new DirectoryNotFoundException(localDirectory));
                }
                string localSeparator = Path.DirectorySeparatorChar.ToString();
                string localPath =
                    localDirectory +
                    (localDirectory.EndsWith(localSeparator, StringComparison.Ordinal) ? string.Empty : localSeparator);
                return DoGetFiles(remotePath, localPath, remove, options, additionalParams);
            }
        }

        public TransferEventArgs GetFileToDirectory(string remoteFilePath, string localDirectory, bool remove = false, TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                const string additionalParams = "-onlyfile";
                TransferOperationResult operationResult = GetEntryToDirectory(remoteFilePath, localDirectory, remove, options, additionalParams);
                return GetOnlyFileOperation(operationResult.Transfers);
            }
        }

        internal TransferOperationResult GetEntryToDirectory(
            string remoteFilePath, string localDirectory, bool remove = false, TransferOptions options = null, string additionalParams = null)
        {
            using (Logger.CreateCallstack())
            {
                ParseRemotePath(remoteFilePath, out string remoteDirectory, out string filemask);

                TransferOperationResult operationResult =
                    DoGetFilesToDirectory(remoteDirectory, localDirectory, filemask, remove, options, additionalParams ?? string.Empty);
                operationResult.Check();
                return operationResult;
            }
        }

        private void ParseRemotePath(string remoteFilePath, out string remoteDirectory, out string filemask)
        {
            if (string.IsNullOrEmpty(remoteFilePath))
            {
                throw Logger.WriteException(new ArgumentException("File to path cannot be empty", nameof(remoteFilePath)));
            }

            remoteDirectory = RemotePath.GetDirectoryName(remoteFilePath);
            filemask = RemotePath.EscapeFileMask(RemotePath.GetFileName(remoteFilePath));
        }

        private T GetOnlyFileOperation<T>(ICollection<T> operations)
        {
            // For "get", this should happen only when the filename is mask-like, otherwise "get" throws straight away.
            // For "put", this should not happen.
            if (operations.Count == 0)
            {
                throw Logger.WriteException(new FileNotFoundException("File not found"));
            }
            if (operations.Count > 1)
            {
                throw Logger.WriteException(new InvalidOperationException("More then one file has been unexpectedly found"));
            }
            return operations.First();
        }

        public Stream GetFile(string remoteFilePath, TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                const string additionalParams = "-onlyfile";
                ParseRemotePath(remoteFilePath, out string remoteDirectory, out string filemask);
                string remotePath = RemotePath.Combine(remoteDirectory, filemask);

                // only to collect failures
                TransferOperationResult result = new TransferOperationResult();

                ElementLogReader groupReader = null;
                IDisposable operationResultGuard = null;
                IDisposable progressHandler = null;

                // should never happen
                if (_process.StdOut != null)
                {
                    throw Logger.WriteException(new InvalidOperationException("Data stream already exist"));
                }

                PipeStream stream = new PipeStream();
                _process.StdOut = stream;

                void onGetEnd()
                {
                    using (Logger.CreateCallstack())
                    {
                        try
                        {
                            // This can throw
                            progressHandler?.Dispose();
                        }
                        finally
                        {
                            try
                            {
                                groupReader?.Dispose();
                            }
                            finally
                            {
                                _process.StdOut = null;
                                // Only after disposing the group reader, so when called from onGetEndWithExit, the Check() has all failures.
                                operationResultGuard?.Dispose();
                            }
                        }
                    }
                }

                void onGetEndWithExit()
                {
                    // Explicitly not Session.CreateCallstackAndLock
                    using (Logger.CreateCallstackAndLock())
                    {
                        Logger.WriteLine("Closing download stream");
                        try
                        {
                            onGetEnd();
                        }
                        finally
                        {
                            Logger.WriteLine("Closed download stream");
                            result.Check();
                        }
                    }
                }

                try
                {
                    StartGetCommand(remotePath, "-", false, options, additionalParams);

                    groupReader = _reader.WaitForGroupAndCreateLogReader();
                    operationResultGuard = RegisterOperationResult(result);
                    progressHandler = CreateProgressHandler();

                    if (_throwStdOut)
                    {
                        throw Logger.WriteException(new InvalidOperationException());
                    }
                    _throwStdOut = true;
                    bool downloadFound;
                    try
                    {
                        // Not using WaitForNonEmptyElement only to allow throwing better exception message below.
                        // Not using ThrowFailures as we need to return the stream, if there's <download>,
                        // even if there's a <failure> as well.
                        downloadFound = groupReader.TryWaitForNonEmptyElement(TransferEventArgs.DownloadTag, 0);
                    }
                    catch (StdOutException)
                    {
                        downloadFound = true;
                    }
                    finally
                    {
                        _throwStdOut = false;
                    }
                    if (downloadFound)
                    {
                        Logger.WriteLine("Download stream started");
                        stream.OnDispose = onGetEndWithExit;
                        return stream;
                    }
                    else
                    {
                        // First throw any specific error from <failure>
                        result.Check();
                        // And only then fallback to the generic one.
                        // See also the comment in GetOnlyFileOperation.
                        throw Logger.WriteException(new FileNotFoundException("File not found"));
                    }
                }
                catch
                {
                    onGetEnd();
                    throw;
                }
            }
        }

        public RemovalOperationResult RemoveFiles(string path)
        {
            using (CreateCallstackAndLock())
            {
                return DoRemoveFiles(path, string.Empty);
            }
        }

        private RemovalOperationResult DoRemoveFiles(string path, string additionalParams)
        {
            using (Logger.CreateCallstack())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "rm {0} -- \"{1}\"", additionalParams, Tools.ArgumentEscape(path)));

                RemovalOperationResult result = new RemovalOperationResult();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (RegisterOperationResult(result))
                {
                    while (groupReader.Read(0))
                    {
                        if (groupReader.IsNonEmptyElement(RemovalEventArgs.Tag))
                        {
                            result.AddRemoval(RemovalEventArgs.Read(groupReader));
                        }
                    }
                }

                return result;
            }
        }

        public RemovalEventArgs RemoveFile(string path)
        {
            using (CreateCallstackAndLock())
            {
                RemovalOperationResult operationResult = RemoveEntry(path, "-onlyfile");
                return GetOnlyFileOperation(operationResult.Removals);
            }
        }

        internal RemovalOperationResult RemoveEntry(string path, string additionalParams = null)
        {
            using (Logger.CreateCallstack())
            {
                if (string.IsNullOrEmpty(path))
                {
                    throw Logger.WriteException(new ArgumentException("File to path cannot be empty", nameof(path)));
                }

                string remoteDirectory = RemotePath.GetDirectoryName(path);
                string filemask = RemotePath.EscapeFileMask(RemotePath.GetFileName(path));
                if (string.IsNullOrEmpty(filemask))
                {
                    throw Logger.WriteException(new ArgumentException("File name cannot be empty", nameof(path)));
                }
                path = RemotePath.Combine(remoteDirectory, filemask);

                RemovalOperationResult operationResult = DoRemoveFiles(path, additionalParams ?? string.Empty);
                operationResult.Check();
                return operationResult;
            }
        }

        public SynchronizationResult SynchronizeDirectories(
            SynchronizationMode mode, string localPath, string remotePath,
            bool removeFiles, bool mirror = false, SynchronizationCriteria criteria = SynchronizationCriteria.Time,
            TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                DoSynchronizeDirectories(mode, localPath, remotePath, removeFiles, mirror, criteria, options, string.Empty);

                return ReadSynchronizeDirectories();
            }
        }

        private void DoSynchronizeDirectories(
            SynchronizationMode mode, string localPath, string remotePath, bool removeFiles, bool mirror,
            SynchronizationCriteria criteria, TransferOptions options, string additionalParameters)
        {
            if (options == null)
            {
                options = new TransferOptions();
            }

            CheckOpened();

            if (removeFiles && (mode == SynchronizationMode.Both))
            {
                throw Logger.WriteException(new ArgumentException("Cannot delete files in synchronization mode Both"));
            }

            if (mirror && (mode == SynchronizationMode.Both))
            {
                throw Logger.WriteException(new ArgumentException("Cannot mirror files in synchronization mode Both"));
            }

            if ((criteria != SynchronizationCriteria.Time) && (mode == SynchronizationMode.Both))
            {
                throw Logger.WriteException(new ArgumentException("Only Time criteria is allowed in synchronization mode Both"));
            }

            string modeName;
            switch (mode)
            {
                case SynchronizationMode.Local:
                    modeName = "local";
                    break;
                case SynchronizationMode.Remote:
                    modeName = "remote";
                    break;
                case SynchronizationMode.Both:
                    modeName = "both";
                    break;
                default:
                    throw Logger.WriteException(new ArgumentOutOfRangeException(nameof(mode)));
            }

            string criteriaName;
            if (criteria == SynchronizationCriteria.None)
            {
                criteriaName = "none";
            }
            else
            {
                var names = new Dictionary<SynchronizationCriteria, string>
                {
                    { SynchronizationCriteria.Time, "time" },
                    { SynchronizationCriteria.Size, "size" },
                    { SynchronizationCriteria.Checksum, "checksum" }
                };
                var c = criteria;
                criteriaName = string.Empty;
                foreach (var name in names)
                {
                    if (c.HasFlag(name.Key))
                    {
                        c -= name.Key;
                        criteriaName += (criteriaName.Length > 0 ? "," : string.Empty) + name.Value;
                    }
                }
                if (c != 0)
                {
                    throw Logger.WriteException(new ArgumentOutOfRangeException(nameof(criteria)));
                }
            }

            WriteCommand(
                string.Format(CultureInfo.InvariantCulture,
                    "synchronize {0} {1} {2} {3} -criteria=\"{4}\" {5} -- \"{6}\" \"{7}\"",
                    modeName,
                    BooleanSwitch(removeFiles, "delete"),
                    BooleanSwitch(mirror, "mirror"),
                    options.ToSwitches(),
                    criteriaName,
                    additionalParameters,
                    Tools.ArgumentEscape(localPath), Tools.ArgumentEscape(remotePath)));
        }

        private SynchronizationResult ReadSynchronizeDirectories()
        {
            using (Logger.CreateCallstack())
            {
                SynchronizationResult result = new SynchronizationResult();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (RegisterOperationResult(result))
                using (CreateProgressHandler())
                {
                    TransferEventArgs transfer = null;
                    bool mkdir = false;

                    while (groupReader.Read(0))
                    {
                        ProgressSide? newSide = null;
                        if (groupReader.IsNonEmptyElement(TransferEventArgs.UploadTag))
                        {
                            newSide = ProgressSide.Local;
                        }
                        else if (groupReader.IsNonEmptyElement(TransferEventArgs.DownloadTag))
                        {
                            newSide = ProgressSide.Remote;
                        }

                        if (newSide.HasValue)
                        {
                            AddSynchronizationTransfer(result, transfer);
                            transfer = TransferEventArgs.Read(newSide.Value, groupReader);
                            mkdir = false;
                        }
                        else if (groupReader.IsNonEmptyElement(RemovalEventArgs.Tag))
                        {
                            result.AddRemoval(RemovalEventArgs.Read(groupReader));
                        }
                        else if (groupReader.IsNonEmptyElement(TransferEventArgs.MkDirTag))
                        {
                            AddSynchronizationTransfer(result, transfer);
                            transfer = null;
                            mkdir = true;
                            // For now, silently ignoring results (even errors)
                            // of mkdir operation, including future chmod/touch
                        }
                        else if (groupReader.IsNonEmptyElement(ChmodEventArgs.Tag))
                        {
                            if (!mkdir)
                            {
                                if (transfer == null)
                                {
                                    throw Logger.WriteException(new InvalidOperationException("Tag chmod before tag download"));
                                }
                                transfer.Chmod = ChmodEventArgs.Read(groupReader);
                            }
                        }
                        else if (groupReader.IsNonEmptyElement(TouchEventArgs.Tag))
                        {
                            if (!mkdir)
                            {
                                if (transfer == null)
                                {
                                    throw Logger.WriteException(new InvalidOperationException("Tag touch before tag download"));
                                }
                                transfer.Touch = TouchEventArgs.Read(groupReader);
                            }
                        }
                    }

                    AddSynchronizationTransfer(result, transfer);
                }
                return result;
            }
        }

        public ComparisonDifferenceCollection CompareDirectories(
            SynchronizationMode mode, string localPath, string remotePath,
            bool removeFiles, bool mirror = false, SynchronizationCriteria criteria = SynchronizationCriteria.Time,
            TransferOptions options = null)
        {
            using (CreateCallstackAndLock())
            {
                DoSynchronizeDirectories(mode, localPath, remotePath, removeFiles, mirror, criteria, options, "-preview");

                return ReadCompareDirectories(localPath, remotePath);
            }
        }

        private ComparisonDifferenceCollection ReadCompareDirectories(string localPath, string remotePath)
        {
            using (Logger.CreateCallstack())
            {
                ComparisonDifferenceCollection result = new ComparisonDifferenceCollection();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                {
                    while (groupReader.TryWaitForNonEmptyElement("difference", LogReadFlags.ThrowFailures))
                    {
                        using (ElementLogReader differenceReader = groupReader.CreateLogReader())
                        {
                            ComparisonDifference difference = new ComparisonDifference(localPath, remotePath);
                            ComparisonFileInfo current = null;

                            while (differenceReader.Read(0))
                            {
                                if (differenceReader.GetEmptyElementValue("action", out string actionName))
                                {
                                    if ((difference.Local != null) || (difference.Remote != null))
                                    {
                                        throw Logger.WriteException(new InvalidOperationException("Tag action after filename"));
                                    }
                                    if (actionName.Equals("uploadnew", StringComparison.OrdinalIgnoreCase))
                                    {
                                        difference.Action = SynchronizationAction.UploadNew;
                                        difference.Local = new ComparisonFileInfo();
                                    }
                                    else if (actionName.Equals("downloadnew", StringComparison.OrdinalIgnoreCase))
                                    {
                                        difference.Action = SynchronizationAction.DownloadNew;
                                        difference.Remote = new ComparisonFileInfo();
                                    }
                                    else if (actionName.Equals("uploadupdate", StringComparison.OrdinalIgnoreCase))
                                    {
                                        difference.Action = SynchronizationAction.UploadUpdate;
                                        difference.Local = new ComparisonFileInfo();
                                        difference.Remote = new ComparisonFileInfo();
                                    }
                                    else if (actionName.Equals("downloadupdate", StringComparison.OrdinalIgnoreCase))
                                    {
                                        difference.Action = SynchronizationAction.DownloadUpdate;
                                        difference.Remote = new ComparisonFileInfo();
                                        difference.Local = new ComparisonFileInfo();
                                    }
                                    else if (actionName.Equals("deleteremote", StringComparison.OrdinalIgnoreCase))
                                    {
                                        difference.Action = SynchronizationAction.DeleteRemote;
                                        difference.Remote = new ComparisonFileInfo();
                                    }
                                    else if (actionName.Equals("deletelocal", StringComparison.OrdinalIgnoreCase))
                                    {
                                        difference.Action = SynchronizationAction.DeleteLocal;
                                        difference.Local = new ComparisonFileInfo();
                                    }
                                    else
                                    {
                                        throw Logger.WriteException(new InvalidOperationException(string.Format(CultureInfo.CurrentCulture, "Unknown synchronization action \"{0}\"", actionName)));
                                    }
                                }
                                else if (differenceReader.GetEmptyElementValue("type", out string fileType))
                                {
                                    difference.IsDirectory = (fileType.Length == 1) && RemoteFileInfo.IsDirectoryFileType(fileType[0]);
                                }
                                else if (differenceReader.GetEmptyElementValue("filename", out string value))
                                {
                                    if (current == null)
                                    {
                                        current = difference.Local ?? difference.Remote;
                                    }
                                    else if (current == difference.Local)
                                    {
                                        current = difference.Remote;
                                    }
                                    if (current == null)
                                    {
                                        throw Logger.WriteException(new InvalidOperationException("Unexpected filename tag"));
                                    }
                                    current.FileName = value;
                                }
                                else if (differenceReader.GetEmptyElementValue("modification", out value))
                                {
                                    if (current == null)
                                    {
                                        throw Logger.WriteException(new InvalidOperationException("Unexpected modification tag"));
                                    }
                                    current.LastWriteTime = XmlConvert.ToDateTime(value, XmlDateTimeSerializationMode.Local);
                                }
                                else if (differenceReader.GetEmptyElementValue("size", out value))
                                {
                                    if (current == null)
                                    {
                                        throw Logger.WriteException(new InvalidOperationException("Unexpected size tag"));
                                    }
                                    current.Length = long.Parse(value, CultureInfo.InvariantCulture);
                                }
                            }

                            if (difference.Action == default)
                            {
                                throw Logger.WriteException(new InvalidOperationException("No action tag found"));
                            }
                            if (((difference.Local != null) && string.IsNullOrEmpty(difference.Local.FileName)) ||
                                ((difference.Remote != null) && string.IsNullOrEmpty(difference.Remote.FileName)))
                            {
                                throw Logger.WriteException(new InvalidOperationException("Missing file information"));
                            }

                            result.InternalAdd(difference);
                        }
                    }
                }

                return result;
            }
        }

        public CommandExecutionResult ExecuteCommand(string command)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "call {0}", command));

                CommandExecutionResult result = new CommandExecutionResult();

                // registering before creating group reader, so that
                // it is still registered, when group reader is read to the end in its .Dispose();
                using (RegisterOperationResult(result))
                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader callReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("call", LogReadFlags.ThrowFailures))
                {
                    while (callReader.Read(0))
                    {
                        if (callReader.GetEmptyElementValue("output", out string value))
                        {
                            result.Output = value;
                        }
                        if (callReader.GetEmptyElementValue("erroroutput", out value))
                        {
                            result.ErrorOutput = value;
                        }
                        if (callReader.GetEmptyElementValue("exitcode", out value))
                        {
                            result.ExitCode = int.Parse(value, CultureInfo.InvariantCulture);
                        }
                    }
                }

                return result;
            }
        }

        public RemoteFileInfo GetFileInfo(string path)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                return DoGetFileInfo(path);
            }
        }

        public bool TryGetFileInfo(string path, out RemoteFileInfo fileInfo)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                try
                {
                    _ignoreFailed = true;
                    try
                    {
                        fileInfo = DoGetFileInfo(path);
                    }
                    finally
                    {
                        _ignoreFailed = false;
                    }
                    return true;
                }
                catch (SessionRemoteException)
                {
                    fileInfo = null;
                    return false;
                }
            }
        }

        public bool FileExists(string path)
        {
            return TryGetFileInfo(path, out _);
        }

        public byte[] CalculateFileChecksum(string algorithm, string path)
        {
            using (CreateCallstackAndLock())
            {
                WriteCommand(string.Format(CultureInfo.InvariantCulture, "checksum -- \"{0}\" \"{1}\"", Tools.ArgumentEscape(algorithm), Tools.ArgumentEscape(path)));

                string hex = null;

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader checksumReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("checksum", LogReadFlags.ThrowFailures))
                {
                    while (checksumReader.Read(0))
                    {
                        if (checksumReader.GetEmptyElementValue("checksum", out string value))
                        {
                            hex = value;
                        }
                    }

                    groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                }

                int len = hex.Length;

                if ((len % 2) != 0)
                {
                    string error = string.Format(CultureInfo.CurrentCulture, "Invalid string representation of checksum - {0}", hex);
                    throw Logger.WriteException(new SessionLocalException(this, error));
                }

                int count = len / 2;
                byte[] bytes = new byte[count];
                for (int i = 0; i < count; i++)
                {
                    bytes[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
                }
                return bytes;
            }
        }

        public void CreateDirectory(string path)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "mkdir \"{0}\"", Tools.ArgumentEscape(path)));

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader mkdirReader = groupReader.WaitForNonEmptyElementAndCreateLogReader(TransferEventArgs.MkDirTag, LogReadFlags.ThrowFailures))
                {
                    ReadElement(mkdirReader, 0);
                    groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                }
            }
        }

        public void MoveFile(string sourcePath, string targetPath)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "mv \"{0}\" \"{1}\"", Tools.ArgumentEscape(sourcePath), Tools.ArgumentEscape(targetPath)));

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                {
                    if (!groupReader.TryWaitForNonEmptyElement("mv", LogReadFlags.ThrowFailures))
                    {
                        throw Logger.WriteException(new SessionRemoteException(this, string.Format(CultureInfo.CurrentCulture, "{0} not found.", sourcePath)));
                    }
                    else
                    {
                        using (ElementLogReader mvReader = groupReader.CreateLogReader())
                        {
                            ReadElement(mvReader, 0);
                            groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                        }
                    }
                }
            }
        }

        public void DuplicateFile(string sourcePath, string targetPath)
        {
            using (CreateCallstackAndLock())
            {
                CheckOpened();

                string sourceArgument = Tools.ArgumentEscape(RemotePath.EscapeFileMask(sourcePath));
                string targetArgument = Tools.ArgumentEscape(targetPath);
                string command = string.Format(CultureInfo.InvariantCulture, "cp \"{0}\" \"{1}\"", sourceArgument, targetArgument);
                WriteCommand(command);

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                {
                    if (!groupReader.TryWaitForNonEmptyElement("cp", LogReadFlags.ThrowFailures))
                    {
                        throw Logger.WriteException(new SessionRemoteException(this, string.Format(CultureInfo.CurrentCulture, "{0} not found.", sourcePath)));
                    }
                    else
                    {
                        using (ElementLogReader cpReader = groupReader.CreateLogReader())
                        {
                            ReadElement(cpReader, 0);
                            groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                        }
                    }
                }
            }
        }

        [Obsolete("Use RemotePath.EscapeFileMask")]
        public string EscapeFileMask(string fileMask)
        {
            return RemotePath.EscapeFileMask(fileMask);
        }

        [Obsolete("Use RemotePath.TranslateRemotePathToLocal")]
        public string TranslateRemotePathToLocal(string remotePath, string remoteRoot, string localRoot)
        {
            return RemotePath.TranslateRemotePathToLocal(remotePath, remoteRoot, localRoot);
        }

        [Obsolete("Use RemotePath.TranslateLocalPathToRemote")]
        public string TranslateLocalPathToRemote(string localPath, string localRoot, string remoteRoot)
        {
            return RemotePath.TranslateLocalPathToRemote(localPath, localRoot, remoteRoot);
        }

        [Obsolete("Use RemotePath.CombinePaths")]
        public string CombinePaths(string path1, string path2)
        {
            return RemotePath.Combine(path1, path2);
        }

        public void AddRawConfiguration(string setting, string value)
        {
            CheckNotOpened();
            RawConfiguration.Add(setting, value);
        }

#if !NETSTANDARD
        [ComRegisterFunction]
        private static void ComRegister(Type t)
        {
            Assembly assembly = Assembly.GetAssembly(t);

            string guid = Marshal.GetTypeLibGuidForAssembly(assembly).ToString();

            int major, minor;
            Marshal.GetTypeLibVersionForAssembly(assembly, out major, out minor);
            string version = $"{major}.{minor}";

            RegistryKey root = Registry.ClassesRoot;
            root.CreateSubKey(GetTypeLibKey(t)).SetValue(null, guid);
            root.CreateSubKey(GetVersionKey(t)).SetValue(null, version);
        }

        [ComUnregisterFunction]
        private static void ComUnregister(Type t)
        {
            RegistryKey root = Registry.ClassesRoot;
            root.DeleteSubKey(GetTypeLibKey(t), false);
            root.DeleteSubKey(GetVersionKey(t), false);
        }

        private static string GetClsidKey(Type t)
        {
            return "CLSID\\" + t.GUID.ToString("B").ToUpperInvariant();
        }

        private static string GetTypeLibKey(Type t)
        {
            return GetClsidKey(t) + "\\TypeLib";
        }

        private static string GetVersionKey(Type t)
        {
            return GetClsidKey(t) + "\\Version";
        }
#endif

        private void ReadFile(RemoteFileInfo fileInfo, CustomLogReader fileReader)
        {
            using (Logger.CreateCallstack())
            {
                if (fileReader.GetEmptyElementValue("type", out string value))
                {
                    fileInfo.FileType = value[0];
                }
                else if (fileReader.GetEmptyElementValue("size", out value))
                {
                    fileInfo.Length = long.Parse(value, CultureInfo.InvariantCulture);
                }
                else if (fileReader.GetEmptyElementValue("modification", out value))
                {
                    fileInfo.LastWriteTime = XmlConvert.ToDateTime(value, XmlDateTimeSerializationMode.Local);
                }
                else if (fileReader.GetEmptyElementValue("permissions", out value))
                {
                    fileInfo.FilePermissions = FilePermissions.CreateReadOnlyFromText(value);
                }
                else if (fileReader.GetEmptyElementValue("owner", out value))
                {
                    fileInfo.Owner = value;
                }
                else if (fileReader.GetEmptyElementValue("group", out value))
                {
                    fileInfo.Group = value;
                }
            }
        }

        internal static string BooleanSwitch(bool flag, string name)
        {
            return flag ? string.Format(CultureInfo.InvariantCulture, "-{0}", name) : null;
        }

        internal static string BooleanSwitch(bool flag, string onName, string offName)
        {
            return flag ? string.Format(CultureInfo.InvariantCulture, "-{0}", onName) : string.Format(CultureInfo.InvariantCulture, "-{0}", offName);
        }

        private void AddSynchronizationTransfer(SynchronizationResult result, TransferEventArgs transfer)
        {
            if (transfer != null)
            {
                if (transfer.Side == ProgressSide.Local)
                {
                    result.AddUpload(transfer);
                }
                else
                {
                    result.AddDownload(transfer);
                }
                RaiseFileTransferredEvent(transfer);
            }
        }

        private static string IncludeTrailingSlash(string path)
        {
            if (!string.IsNullOrEmpty(path) && !path.EndsWith("/", StringComparison.Ordinal))
            {
                path += '/';
            }
            return path;
        }

        private void Cleanup()
        {
            using (Logger.CreateCallstack())
            {
                if (_process != null)
                {
                    Logger.WriteLine("Terminating process");
                    try
                    {
                        try
                        {
                            WriteCommand("exit");
                            _process.Close();
                        }
                        finally
                        {
                            _process.Dispose();
                            _process = null;
                        }
                    }
                    catch (Exception e)
                    {
                        Logger.WriteLine("Process cleanup Exception: {0}", e);
                    }
                }

                Logger.WriteLine("Disposing log readers");

                if (_reader != null)
                {
                    _reader.Dispose();
                    _reader = null;
                }

                if (_logReader != null)
                {
                    _logReader.Dispose();
                    _logReader = null;
                }

                // Cleanup log file
                if ((XmlLogPath != null) && File.Exists(XmlLogPath) && !XmlLogPreserve)
                {
                    Logger.WriteLine("Deleting XML log file [{0}]", XmlLogPath);
                    try
                    {
                        File.Delete(XmlLogPath);
                    }
                    catch (DirectoryNotFoundException e)
                    {
                        Logger.WriteLine("XML log cleanup DirectoryNotFoundException: {0}", e);
                    }
                    catch (IOException e)
                    {
                        Logger.WriteLine("XML log cleanup IOException: {0}", e);
                    }
                    catch (UnauthorizedAccessException e)
                    {
                        Logger.WriteLine("XML log cleanup UnauthorizedAccessException: {0}", e);
                    }

                    _xmlLogPath = null;
                }
            }
        }

        private void WriteCommand(string command)
        {
            WriteCommand(command, command);
        }

        private void WriteCommand(string command, string log)
        {
            Logger.WriteLine("Command: [{0}]", log);
            _process.ExecuteCommand(command, log);
            GotOutput();
        }

        private static void ReadElement(CustomLogReader reader, LogReadFlags flags)
        {
            while (reader.Read(flags))
            {
            }
        }

        private void SessionOptionsToUrlAndSwitches(SessionOptions sessionOptions, bool scanFingerprint, out string command, out string log)
        {
            using (Logger.CreateCallstack())
            {
                if (sessionOptions.Secure)
                {
                    if ((sessionOptions.Protocol != Protocol.Webdav) && (sessionOptions.Protocol != Protocol.S3))
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.Secure is set, but SessionOptions.Protocol is not Protocol.Webdav nor Protocol.S3."));
                    }
                }

                string head;
                switch (sessionOptions.Protocol)
                {
                    case Protocol.Sftp:
                        head = "sftp://";
                        break;

                    case Protocol.Scp:
                        head = "scp://";
                        break;

                    case Protocol.Ftp:
                        head = "ftp://";
                        break;

                    case Protocol.Webdav:
                        if (!sessionOptions.Secure)
                        {
                            head = "dav://";
                        }
                        else
                        {
                            head = "davs://";
                        }
                        break;

                    case Protocol.S3:
                        if (!sessionOptions.Secure)
                        {
                            head = "s3plain://";
                        }
                        else
                        {
                            head = "s3://";
                        }
                        break;

                    default:
                        throw Logger.WriteException(new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not supported", sessionOptions.Protocol)));
                }

                bool hasUsername;
                if (!scanFingerprint)
                {
                    hasUsername = !string.IsNullOrEmpty(sessionOptions.UserName);
                    if (hasUsername)
                    {
                        head += UriEscape(sessionOptions.UserName);
                    }
                }
                else
                {
                    hasUsername = false;
                }

                string url = head;
                string logUrl = head;

                if ((sessionOptions.SecurePassword != null) && !scanFingerprint)
                {
                    if (!hasUsername)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.Password is set, but SessionOptions.UserName is not."));
                    }
                    url += ":" + UriEscape(sessionOptions.Password);
                    logUrl += ":***";
                }

                string tail = string.Empty;

                if (hasUsername)
                {
                    tail += "@";
                }

                if (string.IsNullOrEmpty(sessionOptions.HostName))
                {
                    throw Logger.WriteException(new ArgumentException("SessionOptions.HostName is not set."));
                }

                // We should wrap IPv6 literals to square brackets, instead of URL-encoding them,
                // but it works anyway...
                tail += UriEscape(sessionOptions.HostName);

                if (sessionOptions.PortNumber != 0)
                {
                    tail += ":" + sessionOptions.PortNumber.ToString(CultureInfo.InvariantCulture);
                }

                if (!string.IsNullOrEmpty(sessionOptions.RootPath) && !scanFingerprint)
                {
                    if ((sessionOptions.Protocol != Protocol.Webdav) && (sessionOptions.Protocol != Protocol.S3))
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.RootPath is set, but SessionOptions.Protocol is not Protocol.Webdav nor Protocol.S3."));
                    }

                    tail += sessionOptions.RootPath;
                }

                url += tail;
                logUrl += tail;

                SessionOptionsToSwitches(sessionOptions, scanFingerprint, out string arguments, out string logArguments);

                const string switchName = "-rawsettings";
                Tools.AddRawParameters(ref arguments, sessionOptions.RawSettings, switchName, false);
                Tools.AddRawParameters(ref logArguments, sessionOptions.RawSettings, switchName, false);

                if (!string.IsNullOrEmpty(arguments))
                {
                    arguments = " " + arguments;
                    logArguments = " " + logArguments;
                }

                // Switches should (and particularly the -rawsettings MUST) come after the URL
                command = "\"" + Tools.ArgumentEscape(url) + "\"" + arguments;
                log = "\"" + Tools.ArgumentEscape(logUrl) + "\"" + logArguments;
            }
        }

        private void SessionOptionsToSwitches(SessionOptions sessionOptions, bool scanFingerprint, out string arguments, out string logArguments)
        {
            using (Logger.CreateCallstack())
            {
                List<string> switches = new List<string>();

                if (!string.IsNullOrEmpty(sessionOptions.SshHostKeyFingerprint) ||
                    ((sessionOptions.SshHostKeyPolicy != SshHostKeyPolicy.Check) && !scanFingerprint))
                {
                    if (!sessionOptions.IsSsh)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.SshHostKeyFingerprint is set or sessionOptions.SshHostKeyPolicy has not the default value Check, but SessionOptions.Protocol is neither Protocol.Sftp nor Protocol.Scp."));
                    }
                    string sshHostKeyFingerprint = sessionOptions.SshHostKeyFingerprint;
                    switch (sessionOptions.SshHostKeyPolicy)
                    {
                        case SshHostKeyPolicy.Check:
                            // noop
                            break;
                        case SshHostKeyPolicy.GiveUpSecurityAndAcceptAny:
                            sshHostKeyFingerprint = AddStarToList(sshHostKeyFingerprint);
                            Logger.WriteLine("WARNING! Giving up security and accepting any key as configured");
                            break;
                        case SshHostKeyPolicy.AcceptNew:
                            if (!string.IsNullOrEmpty(sshHostKeyFingerprint))
                            {
                                throw Logger.WriteException(new ArgumentException("SessionOptions.SshHostKeyFingerprint is set and SshHostKeyPolicy is not Check"));
                            }
                            sshHostKeyFingerprint = "acceptnew";
                            break;
                    }
                    switches.Add(FormatSwitch("hostkey", sshHostKeyFingerprint));
                }
                else
                {
                    if (sessionOptions.IsSsh && DefaultConfigurationInternal && !scanFingerprint)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.Protocol is Protocol.Sftp or Protocol.Scp, but SessionOptions.SshHostKeyFingerprint is not set."));
                    }
                }

                bool hasSshPrivateKeyPath = !string.IsNullOrEmpty(sessionOptions.SshPrivateKeyPath);
                bool hasSshPrivateKey = !string.IsNullOrEmpty(sessionOptions.SshPrivateKey);
                if ((hasSshPrivateKeyPath || hasSshPrivateKey) && !scanFingerprint)
                {
                    if (!sessionOptions.IsSsh)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.SshPrivateKeyPath or SessionOptions.SshPrivateKey is set, but SessionOptions.Protocol is neither Protocol.Sftp nor Protocol.Scp."));
                    }
                    if (hasSshPrivateKeyPath && hasSshPrivateKey)
                    {
                        throw Logger.WriteException(new ArgumentException("Set only one of SessionOptions.SshPrivateKeyPath or SessionOptions.SshPrivateKey."));
                    }
                    string privateKey;
                    if (hasSshPrivateKeyPath)
                    {
                        privateKey = sessionOptions.SshPrivateKeyPath;
                    }
                    else
                    {
                        privateKey = "@" + GenerateInMemoryPrivateKey(sessionOptions);
                    }
                    switches.Add(FormatSwitch("privatekey", privateKey));
                }

                if (!string.IsNullOrEmpty(sessionOptions.TlsClientCertificatePath) && !scanFingerprint)
                {
                    if (!sessionOptions.IsTls)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.TlsClientCertificatePath is set, but neither SessionOptions.FtpSecure nor SessionOptions.Secure is enabled."));
                    }
                    switches.Add(FormatSwitch("clientcert", sessionOptions.TlsClientCertificatePath));
                }

                if (sessionOptions.FtpSecure != FtpSecure.None)
                {
                    if (sessionOptions.Protocol != Protocol.Ftp)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.FtpSecure is not FtpSecure.None, but SessionOptions.Protocol is not Protocol.Ftp."));
                    }

                    switch (sessionOptions.FtpSecure)
                    {
                        case FtpSecure.Implicit:
                            switches.Add(FormatSwitch("implicit"));
                            break;

                        case FtpSecure.Explicit:
                            switches.Add(FormatSwitch("explicit"));
                            break;

                        default:
                            throw Logger.WriteException(new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not supported", sessionOptions.FtpSecure)));
                    }
                }

                if ((!string.IsNullOrEmpty(sessionOptions.TlsHostCertificateFingerprint) ||
                     sessionOptions.GiveUpSecurityAndAcceptAnyTlsHostCertificate) &&
                    !scanFingerprint)
                {
                    if (!sessionOptions.IsTls)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.TlsHostCertificateFingerprint or SessionOptions.GiveUpSecurityAndAcceptAnyTlsHostCertificate is set, but neither SessionOptions.FtpSecure nor SessionOptions.Secure is enabled."));
                    }
                    string tlsHostCertificateFingerprint = sessionOptions.TlsHostCertificateFingerprint;
                    if (sessionOptions.GiveUpSecurityAndAcceptAnyTlsHostCertificate)
                    {
                        Logger.WriteLine("WARNING! Giving up security and accepting any certificate as configured");
                        tlsHostCertificateFingerprint = AddStarToList(tlsHostCertificateFingerprint);
                    }
                    switches.Add(FormatSwitch("certificate", tlsHostCertificateFingerprint));
                }

                if ((sessionOptions.Protocol == Protocol.Ftp) && !scanFingerprint)
                {
                    switches.Add(FormatSwitch("passive", (sessionOptions.FtpMode == FtpMode.Passive)));
                }

                switches.Add(FormatSwitch("timeout", (int)sessionOptions.Timeout.TotalSeconds));

                List<string> logSwitches = new List<string>(switches);

                if ((sessionOptions.SecurePrivateKeyPassphrase != null) && !scanFingerprint)
                {
                    if (string.IsNullOrEmpty(sessionOptions.SshPrivateKeyPath) &&
                        string.IsNullOrEmpty(sessionOptions.SshPrivateKey) &&
                        string.IsNullOrEmpty(sessionOptions.TlsClientCertificatePath))
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.PrivateKeyPassphrase is set, but neither SessionOptions.SshPrivateKeyPath, SessionOptions.SshPrivateKey nor SessionOptions.TlsClientCertificatePath is set."));
                    }
                    switches.Add(FormatSwitch("passphrase", sessionOptions.PrivateKeyPassphrase));
                    logSwitches.Add(FormatSwitch("passphrase", "***"));
                }

                if ((sessionOptions.SecureNewPassword != null) && !scanFingerprint)
                {
                    if (sessionOptions.SecurePassword == null)
                    {
                        throw Logger.WriteException(new ArgumentException("SessionOptions.SecureNewPassword is set, but SessionOptions.SecurePassword is not."));
                    }
                    switches.Add(FormatSwitch("newpassword", sessionOptions.NewPassword));
                    logSwitches.Add(FormatSwitch("newpassword", "***"));
                }

                arguments = string.Join(" ", switches.ToArray());
                logArguments = string.Join(" ", logSwitches.ToArray());
            }
        }

        internal static string GenerateInMemoryPrivateKey(SessionOptions sessionOptions)
        {
            byte[] bytes = Encoding.Default.GetBytes(sessionOptions.SshPrivateKey);
            return BitConverter.ToString(bytes).Replace("-", "");
        }

        private static string AddStarToList(string list)
        {
            return (string.IsNullOrEmpty(list) ? string.Empty : list + ";") + "*";
        }

        private RemoteFileInfo DoGetFileInfo(string path)
        {
            using (Logger.CreateCallstack())
            {
                WriteCommand(string.Format(CultureInfo.InvariantCulture, "stat -- \"{0}\"", Tools.ArgumentEscape(path)));

                RemoteFileInfo fileInfo = new RemoteFileInfo();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader statReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("stat", LogReadFlags.ThrowFailures))
                {
                    while (statReader.Read(0))
                    {
                        if (statReader.GetEmptyElementValue("filename", out string value))
                        {
                            string name = value;
                            int p = name.LastIndexOf('/');
                            if (p >= 0)
                            {
                                name = name.Substring(p + 1);
                            }
                            fileInfo.Name = name;
                            fileInfo.FullName = value;
                        }
                        else if (statReader.IsNonEmptyElement("file"))
                        {
                            using (ElementLogReader fileReader = statReader.CreateLogReader())
                            {
                                while (fileReader.Read(0))
                                {
                                    ReadFile(fileInfo, fileReader);
                                }
                            }
                        }
                    }

                    groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                }

                return fileInfo;
            }
        }

        internal static string FormatSwitch(string key)
        {
            return string.Format(CultureInfo.InvariantCulture, "-{0}", key);
        }

        internal static string FormatSwitch(string key, string value)
        {
            return string.Format(CultureInfo.InvariantCulture, "-{0}=\"{1}\"", key, Tools.ArgumentEscape(value));
        }

        internal static string FormatSwitch(string key, int value)
        {
            return string.Format(CultureInfo.InvariantCulture, "-{0}={1}", key, value.ToString(CultureInfo.InvariantCulture));
        }

        internal static string FormatSwitch(string key, bool value)
        {
            return FormatSwitch(key, (value ? 1 : 0));
        }

        private static string UriEscape(string s)
        {
            return Uri.EscapeDataString(s);
        }

        internal void GotOutput()
        {
            _lastOutput = DateTime.Now;
        }

        private void ProcessOutputDataReceived(object sender, OutputDataReceivedEventArgs e)
        {
            if (e == null)
            {
                Logger.WriteLine("Got incomplete progress output");
            }
            else
            {
                Logger.WriteLine("Scheduling output: [{0}]", e.Data);
                string s = e.Data.TrimEnd(new[] { '\r' });

                lock (Output)
                {
                    Output.InternalAdd(s);
                    if (Output.Count > 1000)
                    {
                        Output.InternalRemoveFirst();
                    }
                    if (e.Error)
                    {
                        _error.InternalAdd(s);
                        if (_error.Count > 1000)
                        {
                            _error.InternalRemoveFirst();
                        }
                    }
                }

                ScheduleEvent(() => RaiseOutputDataReceived(e.Data, e.Error));
            }

            GotOutput();
        }

        private void ScheduleEvent(Action action)
        {
            lock (_events)
            {
                _events.Add(action);
                _eventsEvent.Set();
            }
        }

        internal void CheckForTimeout(string additional = null)
        {
            TimeSpan timeout = Timeout;
            if (timeout < _sessionTimeout)
            {
                timeout = _sessionTimeout + TimeSpan.FromSeconds(1);
            }

            if (DateTime.Now - _lastOutput > timeout)
            {
                Logger.WriteLine("Timeout waiting for WinSCP to respond - asking for callstack");
                _process.RequestCallstack();
                string message = "Timeout waiting for WinSCP to respond";
                if (additional != null)
                {
                    message += " - " + additional;
                }
                _logReader?.SetTimeouted();
                Cleanup();
                throw Logger.WriteException(new TimeoutException(message));
            }

            if (_aborted)
            {
                throw Logger.WriteException(new SessionLocalException(this, "Aborted."));
            }

            if (_throwStdOut && (_process.StdOut != null) && _process.StdOut.ReadAvailable(1))
            {
                // This is here to return from GetFile asap (?)
                // Not logging the exception, as it's not really an exception.
                Logger.WriteLine("Got data");
                throw new StdOutException();
            }
        }

        private void RaiseFileTransferredEvent(TransferEventArgs args)
        {
            Logger.WriteLine("FileTransferredEvent: [{0}]", args.FileName);

            FileTransferred?.Invoke(this, args);
        }

        internal void RaiseFailed(SessionRemoteException e)
        {
            Logger.WriteLine("Failed: [{0}]", e);

            if ((Failed != null) && !_ignoreFailed)
            {
                Failed(this, new FailedEventArgs { Error = e });
            }

            foreach (OperationResultBase operationResult in _operationResults)
            {
                operationResult.AddFailure(e);
            }
        }

        private void CheckNotDisposed()
        {
            if (_disposed)
            {
                throw Logger.WriteException(new InvalidOperationException("Object is disposed"));
            }

            if (_aborted)
            {
                throw Logger.WriteException(new InvalidOperationException("Session was aborted"));
            }
        }

        private void CheckOpened()
        {
            if (!Opened)
            {
                throw Logger.WriteException(new InvalidOperationException("Session is not opened"));
            }
        }

        private void CheckNotOpened()
        {
            if (Opened)
            {
                throw Logger.WriteException(new InvalidOperationException("Session is already opened"));
            }
        }

        private void RaiseOutputDataReceived(string data, bool error)
        {
            Logger.WriteLine("Output: [{0}]", data);

            OutputDataReceived?.Invoke(this, new OutputDataReceivedEventArgs(data, error));
        }

        internal void DispatchEvents(int interval)
        {
            DateTime start = DateTime.Now;
            while (_eventsEvent.WaitOne(interval, false))
            {
                lock (_events)
                {
                    foreach (Action action in _events)
                    {
                        action();
                    }
                    _events.Clear();
                }

                interval -= (int) (DateTime.Now - start).TotalMilliseconds;
                if (interval < 0)
                {
                    break;
                }
                start = DateTime.Now;
            }
        }

        private IDisposable RegisterOperationResult(OperationResultBase operationResult)
        {
            _operationResults.Add(operationResult);
            return new OperationResultGuard(this, operationResult);
        }

        internal void UnregisterOperationResult(OperationResultBase operationResult)
        {
            // GetFile relies on this not to throw
            _operationResults.Remove(operationResult);
        }

        internal bool WantsProgress
        {
            get
            {
                return (_fileTransferProgress != null);
            }
        }

        private IDisposable CreateProgressHandler()
        {
            using (Logger.CreateCallstack())
            {
                _progressHandling++;
                return new ProgressHandler(this);
            }
        }

        internal void DisableProgressHandling()
        {
            using (Logger.CreateCallstack())
            {
                if (_progressHandling <= 0)
                {
                    throw Logger.WriteException(new InvalidOperationException("Progress handling not enabled"));
                }

                // make sure we process all pending progress events
                DispatchEvents(0);

                _progressHandling--;
            }
        }

        internal void ProcessChoice(QueryReceivedEventArgs args)
        {
            if (_queryReceived != null) // optimization
            {
                _choiceEvent.Reset();
                ScheduleEvent(() => Choice(args));
                _choiceEvent.WaitOne();
            }
        }

        private void Choice(QueryReceivedEventArgs args)
        {
            _queryReceived?.Invoke(this, args);
            _choiceEvent.Set();
        }

        internal void ProcessProgress(FileTransferProgressEventArgs args)
        {
            ScheduleEvent(() => Progress(args));
        }

        private void Progress(FileTransferProgressEventArgs args)
        {
            if ((_progressHandling >= 0) && WantsProgress)
            {
                _fileTransferProgress(this, args);

                if (args.Cancel)
                {
                    _process.Cancel();
                }
            }
        }

        private void SetupTempPath()
        {
            using (Logger.CreateCallstack())
            {
                if (!string.IsNullOrEmpty(_xmlLogPath))
                {
                    bool exists = File.Exists(_xmlLogPath);
                    Logger.WriteLine("Configured temporary file: {0} - Exists [{1}]", _xmlLogPath, exists);
                    if (exists)
                    {
                        throw Logger.WriteException(new SessionLocalException(this, string.Format(CultureInfo.CurrentCulture, "Configured temporary file {0} already exists", _xmlLogPath)));
                    }
                }
                else
                {
                    string path = Path.GetTempPath();
                    Logger.WriteLine("Temporary folder: {0}", path);
                    string process = Process.GetCurrentProcess().Id.ToString("X4", CultureInfo.InvariantCulture);
                    string instance = GetHashCode().ToString("X8", CultureInfo.InvariantCulture);
                    string filename;
                    bool exists;
                    do
                    {
                        string uniqueStr = (_logUnique > 0 ? "." + _logUnique.ToString(CultureInfo.InvariantCulture) : string.Empty);
                        ++_logUnique;
                        filename = Path.Combine(path, "wscp" + process + "." + instance + uniqueStr + ".tmp");
                        exists = File.Exists(filename);
                        Logger.WriteLine("Temporary file [{0}] - Exists [{1}]", filename, exists);
                    }
                    while (exists);

                    _xmlLogPath = filename;
                }
            }
        }

        private string GetExecutablePath()
        {
            string result;
            if (_process != null)
            {
                result = _process.ExecutablePath;
            }
            else
            {
                // Same as ExeSessionProcess.GetExecutablePath,
                // except that it does not throw when user-provided "executable path" does not exist

                if (!string.IsNullOrEmpty(_executablePath))
                {
                    result = _executablePath;
                }
                else
                {
                    result = ExeSessionProcess.FindExecutable(this);
                }
            }
            return result;
        }

        private void SetSessionLogPath(string value)
        {
            CheckNotOpened();
            const string XmlExtension = ".xml";
            if (Path.GetExtension(value).Equals(XmlExtension, StringComparison.OrdinalIgnoreCase))
            {
                throw Logger.WriteException(new ArgumentException($"Session log cannot have {XmlExtension} extension"));
            }
            _sessionLogPath = value;
        }

        FieldInfo IReflect.GetField(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                FieldInfo result = GetType().GetField(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        FieldInfo[] IReflect.GetFields(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                FieldInfo[] result = GetType().GetFields(bindingAttr);
                Logger.WriteLine("Fields [{0}]", result.Length);
                return result;
            }
        }

        MemberInfo[] IReflect.GetMember(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                MemberInfo[] result = GetType().GetMember(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        MemberInfo[] IReflect.GetMembers(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                MemberInfo[] result = GetType().GetMembers(bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        MethodInfo IReflect.GetMethod(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                MethodInfo result = GetType().GetMethod(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        MethodInfo IReflect.GetMethod(string name, BindingFlags bindingAttr, Binder binder, Type[] types, ParameterModifier[] modifiers)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                MethodInfo result = GetType().GetMethod(name, bindingAttr, binder, types, modifiers);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        MethodInfo[] IReflect.GetMethods(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                MethodInfo[] result = GetType().GetMethods(bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        PropertyInfo[] IReflect.GetProperties(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                PropertyInfo[] result = GetType().GetProperties(bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        PropertyInfo IReflect.GetProperty(string name, BindingFlags bindingAttr, Binder binder, Type returnType, Type[] types, ParameterModifier[] modifiers)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                PropertyInfo result = GetType().GetProperty(name, bindingAttr, binder, returnType, types, modifiers);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        PropertyInfo IReflect.GetProperty(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                PropertyInfo result = GetType().GetProperty(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        private void LogInvokeMember(string name, BindingFlags invokeAttr, Binder binder, object target, object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] namedParameters)
        {
            Logger.WriteLine("Name [{0}]", name);
            Logger.WriteLine("BindingFlags [{0}]", invokeAttr);
            Logger.WriteLine("Binder [{0}]", binder);
            Logger.WriteLine("Target [{0}]", target);
            if (args != null)
            {
                Logger.WriteLine("Args [{0}] [{1}]", args.Length, modifiers != null ? modifiers.Length.ToString(CultureInfo.InvariantCulture) : "null");
                for (int i = 0; i < args.Length; ++i)
                {
                    Logger.WriteLine("Arg [{0}] [{1}] [{1}] [{2}]", i, args[i], (args[i] != null ? args[i].GetType().ToString() : "null"), (modifiers != null ? modifiers[i].ToString() : "null"));
                }
            }
            Logger.WriteLine("Culture [{0}]", culture);
            if (namedParameters != null)
            {
                foreach (string namedParameter in namedParameters)
                {
                    Logger.WriteLine("NamedParameter [{0}]", namedParameter);
                }
            }
        }

        object IReflect.InvokeMember(string name, BindingFlags invokeAttr, Binder binder, object target, object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] namedParameters)
        {
            using (Logger.CreateCallstack())
            {
                object result;

                try
                {
                    bool wasLogging = Logger.Logging;
                    if (wasLogging)
                    {
                        // factored out to reduce this method complexity
                        LogInvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
                    }

                    if (target == null)
                    {
                        throw Logger.WriteException(new ArgumentNullException(nameof(target)));
                    }

                    Type type = target.GetType();

                    // RuntimeType.InvokeMember below calls into Binder.BindToMethod (Binder is OleAutBinder)
                    // that fails to match method, if integer value is provided to enum argument
                    // (SynchronizeDirectories with its SynchronizationMode and SynchronizationCriteria).
                    // This does not happen if we do not implement IReflect, though no idea why.
                    // So as a workaround we check, if the method has no overloads (what is always true for Session),
                    // and call the only instance directly.
                    // Calling MethodInfo.Invoke with int values for enum arguments works.
                    // Only as a fallback, we call InvokeMember (what is currently actually used only when
                    // the method with given name does not exist at all)

                    MethodInfo method = null;
                    PropertyInfo property = null;

                    // would be way too difficult to implement the below involving named arguments
                    if (namedParameters == null)
                    {
                        try
                        {
                            BindingFlags bindingFlags = invokeAttr | BindingFlags.Instance | BindingFlags.Public;
                            method = type.GetMethod(name, bindingFlags);

                            if (args == null)
                            {
                                throw Logger.WriteException(new ArgumentNullException(nameof(args)));
                            }

                            if (method != null)
                            {
                                // MethodInfo.Invoke does not fill-in optional arguments (contrary to RuntimeType.InvokeMember)
                                ParameterInfo[] parameters = method.GetParameters();
                                if (args.Length < parameters.Length)
                                {
                                    Logger.WriteLine("Provided less parameters [{0}] than defined [{1}]", args.Length, parameters.Length);
                                    object[] args2 = new object[parameters.Length];

                                    for (int i = 0; i < parameters.Length; i++)
                                    {
                                        if (i < args.Length)
                                        {
                                            args2[i] = args[i];
                                        }
                                        else
                                        {
                                            if (!parameters[i].IsOptional)
                                            {
                                                Logger.WriteLine("Parameter [{0}] of [{1}] is not optional", i, method);
                                                args2 = null;
                                                break;
                                            }
                                            else
                                            {
                                                Logger.WriteLine("Adding default value [{0}] for optional parameter [{1}]", parameters[i].DefaultValue, i);
                                                args2[i] = parameters[i].DefaultValue;
                                            }
                                        }
                                    }

                                    if (args2 != null)
                                    {
                                        args = args2;
                                    }
                                }
                            }
                            else if (args.Length == 1) // sanity check
                            {
                                property = type.GetProperty(name, bindingFlags);
                            }
                        }
                        catch (AmbiguousMatchException e)
                        {
                            Logger.WriteLine("Unexpected ambiguous match [{0}]", e.Message);
                        }
                    }

                    if (method != null)
                    {
                        Logger.WriteLine("Invoking unambiguous method [{0}]", method);
                        result = method.Invoke(target, invokeAttr, binder, args, culture);
                    }
                    else if (property != null)
                    {
                        Logger.WriteLine("Setting unambiguous property [{0}]", property);
                        property.SetValue(target, args[0], invokeAttr, binder, null, culture);
                        result = null;
                    }
                    else
                    {
                        Logger.WriteLine("Invoking ambiguous/non-existing method 2 [{0}]", name);
                        result = type.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
                    }

                    if (!wasLogging && Logger.Logging)
                    {
                        Logger.WriteLine("Invoking member {0} enabled debug logging", name);
                        // Might call LogInvokeMember here, but it's not really useful.
                    }
                    Logger.WriteLine("Result [{0}] [{1}]", result, (result != null ? result.GetType().ToString() : "null"));
                }
                catch (Exception e)
                {
                    Logger.WriteLine("Error [{0}]", e);
                    throw;
                }
                return result;
            }
        }

        Type IReflect.UnderlyingSystemType
        {
            get { return GetType(); }
        }

        internal const string Namespace = "http://winscp.net/schema/session/1.0";
        internal Logger Logger { get; private set; }
        internal bool GuardProcessWithJobInternal { get { return _guardProcessWithJob; } set { CheckNotOpened(); _guardProcessWithJob = value; } }
        internal bool TestHandlesClosedInternal { get; set; }
        internal Dictionary<string, string> RawConfiguration { get; private set; }
        internal bool DefaultConfigurationInternal { get { return _defaultConfiguration; } }
        internal string IniFilePathInternal { get { return _iniFilePath; } }

        private ExeSessionProcess _process;
        private DateTime _lastOutput;
        private ElementLogReader _reader;
        private SessionLogReader _logReader;
        private readonly IList<OperationResultBase> _operationResults;
        private readonly IList<Action> _events;
        private AutoResetEvent _eventsEvent;
        private ManualResetEvent _choiceEvent;
        private bool _disposed;
        private string _executablePath;
        private string _additionalExecutableArguments;
        private bool _defaultConfiguration;
        private bool _disableVersionCheck;
        private string _iniFilePath;
        private TimeSpan _reconnectTime;
        private string _sessionLogPath;
        private bool _aborted;
        private int _logUnique;
        private string _xmlLogPath;
        private FileTransferProgressEventHandler _fileTransferProgress;
        private int _progressHandling;
        private bool _guardProcessWithJob;
        private string _homePath;
#if !NETSTANDARD
        private string _executableProcessUserName;
        private SecureString _executableProcessPassword;
#endif
        private StringCollection _error;
        private bool _ignoreFailed;
        private TimeSpan _sessionTimeout;
        private QueryReceivedEventHandler _queryReceived;
        private bool _throwStdOut;
    }
}
