using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Threading;
using System.Xml;
using Microsoft.Win32;
using System.Diagnostics;

namespace WinSCP
{
    [Guid("38649D44-B839-4F2C-A9DC-5D45EEA4B5E9")]
    [ComVisible(true)]
    public enum SynchronizationMode
    {
        Local = 0,
        Remote = 1,
        Both = 2,
    };

    [Guid("3F770EC1-35F5-4A7B-A000-46A2F7A213D8")]
    [ComVisible(true)]
    [Flags]
    public enum SynchronizationCriteria
    {
        None = 0x00,
        Time = 0x01,
        Size = 0x02,
        Either = Time | Size,
    };

    public delegate void OutputDataReceivedEventHandler(object sender, OutputDataReceivedEventArgs e);
    public delegate void FileTransferredEventHandler(object sender, TransferEventArgs e);
    public delegate void FileTransferProgressEventHandler(object sender, FileTransferProgressEventArgs e);
    public delegate void FailedEventHandler(object sender, FailedEventArgs e);

    [Guid("56FFC5CE-3867-4EF0-A3B5-CFFBEB99EA35")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComSourceInterfaces(typeof(ISessionEvents))]
    public sealed class Session : IDisposable, IReflect
    {
        public string ExecutablePath { get { return _executablePath; } set { CheckNotOpened(); _executablePath = value; } }
        public string AdditionalExecutableArguments { get { return _additionalExecutableArguments; } set { CheckNotOpened(); _additionalExecutableArguments = value; } }
        public bool DefaultConfiguration { get { return _defaultConfiguration; } set { CheckNotOpened(); _defaultConfiguration = value; } }
        public bool DisableVersionCheck { get { return _disableVersionCheck; } set { CheckNotOpened(); _disableVersionCheck = value; } }
        public string IniFilePath { get { return _iniFilePath; } set { CheckNotOpened(); _iniFilePath = value; } }
        public TimeSpan ReconnectTime { get { return _reconnectTime; } set { CheckNotOpened(); _reconnectTime = value; } }
        public string DebugLogPath { get { CheckNotDisposed(); return Logger.LogPath; } set { CheckNotDisposed(); Logger.LogPath = value; } }
        public string SessionLogPath { get { return _sessionLogPath; } set { CheckNotOpened(); _sessionLogPath = value; } }
        public string XmlLogPath { get { return _xmlLogPath; } set { CheckNotOpened(); _xmlLogPath = value; } }
        #if DEBUG
        public bool GuardProcessWithJob { get { return _guardProcessWithJob; } set { CheckNotOpened(); _guardProcessWithJob = value; } }
        public bool TestHandlesClosed { get { return TestHandlesClosedInternal; } set { TestHandlesClosedInternal = value; } }
        #endif

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
                using (Logger.CreateCallstackAndLock())
                {
                    CheckNotOpened();
                    _fileTransferProgress += value;
                }
            }

            remove
            {
                using (Logger.CreateCallstackAndLock())
                {
                    CheckNotOpened();
                    _fileTransferProgress -= value;
                }
            }
        }

        public Session()
        {
            Logger = new Logger();

            using (Logger.CreateCallstackAndLock())
            {
                Timeout = new TimeSpan(0, 1, 0);
                _reconnectTime = TimeSpan.MaxValue;
                Output = new StringCollection();
                _operationResults = new List<OperationResultBase>();
                _events = new List<Action>();
                _eventsEvent = new AutoResetEvent(false);
                _disposed = false;
                _defaultConfiguration = true;
                _logUnique = 0;
                _guardProcessWithJob = true;
            }
        }

        public void Dispose()
        {
            using (Logger.CreateCallstackAndLock())
            {
                _disposed = true;

                Cleanup();
                Logger.Dispose();

                if (_eventsEvent != null)
                {
                    _eventsEvent.Close();
                    _eventsEvent = null;
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
                if (_process != null)
                {
                    _process.Abort();
                }
            }
        }

        public void Open(SessionOptions sessionOptions)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckNotDisposed();

                if (Opened)
                {
                    throw new InvalidOperationException("Session is already opened");
                }

                try
                {
                    SetupTempPath();

                    _process = new ExeSessionProcess(this);

                    _process.OutputDataReceived += ProcessOutputDataReceived;

                    _process.Start();

                    GotOutput();

                    // setup batch mode
                    WriteCommand("option batch on");
                    WriteCommand("option confirm off");

                    if (ReconnectTime != TimeSpan.MaxValue)
                    {
                        WriteCommand(string.Format(CultureInfo.InvariantCulture, "option reconnecttime {0}", (int)ReconnectTime.TotalSeconds));
                    }

                    WriteCommand("open " + SessionOptionsToOpenArguments(sessionOptions));

                    string logExplanation =
                        string.Format(CultureInfo.CurrentCulture,
                            "(response log file {0} was not created). This could indicate lack of write permissions to the log folder or problems starting WinSCP itself.",
                            XmlLogPath);

                    // Wait until the log file gets created or WinSCP terminates (in case of fatal error)
                    do
                    {
                        if (_process.HasExited && !File.Exists(XmlLogPath))
                        {
                            string[] output = new string[Output.Count];
                            Output.CopyTo(output, 0);
                            Logger.WriteCounters();
                            Logger.WriteProcesses();
                            _process.WriteStatus();
                            throw new SessionLocalException(this,
                                string.Format(CultureInfo.CurrentCulture,
                                    "WinSCP process terminated with exit code {0} and output \"{1}\", without responding {2}",
                                    _process.ExitCode, string.Join(Environment.NewLine, output), logExplanation));
                        }

                        Thread.Sleep(50);

                        CheckForTimeout(
                            string.Format(CultureInfo.CurrentCulture,
                                "WinSCP has not responded in time {0}",
                                logExplanation));

                    } while (!File.Exists(XmlLogPath));

                    _logReader = new SessionLogReader(this);

                    _reader = _logReader.WaitForNonEmptyElementAndCreateLogReader("session", LogReadFlags.ThrowFailures);

                    using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                    {
                        ReadElement(groupReader, LogReadFlags.ThrowFailures);
                    }
                }
                catch(Exception e)
                {
                    Logger.WriteLine("Exception: {0}", e);
                    Cleanup();
                    throw;
                }
            }
        }

        public RemoteDirectoryInfo ListDirectory(string path)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "ls -- \"{0}\"", ArgumentEscape(IncludeTrailingSlash(path))));

                RemoteDirectoryInfo result = new RemoteDirectoryInfo();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader lsReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("ls", LogReadFlags.ThrowFailures))
                {
                    if (lsReader.TryWaitForNonEmptyElement("files", 0))
                    {
                        using (ElementLogReader filesReader = lsReader.CreateLogReader())
                        {
                            while (filesReader.TryWaitForNonEmptyElement("file", 0))
                            {
                                RemoteFileInfo fileInfo = new RemoteFileInfo();

                                using (ElementLogReader fileReader = filesReader.CreateLogReader())
                                {
                                    while (fileReader.Read(0))
                                    {
                                        string value;
                                        if (fileReader.GetEmptyElementValue("filename", out value))
                                        {
                                            fileInfo.Name = value;
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

                        groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                    }
                    else
                    {
                        // "files" not found, keep reading, we expect "failure"
                        groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                        // only if not "failure", throw "files" not found
                        throw SessionLocalException.CreateElementNotFound(this, "files");
                    }
                }

                return result;
            }
        }

        public TransferOperationResult PutFiles(string localPath, string remotePath, bool remove = false, TransferOptions options = null)
        {
            using (Logger.CreateCallstackAndLock())
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
                        ArgumentEscape(localPath), ArgumentEscape(remotePath)));

                TransferOperationResult result = new TransferOperationResult();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (RegisterOperationResult(result))
                using (CreateProgressHandler())
                {
                    TransferEventArgs args = null;

                    while (groupReader.Read(0))
                    {
                        if (groupReader.IsNonEmptyElement(TransferEventArgs.UploadTag))
                        {
                            if (args != null)
                            {
                                result.AddTransfer(args);
                                RaiseFileTransferredEvent(args);
                            }
                            args = TransferEventArgs.Read(groupReader);
                        }
                        else if (groupReader.IsNonEmptyElement(ChmodEventArgs.Tag))
                        {
                            if (args == null)
                            {
                                throw new InvalidOperationException();
                            }
                            args.Chmod = ChmodEventArgs.Read(groupReader);
                        }
                        else if (groupReader.IsNonEmptyElement(TouchEventArgs.Tag))
                        {
                            if (args == null)
                            {
                                throw new InvalidOperationException();
                            }
                            args.Touch = TouchEventArgs.Read(groupReader);
                        }
                    }

                    if (args != null)
                    {
                        result.AddTransfer(args);
                        RaiseFileTransferredEvent(args);
                    }
                }

                return result;
            }
        }

        public TransferOperationResult GetFiles(string remotePath, string localPath, bool remove = false, TransferOptions options = null)
        {
            using (Logger.CreateCallstackAndLock())
            {
                if (options == null)
                {
                    options = new TransferOptions();
                }

                CheckOpened();

                WriteCommand(
                    string.Format(CultureInfo.InvariantCulture, "get {0} {1} -- \"{2}\" \"{3}\"",
                        BooleanSwitch(remove, "delete"), options.ToSwitches(),
                        ArgumentEscape(remotePath), ArgumentEscape(localPath)));

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
                            if (args != null)
                            {
                                result.AddTransfer(args);
                                RaiseFileTransferredEvent(args);
                            }
                            args = TransferEventArgs.Read(groupReader);
                        }
                        else if (groupReader.IsNonEmptyElement(RemovalEventArgs.Tag))
                        {
                            if (args == null)
                            {
                                throw new InvalidOperationException();
                            }
                            args.Removal = RemovalEventArgs.Read(groupReader);
                        }
                    }

                    if (args != null)
                    {
                        result.AddTransfer(args);
                        RaiseFileTransferredEvent(args);
                    }
                }

                return result;
            }
        }

        public RemovalOperationResult RemoveFiles(string path)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "rm -- \"{0}\"", ArgumentEscape(path)));

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

        public SynchronizationResult SynchronizeDirectories(
            SynchronizationMode mode, string localPath, string remotePath,
            bool removeFiles, bool mirror = false, SynchronizationCriteria criteria = SynchronizationCriteria.Time,
            TransferOptions options = null)
        {
            using (Logger.CreateCallstackAndLock())
            {
                if (options == null)
                {
                    options = new TransferOptions();
                }

                CheckOpened();

                if (removeFiles && (mode == SynchronizationMode.Both))
                {
                    throw new ArgumentException("Cannot delete files in synchronization mode Both");
                }

                if (mirror && (mode == SynchronizationMode.Both))
                {
                    throw new ArgumentException("Cannot mirror files in synchronization mode Both");
                }

                if ((criteria != SynchronizationCriteria.Time) && (mode == SynchronizationMode.Both))
                {
                    throw new ArgumentException("Only Time criteria is allowed in synchronization mode Both");
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
                        throw new ArgumentOutOfRangeException("mode");
                }

                List<string> criteriaNames = new List<string>();
                if ((criteria & SynchronizationCriteria.Time) == SynchronizationCriteria.Time)
                {
                    criteria -= SynchronizationCriteria.Time;
                    criteriaNames.Add("time");
                }

                if ((criteria & SynchronizationCriteria.Size) == SynchronizationCriteria.Size)
                {
                    criteria -= SynchronizationCriteria.Size;
                    criteriaNames.Add("size");
                }

                if (criteria != 0)
                {
                    throw new ArgumentOutOfRangeException("criteria");
                }

                WriteCommand(
                    string.Format(CultureInfo.InvariantCulture,
                        "synchronize {0} {1} {2} {3} {4} -- \"{5}\" \"{6}\"",
                        modeName,
                        BooleanSwitch(removeFiles, "delete"),
                        BooleanSwitch(mirror, "mirror"),
                        options.ToSwitches(),
                        FormatSwitch("criteria", string.Join(",", criteriaNames.ToArray())),
                        ArgumentEscape(localPath), ArgumentEscape(remotePath)));

                return ReadSynchronizeDirectories();
            }
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
                    bool transferIsUpload = false;
                    TransferEventArgs transfer = null;

                    while (groupReader.Read(0))
                    {
                        bool transferWillBeUpload;
                        if ((transferWillBeUpload = groupReader.IsNonEmptyElement(TransferEventArgs.UploadTag)) ||
                            groupReader.IsNonEmptyElement(TransferEventArgs.DownloadTag))
                        {
                            if (transfer != null)
                            {
                                AddSynchronizationTransfer(result, transferIsUpload, transfer);
                            }
                            transfer = TransferEventArgs.Read(groupReader);
                            transferIsUpload = transferWillBeUpload;
                        }
                        else if (groupReader.IsNonEmptyElement(RemovalEventArgs.Tag))
                        {
                            result.AddRemoval(RemovalEventArgs.Read(groupReader));
                        }
                        else if (groupReader.IsNonEmptyElement(ChmodEventArgs.Tag))
                        {
                            if (transfer == null)
                            {
                                throw new InvalidOperationException();
                            }
                            transfer.Chmod = ChmodEventArgs.Read(groupReader);
                        }
                        else if (groupReader.IsNonEmptyElement(TouchEventArgs.Tag))
                        {
                            if (transfer == null)
                            {
                                throw new InvalidOperationException();
                            }
                            transfer.Touch = TouchEventArgs.Read(groupReader);
                        }
                    }

                    if (transfer != null)
                    {
                        AddSynchronizationTransfer(result, transferIsUpload, transfer);
                    }
                }
                return result;
            }
        }

        public CommandExecutionResult ExecuteCommand(string command)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "call {0}", command));

                CommandExecutionResult result = new CommandExecutionResult();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader callReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("call", LogReadFlags.ThrowFailures))
                {
                    while (callReader.Read(0))
                    {
                        string value;
                        if (callReader.GetEmptyElementValue("output", out value))
                        {
                            result.Output = value;
                        }
                        if (callReader.GetEmptyElementValue("erroroutput", out value))
                        {
                            result.ErrorOutput = value;
                        }
                    }

                    groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                }

                return result;
            }
        }

        public RemoteFileInfo GetFileInfo(string path)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                return DoGetFileInfo(path);
            }
        }

        public bool FileExists(string path)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                try
                {
                    DoGetFileInfo(path);
                    return true;
                }
                catch (SessionRemoteException)
                {
                    return false;
                }
            }
        }

        public void CreateDirectory(string path)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "mkdir \"{0}\"", ArgumentEscape(path)));

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader mkdirReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("mkdir", LogReadFlags.ThrowFailures))
                {
                    ReadElement(mkdirReader, 0);
                    groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                }
            }
        }

        public void MoveFile(string sourcePath, string targetPath)
        {
            using (Logger.CreateCallstackAndLock())
            {
                CheckOpened();

                WriteCommand(string.Format(CultureInfo.InvariantCulture, "mv \"{0}\" \"{1}\"", ArgumentEscape(sourcePath), ArgumentEscape(targetPath)));

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader mvReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("mv", LogReadFlags.ThrowFailures))
                {
                    ReadElement(mvReader, 0);
                    groupReader.ReadToEnd(LogReadFlags.ThrowFailures);
                }
            }
        }

        // This is not static method only to make it visible to COM
        public string EscapeFileMask(string fileMask)
        {
            if (fileMask == null)
            {
                throw new ArgumentNullException("fileMask");
            }
            int lastSlash = fileMask.LastIndexOf('/');
            string path = lastSlash > 0 ? fileMask.Substring(0, lastSlash + 1) : string.Empty;
            string mask = lastSlash > 0 ? fileMask.Substring(lastSlash + 1) : fileMask;
            mask = mask.Replace("[", "[[]").Replace("*", "[*]").Replace("?", "[?]");
            return path + mask;
        }

        [ComRegisterFunction]
        private static void ComRegister(Type t)
        {
            string subKey = GetTypeLibKey(t);
            Assembly assembly = Assembly.GetAssembly(t);
            object[] attributes = assembly.GetCustomAttributes(typeof(GuidAttribute), false);
            if (attributes.Length == 0)
            {
                throw new InvalidOperationException(string.Format(CultureInfo.CurrentCulture, "Cannot find {0} attribute for assembly {1}", typeof(GuidAttribute), assembly));
            }
            GuidAttribute guidAttribute = (GuidAttribute)attributes[0];
            Registry.ClassesRoot.CreateSubKey(subKey).SetValue(null, guidAttribute.Value);
        }

        [ComUnregisterFunction]
        private static void ComUnregister(Type t)
        {
            string subKey = GetTypeLibKey(t);
            Registry.ClassesRoot.DeleteSubKey(subKey, false);
        }

        private void ReadFile(RemoteFileInfo fileInfo, CustomLogReader fileReader)
        {
            using (Logger.CreateCallstack())
            {
                string value;
                if (fileReader.GetEmptyElementValue("type", out value))
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

        private void AddSynchronizationTransfer(SynchronizationResult result, bool transferIsUpload, TransferEventArgs transfer)
        {
            if (transferIsUpload)
            {
                result.AddUpload(transfer);
            }
            else
            {
                result.AddDownload(transfer);
            }
            RaiseFileTransferredEvent(transfer);
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
                if ((XmlLogPath != null) && File.Exists(XmlLogPath))
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
            Logger.WriteLine("Command: [{0}]", command);
            _process.ExecuteCommand(command);
            GotOutput();
        }

        private static void ReadElement(CustomLogReader reader, LogReadFlags flags)
        {
            while (reader.Read(flags))
            {
            }
        }

        private string SessionOptionsToOpenArguments(SessionOptions sessionOptions)
        {
            using (Logger.CreateCallstack())
            {
                string url;
                switch (sessionOptions.Protocol)
                {
                    case Protocol.Sftp:
                        url = "sftp://";
                        break;

                    case Protocol.Scp:
                        url = "scp://";
                        break;

                    case Protocol.Ftp:
                        url = "ftp://";
                        break;

                    default:
                        throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not supported", sessionOptions.Protocol));
                }

                bool hasUsername = !string.IsNullOrEmpty(sessionOptions.UserName);
                if (hasUsername)
                {
                    url += UriEscape(sessionOptions.UserName);
                }

                if (!string.IsNullOrEmpty(sessionOptions.Password))
                {
                    if (!hasUsername)
                    {
                        throw new ArgumentException("SessionOptions.Password is set, but SessionOptions.UserName is not.");
                    }
                    url += ":" + UriEscape(sessionOptions.Password);
                }

                if (hasUsername)
                {
                    url += "@";
                }

                if (string.IsNullOrEmpty(sessionOptions.HostName))
                {
                    throw new ArgumentException("SessionOptions.HostName is not set.");
                }

                url += UriEscape(sessionOptions.HostName);

                if (sessionOptions.PortNumber != 0)
                {
                    url += ":" + sessionOptions.PortNumber.ToString(CultureInfo.InvariantCulture);
                }

                string arguments = SessionOptionsToOpenSwitches(sessionOptions);

                arguments += (!string.IsNullOrEmpty(arguments) ? " " : "") + "\"" + ArgumentEscape(url) + "\"";

                if (sessionOptions.RawSettings.Count > 0)
                {
                    arguments += " -rawsettings";
                    foreach (KeyValuePair<string, string> rawSetting in sessionOptions.RawSettings)
                    {
                        arguments += string.Format(CultureInfo.InvariantCulture, " {0}=\"{1}\"", rawSetting.Key, ArgumentEscape(rawSetting.Value));
                    }
                }

                return arguments;
            }
        }

        private string SessionOptionsToOpenSwitches(SessionOptions sessionOptions)
        {
            using (Logger.CreateCallstack())
            {
                List<string> switches = new List<string>();

                if (!string.IsNullOrEmpty(sessionOptions.SshHostKeyFingerprint) ||
                    sessionOptions.GiveUpSecurityAndAcceptAnySshHostKey)
                {
                    if (!sessionOptions.IsSsh)
                    {
                        throw new ArgumentException("SessionOptions.SshHostKey or SessionOptions.GiveUpSecurityAndAcceptAnySshHostKey is set, but SessionOptions.Protocol is not Protocol.Sftp nor Protocol.Scp.");
                    }
                    string sshHostKeyFingerprint = sessionOptions.SshHostKeyFingerprint;
                    if (sessionOptions.GiveUpSecurityAndAcceptAnySshHostKey)
                    {
                        Logger.WriteLine("WARNING! Giving up security and accepting any key as configured");
                        sshHostKeyFingerprint = AddStarToList(sshHostKeyFingerprint);
                    }
                    switches.Add(FormatSwitch("hostkey", sshHostKeyFingerprint));
                }
                else
                {
                    if (sessionOptions.IsSsh && DefaultConfiguration)
                    {
                        throw new ArgumentException("SessionOptions.Protocol is Protocol.Sftp or Protocol.Scp, but SessionOptions.HostKey is not set.");
                    }
                }

                if (!string.IsNullOrEmpty(sessionOptions.SshPrivateKeyPath))
                {
                    if (!sessionOptions.IsSsh)
                    {
                        throw new ArgumentException("SessionOptions.SshPrivateKey is set, but SessionOptions.Protocol is not Protocol.Sftp nor Protocol.Scp.");
                    }
                    switches.Add(FormatSwitch("privatekey", sessionOptions.SshPrivateKeyPath));
                }

                if (sessionOptions.FtpSecure != FtpSecure.None)
                {
                    if (sessionOptions.Protocol != Protocol.Ftp)
                    {
                        throw new ArgumentException("SessionOptions.FtpSecure is not FtpSecure.None, but SessionOptions.Protocol is not Protocol.Ftp.");
                    }

                    switch (sessionOptions.FtpSecure)
                    {
                        case FtpSecure.Implicit:
                            switches.Add(FormatSwitch("implicit"));
                            break;

                        case FtpSecure.ExplicitSsl:
                            switches.Add(FormatSwitch("explicitssl"));
                            break;

                        case FtpSecure.ExplicitTls:
                            switches.Add(FormatSwitch("explicittls"));
                            break;

                        default:
                            throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not supported", sessionOptions.FtpSecure));
                    }
                }

                if (!string.IsNullOrEmpty(sessionOptions.SslHostCertificateFingerprint) ||
                    sessionOptions.GiveUpSecurityAndAcceptAnySslHostCertificate)
                {
                    if (sessionOptions.FtpSecure == FtpSecure.None)
                    {
                        throw new ArgumentException("SessionOptions.SslHostCertificateFingerprint or SessionOptions.GiveUpSecurityAndAcceptAnySslHostCertificate is set, but SessionOptions.FtpSecure is FtpSecure.None.");
                    }
                    string sslHostCertificateFingerprint = sessionOptions.SslHostCertificateFingerprint;
                    if (sessionOptions.GiveUpSecurityAndAcceptAnySslHostCertificate)
                    {
                        Logger.WriteLine("WARNING! Giving up security and accepting any certificate as configured");
                        sslHostCertificateFingerprint = AddStarToList(sslHostCertificateFingerprint);
                    }
                    switches.Add(FormatSwitch("certificate", sslHostCertificateFingerprint));
                }

                if (sessionOptions.Protocol == Protocol.Ftp)
                {
                    switches.Add(FormatSwitch("passive", (sessionOptions.FtpMode == FtpMode.Passive)));
                }

                switches.Add(FormatSwitch("timeout", (int)sessionOptions.Timeout.TotalSeconds));

                return string.Join(" ", switches.ToArray());
            }
        }

        private static string AddStarToList(string list)
        {
            return (string.IsNullOrEmpty(list) ? string.Empty : list + ";") + "*";
        }

        private RemoteFileInfo DoGetFileInfo(string path)
        {
            using (Logger.CreateCallstack())
            {
                WriteCommand(string.Format(CultureInfo.InvariantCulture, "stat -- \"{0}\"", ArgumentEscape(path)));

                RemoteFileInfo fileInfo = new RemoteFileInfo();

                using (ElementLogReader groupReader = _reader.WaitForGroupAndCreateLogReader())
                using (ElementLogReader statReader = groupReader.WaitForNonEmptyElementAndCreateLogReader("stat", LogReadFlags.ThrowFailures))
                {
                    while (statReader.Read(0))
                    {
                        string value;
                        if (statReader.GetEmptyElementValue("filename", out value))
                        {
                            fileInfo.Name = value;
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
            return string.Format(CultureInfo.InvariantCulture, "-{0}=\"{1}\"", key, ArgumentEscape(value));
        }

        internal static string FormatSwitch(string key, int value)
        {
            return string.Format(CultureInfo.InvariantCulture, "-{0}={1}", key, value.ToString(CultureInfo.InvariantCulture));
        }

        internal static string FormatSwitch(string key, bool value)
        {
            return FormatSwitch(key, (value ? 1 : 0));
        }

        internal static string ArgumentEscape(string value)
        {
            int i = 0;
            while (i < value.Length)
            {
                if (value[i] == '"')
                {
                    value = value.Insert(i, "\"");
                    ++i;
                }
                ++i;
            }
            return value;
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
            Logger.WriteLine("Scheduling output: [{0}]", e.Data);
            Output.InternalAdd(e.Data);
            GotOutput();
            ScheduleEvent(() => RaiseOutputDataReceived(e.Data));
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
            if (DateTime.Now - _lastOutput > Timeout)
            {
                string message = "Timeout waiting for WinSCP to respond";
                if (additional != null)
                {
                    message += " - " + additional;
                }
                throw new TimeoutException(message);
            }

            if (_aborted)
            {
                throw new SessionLocalException(this, "Aborted.");
            }
        }

        private void RaiseFileTransferredEvent(TransferEventArgs args)
        {
            Logger.WriteLine("FileTransferredEvent: [{0}]", args.FileName);

            if (FileTransferred != null)
            {
                FileTransferred(this, args);
            }
        }

        internal void RaiseFailed(SessionRemoteException e)
        {
            Logger.WriteLine("Failed: [{0}]", e);

            if (Failed != null)
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
                throw new InvalidOperationException("Object is disposed");
            }

            if (_aborted)
            {
                throw new InvalidOperationException("Session was aborted");
            }
        }

        private void CheckOpened()
        {
            if (!Opened)
            {
                throw new InvalidOperationException("Session is not opened");
            }
        }

        private void CheckNotOpened()
        {
            if (Opened)
            {
                throw new InvalidOperationException("Session is already opened");
            }
        }

        private void RaiseOutputDataReceived(string data)
        {
            Logger.WriteLine("Output: [{0}]", data);

            if (OutputDataReceived != null)
            {
                OutputDataReceived(this, new OutputDataReceivedEventArgs(data));
            }
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
            _progressHandling++;
            return new ProgressHandler(this);
        }

        internal void DisableProgressHandling()
        {
            if (_progressHandling <= 0)
            {
                throw new InvalidOperationException();
            }

            // make sure we process all pending progress events
            DispatchEvents(0);

            _progressHandling--;
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
                        throw new SessionLocalException(this, string.Format(CultureInfo.CurrentCulture, "Configured temporary file {0} already exists", _xmlLogPath));
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

        private static string GetTypeLibKey(Type t)
        {
            return "CLSID\\{" + t.GUID.ToString().ToUpperInvariant() + "}\\TypeLib";
        }

        FieldInfo IReflect.GetField(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                FieldInfo result = typeof(Session).GetField(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        FieldInfo[] IReflect.GetFields(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                FieldInfo[] result = typeof(Session).GetFields(bindingAttr);
                Logger.WriteLine("Fields [{0}]", result.Length);
                return result;
            }
        }

        MemberInfo[] IReflect.GetMember(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                MemberInfo[] result = typeof(Session).GetMember(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        MemberInfo[] IReflect.GetMembers(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                MemberInfo[] result = typeof(Session).GetMembers(bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        MethodInfo IReflect.GetMethod(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                MethodInfo result = typeof(Session).GetMethod(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        MethodInfo IReflect.GetMethod(string name, BindingFlags bindingAttr, Binder binder, Type[] types, ParameterModifier[] modifiers)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                MethodInfo result = typeof(Session).GetMethod(name, bindingAttr, binder, types, modifiers);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        MethodInfo[] IReflect.GetMethods(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                MethodInfo[] result = typeof(Session).GetMethods(bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        PropertyInfo[] IReflect.GetProperties(BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                PropertyInfo[] result = typeof(Session).GetProperties(bindingAttr);
                Logger.WriteLine("Result [{0}]", result.Length);
                return result;
            }
        }

        PropertyInfo IReflect.GetProperty(string name, BindingFlags bindingAttr, Binder binder, Type returnType, Type[] types, ParameterModifier[] modifiers)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                PropertyInfo result = typeof(Session).GetProperty(name, bindingAttr, binder, returnType, types, modifiers);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        PropertyInfo IReflect.GetProperty(string name, BindingFlags bindingAttr)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                PropertyInfo result = typeof(Session).GetProperty(name, bindingAttr);
                Logger.WriteLine("Result [{0}]", result != null ? result.Name : "null");
                return result;
            }
        }

        object IReflect.InvokeMember(string name, BindingFlags invokeAttr, Binder binder, object target, object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] namedParameters)
        {
            using (Logger.CreateCallstack())
            {
                Logger.WriteLine("Name [{0}]", name);
                object result;
                try
                {
                    Logger.WriteLine("Target [{0}]", target);
                    if (args != null)
                    {
                        Logger.WriteLine("Args [{0}] [{1}]", args.Length, modifiers != null ? modifiers.Length.ToString(CultureInfo.InvariantCulture) : "null");
                        for (int i = 0; i < args.Length; ++i)
                        {
                            Logger.WriteLine("Arg [{0}] [{1}] [{2}]", i, args[i], (modifiers != null ? modifiers[i].ToString() : "null"));
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

                    Logger.WriteLine("Invoking");
                    result = typeof(Session).InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
                    Logger.WriteLine("Result [{0}]", result);
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
            get { return typeof(Session); }
        }

        internal const string Namespace = "http://winscp.net/schema/session/1.0";
        internal Logger Logger { get; private set; }
        internal bool GuardProcessWithJobInternal { get { return _guardProcessWithJob; } set { CheckNotOpened(); _guardProcessWithJob = value; } }
        internal bool TestHandlesClosedInternal { get; set; }

        private ExeSessionProcess _process;
        private DateTime _lastOutput;
        private ElementLogReader _reader;
        private SessionLogReader _logReader;
        private readonly IList<OperationResultBase> _operationResults;
        private delegate void Action();
        private readonly IList<Action> _events;
        private AutoResetEvent _eventsEvent;
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
    }
}
