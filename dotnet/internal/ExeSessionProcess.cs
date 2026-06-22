using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Threading;
#if !NETSTANDARD
using Microsoft.Win32;
#endif
using Microsoft.Win32.SafeHandles;
using System.Runtime.InteropServices;
using System.Reflection;
#if !NETSTANDARD
using System.Security.Principal;
using System.Security.AccessControl;
#endif
using System.ComponentModel;
using System.Security.Cryptography;
using System.Linq;

namespace WinSCP
{
    internal class ExeSessionProcess : IDisposable
    {
        public event OutputDataReceivedEventHandler OutputDataReceived;

        public bool HasExited { get { return _process.HasExited; } }
        public int ExitCode { get { return _process.ExitCode; } }
        public PipeStream StdOut { get; set; }
        public Stream StdIn { get; set; }
        public string ExecutablePath { get; }

        public static ExeSessionProcess CreateForSession(Session session)
        {
            return new ExeSessionProcess(session, true, null);
        }

        public static ExeSessionProcess CreateForConsole(Session session, string additionalArguments)
        {
            return new ExeSessionProcess(session, false, additionalArguments);
        }

        private ExeSessionProcess(Session session, bool useXmlLog, string additionalArguments)
        {
            _session = session;
            _logger = session.Logger;
            _incompleteLine = string.Empty;

            using (_logger.CreateCallstack())
            {
                ExecutablePath = GetExecutablePath();

                _logger.WriteLine("EXE executable path resolved to {0}", ExecutablePath);

                string assemblyFilePath = _logger.GetAssemblyFilePath();
                FileVersionInfo assemblyVersion = null;
                if (assemblyFilePath != null)
                {
                    assemblyVersion = FileVersionInfo.GetVersionInfo(assemblyFilePath);
                }

                CheckVersion(ExecutablePath, assemblyVersion);

                string configSwitch;
                if (_session.DefaultConfigurationInternal)
                {
                    configSwitch = "/ini=nul ";
                }
                else
                {
                    if (!string.IsNullOrEmpty(_session.IniFilePathInternal))
                    {
                        configSwitch = string.Format(CultureInfo.InvariantCulture, "/ini=\"{0}\" ", _session.IniFilePathInternal);
                    }
                    else
                    {
                        configSwitch = "";
                    }
                }

                string logSwitch = null;
                if (!string.IsNullOrEmpty(_session.SessionLogPath))
                {
                    logSwitch = string.Format(CultureInfo.InvariantCulture, "/log=\"{0}\" ", LogPathEscape(_session.SessionLogPath));
                }

                string xmlLogSwitch;
                if (useXmlLog)
                {
                    xmlLogSwitch = string.Format(CultureInfo.InvariantCulture, "/xmllog=\"{0}\" /xmlgroups /xmllogrequired ", LogPathEscape(_session.XmlLogPath));
                }
                else
                {
                    xmlLogSwitch = "";
                }

                string logLevelSwitch = null;
                if (_session.DebugLogLevel != 0)
                {
                    logLevelSwitch = string.Format(CultureInfo.InvariantCulture, "/loglevel={0} ", _session.DebugLogLevel);
                }

                string assemblyVersionStr =
                    (assemblyVersion == null) ? "unk" :
                    string.Format(CultureInfo.InvariantCulture, "{0}.{1}.{2} ", assemblyVersion.ProductMajorPart, assemblyVersion.ProductMinorPart, assemblyVersion.ProductBuildPart);

                string assemblyVersionSwitch =
                    string.Format(CultureInfo.InvariantCulture, "/dotnet={0} ", assemblyVersionStr);

                string arguments =
                    xmlLogSwitch + "/nointeractiveinput /stdout /stdin " + assemblyVersionSwitch +
                    configSwitch + logSwitch + logLevelSwitch + _session.AdditionalExecutableArguments;

                Tools.AddRawParameters(ref arguments, _session.RawConfiguration, "/rawconfig", false);

                if (!string.IsNullOrEmpty(additionalArguments))
                {
                    arguments += " " + additionalArguments;
                }

                _process = new Process();
                _process.StartInfo.FileName = ExecutablePath;
                _process.StartInfo.WorkingDirectory = Path.GetDirectoryName(ExecutablePath);
                _process.StartInfo.Arguments = arguments;
                _process.StartInfo.UseShellExecute = false;
                _process.Exited += ProcessExited;
            }
        }

        private static string LogPathEscape(string path)
        {
            return Tools.ArgumentEscape(path).Replace("!", "!!");
        }

        public void Abort()
        {
            using (_logger.CreateCallstack())
            {
                lock (_lock)
                {
                    if ((_process != null) && !_process.HasExited)
                    {
                        _process.Kill();
                    }
                }
            }
        }

        public void Start()
        {
            using (_logger.CreateCallstack())
            {
                InitializeConsole();
                InitializeChild();
            }
        }

        private void InitializeChild()
        {
            using (_logger.CreateCallstack())
            {
                // The /console is redundant for CreateForConsole
                _process.StartInfo.Arguments += string.Format(CultureInfo.InvariantCulture, " /console /consoleinstance={0}", _instanceName);

#if !NETSTANDARD
                // When running under IIS in "impersonated" mode, the process starts, but does not do anything.
                // Supposedly it "displayes" some invisible error message when starting and hangs.
                // Running it "as the user" helps, eventhough it already runs as the user.
                // These's probably some difference between "run as" and impersonations
                if (!string.IsNullOrEmpty(_session.ExecutableProcessUserName))
                {
                    _logger.WriteLine("Will run process as {0}", _session.ExecutableProcessUserName);

                    _process.StartInfo.UserName = _session.ExecutableProcessUserName;
                    _process.StartInfo.Password = _session.ExecutableProcessPassword;
                    // One of the hints for resolving C0000142 error (see below)
                    // was setting this property, so that an environment is correctly loaded,
                    // so DLLs can be found and loaded.
                    _process.StartInfo.LoadUserProfile = true;

                    // Without granting both window station and desktop access permissions,
                    // WinSCP process aborts with C0000142 (DLL Initialization Failed) error,
                    // when "running as user"
                    _logger.WriteLine("Granting access to window station");
                    try
                    {
                        IntPtr windowStation = UnsafeNativeMethods.GetProcessWindowStation();
                        GrantAccess(windowStation, (int)WindowStationRights.AllAccess);
                    }
                    catch (Exception e)
                    {
                        throw _logger.WriteException(new SessionLocalException(_session, "Error granting access to window station", e));
                    }

                    _logger.WriteLine("Granting access to desktop");
                    try
                    {
                        IntPtr desktop = UnsafeNativeMethods.GetThreadDesktop(UnsafeNativeMethods.GetCurrentThreadId());
                        GrantAccess(desktop, (int)DesktopRights.AllAccess);
                    }
                    catch (Exception e)
                    {
                        throw _logger.WriteException(new SessionLocalException(_session, "Error granting access to desktop", e));
                    }
                }
#endif

                _logger.WriteLine("Starting \"{0}\" {1}", _process.StartInfo.FileName, _process.StartInfo.Arguments);

                _process.Start();

                _logger.WriteLine("Started process {0}", _process.Id);

                _thread = new Thread(ProcessEvents)
                {
                    IsBackground = true
                };
                _thread.Start();
            }
        }

        // Handles returned by GetProcessWindowStation and GetThreadDesktop should not be closed
        internal class NoopSafeHandle : SafeHandle
        {
            public NoopSafeHandle(IntPtr handle) :
                base(handle, false)
            {
            }

            public override bool IsInvalid
            {
                get { return false; }
            }

            protected override bool ReleaseHandle()
            {
                return true;
            }
        }

#if !NETSTANDARD
        private void GrantAccess(IntPtr handle, int accessMask)
        {
            using (SafeHandle safeHandle = new NoopSafeHandle(handle))
            {
                GenericSecurity security =
                    new GenericSecurity(false, ResourceType.WindowObject, safeHandle, AccessControlSections.Access);

                security.AddAccessRule(
                    new GenericAccessRule(new NTAccount(_session.ExecutableProcessUserName), accessMask, AccessControlType.Allow));
                security.Persist(safeHandle, AccessControlSections.Access);
            }
        }
#endif

        private void ProcessExited(object sender, EventArgs e)
        {
            _logger.WriteLine("Process {0} exited with exit code {1}", _process.Id, _process.ExitCode);
        }

        private bool AbortedOrExited()
        {
            if (_abort)
            {
                _logger.WriteLine("Aborted");
                return true;
            }
            else if (_process.HasExited)
            {
                _logger.WriteLine("Exited");
                return true;
            }
            else
            {
                return false;
            }
        }

        private void ProcessEvents()
        {
            using (_logger.CreateCallstack())
            {
                try
                {
                    while (!AbortedOrExited())
                    {
                        _logger.WriteLineLevel(1, "Waiting for request event");
                        // Keep in sync with a delay in SessionLogReader.DoRead
                        if (_requestEvent.WaitOne(100, false))
                        {
                            _logger.WriteLineLevel(1, "Got request event");
                            ProcessEvent();
                        }

                        if (_logger.LogLevel >= 1)
                        {
                            _logger.WriteLine(string.Format(CultureInfo.InvariantCulture, "2nd generation collection count: {0}", GC.CollectionCount(2)));
                            _logger.WriteLine(string.Format(CultureInfo.InvariantCulture, "Total memory allocated: {0}", GC.GetTotalMemory(false)));
                        }
                    }
                }
                catch (Exception e)
                {
                    _logger.WriteLine("Error while processing events");
                    _logger.WriteException(e);
                    throw;
                }
            }
        }

        private void ProcessEvent()
        {
            using (_logger.CreateCallstack())
            {
                using (ConsoleCommStruct commStruct = AcquireCommStruct())
                {
                    switch (commStruct.Event)
                    {
                        case ConsoleEvent.Print:
                            ProcessPrintEvent(commStruct.PrintEvent);
                            break;

                        case ConsoleEvent.Input:
                            ProcessInputEvent(commStruct.InputEvent);
                            break;

                        case ConsoleEvent.Choice:
                            ProcessChoiceEvent(commStruct.ChoiceEvent);
                            break;

                        case ConsoleEvent.Title:
                            ProcessTitleEvent(commStruct.TitleEvent);
                            break;

                        case ConsoleEvent.Init:
                            ProcessInitEvent(commStruct.InitEvent);
                            break;

                        case ConsoleEvent.Progress:
                            ProcessProgressEvent(commStruct.ProgressEvent);
                            break;

                        case ConsoleEvent.TransferOut:
                            ProcessTransferOutEvent(commStruct.TransferOutEvent);
                            break;

                        case ConsoleEvent.TransferIn:
                            ProcessTransferInEvent(commStruct.TransferInEvent);
                            break;

                        default:
                            throw _logger.WriteException(new NotImplementedException());
                    }
                }

                _responseEvent.Set();
                _logger.WriteLineLevel(1, "Response event set");
            }
        }

        private void ProcessChoiceEvent(ConsoleChoiceEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                _logger.WriteLine(
                    "Options: [{0}], Timer: [{1}], Timeouting: [{2}], Timeouted: [{3}], Break: [{4}]",
                    e.Options, e.Timer, e.Timeouting, e.Timeouted, e.Break);

                QueryReceivedEventArgs args = new QueryReceivedEventArgs
                {
                    Message = e.Message
                };

                _session.ProcessChoice(args);

                if (args.SelectedAction == QueryReceivedEventArgs.Action.None)
                {
                    if (e.Timeouting)
                    {
                        Thread.Sleep((int)e.Timer);
                        e.Result = e.Timeouted;
                    }
                    else
                    {
                        e.Result = e.Break;
                    }
                }
                else if (args.SelectedAction == QueryReceivedEventArgs.Action.Continue)
                {
                    if (e.Timeouting)
                    {
                        Thread.Sleep((int)e.Timer);
                        e.Result = e.Timeouted;
                    }
                    else
                    {
                        e.Result = e.Continue;
                    }
                }
                else if (args.SelectedAction == QueryReceivedEventArgs.Action.Abort)
                {
                    e.Result = e.Break;
                }

                _logger.WriteLine("Options Result: [{0}]", e.Result);
            }
        }

        private void ProcessTitleEvent(ConsoleTitleEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                _logger.WriteLine("Not-supported title event [{0}]", e.Title);
            }
        }

        private void ProcessInputEvent(ConsoleInputEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                while (!AbortedOrExited())
                {
                    lock (_input)
                    {
                        if (_input.Count > 0)
                        {
                            e.Str = _input[0];
                            e.Result = true;
                            _input.RemoveAt(0);
                            Print(false, false, _log[0] + "\n");
                            _log.RemoveAt(0);
                            return;
                        }
                    }

                    _inputEvent.WaitOne(100, false);
                }
            }
        }

        private void Print(bool fromBeginning, bool error, string message)
        {
            if (fromBeginning && ((message.Length == 0) || (message[0] != '\n')))
            {
                _lastFromBeginning = message;
                _logger.WriteLine("Buffered from-beginning message [{0}]", _lastFromBeginning);

                OutputDataReceived?.Invoke(this, null);
            }
            else
            {
                if (!string.IsNullOrEmpty(_lastFromBeginning))
                {
                    AddToOutput(_lastFromBeginning, false);
                    _lastFromBeginning = null;
                }

                if (fromBeginning && (message.Length > 0) && (message[0] == '\n'))
                {
                    AddToOutput("\n", false);
                    _lastFromBeginning = message.Substring(1);
                    _logger.WriteLine("Buffered from-beginning message [{0}]", _lastFromBeginning);
                }
                else
                {
                    AddToOutput(message, error);
                }
            }
        }

        private void AddToOutput(string message, bool error)
        {
            string[] lines = (_incompleteLine + message).Split(new[] { '\n' });

            _incompleteLine = lines[lines.Length - 1];

            for (int i = 0; i < lines.Length - 1; ++i)
            {
                OutputDataReceived?.Invoke(this, new OutputDataReceivedEventArgs(lines[i], error));
            }
        }

        private void ProcessPrintEvent(ConsolePrintEventStruct e)
        {
            _logger.WriteLineLevel(1, string.Format(CultureInfo.CurrentCulture, "Print: {0}", e.Message));
            Print(e.FromBeginning, e.Error, e.Message);
        }

        private void ProcessInitEvent(ConsoleInitEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                if (!e.UseStdErr ||
                    (e.BinaryOutput != ConsoleInitEventStruct.StdInOut.Binary) ||
                    (e.BinaryInput != ConsoleInitEventStruct.StdInOut.Binary))
                {
                    throw _logger.WriteException(new InvalidOperationException("Unexpected console interface options"));
                }

                e.InputType = 3; // pipe
                e.OutputType = 3; // pipe
                e.WantsProgress = _session.WantsProgress;
            }
        }

        private void ProcessProgressEvent(ConsoleProgressEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                _logger.WriteLine(
                    "File Name [{0}] - Directory [{1}] - Overall Progress [{2}] - File Progress [{3}] - CPS [{4}]",
                    e.FileName, e.Directory, e.OverallProgress, e.FileProgress, e.CPS);

                if (!_cancel)
                {
                    FileTransferProgressEventArgs args = new FileTransferProgressEventArgs();

                    switch (e.Operation)
                    {
                        case ConsoleProgressEventStruct.ProgressOperation.Copy:
                            args.Operation = ProgressOperation.Transfer;
                            break;

                        default:
                            throw _logger.WriteException(new ArgumentOutOfRangeException("Unknown progress operation", (Exception)null));
                    }

                    switch (e.Side)
                    {
                        case ConsoleProgressEventStruct.ProgressSide.Local:
                            args.Side = ProgressSide.Local;
                            break;

                        case ConsoleProgressEventStruct.ProgressSide.Remote:
                            args.Side = ProgressSide.Remote;
                            break;

                        default:
                            throw _logger.WriteException(new ArgumentOutOfRangeException("Unknown progress side", (Exception)null));
                    }

                    args.FileName = e.FileName;
                    args.Directory = e.Directory;
                    args.OverallProgress = ((double)e.OverallProgress) / 100;
                    args.FileProgress = ((double)e.FileProgress) / 100;
                    args.CPS = (int)e.CPS;
                    args.Cancel = false;
                    _session.ProcessProgress(args);
                }

                if (_cancel)
                {
                    e.Cancel = true;
                }
            }
        }

        private void ProcessTransferOutEvent(ConsoleTransferEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                _logger.WriteLine("Len [{0}]", e.Len);

                if (StdOut == null)
                {
                    throw _logger.WriteException(new InvalidOperationException("Unexpected data"));
                }
                int len = (int)e.Len;
                if (len > 0)
                {
                    StdOut.WriteInternal(e.Data, 0, len);
                    _logger.WriteLine("Data written to the buffer");
                }
                else
                {
                    StdOut.CloseWrite();
                    _logger.WriteLine("Data buffer closed");
                }
            }
        }

        private void ProcessTransferInEvent(ConsoleTransferEventStruct e)
        {
            using (_logger.CreateCallstack())
            {
                _logger.WriteLine("Len [{0}]", e.Len);

                if (StdIn == null)
                {
                    throw _logger.WriteException(new InvalidOperationException("Unexpected data request"));
                }
                try
                {
                    int len = (int)e.Len;
                    len = StdIn.Read(e.Data, 0, len);
                    _logger.WriteLine("{0} bytes read", len);
                    e.Len = (uint)len;
                }
                catch (Exception ex)
                {
                    _logger.WriteLine("Error reading data stream");
                    _logger.WriteException(ex);
                    e.Error = true;
                }
            }
        }

        private void InitializeConsole()
        {
            using (_logger.CreateCallstack())
            {
                int attempts = 0;
                Random random = new Random();
                int process = Process.GetCurrentProcess().Id;

                do
                {
                    if (attempts > MaxAttempts)
                    {
                        throw _logger.WriteException(new SessionLocalException(_session, "Cannot find unique name for event object."));
                    }

                    int instanceNumber = random.Next(1000);

                    _instanceName = string.Format(CultureInfo.InvariantCulture, "_{0}_{1}_{2}", process, GetHashCode(), instanceNumber);
                    _logger.WriteLine("Trying event {0}", _instanceName);
                    if (!TryCreateEvent(ConsoleEventRequest + _instanceName, out _requestEvent))
                    {
                        _logger.WriteLine("Event {0} is not unique", _instanceName);
                        _requestEvent.Close();
                        _requestEvent = null;
                    }
                    else
                    {
                        _logger.WriteLine("Event {0} is unique", _instanceName);
                        _responseEvent = CreateEvent(ConsoleEventResponse + _instanceName);
                        _cancelEvent = CreateEvent(ConsoleEventCancel + _instanceName);
                        string fileMappingName = ConsoleMapping + _instanceName;
                        _fileMapping = CreateFileMapping(fileMappingName);
                        if (Marshal.GetLastWin32Error() == UnsafeNativeMethods.ERROR_ALREADY_EXISTS)
                        {
                            throw _logger.WriteException(new SessionLocalException(_session, string.Format(CultureInfo.InvariantCulture, "File mapping {0} already exists", fileMappingName)));
                        }
                        if (_fileMapping.IsInvalid)
                        {
                            throw _logger.WriteException(new SessionLocalException(_session, string.Format(CultureInfo.InvariantCulture, "Cannot create file mapping {0}", fileMappingName)));
                        }
                    }
                    ++attempts;
                }
                while (_requestEvent == null);

                using (ConsoleCommStruct commStruct = AcquireCommStruct())
                {
                    commStruct.InitHeader();
                }

                if (_session.GuardProcessWithJobInternal)
                {
                    string jobName = ConsoleJob + _instanceName;
                    _job = new Job(_logger, jobName);
                }
            }
        }

        private SafeFileHandle CreateFileMapping(string fileMappingName)
        {
            unsafe
            {
                IntPtr securityAttributesPtr = IntPtr.Zero;

#if !NETSTANDARD
                // We use the EventWaitHandleSecurity only to generate the descriptor binary form
                // that does not differ for object types, so we abuse the existing "event handle" implementation,
                // not to have to create the file mapping SecurityAttributes via P/Invoke.

                // .NET 4 supports MemoryMappedFile and MemoryMappedFileSecurity natively already

                EventWaitHandleSecurity security = CreateSecurity((EventWaitHandleRights)FileMappingRights.AllAccess);

                if (security != null)
                {
                    SecurityAttributes securityAttributes = new SecurityAttributes();
                    securityAttributes.nLength = (uint)Marshal.SizeOf(securityAttributes);

                    byte[] descriptorBinaryForm = security.GetSecurityDescriptorBinaryForm();
                    byte * buffer = stackalloc byte[descriptorBinaryForm.Length];
                    for (int i = 0; i < descriptorBinaryForm.Length; i++)
                    {
                        buffer[i] = descriptorBinaryForm[i];
                    }
                    securityAttributes.lpSecurityDescriptor = (IntPtr)buffer;

                    int length = Marshal.SizeOf(typeof(SecurityAttributes));
                    securityAttributesPtr = Marshal.AllocHGlobal(length);
                    Marshal.StructureToPtr(securityAttributes, securityAttributesPtr, false);
                }
#endif

                return
                    UnsafeNativeMethods.CreateFileMapping(
                        new SafeFileHandle(new IntPtr(-1), true), securityAttributesPtr, FileMapProtection.PageReadWrite, 0,
                        ConsoleCommStruct.Size, fileMappingName);
            }
        }

        private ConsoleCommStruct AcquireCommStruct()
        {
            return new ConsoleCommStruct(_session, _fileMapping);
        }

        private bool TryCreateEvent(string name, out EventWaitHandle ev)
        {
            _logger.WriteLine("Creating event {0}", name);

            string securityDesc;
#if !NETSTANDARD
            EventWaitHandleSecurity security = CreateSecurity(EventWaitHandleRights.FullControl);

            ev = new EventWaitHandle(false, EventResetMode.AutoReset, name, out bool createdNew, security);
            securityDesc = (security != null ? security.GetSecurityDescriptorSddlForm(AccessControlSections.All) : "none");
#else
            ev = new EventWaitHandle(false, EventResetMode.AutoReset, name, out bool createdNew);
            securityDesc = "not impl";
#endif
            _logger.WriteLine(
                "Created event {0} with handle {1} with security {2}, new {3}",
                name, ev.SafeWaitHandle.DangerousGetHandle(), securityDesc, createdNew);
            return createdNew;
        }

#if !NETSTANDARD
        private EventWaitHandleSecurity CreateSecurity(EventWaitHandleRights eventRights)
        {
            EventWaitHandleSecurity security = null;

            // When "running as user", we have to grant the target user permissions to the objects (events and file mapping) explicitly
            if (!string.IsNullOrEmpty(_session.ExecutableProcessUserName))
            {
                security = new EventWaitHandleSecurity();
                IdentityReference si;
                try
                {
                    si = new NTAccount(_session.ExecutableProcessUserName);
                }
                catch (Exception e)
                {
                    throw _logger.WriteException(new SessionLocalException(_session, string.Format(CultureInfo.CurrentCulture, "Error resolving account {0}", _session.ExecutableProcessUserName), e));
                }

                EventWaitHandleAccessRule rule =
                    new EventWaitHandleAccessRule(
                        si, eventRights, AccessControlType.Allow);
                security.AddAccessRule(rule);
            }

            return security;
        }
#endif

        private EventWaitHandle CreateEvent(string name)
        {
            if (!TryCreateEvent(name, out EventWaitHandle ev))
            {
                throw _logger.WriteException(new SessionLocalException(_session, string.Format(CultureInfo.InvariantCulture, "Event {0} already exists", name)));
            }
            return ev;
        }

        private void TestEventClosed(string name)
        {
            if (_session.TestHandlesClosedInternal)
            {
                _logger.WriteLine("Testing that event {0} is closed", name);
                if (TryCreateEvent(name, out EventWaitHandle ev))
                {
                    ev.Close();
                }
                else
                {
                    _logger.WriteLine("Exception: Event {0} was not closed yet", name);
                }
            }
        }

        private void AddInput(string str, string log)
        {
            Type structType = typeof(ConsoleInputEventStruct);
            FieldInfo strField = structType.GetField("Str");
            object[] attributes = strField.GetCustomAttributes(typeof(MarshalAsAttribute), false);
            if (attributes.Length != 1)
            {
                throw _logger.WriteException(new InvalidOperationException("MarshalAs attribute not found for ConsoleInputEventStruct.Str"));
            }
            MarshalAsAttribute marshalAsAttribute = (MarshalAsAttribute)attributes[0];

            if (marshalAsAttribute.SizeConst <= str.Length)
            {
                throw _logger.WriteException(
                    new SessionLocalException(
                        _session,
                        string.Format(CultureInfo.CurrentCulture, "Input [{0}] is too long ({1} limit)", str, marshalAsAttribute.SizeConst)));
            }

            lock (_input)
            {
                _input.Add(str);
                _log.Add(log);
                _inputEvent.Set();
            }
        }

        public void ExecuteCommand(string command, string log)
        {
            using (_logger.CreateCallstack())
            {
                _cancel = false;
                AddInput(command, log);
            }
        }

        public void Close()
        {
            using (_logger.CreateCallstack())
            {
                int timeout;

                #if DEBUG
                // in debug build, we expect the winscp.exe to run in tracing mode, being very slow
                timeout = 10000;
                #else
                timeout = 2000;
                #endif

                _logger.WriteLine("Waiting for process to exit ({0} ms)", timeout);

                if (!_process.WaitForExit(timeout))
                {
                    _logger.WriteLine("Killing process");
                    _process.Kill();
                }
            }
        }

        public void Dispose()
        {
            using (_logger.CreateCallstack())
            {
                lock (_lock)
                {
                    if (_session.TestHandlesClosedInternal)
                    {
                        _logger.WriteLine("Will test that handles are closed");
                    }
                    _abort = true;
                    if (_thread != null)
                    {
                        _thread.Join();
                        _thread = null;
                    }
                    if (_process != null)
                    {
                        _process.Dispose();
                        _process = null;
                    }
                    if (_requestEvent != null)
                    {
                        _requestEvent.Close();
                        TestEventClosed(ConsoleEventRequest + _instanceName);
                    }
                    if (_responseEvent != null)
                    {
                        _responseEvent.Close();
                        TestEventClosed(ConsoleEventResponse + _instanceName);
                    }
                    if (_cancelEvent != null)
                    {
                        _cancelEvent.Close();
                        TestEventClosed(ConsoleEventCancel + _instanceName);
                    }
                    if (_fileMapping != null)
                    {
                        _fileMapping.Dispose();
                        _fileMapping = null;
                        if (_session.TestHandlesClosedInternal)
                        {
                            _logger.WriteLine("Testing that file mapping is closed");
                            string fileMappingName = ConsoleMapping + _instanceName;
                            SafeFileHandle fileMapping = CreateFileMapping(fileMappingName);
                            if (Marshal.GetLastWin32Error() == UnsafeNativeMethods.ERROR_ALREADY_EXISTS)
                            {
                                _logger.WriteLine("Exception: File mapping {0} was not closed yet", fileMappingName);
                            }
                            if (!fileMapping.IsInvalid)
                            {
                                fileMapping.Dispose();
                            }
                        }
                    }
                    if (_inputEvent != null)
                    {
                        _inputEvent.Close();
                        _inputEvent = null;
                    }
                    if (_job != null)
                    {
                        _job.Dispose();
                        _job = null;
                    }
                }
            }
        }

        private string GetExecutablePath()
        {
            using (_logger.CreateCallstack())
            {
                string executablePath = _session.ExecutablePath;
                string result;
                if (!string.IsNullOrEmpty(executablePath))
                {
                    if (!File.Exists(executablePath))
                    {
                        throw _logger.WriteException(new SessionLocalException(_session, $"{executablePath} does not exists."));
                    }
                    result = executablePath;
                }
                else
                {
                    result = FindExecutable(_session);
                }
                return result;
            }
        }

        // Should be moved to Session class
        internal static string FindExecutable(Session session)
        {
            Logger logger = session.Logger;
            string executablePath;
            List<string> paths = [];
            string assemblyPath = GetAssemblyPath(logger);
            // If the assembly is not loaded from a file, look to the path of the process execuable
            // (particularly useful for single-file bundles)
            // (also limited this way not to for example look into powershell.exe folder)
            string processPath = !string.IsNullOrEmpty(assemblyPath) ? null : Path.GetDirectoryName(Logger.GetProcessPath());
            if (!TryFindExecutableInPath(logger, paths, assemblyPath, out executablePath) &&
                !TryFindExecutableInPath(logger, paths, GetEntryAssemblyPath(logger), out executablePath) &&
                !TryFindExecutableInPath(logger, paths, processPath, out executablePath) &&
#if !NETSTANDARD
                !TryFindExecutableInPath(logger, paths, GetInstallationPath(RegistryHive.CurrentUser), out executablePath) &&
                !TryFindExecutableInPath(logger, paths, GetInstallationPath(RegistryHive.LocalMachine), out executablePath) &&
#endif
                !TryFindExecutableInPath(logger, paths, GetDefaultInstallationPath(), out executablePath))
            {
                string pathsStr = string.Join(", ", paths);
                string filename = ExeExecutableFileName;
                string message =
                    $"The {filename} executable was not found at any of the inspected locations ({pathsStr}). " +
                    $"You may use Session.ExecutablePath property to explicitly set path to {filename}.";
                throw logger.WriteException(new SessionLocalException(session, message));
            }

            return executablePath;
        }

        private static string GetDefaultInstallationPath()
        {
            string programFiles;
            if (IntPtr.Size == 8)
            {
                programFiles = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86);
            }
            else
            {
                programFiles = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
            }
            return Path.Combine(programFiles, "WinSCP");
        }

#if !NETSTANDARD
        private static string GetInstallationPath(RegistryHive hive)
        {
            RegistryKey baseKey = RegistryKey.OpenBaseKey(hive, RegistryView.Registry32);
            RegistryKey key = baseKey.OpenSubKey(@"Software\Microsoft\Windows\CurrentVersion\Uninstall\winscp3_is1");
            string result = (key != null) ? (string)key.GetValue("Inno Setup: App Path") : null;
            return result;
        }
#endif

        private static bool TryFindExecutableInPath(Logger logger, List<string> paths, string path, out string result)
        {
            if (string.IsNullOrEmpty(path))
            {
                result = null;
            }
            else if (paths.Contains(path, StringComparer.CurrentCultureIgnoreCase))
            {
                logger.WriteLine($"Already searched {path}");
                result = null;
            }
            else
            {
                paths.Add(path);
                string executablePath = Path.Combine(path, ExeExecutableFileName);
                if (File.Exists(executablePath))
                {
                    result = executablePath;
                    logger.WriteLine("Executable found in {0}", executablePath);
                }
                else
                {
                    result = null;
                    logger.WriteLine("Executable not found in {0}", executablePath);
                }
            }
            return (result != null);
        }

        private static string GetAssemblyPath(Logger logger)
        {
            return DoGetAssemblyPath(logger.GetAssemblyFilePath());
        }

        private static string GetEntryAssemblyPath(Logger logger)
        {
            return DoGetAssemblyPath(logger.GetEntryAssemblyFilePath());
        }

        private static string DoGetAssemblyPath(string codeBasePath)
        {
            string path = null;
            if (!string.IsNullOrEmpty(codeBasePath))
            {
                path = Path.GetDirectoryName(codeBasePath);
                Debug.Assert(path != null);
            }
            return path;
        }

        [DllImport("version.dll", CharSet = CharSet.Auto, SetLastError = true, BestFitMapping = false)]
        public static extern int GetFileVersionInfoSize(string lptstrFilename, out int handle);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr LoadLibraryEx(string lpFileName, IntPtr hReservedNull, uint dwFlags);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool FreeLibrary(IntPtr hModule);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern IntPtr FindResource(IntPtr hModule, string lpName, string lpType);

        [DllImport("kernel32.dll", SetLastError = true)]
        static extern uint SizeofResource(IntPtr hModule, IntPtr hResInfo);

        private string GetVersionStr(FileVersionInfo version)
        {
            return $"{version.FileVersion}, product {version.ProductName} version is {version.ProductVersion}";
        }

        private void CheckVersion(string exePath, FileVersionInfo assemblyVersion)
        {
            using (_logger.CreateCallstack())
            {
                if (assemblyVersion == null)
                {
                    _logger.WriteLine("Assembly version not known, cannot check version");
                }
                else if (assemblyVersion.ProductVersion == AssemblyConstants.UndefinedProductVersion)
                {
                    _logger.WriteLine("Undefined assembly version, cannot check version");
                }
                else
                {
                    DateTime dateTime = File.GetLastWriteTimeUtc(exePath);
                    _logger.WriteLine($"Timestamp of {exePath} is {dateTime}");

                    bool known;
                    var cacheKey = new Tuple<string, DateTime>(exePath, dateTime);
                    lock (_versionInfoCache)
                    {
                        known = _versionInfoCache.TryGetValue(cacheKey, out FileVersionInfo version);
                        if (known)
                        {
                            _logger.WriteLine(
                                $"Cached version of {exePath} is {GetVersionStr(version)}, and it was already deemed compatible");
                        }
                        else
                        {
                            _logger.WriteLine($"Executable version is not cached yet, cache size is {_versionInfoCache.Count}");
                        }
                    }
                    if (!known)
                    {
                        FileVersionInfo version = FileVersionInfo.GetVersionInfo(exePath);
                        _logger.WriteLine($"Version of {exePath} is {GetVersionStr(version)}");

                        bool incompatible = assemblyVersion.ProductVersion != version.ProductVersion;
                        Exception accessException = null;
                        if (incompatible || _logger.Logging)
                        {
                            try
                            {
                                using (File.OpenRead(exePath))
                                {
                                }
                                long size = new FileInfo(exePath).Length;
                                _logger.WriteLine($"Size of the executable file is {size}");

                                int verInfoSize = GetFileVersionInfoSize(exePath, out int handle);
                                if (verInfoSize == 0)
                                {
                                    throw new Exception($"Cannot retrieve {exePath} version info", new Win32Exception());
                                }
                                else
                                {
                                    _logger.WriteLine($"Size of the executable file version info is {verInfoSize}");
                                }
                            }
                            catch (Exception e)
                            {
                                _logger.WriteLine("Accessing executable file failed");
                                _logger.WriteException(e);
                                accessException = e;
                            }
                        }

                        if (_session.DisableVersionCheck)
                        {
                            _logger.WriteLine("Version check disabled (not recommended)");
                        }
                        else if (incompatible)
                        {
                            if (_logger.Logging)
                            {
                                try
                                {
                                    using (SHA256 SHA256 = SHA256.Create())
                                    using (FileStream stream = File.OpenRead(exePath))
                                    {
                                        string sha256 = string.Concat(Array.ConvertAll(SHA256.ComputeHash(stream), b => b.ToString("x2")));
                                        _logger.WriteLine($"SHA-256 of the executable file is {sha256}");
                                    }
                                }
                                catch (Exception e)
                                {
                                    _logger.WriteLine("Calculating SHA-256 of the executable file failed");
                                    _logger.WriteException(e);
                                }

                                try
                                {
                                    IntPtr library = LoadLibraryEx(exePath, IntPtr.Zero, 0x00000002); // LOAD_LIBRARY_AS_DATAFILE
                                    if (library == IntPtr.Zero)
                                    {
                                        _logger.WriteLine("Cannot load");
                                        _logger.WriteException(new Win32Exception());
                                    }
                                    else
                                    {
                                        IntPtr resource = FindResource(library, "#1", "#16");
                                        if (resource == IntPtr.Zero)
                                        {
                                            _logger.WriteLine("Cannot find version resource");
                                            _logger.WriteException(new Win32Exception());
                                        }
                                        else
                                        {
                                            uint resourceSize = SizeofResource(library, resource);
                                            if (resourceSize == 0)
                                            {
                                                _logger.WriteLine("Cannot find size of version resource");
                                                _logger.WriteException(new Win32Exception());
                                            }
                                            else
                                            {
                                                _logger.WriteLine($"Version resource size is {resourceSize}");
                                            }
                                        }
                                        FreeLibrary(library);
                                    }
                                }
                                catch (Exception e)
                                {
                                    _logger.WriteLine("Querying version resource failed");
                                    _logger.WriteException(e);
                                }
                            }

                            string message;
                            if (string.IsNullOrEmpty(version.ProductVersion) && (accessException != null))
                            {
                                message = $"Cannot use {exePath}";
                            }
                            else
                            {
                                message =
                                    $"The version of {exePath} ({version.ProductVersion}) does not match " +
                                    $"version of this assembly {_logger.GetAssemblyFilePath()} ({assemblyVersion.ProductVersion}).";
                            }
                            throw _logger.WriteException(new SessionLocalException(_session, message, accessException));
                        }
                        else
                        {
                            lock (_versionInfoCache)
                            {
                                _logger.WriteLine("Caching executable version");
                                _versionInfoCache[cacheKey] = version;
                            }
                        }
                    }
                }
            }
        }

        public void WriteStatus()
        {
            _logger.WriteLine("{0} - exists [{1}]", ExecutablePath, File.Exists(ExecutablePath));
        }

        public void RequestCallstack()
        {
            using (_logger.CreateCallstack())
            {
                lock (_lock)
                {
                    if (_process == null)
                    {
                        _logger.WriteLine("Process is closed already");
                    }
                    else
                    {
                        try
                        {
                            string eventName = string.Format(CultureInfo.InvariantCulture, "WinSCPCallstack{0}", _process.Id);
                            using (EventWaitHandle ev = EventWaitHandle.OpenExisting(eventName))
                            {
                                _logger.WriteLine("Setting event {0}", eventName);
                                ev.Set();
                                string callstackFileName = string.Format(CultureInfo.InvariantCulture, "{0}.txt", eventName);
                                string callstackPath = Path.Combine(Path.GetTempPath(), callstackFileName);
                                int timeout = 2000;
                                while (!File.Exists(callstackPath))
                                {
                                    if (timeout < 0)
                                    {
                                        string message = string.Format(CultureInfo.CurrentCulture, "Timeout waiting for callstack file {0} to be created ", callstackPath);
                                        throw new TimeoutException(message);
                                    }

                                    int step = 50;
                                    timeout -= 50;
                                    Thread.Sleep(step);
                                }
                                _logger.WriteLine("Callstack file {0} has been created", callstackPath);
                                // allow writting to be finished
                                Thread.Sleep(100);
                                _logger.WriteLine(File.ReadAllText(callstackPath));
                                File.Delete(callstackPath);
                            }
                        }
                        catch (Exception e)
                        {
                            _logger.WriteException(e);
                        }
                    }
                }
            }
        }

        public void Cancel()
        {
            _cancel = true;
        }

        private const int MaxAttempts = 10;
        private const string ConsoleMapping = "WinSCPConsoleMapping";
        private const string ConsoleEventRequest = "WinSCPConsoleEventRequest";
        private const string ConsoleEventResponse = "WinSCPConsoleEventResponse";
        private const string ConsoleEventCancel = "WinSCPConsoleEventCancel";
        private const string ConsoleJob = "WinSCPConsoleJob";
        private const string ExeExecutableFileName = "winscp.exe";

        private Process _process;
        private readonly object _lock = new object();
        private readonly Logger _logger;
        private readonly Session _session;
        private EventWaitHandle _requestEvent;
        private EventWaitHandle _responseEvent;
        private EventWaitHandle _cancelEvent;
        private SafeFileHandle _fileMapping;
        private string _instanceName;
        private Thread _thread;
        private bool _abort;
        private string _lastFromBeginning;
        private string _incompleteLine;
        private readonly List<string> _input = new List<string>();
        private readonly List<string> _log = new List<string>();
        private AutoResetEvent _inputEvent = new AutoResetEvent(false);
        private Job _job;
        private bool _cancel;
        private static readonly Dictionary<Tuple<string, DateTime>, FileVersionInfo> _versionInfoCache = new Dictionary<Tuple<string, DateTime>, FileVersionInfo>();
    }
}
