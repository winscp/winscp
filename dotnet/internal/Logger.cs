using System;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Reflection;
using System.Diagnostics;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.InteropServices;

namespace WinSCP
{
    internal class Logger : IDisposable
    {
        public string LogPath { get { return _logPath; } set { SetLogPath(value); } }
        public int LogLevel { get { return _logLevel; } set { SetLogLevel(value); } }
        public bool Logging { get { return (_writter != null) && _writter.BaseStream.CanWrite; } }
        public Lock Lock { get; } = new Lock();

        public string GetAssemblyFilePath()
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
            return DoGetAssemblyFilePath(assembly);
        }

        public string GetEntryAssemblyFilePath()
        {
            Assembly assembly = Assembly.GetEntryAssembly();
            return (assembly != null) ? DoGetAssemblyFilePath(assembly) : null;
        }

        private string TryGetCodeBase(Assembly assembly, out Exception e)
        {
            string result;
            try
            {
                e = null;
                result = assembly.CodeBase;
            }
            // CodeBase is not supported on assemblies loaded from a single-file bundle
            catch (NotSupportedException ex)
            {
                e = ex;
                result = null;
            }
            return result;
        }

        private string DoGetAssemblyFilePath(Assembly assembly)
        {
            string path = null;

            // https://learn.microsoft.com/en-us/archive/blogs/suzcook/assembly-codebase-vs-assembly-location
            // The CodeBase is a URL to the place where the file was found,
            // while the Location is the path from where it was actually loaded.
            // For example, if the assembly was downloaded from the internet, its CodeBase may start with "http://",
            // but its Location may start with "C:\".
            // If the file was shadow copied, the Location would be the path to the copy of the file in the shadow-copy dir.

            // It's also good to know that the CodeBase is not guaranteed to be set for assemblies in the GAC.
            // Location will always be set for assemblies loaded from disk, however.
            string codeBase = TryGetCodeBase(assembly, out Exception e);
            if (codeBase == null)
            {
                if (e != null)
                {
                    WriteLine($"CodeBase not supported: {e.Message}");
                }
                codeBase = string.Empty;
            }

            string location = assembly.Location;

            // cannot use Uri.UnescapeDataString, because it treats some characters valid in
            // local path (like #) specially
            const string protocol = "file://";
            if (codeBase.StartsWith(protocol, StringComparison.OrdinalIgnoreCase))
            {
                path = codeBase.Substring(protocol.Length).Replace('/', '\\');
                if (!string.IsNullOrEmpty(path))
                {
                    if (path[0] == '\\')
                    {
                        path = path.Substring(1, path.Length - 1);
                    }
                    else
                    {
                        // UNC path
                        path = @"\\" + path;
                    }
                }
            }

            if (string.IsNullOrEmpty(path) || !File.Exists(path))
            {
                if (File.Exists(location))
                {
                    path = location;
                }
                else
                {
                    WriteLine(
                        string.Format(
                            CultureInfo.CurrentCulture,
                            "Cannot locate path of assembly [{0}] neither from its code base [{1}], nor from its location [{2}]",
                            assembly, codeBase, location));
                    path = null;
                }
            }

            return path;
        }

#if !NETSTANDARD
        private void CreateCounters()
        {
            try
            {
                PerformanceCounterCategory[] categories = PerformanceCounterCategory.GetCategories();
                foreach (PerformanceCounterCategory category in categories)
                {
                    if (category.CategoryName == "Processor")
                    {
                        string[] instances = category.GetInstanceNames();
                        foreach (string instance in instances)
                        {
                            AddCounter(new PerformanceCounter(category.CategoryName, "% Processor Time", instance));
                        }
                    }
                }

                AddCounter(new PerformanceCounter("Memory", "Available KBytes"));
            }
            catch (UnauthorizedAccessException)
            {
                WriteLine("Not authorized to get counters");
            }
            catch (Exception e)
            {
                WriteLine("Error getting counters: {0}", e);
            }
        }

        private void AddCounter(PerformanceCounter counter)
        {
            counter.NextValue();
            _performanceCounters.Add(counter);
        }
#endif

        public void WriteLine(string line)
        {
            lock (_logLock)
            {
                if (Logging)
                {
                    DoWriteLine(line);
                }
            }
        }

        public void WriteLine(string format, params object[] args)
        {
            lock (_logLock)
            {
                if (Logging)
                {
                    DoWriteLine(string.Format(CultureInfo.CurrentCulture, format, args));
                }
            }
        }

        public void WriteLineLevel(int level, string line)
        {
            if (LogLevel >= level)
            {
                WriteLine(line);
            }
        }

        public void WriteLineLevel(int level, string line, params object[] args)
        {
            if (LogLevel >= level)
            {
                WriteLine(line, args);
            }
        }

        private static int GetThread()
        {
            return Thread.CurrentThread.ManagedThreadId;
        }

        public void Indent()
        {
            lock (_logLock)
            {
                int threadId = GetThread();
                if (!_indents.TryGetValue(threadId, out int indent))
                {
                    indent = 0;
                }
                _indents[threadId] = indent + 1;
            }
        }

        public void Unindent()
        {
            lock (_logLock)
            {
                int threadId = GetThread();
                _indents[threadId]--;
            }
        }

        public void Dispose()
        {
            lock (_logLock)
            {
                if (Logging)
                {
#if !NETSTANDARD
                    WriteCounters();
#endif
                    WriteProcesses();
                    _writter.Dispose();
                    _writter = null;
                }

#if !NETSTANDARD
                foreach (PerformanceCounter counter in _performanceCounters)
                {
                    counter.Dispose();
                }
#endif
            }
        }

#if !NETSTANDARD
        public void WriteCounters()
        {
            if (Logging && (LogLevel >= 1))
            {
                try
                {
                    foreach (PerformanceCounter counter in _performanceCounters)
                    {
                        WriteLine("{0}{1}{2} = [{3}]",
                            counter.CounterName,
                            (string.IsNullOrEmpty(counter.InstanceName) ? string.Empty : "/"),
                            counter.InstanceName,
                            counter.NextValue());
                    }
                }
                catch (Exception e)
                {
                    WriteLine("Error reading counters: {0}", e);
                }
            }
        }
#endif

        public void WriteProcesses()
        {
            if (Logging && (LogLevel >= 1))
            {
                try
                {
                    Process[] processes = Process.GetProcesses();

                    foreach (Process process in processes)
                    {
                        WriteLine("{0}:{1} - {2} - {3}", process.Id, process.ProcessName, GetProcessStartTime(process), GetTotalProcessorTime(process));
                    }
                }
                catch (Exception e)
                {
                    WriteLine("Error logging processes: {0}", e);
                }
            }
        }

        private static object GetProcessStartTime(Process process)
        {
            try
            {
                return process.StartTime;
            }
            catch
            {
                return "???";
            }
        }

        private static object GetTotalProcessorTime(Process process)
        {
            try
            {
                return process.TotalProcessorTime;
            }
            catch
            {
                return "???";
            }
        }

        public Callstack CreateCallstack(object token = null)
        {
            return new Callstack(this, token);
        }

        public CallstackAndLock CreateCallstackAndLock()
        {
            return new CallstackAndLock(this, Lock);
        }

        public Exception WriteException(Exception e)
        {
            lock (_logLock)
            {
                if (Logging)
                {
                    DoWriteLine(string.Format(CultureInfo.CurrentCulture, "Exception: {0}", e));
                    if (LogLevel >= 1)
                    {
                        DoWriteLine(new StackTrace().ToString());
                    }
                }
            }
            return e;
        }

        private int GetIndent()
        {
            if (!_indents.TryGetValue(GetThread(), out int indent))
            {
                indent = 0;
            }
            return indent;
        }

        private void DoWriteLine(string message)
        {
            int indent = GetIndent();

            string s =
                string.Format(CultureInfo.InvariantCulture, "[{0:yyyy-MM-dd HH:mm:ss.fff}] [{1:x4}] {2}{3}",
                DateTime.Now, Thread.CurrentThread.ManagedThreadId,
                (indent > 0 ? new string(' ', indent * 2) : string.Empty), message);
            _writter.WriteLine(s);
        }

        private void SetLogPath(string value)
        {
            lock (_logLock)
            {
                if (_logPath != value)
                {
                    Dispose();
                    _logPath = value;
                    if (!string.IsNullOrEmpty(_logPath))
                    {
                        _writter = File.CreateText(_logPath);
                        _writter.AutoFlush = true;
                        WriteEnvironmentInfo();
#if !NETSTANDARD
                        if (_logLevel >= 1)
                        {
                            CreateCounters();
                        }
#endif
                    }
                }
            }
        }

        private void WriteEnvironmentInfo()
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
#if NETSTANDARD
            WriteLine(".NET Standard build");
#else
            WriteLine(".NET Framework build");
#endif
            WriteLine("Executing assembly: {0}", assembly);
            string codeBase =
                TryGetCodeBase(assembly, out Exception e) ?? e?.Message ?? "unknown";
            WriteLine("Executing assembly codebase: {0}", codeBase);
            WriteLine("Executing assembly location: {0}", (assembly.Location ?? "unknown"));
            Assembly entryAssembly = Assembly.GetEntryAssembly();
            WriteLine("Entry Assembly: {0}", (entryAssembly != null ? entryAssembly.ToString() : "unmanaged"));
            WriteLine("Operating system: {0}", Environment.OSVersion);
#if NETSTANDARD
            WriteLine("Operating system information: {0} {1} {2}", RuntimeInformation.OSDescription, RuntimeInformation.OSArchitecture, RuntimeInformation.ProcessArchitecture);
#endif
            WriteLine("Bitness: {0}", Environment.Is64BitProcess ? "64-bit" : "32-bit");
            TimeSpan offset = TimeZoneInfo.Local.GetUtcOffset(DateTime.UtcNow);
            WriteLine(
                "Timezone: {0}; {1}",
                ((offset > TimeSpan.Zero ? "+" : (offset < TimeSpan.Zero ? "-" : string.Empty)) + offset.ToString("hh\\:mm")),
                (TimeZoneInfo.Local.IsDaylightSavingTime(DateTime.Now) ? TimeZoneInfo.Local.DaylightName : TimeZoneInfo.Local.StandardName));
            WriteLine("User: {0}@{1}@{2}; Interactive: {3}", Environment.UserName, Environment.UserDomainName, Environment.MachineName, Environment.UserInteractive);
            WriteLine("Runtime: {0}", Environment.Version);
#if NETSTANDARD
            WriteLine("Framework description: {0}", RuntimeInformation.FrameworkDescription);
#endif
            WriteLine("Console encoding: Input: {0} ({1}); Output: {2} ({3})", Console.InputEncoding.EncodingName, Console.InputEncoding.CodePage, Console.OutputEncoding.EncodingName, Console.OutputEncoding.CodePage);
            WriteLine("Working directory: {0}", Environment.CurrentDirectory);
            string path = GetAssemblyFilePath();
            FileVersionInfo version = string.IsNullOrEmpty(path) ? null : FileVersionInfo.GetVersionInfo(path);
            WriteLine("Assembly path: {0}", path);
            WriteLine("Assembly product version: {0}", ((version != null) ? version.ProductVersion : "unknown"));
            if (Assembly.GetEntryAssembly() != null)
            {
                WriteLine("Entry assembly path: {0}", GetEntryAssemblyFilePath());
            }
            WriteLine($"Process path: {GetProcessPath()}");
        }

        public static string GetProcessPath()
        {
            // Can be replaced with Environment.ProcessPath in .NET 6 and newer
            return Process.GetCurrentProcess().MainModule?.FileName;
        }

        public static string LastWin32ErrorMessage()
        {
            return new Win32Exception(Marshal.GetLastWin32Error()).Message;
        }

        private void SetLogLevel(int value)
        {
            if ((value < -1) || (value > 2))
            {
                throw WriteException(new ArgumentOutOfRangeException(string.Format(CultureInfo.CurrentCulture, "Logging level has to be in range -1 to 2")));
            }
            _logLevel = value;
        }

        private StreamWriter _writter;
        private string _logPath;
        private readonly Dictionary<int, int> _indents = new Dictionary<int, int>();
        private readonly object _logLock = new object();
#if !NETSTANDARD
        private readonly List<PerformanceCounter> _performanceCounters = new List<PerformanceCounter>();
#endif
        private int _logLevel;
    }
}
