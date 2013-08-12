using System;
using System.Globalization;
using System.IO;
using System.Threading;
using System.Reflection;
using System.Diagnostics;
using System.Collections.Generic;

namespace WinSCP
{
    internal class Logger : IDisposable
    {
        public string LogPath { get { return _logPath; } set { SetLogPath(value); } }
        public bool Logging { get { return (_writter != null) && _writter.BaseStream.CanWrite; } }

        public string GetAssemblyFilePath()
        {
            Assembly assembly = Assembly.GetExecutingAssembly();
            string path = null;
            string codeBase = assembly.CodeBase;
            string location = assembly.Location;
            // cannot use Uri.UnescapeDataString, because it treats some characters valid in
            // local path (like #) specially
            const string protocol = "file:///";
            if (codeBase.StartsWith(protocol, StringComparison.OrdinalIgnoreCase))
            {
                path =
                    codeBase.Substring(protocol.Length).Replace('/', '\\');
            }

            if ((path == null) || !File.Exists(path))
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

        public void WriteLine(string message)
        {
            lock (_logLock)
            {
                if (Logging)
                {
                    DoWriteLine(message);
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

        private static int GetThread()
        {
            return Thread.CurrentThread.ManagedThreadId;
        }

        public void Indent()
        {
            lock (_logLock)
            {
                int threadId = GetThread();
                int indent;
                if (!_indents.TryGetValue(threadId, out indent))
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
                    WriteCounters();
                    WriteProcesses();
                    _writter.Dispose();
                    _writter = null;
                }

                foreach (PerformanceCounter counter in _performanceCounters)
                {
                    counter.Dispose();
                }
            }
        }

        public void WriteCounters()
        {
            if (Logging)
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

        public void WriteProcesses()
        {
            if (Logging)
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

        public Callstack CreateCallstack()
        {
            return new Callstack(this);
        }

        public Callstack CreateCallstackAndLock()
        {
            return new CallstackAndLock(this, _lock);
        }

        private int GetIndent()
        {
            int indent;
            if (!_indents.TryGetValue(GetThread(), out indent))
            {
                indent = 0;
            }
            return indent;
        }

        private void DoWriteLine(string message)
        {
            int indent = GetIndent();

            string s =
                string.Format(CultureInfo.InvariantCulture, "[{0:yyyy-MM-dd HH:mm:ss.fffZ}] [{1:x4}] {2}{3}",
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
                        CreateCounters();
                    }
                }
            }
        }

        private void WriteEnvironmentInfo()
        {
            string path = GetAssemblyFilePath();
            FileVersionInfo version = string.IsNullOrEmpty(path) ? null : FileVersionInfo.GetVersionInfo(path);
            Assembly assembly = Assembly.GetExecutingAssembly();
            WriteLine("Executing Assembly: {0}; Path: {1}; Location: {2}; Product: {3}", assembly, path, assembly.Location, ((version != null) ? version.ProductVersion : "unknown"));
            WriteLine("Entry Assembly: {0}", Assembly.GetEntryAssembly());
            WriteLine("Operating system: {0}", Environment.OSVersion);
            WriteLine("User: {0}@{1}@{2}; Interactive: {3}", Environment.UserName, Environment.UserDomainName, Environment.MachineName, Environment.UserInteractive);
            WriteLine("Runtime: {0}", Environment.Version);
            WriteLine("Console encoding: Input: {0} ({1}); Output: {2} ({3})", Console.InputEncoding.EncodingName, Console.InputEncoding.CodePage, Console.OutputEncoding.EncodingName, Console.OutputEncoding.CodePage);
            WriteLine("Working directory: {0}", Environment.CurrentDirectory);
        }

        private StreamWriter _writter;
        private string _logPath;
        private readonly Dictionary<int, int> _indents = new Dictionary<int, int>();
        private readonly object _logLock = new object();
        private readonly Lock _lock = new Lock();
        private List<PerformanceCounter> _performanceCounters = new List<PerformanceCounter>();
    }
}
