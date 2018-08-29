using System;
using System.Globalization;
using System.IO;
using System.Threading;

namespace WinSCP.Internal
{
    internal class FileLogWriter : ILogWriter
    {

        public bool Enabled() => _writer.BaseStream.CanWrite;


        private readonly StreamWriter _writer;

        /// <inheritdoc />
        public FileLogWriter(string logPath)
        {
            _writer = File.CreateText(logPath);
            _writer.AutoFlush = true;
        }


        public void WriteLine(int indent, string message)
        {
            string s =
                string.Format(CultureInfo.InvariantCulture, "[{0:yyyy-MM-dd HH:mm:ss.fffZ}] [{1:x4}] {2}{3}",
                    DateTime.Now, Thread.CurrentThread.ManagedThreadId,
                    (indent > 0 ? new string(' ', indent * 2) : string.Empty), message);
            _writer.WriteLine(s);
        }

        #region Implementation of IDisposable

        /// <inheritdoc />
        void IDisposable.Dispose()
        {
            _writer.Dispose();
        }

        #endregion
    }
}