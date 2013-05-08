using System;
using System.Diagnostics;
using System.IO;
using System.Xml;
using System.Text;

namespace WinSCP
{
    internal class SessionLogReader : CustomLogReader
    {
        public SessionLogReader(Session session) :
            base(session)
        {
            _position = 0;
        }

        public override void Dispose()
        {
            using (Session.Logger.CreateCallstack())
            {
                Cleanup();
            }
            base.Dispose();
        }

        private void Cleanup()
        {
            if (_stream != null)
            {
                Session.Logger.WriteLine("Closing log");
                _stream.Dispose();
                _stream = null;
            }

            ((IDisposable)_reader).Dispose();
            _reader = null;
        }

        public override bool Read(LogReadFlags flags)
        {
            using (Session.Logger.CreateCallstack())
            {
                bool result;
                bool retry;

                do
                {
                    result = DoRead();

                    retry = false;

                    if (result &&
                        IsNonEmptyElement("failure"))
                    {
                        SessionRemoteException e = SessionRemoteException.ReadFailure(this);

                        Session.RaiseFailed(e);

                        if ((flags & LogReadFlags.ThrowFailures) == 0)
                        {
                            retry = true;
                        }
                        else
                        {
                            throw e;
                        }
                    }
                }
                while (retry);

                return result;
            }
        }

        private bool DoRead()
        {
            int interval = 125;
            bool result;

            do
            {
                if (_reader == null)
                {
                    OpenLog();
                }

                Debug.Assert(_reader != null);

                try
                {
                    result = _reader.Read();
                    if (result)
                    {
                        ++_position;
                        Session.Logger.WriteLine("Read node {0}: {1} {2}{3}{4}",
                            _position, _reader.NodeType, _reader.Name,
                            (_reader.HasValue && !string.IsNullOrEmpty(_reader.Name) && !string.IsNullOrEmpty(_reader.Value) ? "=" : string.Empty),
                            _reader.Value);
                        Session.GotOutput();
                    }
                    else
                    {
                        Session.Logger.WriteLine("Cannot read");

                        if (!_closed)
                        {
                            // this should not happen as when the log is not closed,
                            // we should get XmlException on reaching the end
                            _closed = true;
                            Cleanup();
                        }

                        Session.CheckForTimeout();
                    }
                }
                catch (XmlException e)
                {
                    Cleanup();

                    if (!_closed)
                    {
                        // If log was not closed, it is likely the XML is not well-formed
                        // (at least top-level <session/> tag is not closed),
                        // so we swallow the parsing errors here.
                        Session.Logger.WriteLine("Error parsing session log file, but it is not closed yet, will retry");
                        result = false;
                    }
                    else
                    {
                        // check if the the root cause was session abort
                        Session.CheckForTimeout();
                        LogContents();
                        throw new SessionLocalException(Session, "Error parsing session log file", e);
                    }
                }

                if (!result && !_closed)
                {
                    Session.Logger.WriteLine("Waiting for log update and dispatching events for {0}", interval);
                    Session.DispatchEvents(interval);
                    if (interval < 500)
                    {
                        interval *= 2;
                    }
                }
            }
            while (!result && !_closed);

            if (result)
            {
                LogContents();
            }

            return result;
        }

        private void LogContents()
        {
            if (Session.Logger.Logging)
            {
                try
                {
                    // alterative to File.ReadAllText with write-sharing
                    // (note that the StreamReader disposes the Stream)
                    using (StreamReader reader = new StreamReader(OpenLogFileWithWriteSharing(), Encoding.UTF8))
                    {
                        string contents = reader.ReadToEnd();
                        if ((_logged == null) || (_logged != contents))
                        {
                            Session.Logger.WriteLine("Log contents:\n{0}", contents);
                            _logged = contents;
                        }
                        else
                        {
                            Session.Logger.WriteLine("Log contents has not changed");
                        }
                    }
                }
                catch (Exception e)
                {
                    Session.Logger.WriteLine("Error logging log contents [{0}]", e.Message);
                }
            }
        }

        private void OpenLog()
        {
            Session.Logger.WriteLine("Opening log");

            if (_closed)
            {
                throw new InvalidOperationException("Log was closed already");
            }

            try
            {
                // First try to open file without write sharing.
                // This fails, if WinSCP is still writing to the log file.
                // This is done only as a way to detect that log file is not complete yet.
                _stream = File.Open(Session.XmlLogPath, FileMode.Open, FileAccess.Read, FileShare.Read);
                _closed = true;
                Session.Logger.WriteLine("Log opened without write sharing");
                LogContents();
            }
            catch (IOException)
            {
                // If log file is still being written to, open it with write sharing
                _stream = OpenLogFileWithWriteSharing();
                _closed = false;
                Session.Logger.WriteLine("Log opened with write sharing");
            }

            _reader = XmlReader.Create(_stream);

            int skip = _position;
            Session.Logger.WriteLine("Skipping {0} nodes", skip);
            while (skip > 0)
            {
                if (!_reader.Read())
                {
                    throw new SessionLocalException(Session, "Read less nodes than in previous log parsing");
                }
                --skip;
            }
        }

        private FileStream OpenLogFileWithWriteSharing()
        {
            return File.Open(Session.XmlLogPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
        }

        internal override XmlReader Reader
        {
            get
            {
                if (_reader == null)
                {
                    throw new SessionLocalException(Session, "Reading has not commenced yet");
                }
                return _reader;
            }
        }

        private int _position;
        private XmlReader _reader;
        private FileStream _stream;
        private bool _closed;
        private string _logged;
    }
}
