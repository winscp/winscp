using System;
using System.Diagnostics;
using System.IO;
using System.Xml;
using System.Text;
using System.Threading;

namespace WinSCP
{
    internal class SessionLogReader : CustomLogReader
    {
        public SessionLogReader(Session session) :
            base(session)
        {
            _position = 0;
        }

        public void SetTimeouted()
        {
            _timeouted = true;
        }

        public override void Dispose()
        {
            using (Session.Logger.CreateCallstack())
            {
                LogContents();
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

            if (_reader != null)
            {
                ((IDisposable)_reader).Dispose();
                _reader = null;
            }
        }

        public override bool Read(LogReadFlags flags)
        {
            using (Session.Logger.CreateCallstack())
            {
                bool result;
                if (_timeouted)
                {
                    Session.Logger.WriteLine("Not reading, session has timed out");
                    result = false;
                }
                else
                {
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
                                throw Session.Logger.WriteException(e);
                            }
                        }
                    }
                    while (retry);
                }

                return result;
            }
        }

        private bool DoRead()
        {
            int interval = 50;
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

                    // check if the the root cause was session abort
                    Session.CheckForTimeout();
                    LogContents();
                    string message = "Error parsing session log file";
                    // This is possibly a race condition, as we may not have processed the event with the error yet
                    // The ExeSessionProcess loops every 100ms
                    Thread.Sleep(200);
                    string s = Session.GetErrorOutputMessage();
                    if (!string.IsNullOrEmpty(s))
                    {
                        message += " - " + s;
                    }
                    throw Session.Logger.WriteException(new SessionLocalException(Session, message, e));
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
                    using (StreamReader reader = new StreamReader(new FileStream(Session.XmlLogPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite), Encoding.UTF8))
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
            if (_closed)
            {
                throw Session.Logger.WriteException(new InvalidOperationException("Log was closed already"));
            }

            try
            {
                Session.Logger.WriteLine("Opening log without write sharing");
                // First try to open file without write sharing.
                // This fails, if WinSCP is still writing to the log file.
                // This is done only as a way to detect that log file is not complete yet.
                _stream = new PatientFileStream(Session, Session.XmlLogPath, FileMode.Open, FileAccess.Read, FileShare.Read);
                _closed = true;
                LogContents();
            }
            catch (IOException)
            {
                Session.Logger.WriteLine("Opening log with write sharing");
                // If log file is still being written to, open it with write sharing
                _stream = new PatientFileStream(Session, Session.XmlLogPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                _closed = false;
            }

            Session.Logger.WriteLine("Log opened");

            // Allow control characters in log
            var settings = new XmlReaderSettings() { CheckCharacters = false };
            _reader = XmlReader.Create(_stream, settings);

            int skip = _position;
            Session.Logger.WriteLine("Skipping {0} nodes", skip);
            while (skip > 0)
            {
                if (!_reader.Read())
                {
                    throw Session.Logger.WriteException(new SessionLocalException(Session, "Read less nodes than in previous log parsing"));
                }
                --skip;
            }
        }

        internal override XmlReader Reader
        {
            get
            {
                if (_reader == null)
                {
                    throw Session.Logger.WriteException(new SessionLocalException(Session, "Reading has not commenced yet"));
                }
                return _reader;
            }
        }

        private int _position;
        private XmlReader _reader;
        private PatientFileStream _stream;
        private bool _closed;
        private string _logged;
        private bool _timeouted;
    }
}
