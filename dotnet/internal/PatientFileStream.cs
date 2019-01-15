using System.IO;

namespace WinSCP
{
    internal class PatientFileStream : Stream
    {
        public PatientFileStream(Session session, string path, FileMode mode, FileAccess access, FileShare share)
        {
            _stream = new FileStream(path, mode, access, share);
            _session = session;
        }

        override public void Close()
        {
            if (_stream != null)
            {
                _stream.Close();
                _stream = null;
            }

            base.Close();
        }

        private const int InitialInterval = 50;

        public override int Read(byte[] array, int offset, int count)
        {
            int result;

            int interval = InitialInterval;
            do
            {
                result = _stream.Read(array, offset, count);
                if (result == 0)
                {
                    Wait(ref interval);
                }
                else
                {
                    _session.Logger.WriteLineLevel(2, "Read {0} bytes from log", result);
                }
            }
            // We always want to return something.
            // No attempt to detect real end of file is needed,
            // as we should not try to read beyond the final closing tag
            while (result == 0);

            return result;
        }

        private void Wait(ref int interval)
        {
            _session.Logger.WriteLine("Waiting for log update and dispatching events for {0}", interval);
            _session.DispatchEvents(interval);
            _session.CheckForTimeout();
            if (interval < 500)
            {
                interval *= 2;
            }
        }

        public override void Flush()
        {
            throw _session.Logger.WriteException(new System.NotImplementedException());
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            throw _session.Logger.WriteException(new System.NotImplementedException());
        }

        public override void SetLength(long value)
        {
            throw _session.Logger.WriteException(new System.NotImplementedException());
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            throw _session.Logger.WriteException(new System.NotImplementedException());
        }

        public override bool CanRead
        {
            get
            {
                bool result = _stream.CanRead;
                _session.Logger.WriteLineLevel(2, "Can read = {0}", result);
                return result;
            }
        }

        public override bool CanSeek
        {
            get
            {
                bool result = _stream.CanSeek;
                _session.Logger.WriteLineLevel(2, "Can seek = {0}", result);
                return result;
            }
        }

        public override bool CanWrite => false;

        public override long Length
        {
            get
            {
                long result = _stream.Length;

                if (result == 0)
                {
                    _session.Logger.WriteLineLevel(2, "File is empty yet, waiting", result);

                    int interval = InitialInterval;
                    do
                    {
                        result = _stream.Length;
                        if (result == 0)
                        {
                            Wait(ref interval);
                        }
                        else
                        {
                            _session.Logger.WriteLineLevel(2, "File length = {0}", result);
                        }
                    }
                    while (result == 0);
                }

                return result;
            }
        }

        public override long Position
        {
            get
            {
                long result = _stream.Length;
                _session.Logger.WriteLineLevel(2, "File position = {0}", result);
                return result;
            }

            set
            {
                _session.Logger.WriteLineLevel(2, "Setting file position to {0}", value);
                _stream.Position = value;
            }
        }

        private FileStream _stream;
        private Session _session;
    }
}
