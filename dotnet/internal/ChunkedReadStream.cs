using System;
using System.Globalization;
using System.IO;

namespace WinSCP
{
    internal class ChunkedReadStream : Stream
    {
        public ChunkedReadStream(Stream baseStream, Action onDispose)
        {
            _baseStream = baseStream;
            _onDispose = onDispose;
            _remaining = 0;
            _eof = false;
        }

        public override bool CanRead => !_eof;

        public override bool CanSeek => false;

        public override bool CanWrite => false;

        public override long Length => throw new NotImplementedException();

        public override long Position { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }

        public override void Flush()
        {
            throw new NotImplementedException();
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            int result;
            if (_eof)
            {
                result = 0;
            }
            else
            {
                if (_remaining == 0)
                {
                    string lenStr = string.Empty;
                    while (!lenStr.EndsWith("\r\n"))
                    {
                        if (lenStr.Length > 64)
                        {
                            throw new Exception("Too long chunk length line");
                        }
                        int b = _baseStream.ReadByte();
                        if (b < 0)
                        {
                            throw new Exception("End of stream reached while reading chunk length line");
                        }
                        lenStr += (char)b;
                    }
                    lenStr = lenStr.Trim();
                    _remaining = int.Parse(lenStr, NumberStyles.HexNumber);
                    if (_remaining == 0)
                    {
                        _eof = true;
                    }
                }

                // Not sure if it is ok to call Read with 0
                if (_remaining > 0)
                {
                    int read = Math.Min(count, _remaining);
                    result = _baseStream.Read(buffer, offset, read);
                    _remaining -= result;
                }
                else
                {
                    result = 0;
                }

                if (_remaining == 0)
                {
                    int cr = _baseStream.ReadByte();
                    if (cr != '\r')
                    {
                        throw new Exception("Expected CR");
                    }
                    int lf = _baseStream.ReadByte();
                    if (lf != '\n')
                    {
                        throw new Exception("Expected LF");
                    }
                }

                if (_eof)
                {
                    // Throw any pending exception asap, not only once the stream is closed.
                    // Also releases the lock.
                    Closed();
                }
            }

            return result;
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            throw new NotImplementedException();
        }

        public override void SetLength(long value)
        {
            throw new NotImplementedException();
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            throw new NotImplementedException();
        }

        protected override void Dispose(bool disposing)
        {
            try
            {
                // Have to consume the rest of the buffered download data, otherwise we could not continue with other downloads
                while (!_eof)
                {
                    byte[] buf = new byte[10240];
                    Read(buf, 0, buf.Length);
                }
                base.Dispose(disposing);
            }
            finally
            {
                Closed();
            }
        }

        private void Closed()
        {
            Action onDispose = _onDispose;
            _onDispose = null;
            onDispose?.Invoke();
        }

        private Stream _baseStream;
        private Action _onDispose;
        private int _remaining;
        private bool _eof;
    }
}
