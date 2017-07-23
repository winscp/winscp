using System.IO;

namespace WinSCP
{
    internal class PatientFileStream : FileStream
    {
        public PatientFileStream(Session session, string path, FileMode mode, FileAccess access, FileShare share) :
            base(path, mode, access, share)
        {
            _session = session;
        }

        public override int Read(byte[] array, int offset, int count)
        {
            int result;

            int interval = 50;
            do
            {
                result = base.Read(array, offset, count);
                if (result == 0)
                {
                    _session.Logger.WriteLine("Waiting for log update and dispatching events for {0}", interval);
                    _session.DispatchEvents(interval);
                    _session.CheckForTimeout();
                    if (interval < 500)
                    {
                        interval *= 2;
                    }
                }
            }
            // We always want to return something.
            // No attempt to detect end real of file is needed,
            // as we should not try to read beyond the final closing tag
            while (result == 0);

            return result;
        }

        private Session _session;
    }
}
