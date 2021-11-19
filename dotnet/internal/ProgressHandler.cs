using System;

namespace WinSCP
{
    internal class ProgressHandler : IDisposable
    {
        public ProgressHandler(Session session)
        {
            _session = session;
        }

        public void Dispose()
        {
            using (_session.Logger.CreateCallstack())
            {
                _session.DisableProgressHandling();
            }
        }

        private readonly Session _session;
    }
}
