namespace WinSCP
{
    internal class CallstackAndLock : Callstack
    {
        public CallstackAndLock(Logger logger, Lock alock, object token = null) :
            base(logger, token)
        {
            _lock = alock;
            _lock.Enter();
        }

        public override void Dispose()
        {
            if (_lock != null)
            {
                _lock.Exit();
            }
            base.Dispose();
        }

        public void DisarmLock()
        {
            _lock = null;
        }

        private Lock _lock;
    }
}
