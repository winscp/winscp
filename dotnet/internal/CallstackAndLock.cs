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
            _lock.Exit();
            base.Dispose();
        }

        private readonly Lock _lock;
    }
}
