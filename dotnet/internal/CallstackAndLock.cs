namespace WinSCP
{
    internal class CallstackAndLock : Callstack
    {
        public CallstackAndLock(Logger logger, Lock alock) :
            base(logger)
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
