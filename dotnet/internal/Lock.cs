using System;
using System.Threading;

namespace WinSCP
{
    public class Lock
    {
        public void Enter()
        {
            Monitor.Enter(_lock);
            if (_locked)
            {
                Monitor.Exit(_lock);
                throw new InvalidOperationException("Recursive calls not allowed");
            }
            _locked = true;
        }

        public void Exit()
        {
            if (!_locked)
            {
                throw new InvalidOperationException("Not locked");
            }
            _locked = false;
            Monitor.Exit(_lock);
        }

        private readonly object _lock = new object();
        private bool _locked;
    }
}
