using System;
using System.Collections.Generic;
using System.Text;

namespace WinSCP
{
    internal class SessionElementLogReader : ElementLogReader
    {
        public SessionElementLogReader(CustomLogReader parentReader) :
            base(parentReader)
        {
        }

        public override void Dispose()
        {
            // Now it's ok if we encounter </session>.
            _disposing = true;

            base.Dispose();
        }

        public override bool Read(LogReadFlags flags)
        {
            bool result = base.Read(flags);

            if (_read && !_disposing)
            {
                throw new SessionLocalException(Session, "Session has unexpectedly closed");
            }

            return result;
        }

        private bool _disposing;
    }
}
