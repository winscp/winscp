using System;
using System.Collections.Generic;
using System.Text;

namespace WinSCP.Internal
{
    public interface ILogWriter : IDisposable
    {
        void WriteLine(int indent, string message);
        bool Enabled();
    }
}
