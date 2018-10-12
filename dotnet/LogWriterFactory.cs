using System;
using System.Collections.Generic;
using System.Text;
using WinSCP.Internal;

namespace WinSCP
{
    public static class LogWriterFactory
    {
        public static ILogWriterFactory CurrentFactory { get; set; } = new FileLogWriterFactory();
    }
}
