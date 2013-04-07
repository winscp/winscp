using System;
using System.Diagnostics;
using System.Globalization;
using System.Reflection;

namespace WinSCP
{
    internal class Callstack : IDisposable
    {
        public Callstack(Logger logger)
        {
            _logger = logger;
            if (_logger.Logging)
            {
                Type type = GetType();
                StackTrace stackTrace = new StackTrace();
                int i = 0;
                MethodBase method;
                do
                {
                    StackFrame frame = stackTrace.GetFrame(i);
                    method = frame.GetMethod();
                    if ((method.IsConstructor && method.DeclaringType.IsAssignableFrom(type)) ||
                        ((method.MemberType == MemberTypes.Method) && ((MethodInfo)method).ReturnType.IsAssignableFrom(type)))
                    {
                        method = null;
                    }
                    else
                    {
                        break;
                    }
                    i++;
                }
                while (i < stackTrace.FrameCount);

                if (method != null)
                {
                    _name = string.Format(CultureInfo.InvariantCulture, "{0}.{1}", method.DeclaringType.Name, method.Name);
                    _logger.WriteLine("{0} entering", _name);
                    _logger.Indent();
                }
            }
        }

        public virtual void Dispose()
        {
            if (_name != null)
            {
                _logger.Unindent();
                _logger.WriteLine("{0} leaving", _name);
            }
        }

        private readonly Logger _logger;
        private readonly string _name;
    }
}
