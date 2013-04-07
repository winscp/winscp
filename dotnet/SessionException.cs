using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("91109A4F-D81A-4326-BEC5-1AB26EBF89A6")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public class SessionException : Exception
    {
        public Session Session { get; private set; }

        internal SessionException(Session session, string message) :
            this(session, message, null)
        {
        }

        internal SessionException(Session session, string message, Exception innerException) :
            base(message, innerException)
        {
            Session = session;
            session.Logger.WriteLine("Exception: {0}", this);
        }
    }
}
