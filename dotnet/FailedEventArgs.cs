using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("2C29B0BD-4F77-4743-A72A-B91F6D0EAD16")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class FailedEventArgs : OperationEventArgs
    {
        internal FailedEventArgs()
        {
        }

        public override string ToString()
        {
            return (Error != null) ? Error.ToString() : "<no error>";
        }
    }
}
