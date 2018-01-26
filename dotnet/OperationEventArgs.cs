using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("4D79C4F7-0FE2-428D-9908-AB2D38E96C53")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public class OperationEventArgs : EventArgs
    {
        public SessionRemoteException Error { get; internal set; }

        internal OperationEventArgs()
        {
        }
    }
}
