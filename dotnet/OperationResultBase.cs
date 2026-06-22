using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("B4CC583A-B64E-4797-9967-0FCB2F07C977")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public class OperationResultBase : IOperationResultBase
    {
        public SessionRemoteExceptionCollection Failures { get; private set; }

        public bool IsSuccess { get { return (Failures.Count == 0); } }

        internal OperationResultBase()
        {
            Failures = new SessionRemoteExceptionCollection();
        }

        public void Check()
        {
            if (!IsSuccess)
            {
                throw Failures[0];
            }
        }

        internal void AddFailure(SessionRemoteException failure)
        {
            Failures.InternalAdd(failure);
        }
    }
}
