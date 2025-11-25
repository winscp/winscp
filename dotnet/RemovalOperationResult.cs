using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("3BCB18EC-6D98-4BFB-A9C2-893CBD13CDAB")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class RemovalOperationResult : OperationResultBase, IRemovalOperationResult
    {
        public RemovalEventArgsCollection Removals { get; private set; }

        internal RemovalOperationResult()
        {
            Removals = new RemovalEventArgsCollection();
        }

        internal void AddRemoval(RemovalEventArgs operation)
        {
            Removals.InternalAdd(operation);
        }
    }
}
