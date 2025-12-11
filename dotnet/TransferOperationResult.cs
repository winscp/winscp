using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("74F668E6-8EF2-4D01-84D8-DA2FE619C062")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class TransferOperationResult : OperationResultBase, ITransferOperationResult
    {
        public TransferEventArgsCollection Transfers { get; private set; }

        internal TransferOperationResult()
        {
            Transfers = new TransferEventArgsCollection();
        }

        internal void AddTransfer(TransferEventArgs operation)
        {
            Transfers.InternalAdd(operation);
        }
    }
}
