using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("D0ADB4F7-47AE-43AC-AA41-9114650EA51A")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class SynchronizationResult : OperationResultBase, ISynchronizationResult
    {
        public TransferEventArgsCollection Uploads { get; private set; }
        public TransferEventArgsCollection Downloads { get; private set; }
        public RemovalEventArgsCollection Removals { get; private set; }

        internal SynchronizationResult()
        {
            Uploads = new TransferEventArgsCollection();
            Downloads = new TransferEventArgsCollection();
            Removals = new RemovalEventArgsCollection();
        }

        internal void AddUpload(TransferEventArgs upload)
        {
            Uploads.InternalAdd(upload);
        }

        internal void AddDownload(TransferEventArgs download)
        {
            Downloads.InternalAdd(download);
        }

        internal void AddRemoval(RemovalEventArgs removal)
        {
            Removals.InternalAdd(removal);
        }
    }
}
