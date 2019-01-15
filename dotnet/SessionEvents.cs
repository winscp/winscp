using System.Runtime.InteropServices;

namespace WinSCP
{
    [ComVisible(true)]
    [Guid("A1334E32-4EDF-4B51-A069-DA3FF1B19A5A")]
    [InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIDispatch)]
    public interface ISessionEvents
    {
        [DispId(1)]
        void FileTransferred(object sender, TransferEventArgs e);
        [DispId(2)]
        void Failed(object sender, FailedEventArgs e);
        [DispId(3)]
        void OutputDataReceived(object sender, OutputDataReceivedEventArgs e);
        [DispId(4)]
        void FileTransferProgress(object sender, FileTransferProgressEventArgs e);
        [DispId(5)]
        void QueryReceived(object sender, QueryReceivedEventArgs e);
    }
}
