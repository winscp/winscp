using System;

namespace WinSCP
{
    public interface ITransferEventArgs
    {
        string FileName { get; }
        string Destination { get; }
        TransferOptions Upload { get; }
        bool Touch { get; }
        ChmodEventArgs Chmod { get; }
        RemovalEventArgs Removal { get; }
        SessionRemoteException Error { get; }
        ProgressSide Side { get; }
    }

    [System.CLSCompliant(false)]
    public interface IFileTransferProgressEventArgs
    {
        string FileName { get; }
        string Directory { get; }
        ProgressSide Side { get; }
        double FileProgress { get; }
        double OverallProgress { get; }
        uint CPS { get; }
    }

    public interface IFailedEventArgs
    {
        string FileName { get; }
        string Directory { get; }
        SessionException Error { get; }
    }

    public interface IRemovalEventArgs
    {
        string FileName { get; }
        SessionRemoteException Error { get; }
    }

    public interface IChmodEventArgs
    {
        string FileName { get; }
        FilePermissions FilePermissions { get; }
        SessionRemoteException Error { get; }
    }

    public interface ITouchEventArgs
    {
        string FileName { get; }
        DateTime LastWriteTime { get; }
        SessionRemoteException Error { get; }
    }

    public interface IQueryReceivedEventArgs
    {
        string Query { get; }
        bool Result { get; set; }
    }

    public interface IOutputDataReceivedEventArgs
    {
        string Data { get; }
    }
}
