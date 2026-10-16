using System;
using System.Collections.Generic;
using System.IO;
#if !NETSTANDARD
using System.Security;
#endif

namespace WinSCP
{
    public interface ISession : IDisposable
    {
        string ExecutablePath { get; set; }
#if !NETSTANDARD
        string ExecutableProcessUserName { get; set; }
        SecureString ExecutableProcessPassword { get; set; }
#endif
        string AdditionalExecutableArguments { get; set; }
        bool DisableVersionCheck { get; set; }
        TimeSpan ReconnectTime { get; set; }
        int ReconnectTimeInMilliseconds { get; set; }
        string DebugLogPath { get; set; }
        int DebugLogLevel { get; set; }
        string SessionLogPath { get; set; }
        string XmlLogPath { get; set; }
        bool XmlLogPreserve { get; set; }
        string HomePath { get; }
        TimeSpan Timeout { get; set; }
        StringCollection Output { get; }
        bool Opened { get; }

        event FileTransferredEventHandler FileTransferred;
        event FailedEventHandler Failed;
        event OutputDataReceivedEventHandler OutputDataReceived;
        event FileTransferProgressEventHandler FileTransferProgress;
        event QueryReceivedEventHandler QueryReceived;

        void Abort();
        void Open(SessionOptions sessionOptions);
        string ScanFingerprint(SessionOptions sessionOptions, string algorithm);
        void Close();

        RemoteDirectoryInfo ListDirectory(string path);
        IEnumerable<RemoteFileInfo> EnumerateRemoteFiles(string path, string mask, EnumerationOptions options);

        TransferOperationResult PutFiles(string localPath, string remotePath, bool remove = false, TransferOptions options = null);
        TransferOperationResult PutFilesToDirectory(string localPath, string remoteDirectory, string mask = null, bool remove = false, TransferOptions options = null);
        TransferEventArgs PutFileToDirectory(string localFilePath, string remoteDirectory, bool remove = false, TransferOptions options = null);
        void PutFile(Stream stream, string remoteFilePath, TransferOptions options = null);

        TransferOperationResult GetFiles(string remotePath, string localPath, bool remove = false, TransferOptions options = null);
        TransferOperationResult GetFilesToDirectory(string remotePath, string localDirectory, string mask = null, bool remove = false, TransferOptions options = null);
        TransferEventArgs GetFileToDirectory(string remoteFilePath, string localDirectory, bool remove = false, TransferOptions options = null);
        Stream GetFile(string remoteFilePath, TransferOptions options = null);

        RemovalOperationResult RemoveFiles(string path);
        RemovalEventArgs RemoveFile(string path);

        SynchronizationResult SynchronizeDirectories(
            SynchronizationMode mode, string localPath, string remotePath,
            bool removeFiles, bool mirror = false, SynchronizationCriteria criteria = SynchronizationCriteria.Time,
            TransferOptions options = null);

        ComparisonDifferenceCollection CompareDirectories(
            SynchronizationMode mode, string localPath, string remotePath,
            bool removeFiles, bool mirror = false, SynchronizationCriteria criteria = SynchronizationCriteria.Time,
            TransferOptions options = null);

        CommandExecutionResult ExecuteCommand(string command);

        RemoteFileInfo GetFileInfo(string path);
        bool TryGetFileInfo(string path, out RemoteFileInfo fileInfo);
        bool FileExists(string path);
        byte[] CalculateFileChecksum(string algorithm, string path);

        void CreateDirectory(string path);
        void MoveFile(string sourcePath, string targetPath);
        void DuplicateFile(string sourcePath, string targetPath);

        void AddRawConfiguration(string setting, string value);

        string EscapeFileMask(string fileMask);
        string CombinePaths(string path1, string path2);
        string TranslateLocalPathToRemote(string localPath, string localRoot, string remoteRoot);
        string TranslateRemotePathToLocal(string remotePath, string remoteRoot, string localRoot);
    }
}
