using System;
using System.IO;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("B1DAE3A0-5E56-4001-88D8-786F68557E28")]
    [ComVisible(true)]
    public enum SynchronizationAction
    {
        UploadNew = 1,
        DownloadNew = 2,
        UploadUpdate = 3,
        DownloadUpdate = 4,
        DeleteRemote = 5,
        DeleteLocal = 6,
    };

    [Guid("2D6EFFB5-69BA-47AA-90E8-A92953E8B58A")]
    [ComVisible(true)]
    public sealed class ComparisonFileInfo
    {
        public string FileName { get; internal set; }
        public DateTime LastWriteTime { get; internal set; }
        public long Length { get; internal set; }
        public int Length32 { get { return GetLength32(); } }

        internal ComparisonFileInfo()
        {
        }

        private int GetLength32()
        {
            return Tools.LengthTo32Bit(Length);
        }
    }

    [Guid("97F5222E-9379-4C24-9E50-E93C7334BBD5")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class ComparisonDifference
    {
        public SynchronizationAction Action { get; internal set; }
        public bool IsDirectory { get; internal set; }
        public ComparisonFileInfo Local { get; internal set; }
        public ComparisonFileInfo Remote { get; internal set; }

        internal ComparisonDifference(string localPath, string remotePath)
        {
            _localPath = localPath;
            _remotePath = remotePath;
        }

        public FileOperationEventArgs Resolve(Session session, TransferOptions options = null)
        {
            if (session == null)
            {
                throw new ArgumentNullException(nameof(session));
            }

            switch (Action)
            {
                case SynchronizationAction.UploadNew:
                case SynchronizationAction.UploadUpdate:
                    {
                        string remoteDirectory =
                            RemotePath.TranslateLocalPathToRemote(Path.GetDirectoryName(Local.FileName), _localPath, _remotePath);
                        if (!IsDirectory)
                        {
                            return session.PutFileToDirectory(Local.FileName, remoteDirectory, options: options);
                        }
                        else
                        {
                            session.PutEntryToDirectory(Local.FileName, remoteDirectory, options: options);
                            return null;
                        }
                    }

                case SynchronizationAction.DownloadNew:
                case SynchronizationAction.DownloadUpdate:
                    {
                        string localDirectory =
                            RemotePath.TranslateRemotePathToLocal(RemotePath.GetDirectoryName(Remote.FileName), _remotePath, _localPath);
                        if (!IsDirectory)
                        {
                            return session.GetFileToDirectory(Remote.FileName, localDirectory, options: options);
                        }
                        else
                        {
                            session.GetEntryToDirectory(Remote.FileName, localDirectory, options: options);
                            return null;
                        }
                    }

                case SynchronizationAction.DeleteRemote:
                    if (!IsDirectory)
                    {
                        return session.RemoveFile(Remote.FileName);
                    }
                    else
                    {
                        session.RemoveEntry(Remote.FileName);
                        return null;
                    }

                case SynchronizationAction.DeleteLocal:
                    if (!IsDirectory)
                    {
                        File.Delete(Local.FileName);
                    }
                    else
                    {
                        Directory.Delete(Local.FileName, true);
                    }
                    return null;

                default:
                    throw session.Logger.WriteException(new InvalidOperationException());
            }
        }

        public void Reverse()
        {
            switch (Action)
            {
                case SynchronizationAction.UploadNew:
                    Action = SynchronizationAction.DeleteLocal;
                    break;

                case SynchronizationAction.DownloadNew:
                    Action = SynchronizationAction.DeleteRemote;
                    break;

                case SynchronizationAction.UploadUpdate:
                    Action = SynchronizationAction.DownloadUpdate;
                    break;

                case SynchronizationAction.DownloadUpdate:
                    Action = SynchronizationAction.UploadUpdate;
                    break;

                case SynchronizationAction.DeleteRemote:
                    Action = SynchronizationAction.DownloadNew;
                    break;

                case SynchronizationAction.DeleteLocal:
                    Action = SynchronizationAction.UploadNew;
                    break;

                default:
                    throw new InvalidOperationException();
            }
        }

        private readonly string _localPath;
        private readonly string _remotePath;
    }
}
