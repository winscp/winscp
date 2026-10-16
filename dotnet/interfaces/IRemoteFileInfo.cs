using System;

namespace WinSCP
{
    public interface IRemoteFileInfo
    {
        string Name { get; }
        string FullName { get; }
        char FileType { get; }
        long Length { get; }
        int Length32 { get; set; }
        DateTime LastWriteTime { get; }
        FilePermissions FilePermissions { get; }
        string Owner { get; }
        string Group { get; }
        bool IsDirectory { get; }
        bool IsThisDirectory { get; }
        bool IsParentDirectory { get; }
    }

    public interface IRemoteDirectoryInfo
    {
        RemoteFileInfoCollection Files { get; }
    }

    public interface IFilePermissions
    {
        string Text { get; set; }
        string Octal { get; set; }
        int Numeric { get; set; }

        bool UserExecute { get; set; }
        bool UserRead { get; set; }
        bool UserWrite { get; set; }
        bool GroupExecute { get; set; }
        bool GroupRead { get; set; }
        bool GroupWrite { get; set; }
        bool OtherExecute { get; set; }
        bool OtherRead { get; set; }
        bool OtherWrite { get; set; }

        bool Sticky { get; set; }
        bool SetUid { get; set; }
        bool SetGid { get; set; }
    }
}
