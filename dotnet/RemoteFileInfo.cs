using System;
using System.Globalization;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("17FF9C92-B8B6-4506-A7BA-8482D9B0AB07")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class RemoteFileInfo : IRemoteFileInfo
    {
        public string Name { get; internal set; }
        public string FullName { get; internal set; }
        public char FileType { get; internal set; }
        public long Length { get; internal set; }
        public int Length32 { get { return GetLength32(); } set { SetLength32(value); } }

        public DateTime LastWriteTime { get; internal set; }
        public FilePermissions FilePermissions { get; internal set; }

        public string Owner { get; internal set; }
        public string Group { get; internal set; }

        public bool IsDirectory { get { return IsDirectoryFileType(FileType); } }
        public bool IsThisDirectory { get { return IsDirectory && (Name == "."); } }
        public bool IsParentDirectory { get { return IsDirectory && (Name == ".."); } }

        internal RemoteFileInfo()
        {
        }

        public override string ToString()
        {
            return Name;
        }

        private int GetLength32()
        {
            return Tools.LengthTo32Bit(Length);
        }

        private void SetLength32(int value)
        {
            Length = value;
        }

        internal static bool IsDirectoryFileType(char fileType)
        {
            return (char.ToUpper(fileType, CultureInfo.InvariantCulture) == 'D');
        }
    }
}
