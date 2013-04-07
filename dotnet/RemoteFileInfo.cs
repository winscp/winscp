using System;
using System.Globalization;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("17FF9C92-B8B6-4506-A7BA-8482D9B0AB07")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class RemoteFileInfo
    {
        public string Name { get; internal set; }
        public char FileType { get; internal set; }
        public long Length { get; internal set; }
        public DateTime LastWriteTime { get; internal set; }
        public FilePermissions FilePermissions { get; internal set; }

        public bool IsDirectory { get { return (Char.ToUpper(FileType, CultureInfo.InvariantCulture) == 'D'); } }

        internal RemoteFileInfo()
        {
        }

        public override string ToString()
        {
            return Name;
        }
    }
}
