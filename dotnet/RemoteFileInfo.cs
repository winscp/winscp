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
        public int Length32 { get { return GetLength32(); } set { SetLength32(value); } }

        public DateTime LastWriteTime { get; internal set; }
        public FilePermissions FilePermissions { get; internal set; }

        public string Owner { get; internal set; }
        public string Group { get; internal set; }

        public bool IsDirectory { get { return (Char.ToUpper(FileType, CultureInfo.InvariantCulture) == 'D'); } }

        internal RemoteFileInfo()
        {
        }

        public override string ToString()
        {
            return Name;
        }
    
        private int GetLength32()
        {
            if ((Length < int.MinValue) || (Length > int.MaxValue))
            {
                throw new OverflowException(string.Format(CultureInfo.CurrentCulture, "Size {0} cannot be represented using 32-bit value", Length));
            }

            return (int) Length;
        }

        private void SetLength32(int value)
        {
            Length = value;
        }
    }
}
