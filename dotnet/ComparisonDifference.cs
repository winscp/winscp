using System;
using System.Globalization;
using System.Runtime.InteropServices;
using WinSCP;

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

        internal ComparisonDifference()
        {
        }
    }
}
