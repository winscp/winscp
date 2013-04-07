using Microsoft.Win32.SafeHandles;
using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    internal enum JobObjectInfoType
    {
        AssociateCompletionPortInformation = 7,
        BasicLimitInformation = 2,
        BasicUIRestrictions = 4,
        EndOfJobTimeInformation = 6,
        ExtendedLimitInformation = 9,
        SecurityLimitInformation = 5,
        GroupInformation = 11
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct SecurityAttributes
    {
        public int nLength;
        public IntPtr lpSecurityDescriptor;
        public int bInheritHandle;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct JobObjectBasicLimitInformation
    {
        public Int64 PerProcessUserTimeLimit;
        public Int64 PerJobUserTimeLimit;
        public Int16 LimitFlags;
        public UInt32 MinimumWorkingSetSize;
        public UInt32 MaximumWorkingSetSize;
        public Int16 ActiveProcessLimit;
        public Int64 Affinity;
        public Int16 PriorityClass;
        public Int16 SchedulingClass;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct IOCounters
    {
        public UInt64 ReadOperationCount;
        public UInt64 WriteOperationCount;
        public UInt64 OtherOperationCount;
        public UInt64 ReadTransferCount;
        public UInt64 WriteTransferCount;
        public UInt64 OtherTransferCount;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct JobObjectExtendedLimitInformation
    {
        public JobObjectBasicLimitInformation BasicLimitInformation;
        public IOCounters IoInfo;
        public UInt32 ProcessMemoryLimit;
        public UInt32 JobMemoryLimit;
        public UInt32 PeakProcessMemoryUsed;
        public UInt32 PeakJobMemoryUsed;
    }

    internal static class UnsafeNativeMethods
    {
        [DllImport("kernel32", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern SafeFileHandle CreateFileMapping(SafeFileHandle hFile, IntPtr lpAttributes, int fProtect, int dwMaximumSizeHigh, int dwMaximumSizeLow, string lpName);

        [DllImport("kernel32", SetLastError = true, ExactSpelling = true)]
        public static extern IntPtr MapViewOfFile(SafeFileHandle handle, int dwDesiredAccess, uint dwFileOffsetHigh, uint dwFileOffsetLow, UIntPtr dwNumberOfBytesToMap);

        [DllImport("kernel32", ExactSpelling = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool UnmapViewOfFile(IntPtr lpBaseAddress);

        [DllImport("kernel32", CharSet = CharSet.Auto, SetLastError = true, ExactSpelling = true)]
        public static extern int CloseHandle(IntPtr hObject);

        [DllImport("kernel32", CharSet = CharSet.Unicode)]
        public static extern IntPtr CreateJobObject(IntPtr a, string lpName);

        [DllImport("kernel32")]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetInformationJobObject(IntPtr hJob, JobObjectInfoType infoType, IntPtr lpJobObjectInfo, uint cbJobObjectInfoLength);

        [DllImport("kernel32", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool AssignProcessToJobObject(IntPtr job, IntPtr process);
    }
}
