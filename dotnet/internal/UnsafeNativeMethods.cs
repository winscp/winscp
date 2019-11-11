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
        public UInt32 nLength;
        public IntPtr lpSecurityDescriptor;
        public Int32 bInheritHandle;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct JobObjectBasicLimitInformation
    {
        public Int64 PerProcessUserTimeLimit;
        public Int64 PerJobUserTimeLimit;
        public UInt32 LimitFlags;
        public UIntPtr MinimumWorkingSetSize;
        public UIntPtr MaximumWorkingSetSize;
        public UInt32 ActiveProcessLimit;
        public UIntPtr Affinity;
        public UInt32 PriorityClass;
        public UInt32 SchedulingClass;
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
        public UIntPtr ProcessMemoryLimit;
        public UIntPtr JobMemoryLimit;
        public UIntPtr PeakProcessMemoryUsed;
        public UIntPtr PeakJobMemoryUsed;
    }

    [Flags]
    internal enum FileMapProtection : uint
    {
        PageReadonly = 0x02,
        PageReadWrite = 0x04,
        PageWriteCopy = 0x08,
        PageExecuteRead = 0x20,
        PageExecuteReadWrite = 0x40,
        SectionCommit = 0x8000000,
        SectionImage = 0x1000000,
        SectionNoCache = 0x10000000,
        SectionReserve = 0x4000000,
    }

    [Flags]
    internal enum FileMapAccess : int
    {
        FileMapCopy = 0x0001,
        FileMapWrite = 0x0002,
        FileMapRead = 0x0004,
        FileMapAllAccess = 0x001f,
        FileMapExecute = 0x0020,
    }

    [Flags]
    internal enum StandardRights
    {
        Delete = 0x00010000,
        ReadPermissions = 0x00020000,
        WritePermissions = 0x00040000,
        TakeOwnership = 0x00080000,
        Synchronize = 0x00100000,

        Required = Delete | ReadPermissions | WritePermissions | TakeOwnership,
    }

    // Isn't this the same as FileMapAccess?
    [Flags]
    internal enum FileMappingRights
    {
        FileMapCopy = 1,
        FileMapWrite = 2,
        FileMapRead = 4,
        SectionMapExecute = 8,
        SectionExtendSize = 16,
        FileMapExecute = 32,

        AllAccess = FileMapCopy | FileMapWrite | FileMapRead | SectionMapExecute | SectionExtendSize |
            StandardRights.Required
    }

    [Flags]
    internal enum DesktopRights
    {
        ReadObjects       = 1,
        CreateWindow      = 2,
        CreateMenu        = 4,
        HookControl       = 8,
        JournalRecord     = 16,
        JournalPlayback   = 32,
        Enumerate         = 64,
        WriteObjects      = 128,
        SwitchDesktop     = 256,

        AllAccess = ReadObjects | CreateWindow | CreateMenu | HookControl |
            JournalRecord | JournalPlayback | Enumerate | WriteObjects | SwitchDesktop |
            StandardRights.Required
    }

    [Flags]
    internal enum WindowStationRights
    {
        EnumDesktops = 1,
        ReadAttributes = 2,
        AccessClipboard = 4,
        CreateDesktop = 8,
        WriteAttributes = 16,
        AccessGlobalAtoms = 32,
        ExitWindows = 64,
        Enumerate = 256,
        ReadScreen = 512,

        AllAccess = EnumDesktops  | ReadAttributes  | AccessClipboard | CreateDesktop |
            WriteAttributes | AccessGlobalAtoms | ExitWindows | Enumerate | ReadScreen |
            StandardRights.Required
    }

    internal enum RegistryFlags
    {
        RegSz = 0x02,
        SubKeyWow6432Key = 0x00020000,
    }

    internal enum RegistryType
    {
        RegNone = 0,
    }

    internal static class UnsafeNativeMethods
    {
        public const int ERROR_ALREADY_EXISTS = 183;

        [DllImport("kernel32", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern SafeFileHandle CreateFileMapping(SafeFileHandle hFile, IntPtr lpAttributes, FileMapProtection fProtect, int dwMaximumSizeHigh, int dwMaximumSizeLow, string lpName);

        [DllImport("kernel32", SetLastError = true, ExactSpelling = true)]
        public static extern IntPtr MapViewOfFile(SafeFileHandle handle, FileMapAccess dwDesiredAccess, uint dwFileOffsetHigh, uint dwFileOffsetLow, UIntPtr dwNumberOfBytesToMap);

        [DllImport("kernel32", ExactSpelling = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool UnmapViewOfFile(IntPtr lpBaseAddress);

        [DllImport("kernel32", CharSet = CharSet.Auto, SetLastError = true, ExactSpelling = true)]
        public static extern int CloseHandle(IntPtr hObject);

        [DllImport("kernel32", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr CreateJobObject(IntPtr a, string lpName);

        [DllImport("kernel32", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetInformationJobObject(IntPtr hJob, JobObjectInfoType infoType, IntPtr lpJobObjectInfo, uint cbJobObjectInfoLength);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern IntPtr GetProcessWindowStation();

        [DllImport("user32.dll", SetLastError = true)]
        public static extern IntPtr GetThreadDesktop(int dwThreadId);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern int GetCurrentThreadId();

        [DllImport("advapi32", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern int RegGetValue(UIntPtr hkey, string lpSubKey, string lpValue, RegistryFlags dwFlags, out RegistryType pdwType, IntPtr pvData, ref uint pcbData);
    }
}
