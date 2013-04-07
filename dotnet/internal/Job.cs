using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    internal class Job : IDisposable
    {
        public Job()
        {
            _handle = UnsafeNativeMethods.CreateJobObject(IntPtr.Zero, null);

            JobObjectBasicLimitInformation info = new JobObjectBasicLimitInformation();
            info.LimitFlags = 0x2000;

            JobObjectExtendedLimitInformation extendedInfo = new JobObjectExtendedLimitInformation();
            extendedInfo.BasicLimitInformation = info;

            int length = Marshal.SizeOf(typeof(JobObjectExtendedLimitInformation));
            IntPtr extendedInfoPtr = Marshal.AllocHGlobal(length);
            Marshal.StructureToPtr(extendedInfo, extendedInfoPtr, false);

            UnsafeNativeMethods.SetInformationJobObject(_handle, JobObjectInfoType.ExtendedLimitInformation, extendedInfoPtr, (uint)length);
        }

        ~Job()
        {
            DoDispose();
        }

        public void Dispose()
        {
            DoDispose();
            GC.SuppressFinalize(this);
        }

        private void DoDispose()
        {
            Close();
        }

        public void Close()
        {
            UnsafeNativeMethods.CloseHandle(_handle);
            _handle = IntPtr.Zero;
        }

        public bool AddProcess(IntPtr handle)
        {
            return UnsafeNativeMethods.AssignProcessToJobObject(_handle, handle);
        }

        private IntPtr _handle;
    }
}
