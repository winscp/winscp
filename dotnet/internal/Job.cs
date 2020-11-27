using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    internal class Job : IDisposable
    {
        public Job(Logger logger, string name)
        {
            _logger = logger;
            _handle = UnsafeNativeMethods.CreateJobObject(IntPtr.Zero, name);

            if (_handle == IntPtr.Zero)
            {
                _logger.WriteLine("Cannot create job ({0})", Logger.LastWin32ErrorMessage());
            }
            else
            {
                _logger.WriteLine("Job created");

                JobObjectBasicLimitInformation info = new JobObjectBasicLimitInformation
                {
                    LimitFlags = 0x2000 // JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
                };

                JobObjectExtendedLimitInformation extendedInfo = new JobObjectExtendedLimitInformation
                {
                    BasicLimitInformation = info
                };

                int length = Marshal.SizeOf(typeof(JobObjectExtendedLimitInformation));
                IntPtr extendedInfoPtr = Marshal.AllocHGlobal(length);
                Marshal.StructureToPtr(extendedInfo, extendedInfoPtr, false);

                if (UnsafeNativeMethods.SetInformationJobObject(_handle, JobObjectInfoType.ExtendedLimitInformation, extendedInfoPtr, (uint)length))
                {
                    _logger.WriteLine("Job set to kill all processes");
                }
                else
                {
                    _logger.WriteLine("Cannot set job to kill all processes ({0})", Logger.LastWin32ErrorMessage());
                }
            }
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
            _logger.WriteLine("Closing job");
            UnsafeNativeMethods.CloseHandle(_handle);
            _handle = IntPtr.Zero;
        }

        private IntPtr _handle;
        private readonly Logger _logger;
    }
}
