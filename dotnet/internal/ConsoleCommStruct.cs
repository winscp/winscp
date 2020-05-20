using System.Runtime.InteropServices;
using System;
using Microsoft.Win32.SafeHandles;

namespace WinSCP
{
    public enum ConsoleEvent { None, Print, Input, Choice, Title, Init, Progress, TransferOut }

    [StructLayout(LayoutKind.Sequential)]
    internal class ConsoleInitEventStruct
    {
        public uint InputType;
        public uint OutputType;
        [MarshalAs(UnmanagedType.I1)]
        public bool WantsProgress; // since version 6
        [MarshalAs(UnmanagedType.I1)]
        public bool UseStdErr; // since version 10
        public enum StdInOut { Off, Binary, Chunked }
        public StdInOut BinaryOutput; // since version 10
        public StdInOut BinaryInput; // since version 10
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    internal class ConsolePrintEventStruct
    {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 10240)]
        public string Message; // wide since version 4
        [MarshalAs(UnmanagedType.I1)]
        public bool FromBeginning;
        [MarshalAs(UnmanagedType.I1)]
        public bool Error; // since version 7
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    internal class ConsoleInputEventStruct
    {
        [MarshalAs(UnmanagedType.I1)]
        public bool Echo;
        [MarshalAs(UnmanagedType.I1)]
        public bool Result;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 10240)]
        public string Str; // wide since version 4
        public uint Timer; // since version 2
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    internal class ConsoleChoiceEventStruct
    {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 64)]
        public string Options; // wide since version 4
        public int Cancel;
        public int Break;
        public int Result;
        public int Timeouted; // since version 2
        public uint Timer; // since version 2
        [MarshalAs(UnmanagedType.I1)]
        public bool Timeouting; // since version 4
        public int Continue; // since version 9
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 5120)]
        public string Message; // since version 9
    }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    internal class ConsoleTitleEventStruct
    {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 10240)]
        public string Title; // wide since version 4
    }

    // Since version 6
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    internal class ConsoleProgressEventStruct
    {
        public enum ProgressOperation { Copy }
        public enum ProgressSide { Local, Remote }

        public ProgressOperation Operation;
        public ProgressSide Side;

        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 1024)]
        public string FileName;

        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 1024)]
        public string Directory;

        public uint OverallProgress;
        public uint FileProgress;
        public uint CPS;
        [MarshalAs(UnmanagedType.I1)]
        public bool Cancel; // since version 8
    }

    // Since version 10
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    internal class ConsoleTransferEventStruct
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 20480)]
        public byte[] Data;
        public UIntPtr Len;
        [MarshalAs(UnmanagedType.I1)]
        public bool Error; // TransferIn only
    }

    [StructLayout(LayoutKind.Sequential)]
    internal class ConsoleCommHeader
    {
        public uint Size;

        public int Version;

        public ConsoleEvent Event;
    }

    internal class ConsoleCommStruct : IDisposable
    {
        public const int CurrentVersion = 0x000A;

        public ConsoleCommStruct(Session session, SafeFileHandle fileMapping)
        {
            _session = session;
            _fileMapping = fileMapping;
            _session.Logger.WriteLineLevel(1, "Acquiring communication structure");
            _ptr = UnsafeNativeMethods.MapViewOfFile(_fileMapping, FileMapAccess.FileMapAllAccess, 0, 0, UIntPtr.Zero);
            _session.Logger.WriteLineLevel(1, "Acquired communication structure");
            _payloadPtr = new IntPtr(_ptr.ToInt64() + 12);
            _header = (ConsoleCommHeader)Marshal.PtrToStructure(_ptr, typeof(ConsoleCommHeader));
        }

        ~ConsoleCommStruct()
        {
            Dispose();
        }

        public void Dispose()
        {
            if (_ptr != IntPtr.Zero)
            {
                if (_headerInvalidated)
                {
                    Marshal.StructureToPtr(_header, _ptr, false);
                }

                if (_payload != null)
                {
                    if ((Event != ConsoleEvent.Print) && (Event != ConsoleEvent.Title))
                    {
                        Marshal.StructureToPtr(_payload, _payloadPtr, false);
                    }
                }

                _session.Logger.WriteLineLevel(1, "Releasing communication structure");
                if (!UnsafeNativeMethods.UnmapViewOfFile(_ptr))
                {
                    throw _session.Logger.WriteException(new SessionLocalException(_session, "Cannot release file mapping"));
                }
                _session.Logger.WriteLineLevel(1, "Released communication structure");

                _ptr = IntPtr.Zero;
            }
            GC.SuppressFinalize(this);
        }

        public ConsoleEvent Event
        {
            get
            {
                return _header.Event;
            }
        }

        public ConsolePrintEventStruct PrintEvent { get { return UnmarshalPayload<ConsolePrintEventStruct>(ConsoleEvent.Print); } }

        public ConsoleInitEventStruct InitEvent { get { return UnmarshalPayload<ConsoleInitEventStruct>(ConsoleEvent.Init); } }

        public ConsoleInputEventStruct InputEvent { get { return UnmarshalPayload<ConsoleInputEventStruct>(ConsoleEvent.Input); } }

        public ConsoleChoiceEventStruct ChoiceEvent { get { return UnmarshalPayload<ConsoleChoiceEventStruct>(ConsoleEvent.Choice); } }

        public ConsoleTitleEventStruct TitleEvent { get { return UnmarshalPayload<ConsoleTitleEventStruct>(ConsoleEvent.Title); } }

        public ConsoleProgressEventStruct ProgressEvent { get { return UnmarshalPayload<ConsoleProgressEventStruct>(ConsoleEvent.Progress); } }

        public ConsoleTransferEventStruct TransferOutEvent { get { return UnmarshalPayload<ConsoleTransferEventStruct>(ConsoleEvent.TransferOut); } }

        private T UnmarshalPayload<T>(ConsoleEvent e)
        {
            CheckNotDisposed();

            if (e != Event)
            {
                throw _session.Logger.WriteException(new InvalidOperationException("Payload type does not match with event"));
            }

            if (_payload == null)
            {
                _payload = Marshal.PtrToStructure(_payloadPtr, typeof(T));
            }

            return (T)_payload;
        }

        private void CheckNotDisposed()
        {
            if (_ptr == IntPtr.Zero)
            {
                throw _session.Logger.WriteException(new InvalidOperationException("Object is disposed"));
            }
        }

        public static int Size
        {
            get
            {
                Type[] types =
                    new[] {
                        typeof(ConsolePrintEventStruct), typeof(ConsoleInitEventStruct), typeof(ConsoleInputEventStruct),
                        typeof(ConsoleChoiceEventStruct), typeof(ConsoleTitleEventStruct), typeof(ConsoleProgressEventStruct),
                        typeof(ConsoleTransferEventStruct) };

                int maxSize = 0;
                foreach (Type type in types)
                {
                    maxSize = Math.Max(maxSize, Marshal.SizeOf(type));
                }

                return (Marshal.SizeOf(typeof(ConsoleCommHeader)) + maxSize);
            }
        }

        public void InitHeader()
        {
            _headerInvalidated = true;
            _header.Size = (uint) Size;
            _header.Version = CurrentVersion;
            _header.Event = ConsoleEvent.None;
        }

        private IntPtr _ptr;
        private readonly ConsoleCommHeader _header;
        private bool _headerInvalidated;
        private readonly IntPtr _payloadPtr;
        private object _payload;
        private readonly SafeFileHandle _fileMapping;
        private readonly Session _session;
    }
}
