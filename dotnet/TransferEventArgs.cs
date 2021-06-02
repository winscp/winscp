using System.Globalization;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("772FACCC-0786-42E1-B1C8-F08D13C9CD07")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class TransferEventArgs : FileOperationEventArgs
    {
        public ProgressSide Side { get; internal set; }

        public string Destination { get; private set; }

        public long Length { get; private set; }

        public TouchEventArgs Touch { get; internal set; }
        public ChmodEventArgs Chmod { get; internal set; }
        public RemovalEventArgs Removal { get; internal set; }

        internal const string UploadTag = "upload";
        internal const string DownloadTag = "download";
        internal const string MkDirTag = "mkdir";

        internal TransferEventArgs()
        {
        }

        internal static TransferEventArgs Read(ProgressSide side, CustomLogReader areader)
        {
            TransferEventArgs args = new TransferEventArgs() { Side = side };

            using (ElementLogReader reader = new ElementLogReader(areader))
            {
                while (reader.Read(0))
                {
                    if (reader.GetEmptyElementValue("filename", out string value))
                    {
                        args.FileName = value;
                    }
                    else if (reader.GetEmptyElementValue("destination", out value))
                    {
                        args.Destination = value;
                    }
                    else if (reader.GetEmptyElementValue("size", out value))
                    {
                        args.Length = long.Parse(value, CultureInfo.InvariantCulture);
                    }
                    else if (SessionRemoteException.IsResult(reader))
                    {
                        args.Error = SessionRemoteException.ReadResult(reader);
                    }
                }
            }

            return args;
        }
    }
}
