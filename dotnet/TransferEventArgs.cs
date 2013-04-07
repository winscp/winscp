using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("772FACCC-0786-42E1-B1C8-F08D13C9CD07")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class TransferEventArgs : FileOperationEventArgs
    {
        public string Destination { get; private set; }

        public TouchEventArgs Touch { get; internal set; }
        public ChmodEventArgs Chmod { get; internal set; }
        public RemovalEventArgs Removal { get; internal set; }

        internal const string UploadTag = "upload";
        internal const string DownloadTag = "download";

        internal TransferEventArgs()
        {
        }

        internal static TransferEventArgs Read(CustomLogReader areader)
        {
            TransferEventArgs args = new TransferEventArgs();

            using (ElementLogReader reader = new ElementLogReader(areader))
            {
                while (reader.Read(0))
                {
                    string value;
                    if (reader.GetEmptyElementValue("filename", out value))
                    {
                        args.FileName = value;
                    }
                    else if (reader.GetEmptyElementValue("destination", out value))
                    {
                        args.Destination = value;
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
