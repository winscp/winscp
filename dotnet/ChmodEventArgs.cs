using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("DFCA88E2-6A47-4290-AD66-A39C5682D610")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class ChmodEventArgs : FileOperationEventArgs
    {
        public FilePermissions FilePermissions { get; private set; }

        internal const string Tag = "chmod";

        private ChmodEventArgs()
        {
        }

        internal static ChmodEventArgs Read(CustomLogReader areader)
        {
            ChmodEventArgs args = new ChmodEventArgs();

            using (ElementLogReader reader = new ElementLogReader(areader))
            {
                while (reader.Read(0))
                {
                    string value;
                    if (reader.GetEmptyElementValue("filename", out value))
                    {
                        args.FileName = value;
                    }
                    else if (reader.GetEmptyElementValue("permissions", out value))
                    {
                        args.FilePermissions = FilePermissions.CreateReadOnlyFromText(value);
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
