using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("62FB0733-C24F-4DC2-8452-560148931927")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class RemovalEventArgs : FileOperationEventArgs
    {
        internal const string Tag = "rm";

        private RemovalEventArgs()
        {
        }

        internal static RemovalEventArgs Read(CustomLogReader areader)
        {
            RemovalEventArgs args = new RemovalEventArgs();

            using (ElementLogReader reader = new ElementLogReader(areader))
            {
                while (reader.Read(0))
                {
                    if (reader.GetEmptyElementValue("filename", out string value))
                    {
                        args.FileName = value;
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
