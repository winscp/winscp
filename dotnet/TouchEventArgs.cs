using System;
using System.Runtime.InteropServices;
using System.Xml;

namespace WinSCP
{
    [Guid("802FCEF7-E1D3-4205-B171-87A3724E85FA")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class TouchEventArgs : FileOperationEventArgs
    {
        public DateTime LastWriteTime { get; private set; }

        internal const string Tag = "touch";

        private TouchEventArgs()
        {
        }

        internal static TouchEventArgs Read(CustomLogReader areader)
        {
            TouchEventArgs args = new TouchEventArgs();

            using (ElementLogReader reader = new ElementLogReader(areader))
            {
                while (reader.Read(0))
                {
                    string value;
                    if (reader.GetEmptyElementValue("filename", out value))
                    {
                        args.FileName = value;
                    }
                    else if (reader.GetEmptyElementValue("modification", out value))
                    {
                        args.LastWriteTime = XmlConvert.ToDateTime(value, XmlDateTimeSerializationMode.Local);
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
