using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml;

namespace WinSCP
{
    [Guid("0E8BBC73-AF4D-4E7E-995C-EB89D0BFDE9A")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class SessionRemoteException : SessionException
    {
        internal SessionRemoteException(Session session, string message) :
            base(session, message)
        {
        }

        internal static bool IsResult(CustomLogReader reader)
        {
            return reader.IsNonEmptyElement("result");
        }

        internal static SessionRemoteException ReadResult(CustomLogReader areader)
        {
            SessionRemoteException e = null;

            if (areader.GetAttribute("success") == "false")
            {
                e = ReadMessages(areader);
            }

            return e;
        }

        internal static SessionRemoteException ReadFailure(CustomLogReader reader)
        {
            return ReadMessages(reader);
        }

        private static SessionRemoteException ReadMessages(CustomLogReader areader)
        {
            using (ElementLogReader reader = new ElementLogReader(areader))
            {
                string message = null;
                List<string> messages = new List<string>();
                bool inMessage = false;

                while (reader.Read(0))
                {
                    if (reader.IsNonEmptyElement("message"))
                    {
                        inMessage = true;
                        message = null;
                    }
                    else if (inMessage &&
                        (reader.NodeType == XmlNodeType.Text))
                    {
                        message += reader.Value;
                    }
                    else if (inMessage &&
                        reader.IsEndElement("message"))
                    {
                        messages.Add(message);
                        message = null;
                        inMessage = false;
                    }
                }

                string error = string.Join(Environment.NewLine, messages.ToArray());
                return new SessionRemoteException(reader.Session, error);
            }
        }
    }
}
