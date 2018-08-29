using System;
using System.Xml;

namespace WinSCP
{
    [Flags]
    internal enum LogReadFlags
    {
        ThrowFailures = 0x01
    }

    internal abstract class CustomLogReader : IDisposable
    {
        public Session Session { get; private set; }
        public XmlNodeType NodeType { get { return Reader.NodeType; } }
        public string NamespaceURI { get { return Reader.NamespaceURI; } }
        public string LocalName { get { return Reader.LocalName; } }
        public bool IsEmptyElement { get { return Reader.IsEmptyElement; } }
        public int Depth { get { return Reader.Depth; } }
        public string Value { get { return Reader.Value; } }

        internal abstract XmlReader Reader { get; }

        public abstract bool Read(LogReadFlags flags);

        protected CustomLogReader(Session session)
        {
            Session = session;
        }

        public virtual void Dispose()
        {
        }

        public bool IsElement()
        {
            return
                (NodeType == XmlNodeType.Element) &&
                (NamespaceURI == Session.Namespace);
        }

        public bool IsElement(string localName)
        {
            return
                IsElement() &&
                (LocalName == localName);
        }

        public bool IsNonEmptyElement(string localName)
        {
            return
                IsElement(localName) &&
                !IsEmptyElement;
        }

        public bool GetEmptyElementValue(string localName, out string value)
        {
            bool result =
                IsElement(localName) &&
                IsEmptyElement;

            if (result)
            {
                value = GetAttribute("value");
                result = (value != null);
            }
            else
            {
                value = null;
            }

            return result;
        }

        public bool IsEndElement(string localName)
        {
            return
                (NodeType == XmlNodeType.EndElement) &&
                (NamespaceURI == Session.Namespace) &&
                (LocalName == localName);
        }

        public bool TryWaitForNonEmptyElement(string localName, LogReadFlags flags)
        {
            bool result = false;
            while (!result && Read(flags))
            {
                if (IsNonEmptyElement(localName))
                {
                    result = true;
                }
            }
            return result;
        }

        public void WaitForNonEmptyElement(string localName, LogReadFlags flags)
        {
            if (!TryWaitForNonEmptyElement(localName, flags))
            {
                throw Session.Logger.WriteException(SessionLocalException.CreateElementNotFound(Session, localName));
            }
        }

        public bool TryWaitForEmptyElement(string localName, LogReadFlags flags)
        {
            bool result = false;
            while (!result && Read(flags))
            {
                if (IsElement(localName) &&
                    IsEmptyElement)
                {
                    result = true;
                }
            }
            return result;
        }

        public ElementLogReader CreateLogReader()
        {
            return new ElementLogReader(this);
        }

        public ElementLogReader WaitForNonEmptyElementAndCreateLogReader(string localName, LogReadFlags flags)
        {
            WaitForNonEmptyElement(localName, flags);

            return CreateLogReader();
        }

        public ElementLogReader WaitForGroupAndCreateLogReader()
        {
            return WaitForNonEmptyElementAndCreateLogReader("group", LogReadFlags.ThrowFailures);
        }

        public string GetAttribute(string name)
        {
            return Reader.GetAttribute(name);
        }
    }
}
