using System;
using System.Xml;

namespace WinSCP
{
    internal class ElementLogReader : CustomLogReader
    {
        public ElementLogReader(CustomLogReader parentReader) :
            base(parentReader.Session)
        {
            _parentReader = parentReader;

            if ((NodeType != XmlNodeType.Element) ||
                IsEmptyElement)
            {
                throw new InvalidOperationException();
            }

            _localName = _parentReader.Reader.LocalName;
            _depth = _parentReader.Reader.Depth;
            _read = false;
        }

        public override void Dispose()
        {
            using (Session.Logger.CreateCallstack())
            {
                try
                {
                    ReadToEnd(0);
                }
                catch (Exception)
                {
                    // swallow
                    Session.Logger.WriteLine("Swallowing exception");
                }
            }

            base.Dispose();
        }

        public override bool Read(LogReadFlags flags)
        {
            if (_read)
            {
                throw new InvalidOperationException("Element already read to the end");
            }

            bool result = _parentReader.Read(flags);

            if (result &&
                IsEndElement(_localName) &&
                (Depth == _depth))
            {
                result = false;
                _read = true;
            }

            return result;
        }

        public void ReadToEnd(LogReadFlags flags)
        {
            using (Session.Logger.CreateCallstack())
            {
                if (!_read)
                {
                    while (Read(flags))
                    {
                    }
                }
            }
        }

        internal override XmlReader Reader
        {
            get { return _parentReader.Reader; }
        }

        private readonly CustomLogReader _parentReader;
        private readonly string _localName;
        private readonly int _depth;
        protected bool _read;
    }
}
