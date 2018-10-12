using System;
using System.Globalization;
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
                throw Session.Logger.WriteException(new InvalidOperationException("Cannot use ElementLogReader with non-element node or empty element"));
            }

            _localName = _parentReader.Reader.LocalName;
            _depth = _parentReader.Reader.Depth;
            _token = _localName + "@" + _depth;
            _read = false;
        }

        public override void Dispose()
        {
            using (Session.Logger.CreateCallstack(_token))
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
                throw Session.Logger.WriteException(
                    new InvalidOperationException(
                        string.Format(CultureInfo.CurrentCulture, "Element {0} already read to the end", _token)));
            }

            bool result = _parentReader.Read(flags);

            if (result &&
                IsEndElement(_localName) &&
                (Depth == _depth))
            {
                result = false;
                Session.Logger.WriteLineLevel(1, "Element {0} read to the end", _token);
                _read = true;
            }

            return result;
        }

        public void ReadToEnd(LogReadFlags flags)
        {
            using (Session.Logger.CreateCallstack(_token))
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
        private string _token;
    }
}
