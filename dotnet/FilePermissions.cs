using System;
using System.Globalization;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("90A290B2-C8CE-4900-8C42-7736F9E435C6")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class FilePermissions : IFilePermissions
    {
        public int Numeric
        {
            get
            {
                return _numeric;
            }

            set
            {
                if (_readOnly)
                {
                    throw new InvalidOperationException("Cannot change read-only permissions");
                }

                if ((value < 0) || (value > 0xFFF))
                {
                    throw new ArgumentOutOfRangeException(string.Format(CultureInfo.CurrentCulture, "{0} is not valid numerical permissions", value));
                }

                _numeric = value;
            }
        }

        public string Text
        {
            get
            {
                return NumericToText(Numeric);
            }

            set
            {
                Numeric = TextToNumeric(value);
            }
        }

        public string Octal
        {
            get
            {
                string result = Convert.ToString(Numeric, 8);
                return new string('0', Math.Max(3 - result.Length, 0)) + result;
            }

            set
            {
                if ((value == null) || ((value.Length != 3) && (value.Length != 4)))
                {
                    throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not valid octal permissions", value));
                }

                Numeric = Convert.ToInt16(value, 8);
            }
        }

        public bool OtherExecute
        {
            get { return GetBit(0); }
            set { SetBit(0, value); }
        }

        public bool OtherWrite
        {
            get { return GetBit(1); }
            set { SetBit(1, value); }
        }

        public bool OtherRead
        {
            get { return GetBit(2); }
            set { SetBit(2, value); }
        }

        public bool GroupExecute
        {
            get { return GetBit(3); }
            set { SetBit(3, value); }
        }

        public bool GroupWrite
        {
            get { return GetBit(4); }
            set { SetBit(4, value); }
        }

        public bool GroupRead
        {
            get { return GetBit(5); }
            set { SetBit(5, value); }
        }

        public bool UserExecute
        {
            get { return GetBit(6); }
            set { SetBit(6, value); }
        }

        public bool UserWrite
        {
            get { return GetBit(7); }
            set { SetBit(7, value); }
        }

        public bool UserRead
        {
            get { return GetBit(8); }
            set { SetBit(8, value); }
        }

        public bool Sticky
        {
            get { return GetBit(9); }
            set { SetBit(9, value); }
        }

        public bool SetGid
        {
            get { return GetBit(10); }
            set { SetBit(10, value); }
        }

        public bool SetUid
        {
            get { return GetBit(11); }
            set { SetBit(11, value); }
        }

        public FilePermissions() :
            this(0)
        {
        }

        public FilePermissions(int numeric)
        {
            Numeric = numeric;
        }

        public override string ToString()
        {
            return Text;
        }

        private const string BasicSymbols = "rwxrwxrwx";
        private const string CombinedSymbols = "--s--s--t";
        private const string ExtendedSymbols = "--S--S--T";
        private const char UnsetSymbol = '-';

        internal static FilePermissions CreateReadOnlyFromText(string text)
        {
            FilePermissions result = new FilePermissions
            {
                Numeric = TextToNumeric(text),
                _readOnly = true
            };
            return result;
        }

        internal static int TextToNumeric(string text)
        {
            if (text.Length != BasicSymbols.Length)
            {
                throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not valid permissions string", text));
            }

            int result = 0;
            int flag = 1;
            int extendedFlag = 0x200;
            for (int i = text.Length - 1; i >= 0; i--)
            {
                if (text[i] == UnsetSymbol)
                {
                    // noop
                }
                else if (text[i] == CombinedSymbols[i])
                {
                    result |= flag | extendedFlag;
                }
                else if (text[i] == ExtendedSymbols[i])
                {
                    result |= extendedFlag;
                }
                else
                {
                    result |= flag;
                }

                flag <<= 1;
                if (i % 3 == 0)
                {
                    extendedFlag <<= 1;
                }
            }

            return result;
        }

        private static string NumericToText(int numeric)
        {
            char[] buf = new char[BasicSymbols.Length];

            int flag = 1;
            int extendedFlag = 0x200;
            bool extendedPos = true;
            int i = BasicSymbols.Length - 1;
            while (i >= 0)
            {
                char symbol;
                if (extendedPos &&
                    ((numeric & (flag | extendedFlag)) == (flag | extendedFlag)))
                {
                    symbol = CombinedSymbols[i];
                }
                else if ((numeric & flag) != 0)
                {
                    symbol = BasicSymbols[i];
                }
                else if (extendedPos && ((numeric & extendedFlag) != 0))
                {
                    symbol = ExtendedSymbols[i];
                }
                else
                {
                    symbol = UnsetSymbol;
                }

                buf[i] = symbol;

                flag <<= 1;
                i--;
                extendedPos = ((i % 3) == 2);
                if (extendedPos)
                {
                    extendedFlag <<= 1;
                }
            }

            return new string(buf);
        }

        private bool GetBit(int bit)
        {
            return (Numeric & (1 << bit)) != 0;
        }

        private void SetBit(int bit, bool value)
        {
            if (value)
            {
                Numeric |= (1 << bit);
            }
            else
            {
                Numeric &= ~(1 << bit);
            }
        }

        private int _numeric;
        private bool _readOnly;
    }
}
