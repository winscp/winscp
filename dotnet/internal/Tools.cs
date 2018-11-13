using System;
using System.Collections.Generic;
using System.Globalization;

namespace WinSCP
{
    internal static class Tools
    {
        public static int TimeSpanToMilliseconds(TimeSpan value)
        {
            if ((value.TotalMilliseconds > int.MaxValue) || (value.TotalMilliseconds < int.MinValue))
            {
                throw new InvalidCastException(string.Format(CultureInfo.CurrentCulture, "Cannot convert {0} to integer", value));
            }
            return (int)value.TotalMilliseconds;
        }

        public static TimeSpan MillisecondsToTimeSpan(int value)
        {
            return TimeSpan.FromMilliseconds(value);
        }

        public static string ArgumentEscape(string value)
        {
            int i = 0;
            while (i < value.Length)
            {
                if (value[i] == '"')
                {
                    value = value.Insert(i, "\"");
                    ++i;
                }
                ++i;
            }
            return value;
        }

        public static void AddRawParameters(
            ref string arguments, Dictionary<string, string> parameters, string switchName, bool count)
        {
            if (parameters.Count > 0)
            {
                if (!string.IsNullOrEmpty(arguments))
                {
                    arguments += " ";
                }
                arguments += switchName;
                if (count)
                {
                    arguments += string.Format(CultureInfo.InvariantCulture, "[{0}]", parameters.Count);
                }

                foreach (KeyValuePair<string, string> rawSetting in parameters)
                {
                    arguments += string.Format(CultureInfo.InvariantCulture, " {0}=\"{1}\"", rawSetting.Key, ArgumentEscape(rawSetting.Value));
                }
            }
        }

        public static int LengthTo32Bit(long length)
        {
            if (length < int.MinValue || length > int.MaxValue)
            {
                throw new OverflowException(string.Format(CultureInfo.CurrentCulture, "Size {0} cannot be represented using 32-bit value", length));
            }

            return (int)length;
        }
    }
}
