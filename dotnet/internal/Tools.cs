using System;
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
    }
}
