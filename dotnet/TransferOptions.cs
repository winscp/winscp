using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("6B19CBFA-0D81-4B36-A587-E11AA6A06214")]
    [ComVisible(true)]
    public enum TransferMode
    {
        Binary = 0,
        Ascii = 1,
        Automatic = 2,
    }

    [Guid("155B841F-39D4-40C8-BA87-C79675E14CE3")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class TransferOptions
    {
        public bool PreserveTimestamp { get; set; }
        public FilePermissions FilePermissions { get; set; }
        public TransferMode TransferMode { get; set; }
        public string FileMask { get; set; }
        public TransferResumeSupport ResumeSupport { get; private set; }

        public TransferOptions()
        {
            PreserveTimestamp = true;
            TransferMode = TransferMode.Binary;
            ResumeSupport = new TransferResumeSupport();
        }

        internal string ToSwitches()
        {
            List<string> switches = new List<string>();

            if (FilePermissions != null)
            {
                switches.Add(Session.FormatSwitch("permissions", FilePermissions.Octal));
            }
            else
            {
                switches.Add("-nopermissions");
            }

            switches.Add(Session.BooleanSwitch(PreserveTimestamp, "preservetime", "nopreservetime"));

            string transferModeName;
            switch (TransferMode)
            {
                case TransferMode.Binary:
                    transferModeName = "binary";
                    break;
                case TransferMode.Ascii:
                    transferModeName = "ascii";
                    break;
                case TransferMode.Automatic:
                    transferModeName = "automatic";
                    break;
                default:
                    throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not supported", TransferMode));
            }
            switches.Add(Session.FormatSwitch("transfer", transferModeName));

            if (!string.IsNullOrEmpty(FileMask))
            {
                switches.Add(Session.FormatSwitch("filemask", FileMask));
            }

            if (ResumeSupport.State != TransferResumeSupportState.Default)
            {
                switches.Add(Session.FormatSwitch("resumesupport", ResumeSupport.ToString()));
            }

            return string.Join(" ", switches.ToArray());
        }
    }
}
