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

    [Guid("E0F3C3C2-C812-48F1-A711-E0BD0F703976")]
    [ComVisible(true)]
    public enum OverwriteMode
    {
        Overwrite = 0,
        Resume = 1,
        Append = 2,
    }

    [Guid("155B841F-39D4-40C8-BA87-C79675E14CE3")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class TransferOptions : ITransferOptions
    {
        public bool PreserveTimestamp { get; set; }
        public FilePermissions FilePermissions { get; set; }
        public TransferMode TransferMode { get; set; }
        public string FileMask { get; set; }
        public TransferResumeSupport ResumeSupport { get; set; }
        public int SpeedLimit { get; set; }
        public OverwriteMode OverwriteMode { get; set; }

        internal Dictionary<string, string> RawSettings { get; private set; }

        public TransferOptions()
        {
            PreserveTimestamp = true;
            TransferMode = TransferMode.Binary;
            ResumeSupport = new TransferResumeSupport();
            OverwriteMode = OverwriteMode.Overwrite;
            RawSettings = new Dictionary<string,string>();
        }

        public void AddRawSettings(string setting, string value)
        {
            RawSettings.Add(setting, value);
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

            if (SpeedLimit > 0)
            {
                switches.Add(Session.FormatSwitch("speed", SpeedLimit.ToString(CultureInfo.InvariantCulture)));
            }

            switch (OverwriteMode)
            {
                case OverwriteMode.Overwrite:
                    // noop
                    break;
                case OverwriteMode.Resume:
                    switches.Add(Session.FormatSwitch("resume"));
                    break;
                case OverwriteMode.Append:
                    switches.Add(Session.FormatSwitch("append"));
                    break;
                default:
                    throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "{0} is not supported", OverwriteMode));
            }

            string arguments = string.Join(" ", switches.ToArray());
            Tools.AddRawParameters(ref arguments, RawSettings, "-rawtransfersettings", true);

            return arguments;
        }
    }
}
