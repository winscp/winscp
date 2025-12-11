using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Globalization;
using System.Security;

namespace WinSCP
{
    [Guid("F25C49A5-74A6-4E8F-AEB4-5B4E0DDF0EF9")]
    [ComVisible(true)]
    public enum Protocol
    {
        Sftp = 0,
        Scp = 1,
        Ftp = 2,
        Webdav = 3,
        S3 = 4,
    }

    [Guid("D924FAB9-FCE7-47B8-9F23-5717698384D3")]
    [ComVisible(true)]
    public enum FtpMode
    {
        Passive = 0,
        Active = 1,
    }

    [Guid("F2FC81EB-4761-4A4E-A3EC-4AFDD474C18C")]
    [ComVisible(true)]
    public enum FtpSecure
    {
        None = 0,
        Implicit = 1,
        Explicit = 3,
    }

    [Guid("8A98AB8F-30E8-4539-A3DE-A33DDC43B33C")]
    [ComVisible(true)]
    public enum SshHostKeyPolicy
    {
        Check = 0,
        GiveUpSecurityAndAcceptAny = 1,
        AcceptNew = 2,
    }

    [Guid("2D4EF368-EE80-4C15-AE77-D12AEAF4B00A")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class SessionOptions : ISessionOptions
    {
        public SessionOptions()
        {
            Timeout = new TimeSpan(0, 0, 15);
            RawSettings = new Dictionary<string,string>();
        }

        public string Name { get { return GetName(); } set { _name = value; } }
        public Protocol Protocol { get { return _protocol; } set { SetProtocol(value); } }
        public string HostName { get; set; }
        public int PortNumber { get { return _portNumber; } set { SetPortNumber(value); } }
        public string UserName { get; set; }
        public string Password { get { return GetPassword(_securePassword); } set { SetPassword(ref _securePassword, value); } }
        public SecureString SecurePassword { get { return _securePassword; } set { _securePassword = value; } }
        public string NewPassword { get { return GetPassword(_secureNewPassword); } set { SetPassword(ref _secureNewPassword, value); } }
        public SecureString SecureNewPassword { get { return _secureNewPassword; } set { _secureNewPassword = value; } }
        public TimeSpan Timeout { get { return _timeout; } set { SetTimeout(value); } }
        public int TimeoutInMilliseconds { get { return Tools.TimeSpanToMilliseconds(Timeout); } set { Timeout = Tools.MillisecondsToTimeSpan(value); } }
        public string PrivateKeyPassphrase { get { return GetPassword(_securePrivateKeyPassphrase); } set { SetPassword(ref _securePrivateKeyPassphrase, value); } }
        public SecureString SecurePrivateKeyPassphrase { get { return _securePrivateKeyPassphrase; } set { _securePrivateKeyPassphrase = value; } }
        public string RootPath { get { return _rootPath; } set { SetRootPath(value); } }
        public bool Secure { get; set; }

        // SSH
        public string SshHostKeyFingerprint { get { return _sshHostKeyFingerprint; } set { SetSshHostKeyFingerprint(value); } }
        public SshHostKeyPolicy SshHostKeyPolicy { get; set; }
        [Obsolete("Use SshHostKeyPolicy")]
        public bool GiveUpSecurityAndAcceptAnySshHostKey { get { return GetGiveUpSecurityAndAcceptAnySshHostKey(); } set { SetGiveUpSecurityAndAcceptAnySshHostKey(value); } }
        public string SshPrivateKeyPath { get; set; }
        public string SshPrivateKey { get; set; }
        [Obsolete("Use PrivateKeyPassphrase")]
        public string SshPrivateKeyPassphrase { get { return PrivateKeyPassphrase; } set { PrivateKeyPassphrase = value; } }

        // FTP
        public FtpMode FtpMode { get; set; }
        public FtpSecure FtpSecure { get; set; }

        // WebDAV
        [Obsolete("Use Secure")]
        public bool WebdavSecure { get { return Secure; } set { Secure = value; } }
        [Obsolete("Use RootPath")]
        public string WebdavRoot { get { return RootPath; } set { RootPath = value; } }

        // TLS
        public string TlsHostCertificateFingerprint { get { return _tlsHostCertificateFingerprint; } set { SetHostTlsCertificateFingerprint(value); } }
        public bool GiveUpSecurityAndAcceptAnyTlsHostCertificate { get; set; }
        public string TlsClientCertificatePath { get; set; }

        public void AddRawSettings(string setting, string value)
        {
            RawSettings.Add(setting, value);
        }

        public void ParseUrl(string url)
        {
            if (url == null)
            {
                throw new ArgumentNullException(nameof(url));
            }

            url = url.Trim();
            const string protocolSeparator = "://";
            int index = url.IndexOf(protocolSeparator, StringComparison.OrdinalIgnoreCase);
            if (index < 0)
            {
                throw new ArgumentException("Protocol not specified", nameof(url));
            }

            string protocol = url.Substring(0, index).Trim();
            if (!ParseProtocol(protocol))
            {
                throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "Unknown protocol {0}", protocol), nameof(url));
            }

            url = url.Substring(index + protocolSeparator.Length).Trim();
            index = url.IndexOf('/');
            RootPath = null;
            if (index >= 0)
            {
                string path = url.Substring(index).Trim();
                url = url.Substring(0, index).Trim();
                string parameters = path;
                path = CutToChar(ref parameters, ';');
                if (!string.IsNullOrEmpty(path) && (path != "/"))
                {
                    if ((Protocol != Protocol.Webdav) && (Protocol != Protocol.S3))
                    {
                        throw new ArgumentException("Root path can be specified for WebDAV and S3 protocols only", nameof(url));
                    }
                    RootPath = path;
                }

                // forward compatibility
                if (!string.IsNullOrEmpty(parameters))
                {
                    throw new ArgumentException("No session parameters are supported", nameof(url));
                }
            }

            index = url.LastIndexOf('@');

            string hostInfo;
            string userInfo = null;
            if (index >= 0)
            {
                userInfo = url.Substring(0, index).Trim();
                hostInfo = url.Substring(index + 1).Trim();
            }
            else
            {
                hostInfo = url;
            }

            PortNumber = 0;
            string portNumber = null;
            if ((hostInfo.Length >= 2) && (hostInfo[0] == '[') && ((index = hostInfo.IndexOf(']')) > 0))
            {
                HostName = hostInfo.Substring(1, index - 1).Trim();
                hostInfo = hostInfo.Substring(index + 1).Trim();
                if (hostInfo.Length > 0)
                {
                    if (hostInfo[0] != ':')
                    {
                        throw new ArgumentException("Unexpected syntax after ]", nameof(url));
                    }
                    else
                    {
                        portNumber = hostInfo.Substring(1);
                    }
                }
            }
            else
            {
                HostName = UriUnescape(CutToChar(ref hostInfo, ':'));
                portNumber = hostInfo;
            }

            // Contrary to TSessionData::ParseUrl, not converting Webdav to S3 on S3 hostname.
            // Not sure if it is desirable and WinSCP will do the conversion for us later anyway.

            if (string.IsNullOrEmpty(HostName))
            {
                throw new ArgumentException("No host name", nameof(url));
            }

            if (string.IsNullOrEmpty(portNumber))
            {
                PortNumber = 0;
            }
            else
            {
                portNumber = UriUnescape(portNumber);
                if (!int.TryParse(portNumber, 0, CultureInfo.InvariantCulture, out int number))
                {
                    throw new ArgumentException(string.Format(CultureInfo.InvariantCulture, "{0} is not a valid port number", portNumber), nameof(url));
                }
                else
                {
                    PortNumber = number;
                }
            }

            UserName = null;
            Password = null;
            SshHostKeyFingerprint = null;
            SshHostKeyPolicy = SshHostKeyPolicy.Check;
            TlsHostCertificateFingerprint = null;
            GiveUpSecurityAndAcceptAnyTlsHostCertificate = false;
            if (!string.IsNullOrEmpty(userInfo))
            {
                string parameters = userInfo;
                userInfo = CutToChar(ref parameters, ';');

                bool hasPassword = (userInfo.IndexOf(':') >= 0);
                UserName = EmptyToNull(UriUnescape(CutToChar(ref userInfo, ':')));
                Password = hasPassword ? UriUnescape(userInfo) : null;

                while (!string.IsNullOrEmpty(parameters))
                {
                    string parameter = CutToChar(ref parameters, ';');
                    string parameterName = CutToChar(ref parameter, '=');
                    parameter = UriUnescape(parameter);
                    const string RawSettingsPrefix = "x-";
                    if (parameterName.Equals("fingerprint", StringComparison.OrdinalIgnoreCase))
                    {
                        switch (Protocol)
                        {
                            case Protocol.Sftp:
                            case Protocol.Scp:
                                SshHostKeyFingerprint = parameter;
                                break;

                            case Protocol.Ftp:
                            case Protocol.Webdav:
                            case Protocol.S3:
                                TlsHostCertificateFingerprint = parameter;
                                break;

                            default:
                                throw new ArgumentException();
                        }
                    }
                    else if (parameterName.StartsWith(RawSettingsPrefix, StringComparison.OrdinalIgnoreCase))
                    {
                        parameterName = UriUnescape(parameterName.Substring(RawSettingsPrefix.Length));
                        if (parameterName.Equals("name", StringComparison.OrdinalIgnoreCase))
                        {
                            Name = parameter;
                        }
                        else
                        {
                            AddRawSettings(parameterName, parameter);
                        }
                    }
                    else
                    {
                        throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "Unsupported connection parameter {0}", parameterName), nameof(url));
                    }
                }
            }
        }

        private bool ParseProtocol(string protocol)
        {
            bool result = true;
            FtpSecure = FtpSecure.None;

            if (protocol.Equals("sftp", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Sftp;
            }
            else if (protocol.Equals("scp", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Scp;
            }
            else if (protocol.Equals("ftp", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Ftp;
            }
            else if (protocol.Equals("ftps", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Ftp;
                FtpSecure = FtpSecure.Implicit;
            }
            else if (protocol.Equals("ftpes", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Ftp;
                FtpSecure = FtpSecure.Explicit;
            }
            else if (protocol.Equals("dav", StringComparison.OrdinalIgnoreCase) ||
                     protocol.Equals("http", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Webdav;
            }
            else if (protocol.Equals("davs", StringComparison.OrdinalIgnoreCase) ||
                     protocol.Equals("https", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Webdav;
                Secure = true;
            }
            else if (protocol.Equals("s3plain", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.S3;
                Secure = false;
            }
            else if (protocol.Equals("s3", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.S3;
            }
            else
            {
                result = false;
            }
            return result;
        }

        private static string EmptyToNull(string s)
        {
            if (string.IsNullOrEmpty(s))
            {
                return null;
            }
            else
            {
                return s;
            }
        }

        private static string UriUnescape(string s)
        {
            return Uri.UnescapeDataString(s);
        }

        private static string CutToChar(ref string s, char c)
        {
            int index = s.IndexOf(c);
            string result;
            if (index >= 0)
            {
                result = s.Substring(0, index).Trim();
                s = s.Substring(index + 1).Trim();
            }
            else
            {
                result = s;
                s = string.Empty;
            }
            return result;
        }

        internal Dictionary<string, string> RawSettings { get; private set; }
        internal bool IsSsh { get { return (Protocol == Protocol.Sftp) || (Protocol == Protocol.Scp); } }
        internal bool IsTls { get { return GetIsTls(); } }

        private bool GetIsTls()
        {
            return
                ((Protocol == Protocol.Ftp) && (FtpSecure != FtpSecure.None)) ||
                (((Protocol == Protocol.Webdav) || (Protocol == Protocol.S3)) && Secure);
        }

        private void SetSshHostKeyFingerprint(string s)
        {
            if (s != null)
            {
                Match match = _sshHostKeyRegex.Match(s);

                if (!match.Success || (match.Length != s.Length))
                {
                    throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "SSH host key fingerprint \"{0}\" does not match pattern /{1}/", s, _sshHostKeyRegex));
                }
            }

            _sshHostKeyFingerprint = s;
        }

        private void SetHostTlsCertificateFingerprint(string s)
        {
            if (s != null)
            {
                Match match = _tlsCertificateRegex.Match(s);

                if (!match.Success || (match.Length != s.Length))
                {
                    throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "TLS host certificate fingerprint \"{0}\" does not match pattern /{1}/", s, _tlsCertificateRegex));
                }
            }

            _tlsHostCertificateFingerprint = s;
        }

        private void SetTimeout(TimeSpan value)
        {
            if (value <= TimeSpan.Zero)
            {
                throw new ArgumentException("Timeout has to be positive non-zero value");
            }

            _timeout = value;
        }

        private void SetPortNumber(int value)
        {
            if ((value < 0) || (value > 65535))
            {
                throw new ArgumentOutOfRangeException("Port number has to be in range from 0 to 65535");
            }

            _portNumber = value;
        }

        private void SetProtocol(Protocol value)
        {
            _protocol = value;
            if ((_protocol == Protocol.S3) && string.IsNullOrEmpty(HostName))
            {
                HostName = "s3.amazonaws.com";
                Secure = true;
            }
        }

        private void SetRootPath(string value)
        {
            if (!string.IsNullOrEmpty(value) && (value[0] != '/'))
            {
                throw new ArgumentException("Root path has to start with a slash");
            }
            _rootPath = value;
        }

        private static void SetPassword(ref SecureString securePassword, string value)
        {
            if (value == null)
            {
                securePassword = null;
            }
            else
            {
                securePassword = new SecureString();
                foreach (char c in value)
                {
                    securePassword.AppendChar(c);
                }
            }
        }

        private static string GetPassword(SecureString securePassword)
        {
            if (securePassword == null)
            {
                return null;
            }
            else
            {
                IntPtr ptr = IntPtr.Zero;
                try
                {
                    ptr = Marshal.SecureStringToGlobalAllocUnicode(securePassword);
                    return Marshal.PtrToStringUni(ptr);
                }
                finally
                {
                    Marshal.ZeroFreeGlobalAllocUnicode(ptr);
                }
            }
        }

        private string GetName()
        {
            string result;
            if (_name != null)
            {
                result = _name;
            }
            else
            {
                if (!string.IsNullOrEmpty(HostName) && !string.IsNullOrEmpty(UserName))
                {
                    result = $"{UserName}@{HostName}";
                }
                else if (!string.IsNullOrEmpty(HostName))
                {
                    result = HostName;
                }
                else
                {
                    result = "session";
                }
            }
            return result;
        }

        public override string ToString()
        {
            return Name;
        }

        private void SetGiveUpSecurityAndAcceptAnySshHostKey(bool value)
        {
            SshHostKeyPolicy = value ? SshHostKeyPolicy.GiveUpSecurityAndAcceptAny : SshHostKeyPolicy.Check;
        }

        private bool GetGiveUpSecurityAndAcceptAnySshHostKey()
        {
            return (SshHostKeyPolicy == SshHostKeyPolicy.GiveUpSecurityAndAcceptAny);
        }

        private SecureString _securePassword;
        private SecureString _secureNewPassword;
        private SecureString _securePrivateKeyPassphrase;
        private string _sshHostKeyFingerprint;
        private string _tlsHostCertificateFingerprint;
        private TimeSpan _timeout;
        private int _portNumber;
        private string _rootPath;
        private Protocol _protocol;
        private string _name;

        private const string _listPattern = @"{0}(;{0})*";
        private const string _sshHostKeyPattern = @"((ssh-rsa|ssh-dss|ssh-ed25519|ecdsa-sha2-nistp(256|384|521))( |-))?(\d+ )?(([0-9a-fA-F]{2}(:|-)){15}[0-9a-fA-F]{2}|[0-9a-zA-Z+/\-_]{43}=?)";
        private static readonly Regex _sshHostKeyRegex =
            new Regex(string.Format(CultureInfo.InvariantCulture, _listPattern, _sshHostKeyPattern));
        private const string _tlsCertificatePattern = @"((([0-9a-fA-F]{2}[:\-]){31})|(([0-9a-fA-F]{2}[:\-]){19}))[0-9a-fA-F]{2}";
        private static readonly Regex _tlsCertificateRegex =
            new Regex(string.Format(CultureInfo.InvariantCulture, _listPattern, _tlsCertificatePattern));
    }
}
