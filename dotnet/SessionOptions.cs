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
        [Obsolete("Use FtpSecure.Explicit")]
        ExplicitTls = Explicit,
        [Obsolete("Use FtpSecure.Explicit")]
        ExplicitSsl = 2,
    }

    [Guid("2D4EF368-EE80-4C15-AE77-D12AEAF4B00A")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class SessionOptions
    {
        public SessionOptions()
        {
            Timeout = new TimeSpan(0, 0, 15);
            RawSettings = new Dictionary<string,string>();
        }

        public Protocol Protocol { get; set; }
        public string HostName { get; set; }
        public int PortNumber { get { return _portNumber; } set { SetPortNumber(value); } }
        public string UserName { get; set; }
        public string Password { get { return GetPassword(); } set { SetPassword(value); } }
        public SecureString SecurePassword { get; set; }
        public TimeSpan Timeout { get { return _timeout; } set { SetTimeout(value); } }
        public int TimeoutInMilliseconds { get { return Tools.TimeSpanToMilliseconds(Timeout); } set { Timeout = Tools.MillisecondsToTimeSpan(value); } }

        // SSH
        public string SshHostKeyFingerprint { get { return _sshHostKeyFingerprint; } set { SetSshHostKeyFingerprint(value); } }
        public bool GiveUpSecurityAndAcceptAnySshHostKey { get; set; }
        public string SshPrivateKeyPath { get; set; }
        public string SshPrivateKeyPassphrase { get; set; }

        // FTP
        public FtpMode FtpMode { get; set; }
        public FtpSecure FtpSecure { get; set; }

        // WebDAV
        public bool WebdavSecure { get; set; }
        public string WebdavRoot { get { return _webdavRoot; } set { SetWebdavRoot(value); } }

        // TLS
        public string TlsHostCertificateFingerprint { get { return _tlsHostCertificateFingerprint; } set { SetHostTlsCertificateFingerprint(value); } }
        public bool GiveUpSecurityAndAcceptAnyTlsHostCertificate { get; set; }

        public void AddRawSettings(string setting, string value)
        {
            RawSettings.Add(setting, value);
        }

        public void ParseUrl(string url)
        {
            url = url.Trim();
            const string protocolSeparator = "://";
            int index = url.IndexOf(protocolSeparator, StringComparison.OrdinalIgnoreCase);
            if (index < 0)
            {
                throw new ArgumentException("Protocol not specified", "url");
            }
            string protocol = url.Substring(0, index).Trim();

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
            else if (protocol.Equals("http", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Webdav;
            }
            else if (protocol.Equals("https", StringComparison.OrdinalIgnoreCase))
            {
                Protocol = Protocol.Webdav;
                WebdavSecure = true;
            }
            else
            {
                throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "Unknown protocol {0}", protocol), "url");
            }

            url = url.Substring(index + protocolSeparator.Length).Trim();
            index = url.IndexOf('/');
            WebdavRoot = null;
            if (index >= 0)
            {
                string path = url.Substring(index).Trim();
                url = url.Substring(0, index).Trim();
                string parameters = path;
                path = CutToChar(ref parameters, ';');
                if (!string.IsNullOrEmpty(path) && (path != "/"))
                {
                    if (Protocol != Protocol.Webdav)
                    {
                        throw new ArgumentException("Root folder can be specified for WebDAV protocol only", "url");
                    }
                    WebdavRoot = path;
                }

                // forward compatibility
                if (!string.IsNullOrEmpty(parameters))
                {
                    throw new ArgumentException("No session parameters are supported", "url");
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
                        throw new ArgumentException("Unexpected syntax after ]", "url");
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

            if (string.IsNullOrEmpty(HostName))
            {
                throw new ArgumentException("No host name", "url");
            }

            if (string.IsNullOrEmpty(portNumber))
            {
                PortNumber = 0;
            }
            else
            {
                portNumber = UriUnescape(portNumber);
                int number;
                if (!int.TryParse(portNumber, 0, CultureInfo.InvariantCulture, out number))
                {
                    throw new ArgumentException(string.Format(CultureInfo.InvariantCulture, "{0} is not a valid port number", portNumber), "url");
                }
                else
                {
                    PortNumber = number;
                }
            }

            UserName = null;
            Password = null;
            SshHostKeyFingerprint = null;
            GiveUpSecurityAndAcceptAnySshHostKey = false;
            TlsHostCertificateFingerprint = null;
            GiveUpSecurityAndAcceptAnyTlsHostCertificate = false;
            if (!string.IsNullOrEmpty(userInfo))
            {
                string parameters = userInfo;
                userInfo = CutToChar(ref parameters, ';');

                UserName = EmptyToNull(UriUnescape(CutToChar(ref userInfo, ':')));
                Password = EmptyToNull(UriUnescape(userInfo));

                while (!string.IsNullOrEmpty(parameters))
                {
                    string parameter = CutToChar(ref parameters, ';');
                    string parameterName = CutToChar(ref parameter, '=');
                    if (parameterName.Equals("fingerprint", StringComparison.OrdinalIgnoreCase))
                    {
                        SshHostKeyFingerprint = parameter;
                    }
                    else
                    {
                        throw new ArgumentException(string.Format(CultureInfo.CurrentCulture, "Unsupported connection parameter {0}", parameterName), "url");
                    }
                }
            }
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
            if (value < 0)
            {
                throw new ArgumentException("Port number cannot be negative");
            }

            _portNumber = value;
        }

        private void SetWebdavRoot(string value)
        {
            if (!string.IsNullOrEmpty(value) && (value[0] != '/'))
            {
                throw new ArgumentException("WebDAV root path has to start with slash");
            }
            _webdavRoot = value;
        }

        private void SetPassword(string value)
        {
            if (value == null)
            {
                SecurePassword = null;
            }
            else
            {
                SecurePassword = new SecureString();
                foreach (char c in value)
                {
                    SecurePassword.AppendChar(c);
                }
            }
        }

        private string GetPassword()
        {
            if (SecurePassword == null)
            {
                return null;
            }
            else
            {
                IntPtr ptr = IntPtr.Zero;
                try
                {
                    ptr = Marshal.SecureStringToGlobalAllocUnicode(SecurePassword);
                    return Marshal.PtrToStringUni(ptr);
                }
                finally
                {
                    Marshal.ZeroFreeGlobalAllocUnicode(ptr);
                }
            }
        }

        private string _sshHostKeyFingerprint;
        private string _tlsHostCertificateFingerprint;
        private TimeSpan _timeout;
        private int _portNumber;
        private string _webdavRoot;

        private const string _listPattern = @"{0}(;{0})*";
        private const string _sshHostKeyPattern = @"((ssh-rsa|ssh-dss)( |-))?(\d+ )?([0-9a-f]{2}(:|-)){15}[0-9a-f]{2}";
        private static readonly Regex _sshHostKeyRegex =
            new Regex(string.Format(CultureInfo.InvariantCulture, _listPattern, _sshHostKeyPattern));
        private const string _tlsCertificatePattern = @"([0-9a-f]{2}:){19}[0-9a-f]{2}";
        private static readonly Regex _tlsCertificateRegex =
            new Regex(string.Format(CultureInfo.InvariantCulture, _listPattern, _tlsCertificatePattern));
    }
}
