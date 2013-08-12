using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Globalization;

namespace WinSCP
{
    [Guid("F25C49A5-74A6-4E8F-AEB4-5B4E0DDF0EF9")]
    [ComVisible(true)]
    public enum Protocol
    {
        Sftp = 0,
        Scp = 1,
        Ftp = 2,
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
        ExplicitTls = 3,
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
        public string Password { get; set; }
        public TimeSpan Timeout { get { return _timeout; } set { SetTimeout(value); } }

        // SSH
        public string SshHostKeyFingerprint { get { return _sshHostKeyFingerprint; } set { SetSshHostKeyFingerprint(value); } }
        public bool GiveUpSecurityAndAcceptAnySshHostKey { get; set; }
        public string SshPrivateKeyPath { get; set; }

        // FTP
        public FtpMode FtpMode { get; set; }
        public FtpSecure FtpSecure { get; set; }
        public string TlsHostCertificateFingerprint { get { return _tlsHostCertificateFingerprint; } set { SetHostTlsCertificateFingerprint(value); } }
        [Obsolete("Use TlsHostCertificateFingerprint")]
        public string SslHostCertificateFingerprint { get { return TlsHostCertificateFingerprint; } set { TlsHostCertificateFingerprint = value; } }
        public bool GiveUpSecurityAndAcceptAnyTlsHostCertificate { get; set; }
        [Obsolete("Use GiveUpSecurityAndAcceptAnyTlsHostCertificate")]
        public bool GiveUpSecurityAndAcceptAnySslHostCertificate { get { return GiveUpSecurityAndAcceptAnyTlsHostCertificate; } set { GiveUpSecurityAndAcceptAnyTlsHostCertificate = value; } }

        public void AddRawSettings(string setting, string value)
        {
            RawSettings.Add(setting, value);
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

        private string _sshHostKeyFingerprint;
        private string _tlsHostCertificateFingerprint;
        private TimeSpan _timeout;
        private int _portNumber;

        private const string _listPattern = @"{0}(;{0})*";
        private const string _sshHostKeyPattern = @"(ssh-rsa |ssh-dss )?\d+ ([0-9a-f]{2}:){15}[0-9a-f]{2}";
        private static readonly Regex _sshHostKeyRegex =
            new Regex(string.Format(CultureInfo.InvariantCulture, _listPattern, _sshHostKeyPattern));
        private const string _tlsCertificatePattern = @"([0-9a-f]{2}:){19}[0-9a-f]{2}";
        private static readonly Regex _tlsCertificateRegex =
            new Regex(string.Format(CultureInfo.InvariantCulture, _listPattern, _tlsCertificatePattern));
    }
}
