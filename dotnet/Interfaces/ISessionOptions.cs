using System;
using System.Security;

namespace WinSCP
{
    public interface ISessionOptions
    {
        string Name { get; set; }
        Protocol Protocol { get; set; }
        string HostName { get; set; }
        int PortNumber { get; set; }
        string UserName { get; set; }
        string Password { get; set; }
        SecureString SecurePassword { get; set; }
        string NewPassword { get; set; }
        SecureString SecureNewPassword { get; set; }
        TimeSpan Timeout { get; set; }
        int TimeoutInMilliseconds { get; set; }
        string PrivateKeyPassphrase { get; set; }
        SecureString SecurePrivateKeyPassphrase { get; set; }
        string RootPath { get; set; }
        bool Secure { get; set; }

        // SSH specific
        string SshHostKeyFingerprint { get; set; }
        SshHostKeyPolicy SshHostKeyPolicy { get; set; }
        string SshPrivateKeyPath { get; set; }
        string SshPrivateKey { get; set; }

        // FTP specific
        FtpMode FtpMode { get; set; }
        FtpSecure FtpSecure { get; set; }

        // TLS specific
        string TlsHostCertificateFingerprint { get; set; }
        bool GiveUpSecurityAndAcceptAnyTlsHostCertificate { get; set; }
        string TlsClientCertificatePath { get; set; }

        void AddRawSettings(string setting, string value);
    }
}
