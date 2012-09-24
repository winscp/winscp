This is the README file for source code package of WinSCP.

To build WinSCP you need:
- Embarcadero C++ Builder XE2 Professional.
- Copy MFC source code from Borland C++ Builder 6 Professional and
  build its Unicode version (see readme_mfc.txt).
- nasm from http://www.nasm.us/
- To build 64-bit version of drag&drop shell extension, you need
  Windows Platform SDK:
  http://msdn.microsoft.com/en-us/windows/bb980924

To build WinSCP from source by yourself, modify and use 'build.bat' in root
folder of source code package.

Directory structure:
/            project files of all native libraries and executables
/components  native WinSCP visual components
/console     console interface
/core        core (non-visual) part of WinSCP (SSH, SFTP, FTP and SCP code)
/deployment  Inno Setup script to create setup package (see /deployment/readme)
/dragext     drag&drop shell extension
/filezilla   source code of FileZilla FTP client
/forms       visual part of WinSCP (dialogs and windows)
/openssl     source code of subset of OpenSSL for SSL/TLS support for FTP
/packages    general visual components (both mine and 3rd party)
/putty       source code of Putty SSH client
/resource    resources strings
/windows     other sources

WinSCP homepage is http://winscp.net/

See the file 'licence.txt' for the licence conditions.
