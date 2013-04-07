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
/source             project files of all native libraries and executables
/source/components  native WinSCP visual components
/source/console     console interface
/source/core        core (non-visual) part of WinSCP
                    (SSH, SFTP, FTP and SCP code)
/source/dragext     drag&drop shell extension
/source/filezilla   source code of FileZilla FTP client
/source/forms       visual part of WinSCP (dialogs and windows)
/source/packages    general visual components (both mine and 3rd party)
/source/putty       source code of Putty SSH client
/source/resource    resources strings
/source/windows     other sources
/deployment         Inno Setup script to create setup package
                    (see /deployment/readme)
/dotnet             source code of WinSCP .NET assembly
/libs               3rd party libraries

WinSCP homepage is http://winscp.net/

See the file 'licence.txt' for the licence conditions.
