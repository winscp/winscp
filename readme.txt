This is the README file for source code package of WinSCP.

To build WinSCP you need:
- Embarcadero C++ Builder XE6 Professional.
  https://www.embarcadero.com/products/cbuilder
- Build Tools and Agents for Visual Studio 2017 (for C# 7.0)
  https://www.visualstudio.com/
- nasm from http://www.nasm.us/
  (store it to buildtools/tools/nasm.exe)
- Object file converter from http://www.agner.org/optimize/#objconv
  (store it to buildtools/tools/objconv.exe)
- Build MFC (see readme_mfc.txt).

To build WinSCP from source by yourself, modify and use 'build.bat' in root
folder of source code package.

Directory structure:
/source             project files of all native libraries and executables
/source/components  native WinSCP visual components
/source/console     console interface
/source/core        core (non-visual) part of WinSCP
                    (SSH, SFTP, FTP, WebDAV, S3 and SCP code)
/source/dragext     drag&drop shell extension
/source/filezilla   source code of FileZilla FTP client
/source/forms       visual part of WinSCP (dialogs and windows)
/source/packages    general visual components (both mine and 3rd party)
/source/putty       source code of PuTTY SSH client
/source/resource    resources strings
/source/windows     other sources
/deployment         Inno Setup script to create setup package
                    (see /deployment/readme)
/dotnet             source code of WinSCP .NET assembly
/libs               3rd party libraries

WinSCP homepage is https://winscp.net/

See the file 'license.txt' for the license conditions.
