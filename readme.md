[WinSCP](https://winscp.net/) is a popular free file manager for Windows supporting SFTP, FTP, FTPS, SCP, S3, WebDAV and local-to-local file transfers. A powerful tool to enhance your productivity with a user-friendly interface and automation options like .NET assembly or batch file scripting. Use WinSCP also for file editing, directory synchronization and site management. WinSCP is open-source and well documented. It is available in English and many other languages.

To build WinSCP you need:
- [Embarcadero C++Builder 11 Professional](https://www.embarcadero.com/products/cbuilder).
- [Build Tools and Agents for Visual Studio 2022](https://visualstudio.microsoft.com/downloads/) (for C# 9.0)
- [nasm](https://www.nasm.us/) (store it to `buildtools/tools/nasm.exe`)
- [Object file converter](https://www.agner.org/optimize/#objconv) (store it to `buildtools/tools/objconv.exe`)

To build WinSCP from source by yourself, modify and use [`build.bat`](build.bat) in root folder.

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
    /translations       translations of WinSCP strings

See the file [`license.txt`](license.txt) for the license conditions.

[![NuGet](https://img.shields.io/nuget/v/WinSCP.svg)](https://www.nuget.org/packages/WinSCP/)
