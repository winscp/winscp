The WinSCP .NET assembly is a .NET wrapper around WinSCP’s scripting interface that allows your code to connect to a remote machine and manipulate remote files over SFTP, FTP, WebDAV, S3 and SCP sessions.

The library is primarily intended for advanced automation tasks on Microsoft Windows that require conditional processing, loops or other control structures for which the basic scripting interface is too limited. The library is not a general purpose file transfer library. It particularly has a limited support for an interactive processing, and as such it is not well suited for use in GUI applications. For the same reason it is also difficult to use the assembly within a restricted environment like a web server, that limits or even restricts execution of external processes.

The NuGet package includes the assembly itself and a required WinSCP executable. When installed, it adds the assembly as reference to your project and sets up WinSCP executable to be copied to project output directory, so that it can be found on run-time.

For more information, visit WinSCP website:

* [Documentation](https://winscp.net/eng/docs/library)
* [Examples](https://winscp.net/eng/docs/library_examples)
