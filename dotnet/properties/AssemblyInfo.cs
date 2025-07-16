using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[assembly: AssemblyTitle("WinSCPnet")]
[assembly: AssemblyDescription("WinSCP scripting interface .NET wrapper")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("Martin Prikryl")]
[assembly: AssemblyProduct("WinSCP")]
[assembly: AssemblyCopyright("")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

[assembly: ComVisible(false)]

// The following GUID is for the ID of the typelib if this project is exposed to COM
// Duplicated in ConsoleRunner.cpp
[assembly: Guid("a0b93468-d98a-4845-a234-8076229ad93f")]

[assembly: AssemblyVersion(WinSCP.AssemblyConstants.Version)]
[assembly: AssemblyFileVersion(WinSCP.AssemblyConstants.Version)]
[assembly: AssemblyInformationalVersion(WinSCP.AssemblyConstants.ProductVersion)]

[assembly: CLSCompliant(true)]

[assembly: InternalsVisibleTo("Tests")]

namespace WinSCP
{
    internal static class AssemblyConstants
    {
        public const string UndefinedProductVersion = "9.9.9.9";

        public const string Version = "1.16.0.0";
        public const string ProductVersion = "6.5.4.0";
    }
}
