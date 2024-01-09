using System.Runtime.InteropServices;

namespace WinSCP
{
    internal static class Constants
    {
        public const ClassInterfaceType ClassInterface = ClassInterfaceType.AutoDispatch;
        // Avoids warning from regasm and is probably the right choice anyway
        public const ClassInterfaceType CollectionClassInterface = ClassInterfaceType.None;
    }
}
