using System.Collections;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("574FF430-FD40-41F9-9A04-971D3CF844B7")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))] // See the comment at ReadOnlyInteropCollection
    public class RemovalEventArgsCollection : ReadOnlyInteropCollection<RemovalEventArgs>
    {
    }
}
