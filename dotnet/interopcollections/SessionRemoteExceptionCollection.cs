using System.Collections;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("2309282F-B89B-4F6B-AEB1-D3E1629B7033")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))] // See the comment at ReadOnlyInteropCollection
    public class SessionRemoteExceptionCollection : ReadOnlyInteropCollection<SessionRemoteException>
    {
    }
}
