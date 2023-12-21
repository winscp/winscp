using System.Collections;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("39AA3D00-578C-49AF-B3E4-16CE26C710C6")]
    [ClassInterface(Constants.CollectionClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))] // See the comment at ReadOnlyInteropCollection
    public class RemoteFileInfoCollection : ReadOnlyInteropCollection<RemoteFileInfo>
    {
    }
}
