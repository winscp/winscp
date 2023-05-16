using System.Collections;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("0285917B-581A-4F6F-9A9D-1C34ABFB4E38")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))] // See the comment at ReadOnlyInteropCollection
    public class TransferEventArgsCollection : ReadOnlyInteropCollection<TransferEventArgs>
    {
    }
}
