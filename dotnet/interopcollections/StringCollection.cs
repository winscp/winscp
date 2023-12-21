using System.Collections;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("E402CB1F-6219-4C79-9EDF-1914D9589909")]
    [ClassInterface(Constants.CollectionClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))] // See the comment at ReadOnlyInteropCollection
    public class StringCollection : ReadOnlyInteropCollection<string>
    {
    }
}
