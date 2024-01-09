using System.Collections;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("28957CC8-DEBC-48D0-841B-48AD3CB3B49F")]
    [ClassInterface(Constants.CollectionClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))] // See the comment at ReadOnlyInteropCollection
    public class ComparisonDifferenceCollection : ReadOnlyInteropCollection<ComparisonDifference>
    {
    }
}
