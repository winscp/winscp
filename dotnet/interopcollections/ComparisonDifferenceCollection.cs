using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("28957CC8-DEBC-48D0-841B-48AD3CB3B49F")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public class ComparisonDifferenceCollection : ICollection<ComparisonDifference>
    {
        public ComparisonDifference this[int index]
        {
            get
            {
                return _helper[index];
            }
            set
            {
                _helper[index] = value;
            }
        }

        #region ICollection<ComparisonDifference> Members

        public void Add(ComparisonDifference item)
        {
            _helper.Add(item);
        }

        public void Clear()
        {
            _helper.Clear();
        }

        public bool Contains(ComparisonDifference item)
        {
            return _helper.Contains(item);
        }

        public void CopyTo(ComparisonDifference[] array, int arrayIndex)
        {
            _helper.CopyTo(array, arrayIndex);
        }

        public int Count
        {
            get { return _helper.Count; }
        }

        public bool IsReadOnly
        {
            get { return _helper.IsReadOnly; }
        }

        public bool Remove(ComparisonDifference item)
        {
            return _helper.Remove(item);
        }

        #endregion

        #region IEnumerable<ComparisonDifference> Members

        IEnumerator<ComparisonDifference> IEnumerable<ComparisonDifference>.GetEnumerator()
        {
            return _helper.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        public IEnumerator GetEnumerator()
        {
            return _helper.GetEnumerator();
        }

        #endregion

        internal void InternalAdd(ComparisonDifference item)
        {
            _helper.InternalAdd(item);
        }

        private readonly ReadOnlyInteropCollectionHelper<ComparisonDifference> _helper = new ReadOnlyInteropCollectionHelper<ComparisonDifference>();
    }
}
