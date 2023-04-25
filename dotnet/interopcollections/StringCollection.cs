using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("E402CB1F-6219-4C79-9EDF-1914D9589909")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public class StringCollection : ICollection<string>
    {
        internal StringCollection()
        {
        }

        public string this[int index]
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

        #region ICollection<string> Members

        public void Add(string item)
        {
            _helper.Add(item);
        }

        public void Clear()
        {
            _helper.Clear();
        }

        public bool Contains(string item)
        {
            return _helper.Contains(item);
        }

        public void CopyTo(string[] array, int arrayIndex)
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

        public bool Remove(string item)
        {
            return _helper.Remove(item);
        }

        #endregion

        #region IEnumerable<SessionRemoteException> Members

        IEnumerator<string> IEnumerable<string>.GetEnumerator()
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

        internal void InternalAdd(string item)
        {
            _helper.InternalAdd(item);
        }

        internal void InternalRemoveFirst()
        {
            _helper.InternalRemoveFirst();
        }

        private readonly ReadOnlyInteropCollectionHelper<string> _helper = new ReadOnlyInteropCollectionHelper<string>();
    }
}
