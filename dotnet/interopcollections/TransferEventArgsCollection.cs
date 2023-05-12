using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("0285917B-581A-4F6F-9A9D-1C34ABFB4E38")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))]
    public class TransferEventArgsCollection : ICollection<TransferEventArgs>
    {
        internal TransferEventArgsCollection()
        {
        }

        public TransferEventArgs this[int index]
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

        #region ICollection<TransferEventArgs> Members

        public void Add(TransferEventArgs item)
        {
            _helper.Add(item);
        }

        public void Clear()
        {
            _helper.Clear();
        }

        public bool Contains(TransferEventArgs item)
        {
            return _helper.Contains(item);
        }

        public void CopyTo(TransferEventArgs[] array, int arrayIndex)
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

        public bool Remove(TransferEventArgs item)
        {
            return _helper.Remove(item);
        }

        #endregion

        #region IEnumerable<SessionRemoteException> Members

        public IEnumerator<TransferEventArgs> GetEnumerator()
        {
            return _helper.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _helper.GetEnumerator();
        }

        #endregion

        internal void InternalAdd(TransferEventArgs item)
        {
            _helper.InternalAdd(item);
        }

        private readonly ReadOnlyInteropCollectionHelper<TransferEventArgs> _helper = new ReadOnlyInteropCollectionHelper<TransferEventArgs>();
    }
}
