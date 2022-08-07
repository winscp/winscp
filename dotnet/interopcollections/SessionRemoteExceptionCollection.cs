using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("2309282F-B89B-4F6B-AEB1-D3E1629B7033")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public class SessionRemoteExceptionCollection : ICollection<SessionRemoteException>
    {
        internal SessionRemoteExceptionCollection()
        {
        }

        public SessionRemoteException this[int index]
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

        #region ICollection<SessionRemoteException> Members

        public void Add(SessionRemoteException item)
        {
            _helper.Add(item);
        }

        public void Clear()
        {
            _helper.Clear();
        }

        public bool Contains(SessionRemoteException item)
        {
            return _helper.Contains(item);
        }

        public void CopyTo(SessionRemoteException[] array, int arrayIndex)
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

        public bool Remove(SessionRemoteException item)
        {
            return _helper.Remove(item);
        }

        #endregion

        #region IEnumerable<SessionRemoteException> Members

        IEnumerator<SessionRemoteException> IEnumerable<SessionRemoteException>.GetEnumerator()
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

        internal void InternalAdd(SessionRemoteException item)
        {
            _helper.InternalAdd(item);
        }

        private readonly ReadOnlyInteropCollectionHelper<SessionRemoteException> _helper = new ReadOnlyInteropCollectionHelper<SessionRemoteException>();
    }
}
