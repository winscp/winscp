using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("574FF430-FD40-41F9-9A04-971D3CF844B7")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))]
    public class RemovalEventArgsCollection : ICollection<RemovalEventArgs>
    {
        internal RemovalEventArgsCollection()
        {
        }

        public RemovalEventArgs this[int index]
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

        #region ICollection<RemovalEventArgs> Members

        public void Add(RemovalEventArgs item)
        {
            _helper.Add(item);
        }

        public void Clear()
        {
            _helper.Clear();
        }

        public bool Contains(RemovalEventArgs item)
        {
            return _helper.Contains(item);
        }

        public void CopyTo(RemovalEventArgs[] array, int arrayIndex)
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

        public bool Remove(RemovalEventArgs item)
        {
            return _helper.Remove(item);
        }

        #endregion

        #region IEnumerable<RemovalEventArgs> Members

        public IEnumerator<RemovalEventArgs> GetEnumerator()
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

        internal void InternalAdd(RemovalEventArgs item)
        {
            _helper.InternalAdd(item);
        }

        private readonly ReadOnlyInteropCollectionHelper<RemovalEventArgs> _helper = new ReadOnlyInteropCollectionHelper<RemovalEventArgs>();
    }
}
