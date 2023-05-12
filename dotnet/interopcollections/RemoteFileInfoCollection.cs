using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("39AA3D00-578C-49AF-B3E4-16CE26C710C6")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    [ComDefaultInterface(typeof(IEnumerable))]
    public class RemoteFileInfoCollection : ICollection<RemoteFileInfo>
    {
        internal RemoteFileInfoCollection()
        {
        }

        public RemoteFileInfo this[int index]
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

        #region ICollection<RemoteFileInfo> Members

        public void Add(RemoteFileInfo item)
        {
            _helper.Add(item);
        }

        public void Clear()
        {
            _helper.Clear();
        }

        public bool Contains(RemoteFileInfo item)
        {
            return _helper.Contains(item);
        }

        public void CopyTo(RemoteFileInfo[] array, int arrayIndex)
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

        public bool Remove(RemoteFileInfo item)
        {
            return _helper.Remove(item);
        }

        #endregion

        #region IEnumerable<RemoteFileInfo> Members

        public IEnumerator<RemoteFileInfo> GetEnumerator()
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

        internal void InternalAdd(RemoteFileInfo item)
        {
            _helper.InternalAdd(item);
        }

        private readonly ReadOnlyInteropCollectionHelper<RemoteFileInfo> _helper = new ReadOnlyInteropCollectionHelper<RemoteFileInfo>();
    }
}
