using System;
using System.Collections;
using System.Collections.Generic;

namespace WinSCP
{
    internal class ReadOnlyInteropCollectionHelper<T> : ICollection<T>
    {
        public void InternalAdd(T item)
        {
            _list.Add(item);
        }

        public T this[int index]
        {
            get
            {
                return _list[index];
            }
            set
            {
                throw CreateReadOnlyException();
            }
        }

        #region ICollection<T> Members

        public void Add(T item)
        {
            throw CreateReadOnlyException();
        }

        public void Clear()
        {
            throw CreateReadOnlyException();
        }

        public bool Contains(T item)
        {
            return _list.Contains(item);
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            _list.CopyTo(array, arrayIndex);
        }

        public int Count
        {
            get { return _list.Count; }
        }

        public bool IsReadOnly
        {
            get { return true; }
        }

        public bool Remove(T item)
        {
            throw CreateReadOnlyException();
        }

        #endregion

        #region IEnumerable<T> Members

        public IEnumerator<T> GetEnumerator()
        {
            return _list.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _list.GetEnumerator();
        }

        #endregion

        private static Exception CreateReadOnlyException()
        {
            return new InvalidOperationException("Collection is read-only.");
        }

        private readonly List<T> _list = new List<T>();
    }
}
