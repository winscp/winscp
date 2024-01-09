using System;
using System.Collections;
using System.Collections.Generic;

namespace WinSCP
{
    // Originally, the interop classes were each implemented separately. Probably because at the time they were
    // originally implemented, it was not possible to implement them via generic parent class.
    // Now it seems to work (something was improved in in .NET interop meanwhile?)
    //
    // Before reimplementing using this common parent class, the actual collection classes had to have ComDefaultInterface attribute.
    // Now it is not needed for some reason. Possibly the implicit default interface is picked rather randomly.
    // So we still keep the attribute, just in case.
    public class ReadOnlyInteropCollection<T> : ICollection<T>
    {
        internal ReadOnlyInteropCollection()
        {
        }

        public T this[int index]
        {
            get => _list[index];
            set => throw CreateReadOnlyException();
        }

        #region ICollection<T> Members

        public void Add(T item) => throw CreateReadOnlyException();

        public void Clear() => throw CreateReadOnlyException();

        public bool Contains(T item) => _list.Contains(item);

        public void CopyTo(T[] array, int arrayIndex)
        {
            _list.CopyTo(array, arrayIndex);
        }

        public int Count => _list.Count;

        public bool IsReadOnly => true;

        public bool Remove(T item)
        {
            throw CreateReadOnlyException();
        }

        #endregion

        #region IEnumerable<T> Members

        public IEnumerator<T> GetEnumerator() => _list.GetEnumerator();

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator() => _list.GetEnumerator();

        #endregion

        internal void InternalAdd(T item)
        {
            _list.Add(item);
        }

        internal void InternalRemoveFirst()
        {
            _list.RemoveAt(0);
        }

        private static Exception CreateReadOnlyException() => new InvalidOperationException("Collection is read-only.");

        private readonly List<T> _list = new List<T>();
    }
}
