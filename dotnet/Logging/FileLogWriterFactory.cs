namespace WinSCP.Internal
{
    class FileLogWriterFactory : ILogWriterFactory
    {
        #region Implementation of ILogWriterFactory

        /// <inheritdoc />
        public ILogWriter Create(string fileName)
        {
            return new FileLogWriter(fileName);
        }

        #endregion
    }
}