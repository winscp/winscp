namespace WinSCP.Internal
{
    public interface ILogWriterFactory
    {
        ILogWriter Create(string fileName);
    }
}