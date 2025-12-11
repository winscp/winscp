namespace WinSCP
{
    public interface IOperationResultBase
    {
        bool IsSuccess { get; }
        SessionRemoteExceptionCollection Failures { get; }
        
        void Check();
    }
    
    public interface ITransferOperationResult : IOperationResultBase
    {
        TransferEventArgsCollection Transfers { get; }
    }
    
    public interface IRemovalOperationResult : IOperationResultBase
    {
        RemovalEventArgsCollection Removals { get; }
    }
    
    public interface ISynchronizationResult : IOperationResultBase
    {
        TransferEventArgsCollection Downloads { get; }
        TransferEventArgsCollection Uploads { get; }
        RemovalEventArgsCollection Removals { get; }
    }
    
    public interface ICommandExecutionResult
    {
        string Output { get; }
        string ErrorOutput { get; }
        int ExitCode { get; }
        bool IsSuccess { get; }
    }
}
