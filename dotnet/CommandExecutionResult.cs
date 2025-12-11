using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("70C312F8-9A09-4D9B-B8EC-FB6ED753892B")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class CommandExecutionResult : OperationResultBase, ICommandExecutionResult
    {
        public string Output { get; internal set; }
        public string ErrorOutput { get; internal set; }
        public int ExitCode { get; internal set; }

        internal CommandExecutionResult()
        {
        }
    }
}
