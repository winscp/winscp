using System;

namespace WinSCP
{
    internal class OperationResultGuard : IDisposable
    {
        public OperationResultGuard(Session session, OperationResultBase operationResult)
        {
            _session = session;
            _operationResult = operationResult;
        }

        public void Dispose()
        {
            _session.UnregisterOperationResult(_operationResult);
        }

        private readonly Session _session;
        private readonly OperationResultBase _operationResult;
    }
}
