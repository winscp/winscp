using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("1C2C3740-CB42-4B10-B240-2EF64E03DAA3")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class QueryReceivedEventArgs : EventArgs
    {
        public string Message { get; internal set; }

        internal enum Action { None, Abort, Continue }
        internal Action SelectedAction { get; set; }

        internal QueryReceivedEventArgs()
        {
            SelectedAction = Action.None;
        }

        public void Abort()
        {
            SelectedAction = Action.Abort;
        }

        public void Continue()
        {
            SelectedAction = Action.Continue;
        }
    }
}
