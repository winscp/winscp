using System;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("42861F26-1ECA-43BA-8A43-ADF3291D8C81")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class OutputDataReceivedEventArgs : EventArgs
    {
        public string Data { get; private set; }
        public bool Error { get; private set; }

        internal OutputDataReceivedEventArgs(string data, bool error)
        {
            Data = data;
            Error = error;
        }

        public override string ToString()
        {
            return Data;
        }
    }
}
