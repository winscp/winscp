using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("FBE2FACF-F1D5-493D-9E41-4B9B7243A676")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class RemoteDirectoryInfo : IRemoteDirectoryInfo
    {
        public RemoteFileInfoCollection Files { get; private set; }

        internal RemoteDirectoryInfo()
        {
            Files = new RemoteFileInfoCollection();
        }

        internal void AddFile(RemoteFileInfo file)
        {
            Files.InternalAdd(file);
        }
    }
}
