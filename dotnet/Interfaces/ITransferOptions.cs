namespace WinSCP
{
    public interface ITransferOptions
    {
        bool PreserveTimestamp { get; set; }
        FilePermissions FilePermissions { get; set; }
        TransferMode TransferMode { get; set; }
        string FileMask { get; set; }
        TransferResumeSupport ResumeSupport { get; set; }
        int SpeedLimit { get; set; }
        OverwriteMode OverwriteMode { get; set; }
        
        void AddRawSettings(string setting, string value);
    }
}
