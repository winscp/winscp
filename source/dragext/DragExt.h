//---------------------------------------------------------------------------
#ifndef DragExtH
#define DragExtH
//---------------------------------------------------------------------------
#define DRAG_EXT_MAPPING L"WinSCPDragExtMapping"
#define DRAG_EXT_MUTEX L"WinSCPDragExtMutex"
#define DRAG_EXT_RUNNING_MUTEX L"WinSCPDragExtLogMutex"
#define DRAG_EXT_DUMMY_DIR_PREFIX L"scp"
#define DRAG_EXT_DUMMY_DIR_PREFIX_LEN 3
//---------------------------------------------------------------------------
#ifdef _WIN64
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpadded"
#pragma clang diagnostic ignored "-Wmissing-variable-declarations"
#endif
//---------------------------------------------------------------------------
DEFINE_GUID(CLSID_ShellExtension, 0xe15e1d68, 0x0d1c, 0x49f7,
  0xbe, 0xb8, 0x81, 0x2b, 0x1e, 0x00, 0xfa, 0x60 );
//---------------------------------------------------------------------------
#pragma pack(push, 4)
//---------------------------------------------------------------------------
// Note that the change between 0 and 1 was incompatible in both directions.
// So unfortunately version 0 extension (4.x and older) will accept incompatible request
// from version 1 application (5.x and newer).
// Luckily the extension will gracefully fail, when using the Unicode path in
// GetShortPathName(CommStruct->DropDest, ...) and will ignore the request.
struct TDragExtCommStruct
{
  enum TVersion
  {
    Version0 = 0,
    Version1 = 1,
    CurrentVersion = Version1,
    MinVersion = Version1,
    MaxVersion = CurrentVersion
  };

  int Version;
  bool Dragging;
  wchar_t DropDest[MAX_PATH];
};
//---------------------------------------------------------------------------
#pragma pack(pop)
#ifdef _WIN64
#pragma clang diagnostic pop
#endif
//---------------------------------------------------------------------------
#endif // DragExtH
