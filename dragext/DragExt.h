//---------------------------------------------------------------------------
#ifndef DragExtH
#define DragExtH
//---------------------------------------------------------------------------
#define DRAG_EXT_MAPPING "WinSCPDragExtMapping"
#define DRAG_EXT_MUTEX "WinSCPDragExtMutex"
#define DRAG_EXT_DUMMY_DIR_PREFIX "scp"
#define DRAG_EXT_DUMMY_DIR_PREFIX_LEN 3
//---------------------------------------------------------------------------
DEFINE_GUID(CLSID_ShellExtension, 0xe15e1d68, 0x0d1c, 0x49f7,
  0xbe, 0xb8, 0x81, 0x2b, 0x1e, 0x00, 0xfa, 0x60 );
//---------------------------------------------------------------------------
#pragma pack(push, 4)
struct TDragExtCommStruct
{
  enum TVersion
  {
    Version0 = 0,
    CurrentVersion = Version0,
    MinVersion = Version0,
    MaxVersion = CurrentVersion
  };

  int Version;
  bool Dragging;
  char DropDest[MAX_PATH];
};
#pragma pack(pop)
//---------------------------------------------------------------------------
#endif // DragExtH
