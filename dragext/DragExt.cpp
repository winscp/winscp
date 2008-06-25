//---------------------------------------------------------------------------
#pragma hdrstop
//---------------------------------------------------------------------------
#ifndef STRICT
#define STRICT
#endif
//---------------------------------------------------------------------------
#ifdef _MSC_VER
#include <objbase.h>
#define snprintf _snprintf
#endif
//---------------------------------------------------------------------------
#include <initguid.h>
#include <shlguid.h>
#include <stdio.h>
#include <shlobj.h>
#include <olectl.h>
#include <time.h>
#include "DragExt.h"
//---------------------------------------------------------------------------
#ifdef __BORLANDC__
#undef STDAPI
#define STDAPI EXTERN_C __declspec(dllexport) HRESULT STDAPICALLTYPE
#endif
//---------------------------------------------------------------------------
#define DEBUG(MSG) \
  if (GLogOn) \
  { \
    Debug(MSG); \
  }
//---------------------------------------------------------------------------
#define DRAG_EXT_REG_KEY "Software\\Martin Prikryl\\WinSCP 2\\DragExt"
#define DRAG_EXT_NAME "WinSCP Shell Extension"
#define THREADING_MODEL "Apartment"
#define CLSID_SIZE 39
//---------------------------------------------------------------------------
class CShellExtClassFactory : public IClassFactory
{
public:
  CShellExtClassFactory();
  virtual ~CShellExtClassFactory();

  // IUnknown members
  STDMETHODIMP         QueryInterface(REFIID, LPVOID FAR*);
  STDMETHODIMP_(ULONG) AddRef();
  STDMETHODIMP_(ULONG) Release();

  // IClassFactory members
  STDMETHODIMP CreateInstance(LPUNKNOWN, REFIID, LPVOID FAR*);
  STDMETHODIMP LockServer(BOOL);

protected:
  unsigned long FReferenceCounter;
};
//---------------------------------------------------------------------------
class CShellExt : public IShellExtInit, ICopyHook
{
public:
  CShellExt();
  virtual ~CShellExt();

  // IUnknown members
  STDMETHODIMP         QueryInterface(REFIID, LPVOID FAR*);
  STDMETHODIMP_(ULONG) AddRef();
  STDMETHODIMP_(ULONG) Release();

  // IShellExtInit methods
  STDMETHODIMP Initialize(LPCITEMIDLIST pIDFolder, LPDATAOBJECT pDataObj, HKEY hKeyID);

  // ICopyHook method
  STDMETHODIMP_(UINT) CopyCallback(HWND Hwnd, UINT Func, UINT Flags,
    LPCSTR SrcFile, DWORD SrcAttribs, LPCSTR DestFile, DWORD DestAttribs);

protected:
  unsigned long FReferenceCounter;
  LPDATAOBJECT FDataObj;
  HANDLE FMutex;
  unsigned long FLastTicks;
};
//---------------------------------------------------------------------------
unsigned int GRefThisDll = 0;
bool GEnabled = false;
char GLogFile[MAX_PATH] = "";
bool GLogOn = false;
FILE* GLogHandle = NULL;
HANDLE GLogMutex;
HINSTANCE GInstance;
//---------------------------------------------------------------------------
void Debug(const char* Message)
{
  if (GLogOn)
  {
    unsigned long WaitResult = WaitForSingleObject(GLogMutex, 1000);
    if (WaitResult != WAIT_TIMEOUT)
    {
      try
      {
        if (GLogHandle == NULL)
        {
          if (strlen(GLogFile) == 0)
          {
            GLogOn = false;
          }
          else
          {
            GLogHandle = fopen(GLogFile, "at");
            if (GLogHandle == NULL)
            {
              GLogOn = false;
            }
            else
            {
              setbuf(GLogHandle, NULL);
              fprintf(GLogHandle, "----------------------------\n");
            }
          }
        }

        if (GLogOn)
        {
          SYSTEMTIME Time;
          GetSystemTime(&Time);

          fprintf(GLogHandle, "[%2d/%2d/%4d %2d:%02d:%02d.%03d][%04x] %s\n",
            Time.wDay, Time.wMonth, Time.wYear, Time.wHour, Time.wMinute,
            Time.wSecond, Time.wMilliseconds, GetCurrentThreadId(), Message);
        }
      }
      catch(...)
      {
      }
      ReleaseMutex(GLogMutex);
    }
  }
}
//---------------------------------------------------------------------------
void LogVersion(HINSTANCE HInstance)
{
  if (GLogOn)
  {
    char FileName[MAX_PATH];
    if (GetModuleFileName(HInstance, FileName, sizeof(FileName)) > 0)
    {
      Debug(FileName);

      unsigned long InfoHandle, Size;
      Size = GetFileVersionInfoSize(FileName, &InfoHandle);
      if (Size > 0)
      {
        void* Info;
        Info = new char[Size];
        if (GetFileVersionInfo(FileName, InfoHandle, Size, Info) != 0)
        {
          VS_FIXEDFILEINFO* VersionInfo;
          unsigned int VersionInfoSize;
          if (VerQueryValue(Info, "\\", reinterpret_cast<void**>(&VersionInfo),
                &VersionInfoSize) != 0)
          {
            char VersionStr[100];
            snprintf(VersionStr, sizeof(VersionStr), "LogVersion %d.%d.%d.%d",
              HIWORD(VersionInfo->dwFileVersionMS),
              LOWORD(VersionInfo->dwFileVersionMS),
              HIWORD(VersionInfo->dwFileVersionLS),
              LOWORD(VersionInfo->dwFileVersionLS));
            Debug(VersionStr);
          }
          else
          {
            Debug("LogVersion no fixed version info");
          }
        }
        else
        {
          Debug("LogVersion cannot read version info");
        }
      }
      else
      {
        Debug("LogVersion no version info");
      }
    }
  }
}
//---------------------------------------------------------------------------
extern "C" int APIENTRY
DllMain(HINSTANCE HInstance, DWORD Reason, LPVOID Reserved)
{
  if (Reason == DLL_PROCESS_ATTACH)
  {
    GInstance = HInstance;
  }

  if (GRefThisDll == 0)
  {
    GLogMutex = CreateMutex(NULL, false, "WinSCPDragExtLogMutex");

    for (int Root = 0; Root <= 1; Root++)
    {
      HKEY Key;
      if (RegOpenKeyEx(Root == 0 ? HKEY_LOCAL_MACHINE : HKEY_CURRENT_USER,
            DRAG_EXT_REG_KEY, 0,
            STANDARD_RIGHTS_READ | KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS,
            &Key) == ERROR_SUCCESS)
      {
        unsigned long Type;
        unsigned long Value;
        unsigned long Size;
        char Buf[MAX_PATH];

        Size = sizeof(Value);
        if ((RegQueryValueEx(Key, "Enable", NULL, &Type,
               reinterpret_cast<unsigned char*>(&Value), &Size) == ERROR_SUCCESS) &&
            (Type == REG_DWORD))
        {
          GEnabled = (Value != 0);
        }

        Size = sizeof(Buf);
        if ((RegQueryValueEx(Key, "LogFile", NULL, &Type,
               reinterpret_cast<unsigned char*>(&Buf), &Size) == ERROR_SUCCESS) &&
            (Type == REG_SZ))
        {
          strncpy(GLogFile, Buf, sizeof(GLogFile));
          GLogFile[sizeof(GLogFile) - 1] = '\0';
          GLogOn = true;
        }

        RegCloseKey(Key);
      }
    }
    DEBUG("DllMain loaded settings");
    DEBUG(GEnabled ? "DllMain enabled" : "DllMain disabled");
    LogVersion(HInstance);
  }
  else
  {
    DEBUG("DllMain settings already loaded");
  }

  DEBUG("DllMain leave");

  return 1;   // ok
}
//---------------------------------------------------------------------------
STDAPI DllCanUnloadNow(void)
{
  bool CanUnload = (GRefThisDll == 0);
  DEBUG(CanUnload ? "DllCanUnloadNow can" : "DllCanUnloadNow cannot");
  return (CanUnload ? S_OK : S_FALSE);
}
//---------------------------------------------------------------------------
STDAPI DllGetClassObject(REFCLSID Rclsid, REFIID Riid, LPVOID* PpvOut)
{
  DEBUG("DllGetClassObject");

  *PpvOut = NULL;

  if (IsEqualIID(Rclsid, CLSID_ShellExtension))
  {
    DEBUG("DllGetClassObject is ShellExtension");

    CShellExtClassFactory* Pcf = new CShellExtClassFactory;

    return Pcf->QueryInterface(Riid, PpvOut);
  }

  return CLASS_E_CLASSNOTAVAILABLE;
}
//---------------------------------------------------------------------------
bool RegisterServer(bool AllUsers)
{
  DEBUG("RegisterServer enter");

  DEBUG(AllUsers ? "RegisterServer all users" : "RegisterServer current users");

  bool Result = false;
  HKEY RootKey = AllUsers ? HKEY_LOCAL_MACHINE : HKEY_CURRENT_USER;
  HKEY HKey;
  DWORD Unused;
  wchar_t wClassID[CLSID_SIZE];
  // double size will guarante char to fit space
  char ClassID[CLSID_SIZE * 2];

  StringFromGUID2(CLSID_ShellExtension, wClassID, CLSID_SIZE);
  ZeroMemory(ClassID, CLSID_SIZE * 2);
  wcstombs(ClassID, wClassID, CLSID_SIZE * 2);

  if ((RegOpenKeyEx(RootKey, "Software\\Classes", 0, KEY_WRITE, &HKey) ==
         ERROR_SUCCESS) &&
      (RegCreateKeyEx(HKey, "CLSID", 0, NULL,
         REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &HKey, &Unused) ==
           ERROR_SUCCESS))
  {
    if (RegCreateKey(HKey, ClassID, &HKey) == ERROR_SUCCESS)
    {
      RegSetValueEx(HKey, NULL, 0, REG_SZ,
        reinterpret_cast<const unsigned char*>(DRAG_EXT_NAME), sizeof(DRAG_EXT_NAME));

      if (RegCreateKey(HKey, "InProcServer32", &HKey) == ERROR_SUCCESS)
      {
        char Filename[MAX_PATH];
        GetModuleFileName(GInstance, Filename, sizeof(Filename));
        RegSetValueEx(HKey, NULL, 0, REG_SZ,
          reinterpret_cast<unsigned char*>(Filename), strlen(Filename) + 1);

        RegSetValueEx(HKey, "ThreadingModel", 0, REG_SZ,
          reinterpret_cast<unsigned char*>(THREADING_MODEL),
          sizeof(THREADING_MODEL));
      }
    }
    RegCloseKey(HKey);

    if ((RegOpenKeyEx(RootKey, "Software\\Classes",
           0, KEY_WRITE, &HKey) == ERROR_SUCCESS) &&
        (RegCreateKeyEx(HKey,
           "directory\\shellex\\CopyHookHandlers\\WinSCPCopyHook",
           0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &HKey,
           &Unused) == ERROR_SUCCESS))
    {
      RegSetValueEx(HKey, NULL, 0, REG_SZ,
        reinterpret_cast<unsigned char*>(ClassID), strlen(ClassID) + 1);
      RegCloseKey(HKey);

      if ((RegCreateKeyEx(RootKey, DRAG_EXT_REG_KEY,
             0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &HKey,
             &Unused) == ERROR_SUCCESS))
      {
        unsigned long Value = 1;
        RegSetValueEx(HKey, "Enable", 0, REG_DWORD,
          reinterpret_cast<unsigned char*>(&Value), sizeof(Value));

        RegCloseKey(HKey);

        Result = true;
      }

      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);
    }
  }

  DEBUG("RegisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
STDAPI DllRegisterServer()
{
  DEBUG("DllRegisterServer enter");

  HRESULT Result;
  if (RegisterServer(true) || RegisterServer(false))
  {
    Result = S_OK;
  }
  else
  {
    Result = SELFREG_E_CLASS;
  }

  DEBUG("DllRegisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
bool UnregisterServer(bool AllUsers)
{
  DEBUG("UnregisterServer enter");

  DEBUG(AllUsers ? "UnregisterServer all users" : "UnregisterServer current users");

  bool Result = false;
  wchar_t wClassID[CLSID_SIZE];
  char ClassID[CLSID_SIZE * 2];

  StringFromGUID2(CLSID_ShellExtension, wClassID, CLSID_SIZE);
  ZeroMemory(ClassID, CLSID_SIZE * 2);
  wcstombs(ClassID, wClassID, CLSID_SIZE * 2);

  HKEY RootKey = AllUsers ? HKEY_LOCAL_MACHINE : HKEY_CURRENT_USER;
  HKEY HKey;

  if ((RegOpenKeyEx(RootKey, "Software\\Classes", 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS) &&
      (RegOpenKeyEx(HKey, "directory\\shellex\\CopyHookHandlers",
        0, KEY_WRITE, &HKey) == ERROR_SUCCESS))
  {
    RegDeleteKey(HKey, "WinSCPCopyHook");

    RegCloseKey(HKey);
  }

  if ((RegOpenKeyEx(RootKey, "Software\\Classes", 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS) &&
      (RegOpenKeyEx(HKey, "CLSID", 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS))
  {
    if (RegOpenKeyEx(HKey, ClassID, 0, KEY_WRITE, &HKey) == ERROR_SUCCESS)
    {
      RegDeleteKey(HKey, "InProcServer32");

      RegCloseKey(HKey);

      if ((RegOpenKeyEx(RootKey, "Software\\Classes", 0, KEY_WRITE, &HKey) ==
             ERROR_SUCCESS) &&
          (RegOpenKeyEx(HKey, "CLSID", 0, KEY_WRITE, &HKey) ==
             ERROR_SUCCESS))
      {
        RegDeleteKey(HKey, ClassID);

        RegCloseKey(HKey);

        Result = true;
      }
    }
  }

  if ((RegOpenKeyEx(RootKey, DRAG_EXT_REG_KEY, 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS))
  {
    unsigned long Value = 0;
    RegSetValueEx(HKey, "Enable", 0, REG_DWORD,
      reinterpret_cast<unsigned char*>(&Value), sizeof(Value));

    RegCloseKey(HKey);

    Result = true;
  }

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);

  DEBUG("UnregisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
STDAPI DllUnregisterServer()
{
  DEBUG("DllUnregisterServer enter");

  HRESULT Result = SELFREG_E_CLASS;
  if (UnregisterServer(true))
  {
    Result = S_OK;
  }

  if (UnregisterServer(false))
  {
    Result = S_OK;
  }

  DEBUG("DllUnregisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
CShellExtClassFactory::CShellExtClassFactory()
{
  DEBUG("CShellExtClassFactory");

  FReferenceCounter = 0;

  GRefThisDll++;
}
//---------------------------------------------------------------------------
CShellExtClassFactory::~CShellExtClassFactory()
{
  DEBUG("~CShellExtClassFactory");

  GRefThisDll--;
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExtClassFactory::QueryInterface(REFIID Riid, LPVOID FAR* Ppv)
{
  DEBUG("QueryInterface");

  *Ppv = NULL;

  // Any interface on this object is the object pointer

  if (IsEqualIID(Riid, IID_IUnknown) || IsEqualIID(Riid, IID_IClassFactory))
  {
    DEBUG("QueryInterface is IUnknown or IClassFactory");

    *Ppv = (LPCLASSFACTORY)this;

    AddRef();

    return NOERROR;
  }

  return E_NOINTERFACE;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExtClassFactory::AddRef()
{
  DEBUG("AddRef");
  return ++FReferenceCounter;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExtClassFactory::Release()
{
  DEBUG("Release");

  if (--FReferenceCounter)
  {
    return FReferenceCounter;
  }

  delete this;

  return 0;
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExtClassFactory::CreateInstance(LPUNKNOWN UnkOuter,
  REFIID Riid, LPVOID* PpvObj)
{
  DEBUG("CreateInstance");

  *PpvObj = NULL;

  // Shell extensions typically don't support aggregation (inheritance)

  if (UnkOuter)
  {
    return CLASS_E_NOAGGREGATION;
  }

  // Create the main shell extension object.  The shell will then call
  // QueryInterface with IID_IShellExtInit--this is how shell extensions are
  // initialized.

  CShellExt* ShellExt = new CShellExt();  //Create the CShellExt object

  if (NULL == ShellExt)
  {
    return E_OUTOFMEMORY;
  }

  return ShellExt->QueryInterface(Riid, PpvObj);
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExtClassFactory::LockServer(BOOL Lock)
{
  DEBUG("LockServer");

  return NOERROR;
}
//---------------------------------------------------------------------------
// CShellExt
CShellExt::CShellExt()
{
  DEBUG("CShellExt enter");

  FReferenceCounter = 0L;
  FDataObj = NULL;

  FMutex = CreateMutex(NULL, false, DRAG_EXT_MUTEX);
  FLastTicks = 0;

  GRefThisDll++;

  DEBUG("CShellExt leave");
}
//---------------------------------------------------------------------------
CShellExt::~CShellExt()
{
  DEBUG("~CShellExt enter");

  if (FDataObj)
  {
    FDataObj->Release();
  }

  CloseHandle(FMutex);

  GRefThisDll--;

  DEBUG("~CShellExt leave");
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExt::QueryInterface(REFIID Riid, LPVOID FAR* Ppv)
{
  DEBUG("CShellExt::QueryInterface enter");

  HRESULT Result = E_NOINTERFACE;
  *Ppv = NULL;

  if (!GEnabled)
  {
    DEBUG("CShellExt::QueryInterface shelext disabled");
  }
  else
  {
    if (IsEqualIID(Riid, IID_IShellExtInit) || IsEqualIID(Riid, IID_IUnknown))
    {
      DEBUG("CShellExt::QueryInterface is IShellExtInit or IUnknown");
      *Ppv = (LPSHELLEXTINIT)this;
    }
    else if (IsEqualIID(Riid, IID_IShellCopyHook))
    {
      DEBUG("CShellExt::QueryInterface is IShellCopyHook");
      *Ppv = (LPCOPYHOOK)this;
    }

    if (*Ppv)
    {
      AddRef();

      Result = NOERROR;
    }
  }

  DEBUG("CShellExt::QueryInterface leave");

  return Result;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExt::AddRef()
{
  DEBUG("CShellExt::AddRef");

  return ++FReferenceCounter;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExt::Release()
{
  DEBUG("CShellExt::Release");
  if (--FReferenceCounter)
  {
    return FReferenceCounter;
  }

  delete this;

  return 0;
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExt::Initialize(LPCITEMIDLIST IDFolder,
  LPDATAOBJECT DataObj, HKEY RegKey)
{
  DEBUG("CShellExt::Initialize enter");

  if (FDataObj != NULL)
  {
    FDataObj->Release();
    FDataObj = NULL;
  }

  // duplicate the object pointer and registry handle

  if (DataObj != NULL)
  {
    FDataObj = DataObj;
    DataObj->AddRef();
  }

  DEBUG("CShellExt::Initialize leave");

  return NOERROR;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(UINT) CShellExt::CopyCallback(HWND Hwnd, UINT Func, UINT Flags,
  LPCSTR SrcFile, DWORD SrcAttribs, LPCSTR DestFile, DWORD DestAttribs)
{
  DEBUG("CShellExt::CopyCallback enter");

  UINT Result = IDYES;

  if (GEnabled && ((Func == FO_COPY) || (Func == FO_MOVE)))
  {
    DEBUG("CShellExt::CopyCallback copy or move");

    unsigned long Ticks = GetTickCount();
    if (((Ticks - FLastTicks) >= 100) ||
        (FLastTicks > Ticks))
    {
      DEBUG("CShellExt::CopyCallback interval elapsed");

      DEBUG("CShellExt::CopyCallback source / dest:");
      DEBUG(SrcFile);
      DEBUG(DestFile);

      FLastTicks = Ticks;
      const char* BackPtr = strrchr(SrcFile, '\\');

      if ((BackPtr != NULL) &&
          (strncmp(BackPtr + 1, DRAG_EXT_DUMMY_DIR_PREFIX,
            DRAG_EXT_DUMMY_DIR_PREFIX_LEN) == 0))
      {
        DEBUG("CShellExt::CopyCallback filename has prefix");

        HANDLE MapFile = OpenFileMapping(FILE_MAP_ALL_ACCESS,
          false, DRAG_EXT_MAPPING);

        if (MapFile != NULL)
        {
          DEBUG("CShellExt::CopyCallback mapfile found");

          TDragExtCommStruct* CommStruct;
          CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(MapFile,
            FILE_MAP_ALL_ACCESS, 0, 0, 0));

          if (CommStruct != NULL)
          {
            DEBUG("CShellExt::CopyCallback mapview created");
            unsigned long WaitResult = WaitForSingleObject(FMutex, 1000);
            if (WaitResult != WAIT_TIMEOUT)
            {
              DEBUG("CShellExt::CopyCallback mutex got");
              if (CommStruct->Version >= TDragExtCommStruct::MinVersion)
              {
                DEBUG("CShellExt::CopyCallback supported structure version");
                if (CommStruct->Dragging)
                {
                  DEBUG("CShellExt::CopyCallback dragging");
                  DEBUG(CommStruct->DropDest);
                  bool IsDropDest;
                  if (stricmp(CommStruct->DropDest, SrcFile) == 0)
                  {
                    IsDropDest = true;
                    DEBUG("CShellExt::CopyCallback dragged file match as is");
                  }
                  else
                  {
                    char DropDestShort[MAX_PATH];
                    char SrcFileShort[MAX_PATH];
                    size_t DropDestSize = GetShortPathName(CommStruct->DropDest,
                      DropDestShort, sizeof(DropDestShort));
                    size_t SrcFileSize = GetShortPathName(SrcFile,
                      SrcFileShort, sizeof(SrcFileShort));
                    if ((DropDestSize == 0) || (SrcFileSize == 0))
                    {
                      DEBUG("CShellExt::CopyCallback cannot convert paths to short form");
                      IsDropDest = false;
                    }
                    else if ((DropDestSize >= sizeof(DropDestShort)) ||
                        (SrcFileSize >= sizeof(SrcFileShort)))
                    {
                      DEBUG("CShellExt::CopyCallback short paths too long");
                      IsDropDest = false;
                    }
                    else
                    {
                      DEBUG(DropDestShort);
                      DEBUG(SrcFileShort);

                      if (stricmp(DropDestShort, SrcFileShort) == 0)
                      {
                        DEBUG("CShellExt::CopyCallback dragged file match after converted to short form");
                        IsDropDest = true;
                      }
                      else
                      {
                        DEBUG("CShellExt::CopyCallback dragged file does NOT match");
                        IsDropDest = false;
                      }
                    }
                  }

                  if (IsDropDest)
                  {
                    CommStruct->Dragging = false;
                    strncpy(CommStruct->DropDest, DestFile, sizeof(CommStruct->DropDest));
                    CommStruct->DropDest[sizeof(CommStruct->DropDest)-1] = '\0';
                    Result = IDNO;
                    DEBUG("CShellExt::CopyCallback dragging refused");
                  }
                }
                else
                {
                  DEBUG("CShellExt::CopyCallback NOT dragging");
                }
              }
              else
              {
                DEBUG("CShellExt::CopyCallback unsupported structure version");
              }
              ReleaseMutex(FMutex);
              DEBUG("CShellExt::CopyCallback mutex released");
            }
            else
            {
              DEBUG("CShellExt::CopyCallback mutex timeout");
            }
            UnmapViewOfFile(CommStruct);
          }
          else
          {
            DEBUG("CShellExt::CopyCallback mapview NOT created");
          }

          CloseHandle(MapFile);
        }
        else
        {
          DEBUG("CShellExt::CopyCallback mapfile NOT found");
        }
      }
      else
      {
        DEBUG("CShellExt::CopyCallback filename has NOT prefix");
      }
    }
    else
    {
      DEBUG("CShellExt::CopyCallback interval NOT elapsed");
    }
  }
  else
  {
    DEBUG("CShellExt::CopyCallback NOT copy nor move");
  }

  DEBUG("CShellExt::CopyCallback leave");

  return Result;
}
