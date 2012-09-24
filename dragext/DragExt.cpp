//---------------------------------------------------------------------------
#pragma hdrstop
//---------------------------------------------------------------------------
#ifndef STRICT
#define STRICT
#endif
//---------------------------------------------------------------------------
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
//---------------------------------------------------------------------------
#ifdef _MSC_VER
#include <objbase.h>
#define snwprintf _snwprintf
#endif
//---------------------------------------------------------------------------
#include <initguid.h>
#include <shlguid.h>
#include <stdio.h>
#pragma warn -inl
#include <shlobj.h>
#pragma warn .inl
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
#define DRAG_EXT_REG_KEY L"Software\\Martin Prikryl\\WinSCP 2\\DragExt"
#define DRAG_EXT_NAME L"WinSCP Shell Extension"
#define THREADING_MODEL L"Apartment"
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
    LPCWSTR SrcFile, DWORD SrcAttribs, LPCWSTR DestFile, DWORD DestAttribs);

protected:
  unsigned long FReferenceCounter;
  LPDATAOBJECT FDataObj;
  HANDLE FMutex;
  unsigned long FLastTicks;
};
//---------------------------------------------------------------------------
unsigned int GRefThisDll = 0;
bool GEnabled = false;
wchar_t GLogFile[MAX_PATH] = L"";
bool GLogOn = false;
FILE* GLogHandle = NULL;
HANDLE GLogMutex;
HINSTANCE GInstance;
//---------------------------------------------------------------------------
void Debug(const wchar_t* Message)
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
          if (wcslen(GLogFile) == 0)
          {
            GLogOn = false;
          }
          else
          {
            GLogHandle = _wfopen(GLogFile, L"at");
            if (GLogHandle == NULL)
            {
              GLogOn = false;
            }
            else
            {
              setbuf(GLogHandle, NULL);
              fwprintf(GLogHandle, L"----------------------------\n");
            }
          }
        }

        if (GLogOn)
        {
          SYSTEMTIME Time;
          GetSystemTime(&Time);

          fwprintf(GLogHandle, L"[%2d/%2d/%4d %2d:%02d:%02d.%03d][%04x] %s\n",
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
    wchar_t FileName[MAX_PATH];
    if (GetModuleFileName(HInstance, FileName, LENOF(FileName)) > 0)
    {
      Debug(FileName);

      unsigned long InfoHandle, Size;
      Size = GetFileVersionInfoSize(FileName, &InfoHandle);
      if (Size > 0)
      {
        void* Info;
        Info = new wchar_t[Size];
        if (GetFileVersionInfo(FileName, InfoHandle, Size, Info) != 0)
        {
          VS_FIXEDFILEINFO* VersionInfo;
          unsigned int VersionInfoSize;
          if (VerQueryValue(Info, L"\\", reinterpret_cast<void**>(&VersionInfo),
                &VersionInfoSize) != 0)
          {
            wchar_t VersionStr[100];
            snwprintf(VersionStr, LENOF(VersionStr), L"LogVersion %d.%d.%d.%d",
              HIWORD(VersionInfo->dwFileVersionMS),
              LOWORD(VersionInfo->dwFileVersionMS),
              HIWORD(VersionInfo->dwFileVersionLS),
              LOWORD(VersionInfo->dwFileVersionLS));
            Debug(VersionStr);
          }
          else
          {
            Debug(L"LogVersion no fixed version info");
          }
        }
        else
        {
          Debug(L"LogVersion cannot read version info");
        }
      }
      else
      {
        Debug(L"LogVersion no version info");
      }
    }
  }
}
//---------------------------------------------------------------------------
extern "C" int APIENTRY
DllMain(HINSTANCE HInstance, DWORD Reason, LPVOID /*Reserved*/)
{
  if (Reason == DLL_PROCESS_ATTACH)
  {
    GInstance = HInstance;

    if (GRefThisDll == 0)
    {
      GLogMutex = CreateMutex(NULL, false, L"WinSCPDragExtLogMutex");

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
          wchar_t Buf[MAX_PATH];

          Size = sizeof(Value);
          if ((RegQueryValueEx(Key, L"Enable", NULL, &Type,
                 reinterpret_cast<unsigned char*>(&Value), &Size) == ERROR_SUCCESS) &&
              (Type == REG_DWORD))
          {
            GEnabled = (Value != 0);
          }

          Size = sizeof(Buf);
          if ((RegQueryValueEx(Key, L"LogFile", NULL, &Type,
                 reinterpret_cast<unsigned char*>(&Buf), &Size) == ERROR_SUCCESS) &&
              (Type == REG_SZ))
          {
            wcsncpy(GLogFile, Buf, LENOF(GLogFile));
            GLogFile[LENOF(GLogFile) - 1] = L'\0';
            GLogOn = true;
          }

          RegCloseKey(Key);
        }
      }
      DEBUG(L"DllMain loaded settings");
      DEBUG(GEnabled ? L"DllMain enabled" : L"DllMain disabled");
      #ifdef UNICODE
      DEBUG(L"Unicode");
      #else
      DEBUG(L"Ansi");
      #endif
      LogVersion(HInstance);
    }
    else
    {
      DEBUG(L"DllMain settings already loaded");
    }

    DEBUG(L"DllMain attach leave");
  }
  else if (Reason == DLL_PROCESS_DETACH)
  {
    DEBUG(L"DllMain detach enter");
    CloseHandle(GLogMutex);
  }

  return 1;   // ok
}
//---------------------------------------------------------------------------
STDAPI DllCanUnloadNow(void)
{
  bool CanUnload = (GRefThisDll == 0);
  DEBUG(CanUnload ? L"DllCanUnloadNow can" : L"DllCanUnloadNow cannot");
  return (CanUnload ? S_OK : S_FALSE);
}
//---------------------------------------------------------------------------
STDAPI DllGetClassObject(REFCLSID Rclsid, REFIID Riid, LPVOID* PpvOut)
{
  DEBUG(L"DllGetClassObject");

  *PpvOut = NULL;

  if (IsEqualIID(Rclsid, CLSID_ShellExtension))
  {
    DEBUG(L"DllGetClassObject is ShellExtension");

    CShellExtClassFactory* Pcf = new CShellExtClassFactory;

    return Pcf->QueryInterface(Riid, PpvOut);
  }

  return CLASS_E_CLASSNOTAVAILABLE;
}
//---------------------------------------------------------------------------
bool RegisterServer(bool AllUsers)
{
  DEBUG(L"RegisterServer enter");

  DEBUG(AllUsers ? L"RegisterServer all users" : L"RegisterServer current users");

  bool Result = false;
  HKEY RootKey = AllUsers ? HKEY_LOCAL_MACHINE : HKEY_CURRENT_USER;
  HKEY HKey;
  DWORD Unused;
  wchar_t ClassID[CLSID_SIZE];

  StringFromGUID2(CLSID_ShellExtension, ClassID, CLSID_SIZE);

  if ((RegOpenKeyEx(RootKey, L"Software\\Classes", 0, KEY_WRITE, &HKey) ==
         ERROR_SUCCESS) &&
      (RegCreateKeyEx(HKey, L"CLSID", 0, NULL,
         REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &HKey, &Unused) ==
           ERROR_SUCCESS))
  {
    DEBUG(L"RegisterServer CLSID created");

    if (RegCreateKey(HKey, ClassID, &HKey) == ERROR_SUCCESS)
    {
      DEBUG(L"RegisterServer class ID created");

      RegSetValueEx(HKey, NULL, 0, REG_SZ,
        reinterpret_cast<const unsigned char*>(DRAG_EXT_NAME), sizeof(DRAG_EXT_NAME));

      if (RegCreateKey(HKey, L"InProcServer32", &HKey) == ERROR_SUCCESS)
      {
        DEBUG(L"RegisterServer InProcServer32 created");

        wchar_t Filename[MAX_PATH];
        GetModuleFileName(GInstance, Filename, LENOF(Filename));
        RegSetValueEx(HKey, NULL, 0, REG_SZ,
          reinterpret_cast<unsigned char*>(Filename), (wcslen(Filename) + 1) * sizeof(wchar_t));

        RegSetValueEx(HKey, L"ThreadingModel", 0, REG_SZ,
          reinterpret_cast<const unsigned char*>(THREADING_MODEL),
          sizeof(THREADING_MODEL));
      }
    }
    RegCloseKey(HKey);

    if ((RegOpenKeyEx(RootKey, L"Software\\Classes",
           0, KEY_WRITE, &HKey) == ERROR_SUCCESS) &&
        (RegCreateKeyEx(HKey,
           L"directory\\shellex\\CopyHookHandlers\\WinSCPCopyHook",
           0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &HKey,
           &Unused) == ERROR_SUCCESS))
    {
      DEBUG(L"RegisterServer WinSCPCopyHook created");

      RegSetValueEx(HKey, NULL, 0, REG_SZ,
        reinterpret_cast<unsigned char*>(ClassID), (wcslen(ClassID) + 1) * sizeof(wchar_t));
      RegCloseKey(HKey);

      if ((RegCreateKeyEx(RootKey, DRAG_EXT_REG_KEY,
             0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &HKey,
             &Unused) == ERROR_SUCCESS))
      {
        DEBUG(L"RegisterServer drag ext key created");

        unsigned long Value = 1;
        RegSetValueEx(HKey, L"Enable", 0, REG_DWORD,
          reinterpret_cast<unsigned char*>(&Value), sizeof(Value));

        RegCloseKey(HKey);

        Result = true;
      }

      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);
    }
  }

  DEBUG(L"RegisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
STDAPI DllRegisterServer()
{
  DEBUG(L"DllRegisterServer enter");

  HRESULT Result;
  if (RegisterServer(true) || RegisterServer(false))
  {
    Result = S_OK;
  }
  else
  {
    Result = SELFREG_E_CLASS;
  }

  DEBUG(L"DllRegisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
bool UnregisterServer(bool AllUsers)
{
  DEBUG(L"UnregisterServer enter");

  DEBUG(AllUsers ? L"UnregisterServer all users" : L"UnregisterServer current users");

  bool Result = false;
  wchar_t ClassID[CLSID_SIZE];

  StringFromGUID2(CLSID_ShellExtension, ClassID, CLSID_SIZE);

  HKEY RootKey = AllUsers ? HKEY_LOCAL_MACHINE : HKEY_CURRENT_USER;
  HKEY HKey;

  if ((RegOpenKeyEx(RootKey, L"Software\\Classes", 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS) &&
      (RegOpenKeyEx(HKey, L"directory\\shellex\\CopyHookHandlers",
        0, KEY_WRITE, &HKey) == ERROR_SUCCESS))
  {
    RegDeleteKey(HKey, L"WinSCPCopyHook");

    RegCloseKey(HKey);
  }

  if ((RegOpenKeyEx(RootKey, L"Software\\Classes", 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS) &&
      (RegOpenKeyEx(HKey, L"CLSID", 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS))
  {
    if (RegOpenKeyEx(HKey, ClassID, 0, KEY_WRITE, &HKey) == ERROR_SUCCESS)
    {
      RegDeleteKey(HKey, L"InProcServer32");

      RegCloseKey(HKey);

      if ((RegOpenKeyEx(RootKey, L"Software\\Classes", 0, KEY_WRITE, &HKey) ==
             ERROR_SUCCESS) &&
          (RegOpenKeyEx(HKey, L"CLSID", 0, KEY_WRITE, &HKey) ==
             ERROR_SUCCESS))
      {
        RegDeleteKey(HKey, ClassID);

        RegCloseKey(HKey);

        Result = true;
      }
    }
  }

  if (RegOpenKeyEx(RootKey, DRAG_EXT_REG_KEY, 0, KEY_WRITE, &HKey) ==
        ERROR_SUCCESS)
  {
    unsigned long Value = 0;
    RegSetValueEx(HKey, L"Enable", 0, REG_DWORD,
      reinterpret_cast<unsigned char*>(&Value), sizeof(Value));

    RegCloseKey(HKey);

    Result = true;
  }

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);

  DEBUG(L"UnregisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
STDAPI DllUnregisterServer()
{
  DEBUG(L"DllUnregisterServer enter");

  HRESULT Result = SELFREG_E_CLASS;
  if (UnregisterServer(true))
  {
    Result = S_OK;
  }

  if (UnregisterServer(false))
  {
    Result = S_OK;
  }

  DEBUG(L"DllUnregisterServer leave");

  return Result;
}
//---------------------------------------------------------------------------
CShellExtClassFactory::CShellExtClassFactory()
{
  DEBUG(L"CShellExtClassFactory");

  FReferenceCounter = 0;

  GRefThisDll++;
}
//---------------------------------------------------------------------------
CShellExtClassFactory::~CShellExtClassFactory()
{
  DEBUG(L"~CShellExtClassFactory");

  GRefThisDll--;
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExtClassFactory::QueryInterface(REFIID Riid, LPVOID FAR* Ppv)
{
  DEBUG(L"QueryInterface");

  *Ppv = NULL;

  // Any interface on this object is the object pointer

  if (IsEqualIID(Riid, IID_IUnknown) || IsEqualIID(Riid, IID_IClassFactory))
  {
    DEBUG(L"QueryInterface is IUnknown or IClassFactory");

    *Ppv = (LPCLASSFACTORY)this;

    AddRef();

    return NOERROR;
  }

  return E_NOINTERFACE;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExtClassFactory::AddRef()
{
  DEBUG(L"AddRef");
  return ++FReferenceCounter;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExtClassFactory::Release()
{
  DEBUG(L"Release");

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
  DEBUG(L"CreateInstance");

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
STDMETHODIMP CShellExtClassFactory::LockServer(BOOL /*Lock*/)
{
  DEBUG(L"LockServer");

  return NOERROR;
}
//---------------------------------------------------------------------------
// CShellExt
CShellExt::CShellExt()
{
  DEBUG(L"CShellExt enter");

  FReferenceCounter = 0L;
  FDataObj = NULL;

  FMutex = CreateMutex(NULL, false, DRAG_EXT_MUTEX);
  FLastTicks = 0;

  GRefThisDll++;

  DEBUG(L"CShellExt leave");
}
//---------------------------------------------------------------------------
CShellExt::~CShellExt()
{
  DEBUG(L"~CShellExt enter");

  if (FDataObj)
  {
    FDataObj->Release();
  }

  CloseHandle(FMutex);

  GRefThisDll--;

  DEBUG(L"~CShellExt leave");
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExt::QueryInterface(REFIID Riid, LPVOID FAR* Ppv)
{
  DEBUG(L"CShellExt::QueryInterface enter");

  HRESULT Result = E_NOINTERFACE;
  *Ppv = NULL;

  if (!GEnabled)
  {
    DEBUG(L"CShellExt::QueryInterface shellext disabled");
  }
  else
  {
    if (IsEqualIID(Riid, IID_IShellExtInit) || IsEqualIID(Riid, IID_IUnknown))
    {
      DEBUG(L"CShellExt::QueryInterface is IShellExtInit or IUnknown");
      *Ppv = (LPSHELLEXTINIT)this;
    }
    else if (IsEqualIID(Riid, IID_IShellCopyHook))
    {
      DEBUG(L"CShellExt::QueryInterface is IShellCopyHook");
      *Ppv = (LPCOPYHOOK)this;
    }

    if (*Ppv)
    {
      AddRef();

      Result = NOERROR;
    }
  }

  DEBUG(L"CShellExt::QueryInterface leave");

  return Result;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExt::AddRef()
{
  DEBUG(L"CShellExt::AddRef");

  return ++FReferenceCounter;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExt::Release()
{
  DEBUG(L"CShellExt::Release");
  if (--FReferenceCounter)
  {
    return FReferenceCounter;
  }

  delete this;

  return 0;
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExt::Initialize(LPCITEMIDLIST /*IDFolder*/,
  LPDATAOBJECT DataObj, HKEY /*RegKey*/)
{
  DEBUG(L"CShellExt::Initialize enter");

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

  DEBUG(L"CShellExt::Initialize leave");

  return NOERROR;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(UINT) CShellExt::CopyCallback(HWND /*Hwnd*/, UINT Func, UINT /*Flags*/,
  LPCWSTR SrcFile, DWORD /*SrcAttribs*/, LPCWSTR DestFile, DWORD /*DestAttribs*/)
{
  DEBUG(L"CShellExt::CopyCallback enter");

  UINT Result = IDYES;

  if (GEnabled && ((Func == FO_COPY) || (Func == FO_MOVE)))
  {
    DEBUG(L"CShellExt::CopyCallback copy or move");

    unsigned long Ticks = GetTickCount();
    if (((Ticks - FLastTicks) >= 100) ||
        (FLastTicks > Ticks))
    {
      DEBUG(L"CShellExt::CopyCallback interval elapsed");

      DEBUG(L"CShellExt::CopyCallback source / dest:");
      DEBUG(SrcFile);
      DEBUG(DestFile);

      FLastTicks = Ticks;
      const wchar_t* BackPtr = wcsrchr(SrcFile, L'\\');

      if ((BackPtr != NULL) &&
          (wcsncmp(BackPtr + 1, DRAG_EXT_DUMMY_DIR_PREFIX,
            DRAG_EXT_DUMMY_DIR_PREFIX_LEN) == 0))
      {
        DEBUG(L"CShellExt::CopyCallback filename has prefix");

        HANDLE MapFile = OpenFileMapping(FILE_MAP_ALL_ACCESS,
          false, DRAG_EXT_MAPPING);

        if (MapFile != NULL)
        {
          DEBUG(L"CShellExt::CopyCallback mapfile found");

          TDragExtCommStruct* CommStruct;
          CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(MapFile,
            FILE_MAP_ALL_ACCESS, 0, 0, 0));

          if (CommStruct != NULL)
          {
            DEBUG(L"CShellExt::CopyCallback mapview created");
            unsigned long WaitResult = WaitForSingleObject(FMutex, 1000);
            if (WaitResult != WAIT_TIMEOUT)
            {
              DEBUG(L"CShellExt::CopyCallback mutex got");
              if (CommStruct->Version >= TDragExtCommStruct::MinVersion)
              {
                DEBUG(L"CShellExt::CopyCallback supported structure version");
                if (CommStruct->Dragging)
                {
                  DEBUG(L"CShellExt::CopyCallback dragging");
                  DEBUG(CommStruct->DropDest);
                  bool IsDropDest;
                  if (_wcsicmp(CommStruct->DropDest, SrcFile) == 0)
                  {
                    IsDropDest = true;
                    DEBUG(L"CShellExt::CopyCallback dragged file match as is");
                  }
                  else
                  {
                    wchar_t DropDestShort[MAX_PATH];
                    wchar_t SrcFileShort[MAX_PATH];
                    size_t DropDestSize = GetShortPathName(CommStruct->DropDest,
                      DropDestShort, LENOF(DropDestShort));
                    size_t SrcFileSize = GetShortPathName(SrcFile,
                      SrcFileShort, LENOF(SrcFileShort));
                    if ((DropDestSize == 0) || (SrcFileSize == 0))
                    {
                      DEBUG(L"CShellExt::CopyCallback cannot convert paths to short form");
                      IsDropDest = false;
                    }
                    else if ((DropDestSize >= LENOF(DropDestShort)) ||
                        (SrcFileSize >= LENOF(SrcFileShort)))
                    {
                      DEBUG(L"CShellExt::CopyCallback short paths too long");
                      IsDropDest = false;
                    }
                    else
                    {
                      DEBUG(DropDestShort);
                      DEBUG(SrcFileShort);

                      if (_wcsicmp(DropDestShort, SrcFileShort) == 0)
                      {
                        DEBUG(L"CShellExt::CopyCallback dragged file match after converted to short form");
                        IsDropDest = true;
                      }
                      else
                      {
                        DEBUG(L"CShellExt::CopyCallback dragged file does NOT match");
                        IsDropDest = false;
                      }
                    }
                  }

                  if (IsDropDest)
                  {
                    CommStruct->Dragging = false;
                    wcsncpy(CommStruct->DropDest, DestFile, LENOF(CommStruct->DropDest));
                    CommStruct->DropDest[LENOF(CommStruct->DropDest)-1] = L'\0';
                    Result = IDNO;
                    DEBUG(L"CShellExt::CopyCallback dragging refused");
                  }
                }
                else
                {
                  DEBUG(L"CShellExt::CopyCallback NOT dragging");
                }
              }
              else
              {
                DEBUG(L"CShellExt::CopyCallback unsupported structure version");
              }
              ReleaseMutex(FMutex);
              DEBUG(L"CShellExt::CopyCallback mutex released");
            }
            else
            {
              DEBUG(L"CShellExt::CopyCallback mutex timeout");
            }
            UnmapViewOfFile(CommStruct);
          }
          else
          {
            DEBUG(L"CShellExt::CopyCallback mapview NOT created");
          }

          CloseHandle(MapFile);
        }
        else
        {
          DEBUG(L"CShellExt::CopyCallback mapfile NOT found");
        }
      }
      else
      {
        DEBUG(L"CShellExt::CopyCallback filename has NOT prefix");
      }
    }
    else
    {
      DEBUG(L"CShellExt::CopyCallback interval NOT elapsed");
    }
  }
  else
  {
    DEBUG(L"CShellExt::CopyCallback NOT copy nor move");
  }

  DEBUG(L"CShellExt::CopyCallback leave");

  return Result;
}
