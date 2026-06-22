//---------------------------------------------------------------------------
#pragma hdrstop
//---------------------------------------------------------------------------
#ifndef STRICT
#define STRICT
#endif
//---------------------------------------------------------------------------
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
//---------------------------------------------------------------------------
#include <initguid.h>
#include <shlguid.h>
#include <stdlib.h>
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
#define Debug(MSG) \
  if (GLogOn) \
  { \
    DoDebug(__FUNC__, MSG); \
  }
//---------------------------------------------------------------------------
#define DRAG_EXT_REG_KEY L"Software\\Martin Prikryl\\WinSCP 2\\DragExt"
#define DRAG_EXT_NAME L"WinSCP Shell Extension"
#define THREADING_MODEL L"Apartment"
#define CLSID_SIZE 39
//---------------------------------------------------------------------------
// WORKAROUND
// While the import is in import32.lib or ole32.lib, the declaration in guiddef.h seems to be for C++,
// so the symbol does not match
int __stdcall IsEqualGUID(REFGUID rguid1, REFGUID rguid2)
{
  return !memcmp(&rguid1, &rguid2, sizeof(GUID));
}
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
#ifdef _WIN64
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpadded"
#endif
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
#ifdef WIN64
#pragma clang diagnostic pop
#endif
//---------------------------------------------------------------------------
#ifndef __clang__
#define nullptr 0
#endif
static unsigned int GRefThisDll = 0;
static bool GEnabled = false;
static wchar_t GLogFile[MAX_PATH] = L"";
static bool GLogOn = false;
static FILE* GLogHandle = nullptr;
static HANDLE GLogMutex;
static HINSTANCE GInstance;
//---------------------------------------------------------------------------
static void DoDebug(const char* Func, const wchar_t* Message)
{
  if (GLogOn)
  {
    unsigned long WaitResult = WaitForSingleObject(GLogMutex, 1000);
    if (WaitResult != WAIT_TIMEOUT)
    {
      try
      {
        if (GLogHandle == nullptr)
        {
          if (wcslen(GLogFile) == 0)
          {
            GLogOn = false;
          }
          else
          {
            GLogHandle = _wfopen(GLogFile, L"at");
            if (GLogHandle == nullptr)
            {
              GLogOn = false;
            }
            else
            {
              setbuf(GLogHandle, nullptr);
              fwprintf(GLogHandle, L"----------------------------\n");
            }
          }
        }

        if (GLogOn)
        {
          SYSTEMTIME Time;
          GetSystemTime(&Time);

          // cannot use TEXT(__FUNC__) as that does not work in clang,
          // where __FUCT__ behaves like a static variable
          const size_t FuncLen = strlen(Func) + 1;
          wchar_t* WideFunc = new wchar_t[FuncLen];
          mbstowcs(WideFunc, Func, FuncLen);

          fwprintf(GLogHandle, L"[%2d/%2d/%4d %2d:%02d:%02d.%03d][%04x] [%s] %s\n",
            Time.wDay, Time.wMonth, Time.wYear, Time.wHour, Time.wMinute,
            Time.wSecond, Time.wMilliseconds, GetCurrentThreadId(), WideFunc, Message);

          delete[] WideFunc;
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
static void LogVersion(HINSTANCE HInstance)
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
            snwprintf(VersionStr, LENOF(VersionStr), L"%d.%d.%d.%d",
              HIWORD(VersionInfo->dwFileVersionMS),
              LOWORD(VersionInfo->dwFileVersionMS),
              HIWORD(VersionInfo->dwFileVersionLS),
              LOWORD(VersionInfo->dwFileVersionLS));
            Debug(VersionStr);
          }
          else
          {
            Debug(L"no fixed version info");
          }
        }
        else
        {
          Debug(L"cannot read version info");
        }
      }
      else
      {
        Debug(L"no version info");
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
      GLogMutex = CreateMutex(nullptr, false, DRAG_EXT_RUNNING_MUTEX);

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
          if ((RegQueryValueEx(Key, L"Enable", nullptr, &Type,
                 reinterpret_cast<unsigned char*>(&Value), &Size) == ERROR_SUCCESS) &&
              (Type == REG_DWORD))
          {
            GEnabled = (Value != 0);
          }

          Size = sizeof(Buf);
          if ((RegQueryValueEx(Key, L"LogFile", nullptr, &Type,
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

      if (GLogOn)
      {
        Debug(L"loaded settings");
        Debug(GEnabled ? L"enabled" : L"disabled");
        #ifdef UNICODE
        Debug(L"Unicode");
        #else
        Debug(L"Ansi");
        #endif
        #ifdef _WIN64
        Debug(L"Win64");
        #else
        Debug(L"Win32");
        #endif
        LogVersion(HInstance);

        TDragExtCommStruct CommStruct;
        const char * CommStructPtr = reinterpret_cast<const char *>(&CommStruct);
        wchar_t Buf[1024];
        swprintf(Buf, L"Comm struct layout - Size %d - Version @%d + %d - Dragging @%d + %d - DropDest @%d + %d",
          sizeof(CommStruct), reinterpret_cast<const char *>(&CommStruct.Version) - CommStructPtr, sizeof(CommStruct.Version),
          reinterpret_cast<const char *>(&CommStruct.Dragging) - CommStructPtr, sizeof(CommStruct.Dragging),
          reinterpret_cast<const char *>(&CommStruct.DropDest) - CommStructPtr, sizeof(CommStruct.DropDest));
        Debug(Buf);
      }
    }
    else
    {
      Debug(L"settings already loaded");
    }

    Debug(L"attach leave");
  }
  else if (Reason == DLL_PROCESS_DETACH)
  {
    Debug(L"detach enter");
    CloseHandle(GLogMutex);
  }

  return 1;   // ok
}
//---------------------------------------------------------------------------
#ifdef _WIN64
#pragma clang diagnostic ignored "-Wdll-attribute-on-redeclaration"
#endif
//---------------------------------------------------------------------------
STDAPI DllCanUnloadNow(void)
{
  bool CanUnload = (GRefThisDll == 0);
  Debug(CanUnload ? L"can" : L"cannot");
  return (CanUnload ? S_OK : S_FALSE);
}
//---------------------------------------------------------------------------
STDAPI DllGetClassObject(REFCLSID Rclsid, REFIID Riid, LPVOID* PpvOut)
{
  Debug(L"enter");

  *PpvOut = nullptr;

  if (IsEqualIID(Rclsid, CLSID_ShellExtension))
  {
    Debug(L"is ShellExtension");

    CShellExtClassFactory* Pcf = new CShellExtClassFactory;

    return Pcf->QueryInterface(Riid, PpvOut);
  }

  return CLASS_E_CLASSNOTAVAILABLE;
}
//---------------------------------------------------------------------------
static bool RegisterServer(bool AllUsers)
{
  Debug(L"enter");

  Debug(AllUsers ? L"all users" : L"current users");

  bool Result = false;
  HKEY RootKey = AllUsers ? HKEY_LOCAL_MACHINE : HKEY_CURRENT_USER;
  HKEY HKey;
  DWORD Unused;
  wchar_t ClassID[CLSID_SIZE];

  StringFromGUID2(CLSID_ShellExtension, ClassID, LENOF(ClassID));

  if ((RegOpenKeyEx(RootKey, L"Software\\Classes", 0, KEY_WRITE, &HKey) ==
         ERROR_SUCCESS) &&
      (RegCreateKeyEx(HKey, L"CLSID", 0, nullptr,
         REG_OPTION_NON_VOLATILE, KEY_WRITE, nullptr, &HKey, &Unused) ==
           ERROR_SUCCESS))
  {
    Debug(L"CLSID created");

    if (RegCreateKey(HKey, ClassID, &HKey) == ERROR_SUCCESS)
    {
      Debug(L"class ID created");

      RegSetValueEx(HKey, nullptr, 0, REG_SZ,
        reinterpret_cast<const unsigned char*>(DRAG_EXT_NAME), sizeof(DRAG_EXT_NAME));

      if (RegCreateKey(HKey, L"InProcServer32", &HKey) == ERROR_SUCCESS)
      {
        Debug(L"InProcServer32 created");

        wchar_t Filename[MAX_PATH];
        GetModuleFileName(GInstance, Filename, LENOF(Filename));
        RegSetValueEx(HKey, nullptr, 0, REG_SZ,
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
           0, nullptr, REG_OPTION_NON_VOLATILE, KEY_WRITE, nullptr, &HKey,
           &Unused) == ERROR_SUCCESS))
    {
      Debug(L"WinSCPCopyHook created");

      RegSetValueEx(HKey, nullptr, 0, REG_SZ,
        reinterpret_cast<unsigned char*>(ClassID), (wcslen(ClassID) + 1) * sizeof(wchar_t));
      RegCloseKey(HKey);

      if ((RegCreateKeyEx(RootKey, DRAG_EXT_REG_KEY,
             0, nullptr, REG_OPTION_NON_VOLATILE, KEY_WRITE, nullptr, &HKey,
             &Unused) == ERROR_SUCCESS))
      {
        Debug(L"drag ext key created");

        unsigned long Value = 1;
        RegSetValueEx(HKey, L"Enable", 0, REG_DWORD,
          reinterpret_cast<unsigned char*>(&Value), sizeof(Value));

        RegCloseKey(HKey);

        Result = true;
      }

      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nullptr, nullptr);
    }
  }

  Debug(L"leave");

  return Result;
}
//---------------------------------------------------------------------------
STDAPI DllRegisterServer()
{
  Debug(L"enter");

  HRESULT Result;
  if (RegisterServer(true) || RegisterServer(false))
  {
    Result = S_OK;
  }
  else
  {
    Result = SELFREG_E_CLASS;
  }

  Debug(L"leave");

  return Result;
}
//---------------------------------------------------------------------------
static bool UnregisterServer(bool AllUsers)
{
  Debug(L"enter");

  Debug(AllUsers ? L"all users" : L"current users");

  bool Result = false;
  wchar_t ClassID[CLSID_SIZE];

  StringFromGUID2(CLSID_ShellExtension, ClassID, LENOF(ClassID));

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
    // Previously we were setting the value explicitly to 0,
    // but doing that for both HKLM and HKCU.
    // While on register, we set it to 1 for HKLM only,
    // what makes the extension disabled effectively,
    // as KHCU value 0, is kept even after re-registration
    RegDeleteValue(HKey, L"Enable");

    RegCloseKey(HKey);

    Result = true;
  }

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nullptr, nullptr);

  Debug(L"leave");

  return Result;
}
//---------------------------------------------------------------------------
STDAPI DllUnregisterServer()
{
  Debug(L"enter");

  HRESULT Result = SELFREG_E_CLASS;
  if (UnregisterServer(true))
  {
    Result = S_OK;
  }

  if (UnregisterServer(false))
  {
    Result = S_OK;
  }

  Debug(L"leave");

  return Result;
}
//---------------------------------------------------------------------------
CShellExtClassFactory::CShellExtClassFactory()
{
  Debug(L"enter");

  FReferenceCounter = 0;

  GRefThisDll++;
}
//---------------------------------------------------------------------------
CShellExtClassFactory::~CShellExtClassFactory()
{
  Debug(L"enter");

  GRefThisDll--;
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExtClassFactory::QueryInterface(REFIID Riid, LPVOID FAR* Ppv)
{
  Debug(L"enter");

  *Ppv = nullptr;

  // Any interface on this object is the object pointer

  if (IsEqualIID(Riid, IID_IUnknown) || IsEqualIID(Riid, IID_IClassFactory))
  {
    Debug(L"is IUnknown or IClassFactory");

    *Ppv = static_cast<LPCLASSFACTORY>(this);

    AddRef();

    return NOERROR;
  }

  return E_NOINTERFACE;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExtClassFactory::AddRef()
{
  Debug(L"enter");
  return ++FReferenceCounter;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExtClassFactory::Release()
{
  Debug(L"enter");

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
  Debug(L"enter");

  *PpvObj = nullptr;

  // Shell extensions typically don't support aggregation (inheritance)

  if (UnkOuter)
  {
    return CLASS_E_NOAGGREGATION;
  }

  // Create the main shell extension object.  The shell will then call
  // QueryInterface with IID_IShellExtInit--this is how shell extensions are
  // initialized.

  CShellExt* ShellExt = new CShellExt();  //Create the CShellExt object

  return ShellExt->QueryInterface(Riid, PpvObj);
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExtClassFactory::LockServer(BOOL /*Lock*/)
{
  Debug(L"enter");

  return NOERROR;
}
//---------------------------------------------------------------------------
// CShellExt
CShellExt::CShellExt()
{
  Debug(L"enter");

  FReferenceCounter = 0L;
  FDataObj = nullptr;

  FMutex = CreateMutex(nullptr, false, DRAG_EXT_MUTEX);
  FLastTicks = 0;

  GRefThisDll++;

  Debug(L"leave");
}
//---------------------------------------------------------------------------
CShellExt::~CShellExt()
{
  Debug(L"enter");

  if (FDataObj)
  {
    FDataObj->Release();
  }

  CloseHandle(FMutex);

  GRefThisDll--;

  Debug(L"leave");
}
//---------------------------------------------------------------------------
STDMETHODIMP CShellExt::QueryInterface(REFIID Riid, LPVOID FAR* Ppv)
{
  Debug(L"enter");

  HRESULT Result = E_NOINTERFACE;
  *Ppv = nullptr;

  if (!GEnabled)
  {
    Debug(L"shellext disabled");
  }
  else
  {
    if (IsEqualIID(Riid, IID_IShellExtInit) || IsEqualIID(Riid, IID_IUnknown))
    {
      Debug(L"is IShellExtInit or IUnknown");
      *Ppv = static_cast<LPSHELLEXTINIT>(this);
    }
    else if (IsEqualIID(Riid, IID_IShellCopyHook))
    {
      Debug(L"is IShellCopyHook");
      *Ppv = static_cast<LPCOPYHOOK>(this);
    }

    if (*Ppv)
    {
      AddRef();

      Result = NOERROR;
    }
  }

  Debug(L"leave");

  return Result;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExt::AddRef()
{
  Debug(L"enter");

  return ++FReferenceCounter;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(ULONG) CShellExt::Release()
{
  Debug(L"enter");
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
  Debug(L"enter");

  if (FDataObj != nullptr)
  {
    FDataObj->Release();
    FDataObj = nullptr;
  }

  // duplicate the object pointer and registry handle

  if (DataObj != nullptr)
  {
    FDataObj = DataObj;
    DataObj->AddRef();
  }

  Debug(L"leave");

  return NOERROR;
}
//---------------------------------------------------------------------------
STDMETHODIMP_(UINT) CShellExt::CopyCallback(HWND /*Hwnd*/, UINT Func, UINT /*Flags*/,
  LPCWSTR SrcFile, DWORD /*SrcAttribs*/, LPCWSTR DestFile, DWORD /*DestAttribs*/)
{
  Debug(L"enter");

  UINT Result = IDYES;

  if (GEnabled && ((Func == FO_COPY) || (Func == FO_MOVE)))
  {
    Debug(L"copy or move");

    unsigned long Ticks = GetTickCount();
    if (((Ticks - FLastTicks) >= 100) ||
        (FLastTicks > Ticks))
    {
      Debug(L"interval elapsed");

      Debug(L"source / dest:");
      Debug(SrcFile);
      Debug(DestFile);

      FLastTicks = Ticks;
      const wchar_t* BackPtr = wcsrchr(SrcFile, L'\\');

      // WORKAROUND: Windows 10 1803 sends empty DestFile
      if (wcslen(DestFile) == 0)
      {
        Debug(L"empty dest file");
      }
      else if ((BackPtr != nullptr) &&
          (wcsncmp(BackPtr + 1, DRAG_EXT_DUMMY_DIR_PREFIX,
            DRAG_EXT_DUMMY_DIR_PREFIX_LEN) == 0))
      {
        Debug(L"filename has prefix");

        HANDLE MapFile = OpenFileMapping(FILE_MAP_ALL_ACCESS,
          false, DRAG_EXT_MAPPING);

        if (MapFile != nullptr)
        {
          Debug(L"mapfile found");

          TDragExtCommStruct* CommStruct;
          CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(MapFile,
            FILE_MAP_ALL_ACCESS, 0, 0, 0));

          if (CommStruct != nullptr)
          {
            Debug(L"mapview created");
            unsigned long WaitResult = WaitForSingleObject(FMutex, 1000);
            if (WaitResult != WAIT_TIMEOUT)
            {
              Debug(L"mutex got");
              if (CommStruct->Version >= TDragExtCommStruct::MinVersion)
              {
                Debug(L"supported structure version");
                if (CommStruct->Dragging)
                {
                  Debug(L"dragging");
                  Debug(CommStruct->DropDest);
                  bool IsDropDest;
                  if (_wcsicmp(CommStruct->DropDest, SrcFile) == 0)
                  {
                    IsDropDest = true;
                    Debug(L"dragged file match as is");
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
                      Debug(L"cannot convert paths to short form");
                      IsDropDest = false;
                    }
                    else if ((DropDestSize >= LENOF(DropDestShort)) ||
                        (SrcFileSize >= LENOF(SrcFileShort)))
                    {
                      Debug(L"short paths too long");
                      IsDropDest = false;
                    }
                    else
                    {
                      Debug(DropDestShort);
                      Debug(SrcFileShort);

                      if (_wcsicmp(DropDestShort, SrcFileShort) == 0)
                      {
                        Debug(L"dragged file match after converted to short form");
                        IsDropDest = true;
                      }
                      else
                      {
                        Debug(L"dragged file does NOT match");
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
                    Debug(L"dragging refused");
                  }
                }
                else
                {
                  Debug(L"NOT dragging");
                }
              }
              else
              {
                Debug(L"unsupported structure version");
              }
              ReleaseMutex(FMutex);
              Debug(L"mutex released");
            }
            else
            {
              Debug(L"mutex timeout");
            }
            UnmapViewOfFile(CommStruct);
          }
          else
          {
            Debug(L"mapview NOT created");
          }

          CloseHandle(MapFile);
        }
        else
        {
          Debug(L"mapfile NOT found");
        }
      }
      else
      {
        Debug(L"filename has NOT prefix");
      }
    }
    else
    {
      Debug(L"interval NOT elapsed");
    }
  }
  else
  {
    if (GLogOn)
    {
      if (!GEnabled)
      {
        Debug(L"disabled");
      }
      else
      {
        wchar_t Buf[1024];
        swprintf(Buf, L"NOT copy nor move - %d", Func);
        Debug(Buf);
      }
    }
  }

  Debug(L"leave");

  return Result;
}
