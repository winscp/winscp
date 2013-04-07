// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"

#include <imagehlp.h>
#pragma comment(lib, "imagehlp.lib")

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
// Routine to produce stack dump

static LPVOID __stdcall FunctionTableAccess(HANDLE hProcess, DWORD dwPCAddress);
static DWORD __stdcall GetModuleBase(HANDLE hProcess, DWORD dwReturnAddress);

#define MODULE_NAME_LEN 64
#define SYMBOL_NAME_LEN 128

struct SYMBOL_INFO
{
	DWORD dwAddress;
	DWORD dwOffset;
	CHAR    szModule[MODULE_NAME_LEN];
	CHAR    szSymbol[SYMBOL_NAME_LEN];
};

static LPVOID __stdcall FunctionTableAccess(HANDLE hProcess, DWORD dwPCAddress)
{
	return SymFunctionTableAccess(hProcess, dwPCAddress);
}

static DWORD __stdcall GetModuleBase(HANDLE hProcess, DWORD dwReturnAddress)
{
	IMAGEHLP_MODULE moduleInfo;

	if (SymGetModuleInfo(hProcess, dwReturnAddress, &moduleInfo))
		return moduleInfo.BaseOfImage;
	else
	{
		MEMORY_BASIC_INFORMATION memoryBasicInfo;

		if (::VirtualQueryEx(hProcess, (LPVOID) dwReturnAddress,
			&memoryBasicInfo, sizeof(memoryBasicInfo)))
		{
			DWORD cch = 0;
			char szFile[MAX_PATH] = { 0 };

		 cch = GetModuleFileNameA((HINSTANCE)memoryBasicInfo.AllocationBase,
										 szFile, MAX_PATH);

		 // Ignore the return code since we can't do anything with it.
		 if (!SymLoadModule(hProcess,
			   NULL, ((cch) ? szFile : NULL),
			   NULL, (DWORD) memoryBasicInfo.AllocationBase, 0))
			{
				DWORD dwError = GetLastError();
				TRACE1("Error: %d\n", dwError);
			}
		 return (DWORD) memoryBasicInfo.AllocationBase;
	  }
		else
			TRACE1("Error is %d\n", GetLastError());
	}

	return 0;
}

static BOOL ResolveSymbol(HANDLE hProcess, DWORD dwAddress,
	SYMBOL_INFO &siSymbol)
{
	BOOL fRetval = TRUE;

	siSymbol.dwAddress = dwAddress;

	union {
		CHAR rgchSymbol[sizeof(IMAGEHLP_SYMBOL) + 255];
		IMAGEHLP_SYMBOL  sym;
	};

	CHAR szUndec[256];
	CHAR szWithOffset[256];
	LPSTR pszSymbol = NULL;
	IMAGEHLP_MODULE mi;

	memset(&siSymbol, 0, sizeof(SYMBOL_INFO));
	mi.SizeOfStruct = sizeof(IMAGEHLP_MODULE);

	if (!SymGetModuleInfo(hProcess, dwAddress, &mi))
		lstrcpyA(siSymbol.szModule, "<no module>");
	else
	{
		LPSTR pszModule = strchr(mi.ImageName, '\\');
		if (pszModule == NULL)
			pszModule = mi.ImageName;
		else
			pszModule++;

		lstrcpynA(siSymbol.szModule, pszModule, _countof(siSymbol.szModule));
	   lstrcatA(siSymbol.szModule, "! ");
	}

	__try
	{
		sym.SizeOfStruct = sizeof(IMAGEHLP_SYMBOL);
		sym.Address = dwAddress;
		sym.MaxNameLength = 255;

		if (SymGetSymFromAddr(hProcess, dwAddress, &(siSymbol.dwOffset), &sym))
		{
			pszSymbol = sym.Name;

			if (UnDecorateSymbolName(sym.Name, szUndec, _countof(szUndec),
				UNDNAME_NO_MS_KEYWORDS | UNDNAME_NO_ACCESS_SPECIFIERS))
			{
				pszSymbol = szUndec;
			}
			else if (SymUnDName(&sym, szUndec, _countof(szUndec)))
			{
				pszSymbol = szUndec;
			}

			if (siSymbol.dwOffset != 0)
			{
				wsprintfA(szWithOffset, "%s + %d bytes", pszSymbol, siSymbol.dwOffset);
				pszSymbol = szWithOffset;
			}
	  }
	  else
		  pszSymbol = "<no symbol>";
	}
	__except (EXCEPTION_EXECUTE_HANDLER)
	{
		pszSymbol = "<EX: no symbol>";
		siSymbol.dwOffset = dwAddress - mi.BaseOfImage;
	}

	lstrcpynA(siSymbol.szSymbol, pszSymbol, _countof(siSymbol.szSymbol));
	return fRetval;
}

class CTraceClipboardData
{
	HGLOBAL m_hMemory;
	DWORD   m_dwSize;
	DWORD m_dwUsed;
	DWORD m_dwTarget;

public:
	void SendOut(LPCSTR pszData);
	CTraceClipboardData(DWORD dwTarget);
	~CTraceClipboardData();
};

CTraceClipboardData::CTraceClipboardData(DWORD dwTarget)
	: m_dwTarget(dwTarget), m_dwSize(0), m_dwUsed(0), m_hMemory(NULL)
{
}

CTraceClipboardData::~CTraceClipboardData()
{
	if (m_hMemory != NULL)
	{
		// chuck it onto the clipboard
		// don't free it unless there's an error

		if (!OpenClipboard(NULL))
			GlobalFree(m_hMemory);
		else if (!EmptyClipboard() ||
				SetClipboardData(CF_TEXT, m_hMemory) == NULL)
		{
			GlobalFree(m_hMemory);
		}
		else
			CloseClipboard();
	}
}

void CTraceClipboardData::SendOut(LPCSTR pszData)
{
	int nLength;
	if (pszData == NULL || (nLength = lstrlenA(pszData)) == 0)
		return;

	// send it to TRACE (can be redirected)
	if (m_dwTarget & AFX_STACK_DUMP_TARGET_TRACE)
		TRACE1("%hs", pszData);

	// send it to OutputDebugString() (can't redirect)
	if (m_dwTarget & AFX_STACK_DUMP_TARGET_ODS)
		OutputDebugStringA(pszData);

	// build a buffer for the clipboard
	if (m_dwTarget & AFX_STACK_DUMP_TARGET_CLIPBOARD)
	{
		if (m_hMemory == NULL)
		{
		   m_hMemory = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, 1024);
			if (m_hMemory == NULL)
			{
				TRACE0("AfxDumpStack Error: No memory available for clipboard.\n");
				m_dwTarget &= ~AFX_STACK_DUMP_TARGET_CLIPBOARD;
			}
			else
			{
				m_dwUsed = nLength;
				m_dwSize = 1024;
				LPSTR pstr = (LPSTR) GlobalLock(m_hMemory);
				if (pstr != NULL)
				{
					lstrcpyA(pstr, pszData);
					GlobalUnlock(m_hMemory);
				}
				else
				{
					TRACE0("AfxDumpStack Error: Couldn't lock memory!\n");
					GlobalFree(m_hMemory);
					m_hMemory = NULL;
					m_dwTarget &= ~AFX_STACK_DUMP_TARGET_CLIPBOARD;
				}
			}
		}
		else
		{
			if ((m_dwUsed + nLength + 1) >= m_dwSize)
			{
				// grow by leaps and bounds
				m_dwSize *= 2;
				if (m_dwSize > (1024L*1024L))
				{
					TRACE0("AfxDumpStack Error: more than one megabyte on clipboard.\n");
					m_dwTarget &= ~AFX_STACK_DUMP_TARGET_CLIPBOARD;
				}

				HGLOBAL hMemory = GlobalReAlloc(m_hMemory, m_dwSize, GMEM_MOVEABLE);
				if (hMemory == NULL)
				{
					TRACE1("AfxDumpStack Error: Couldn't get %d bytes!\n", m_dwSize);
					m_dwTarget &= ~AFX_STACK_DUMP_TARGET_CLIPBOARD;
				}
				else
					m_hMemory = hMemory;
			}

			LPSTR pstr = (LPSTR) GlobalLock(m_hMemory);
			if (pstr != NULL)
			{
				lstrcpyA(pstr + m_dwUsed, pszData);
				m_dwUsed += nLength;
				GlobalUnlock(m_hMemory);
			}
			else
			{
				TRACE0("AfxDumpStack Error: Couldn't lock memory!\n");
				m_dwTarget &= ~AFX_STACK_DUMP_TARGET_CLIPBOARD;
			}
		}
	}

	return;
}

/////////////////////////////////////////////////////////////////////////////
// AfxDumpStack API

void AFXAPI AfxDumpStack(DWORD dwTarget /* = AFX_STACK_DUMP_TARGET_DEFAULT */)
{
	CTraceClipboardData clipboardData(dwTarget);

	clipboardData.SendOut("=== begin AfxDumpStack output ===\r\n");

	CDWordArray adwAddress;
	HANDLE hProcess = ::GetCurrentProcess();
	if (SymInitialize(hProcess, NULL, FALSE))
	{
		// force undecorated names to get params
		DWORD dw = SymGetOptions();
		dw &= ~SYMOPT_UNDNAME;
		SymSetOptions(dw);

		HANDLE hThread = ::GetCurrentThread();
		CONTEXT threadContext;

		threadContext.ContextFlags = CONTEXT_FULL;

		if (::GetThreadContext(hThread, &threadContext))
		{
			STACKFRAME stackFrame;
			memset(&stackFrame, 0, sizeof(stackFrame));
			stackFrame.AddrPC.Mode = AddrModeFlat;

			DWORD dwMachType;

#if defined(_M_IX86)
			dwMachType                  = IMAGE_FILE_MACHINE_I386;

			// program counter, stack pointer, and frame pointer
			stackFrame.AddrPC.Offset    = threadContext.Eip;
			stackFrame.AddrStack.Offset = threadContext.Esp;
			stackFrame.AddrStack.Mode   = AddrModeFlat;
			stackFrame.AddrFrame.Offset = threadContext.Ebp;
			stackFrame.AddrFrame.Mode   = AddrModeFlat;
#elif defined(_M_MRX000)
			// only program counter
			dwMachType                  = IMAGE_FILE_MACHINE_R4000;
			stackFrame.AddrPC. Offset    = treadContext.Fir;
#elif defined(_M_ALPHA)
			// only program counter
			dwMachType                  = IMAGE_FILE_MACHINE_ALPHA;
			stackFrame.AddrPC.Offset    = (unsigned long) threadContext.Fir;
#elif defined(_M_PPC)
			// only program counter
			dwMachType                  = IMAGE_FILE_MACHINE_POWERPC;
			stackFrame.AddrPC.Offset    = threadContext.Iar;
#elif
#error("Unknown Target Machine");
#endif

			adwAddress.SetSize(0, 16);

			int nFrame;
			for (nFrame = 0; nFrame < 1024; nFrame++)
			{
				if (!StackWalk(dwMachType, hProcess, hProcess,
					&stackFrame, &threadContext, NULL,
					FunctionTableAccess, GetModuleBase, NULL))
				{
					break;
				}

				adwAddress.SetAtGrow(nFrame, stackFrame.AddrPC.Offset);
			}
		}
	}
	else
	{
		DWORD dw = GetLastError();
		char sz[100];
		wsprintfA(sz,
			"AfxDumpStack Error: IMAGEHLP.DLL wasn't found. "
			"GetLastError() returned 0x%8.8X\r\n", dw);
		clipboardData.SendOut(sz);
	}

	// dump it out now
	int nAddress;
	int cAddresses = adwAddress.GetSize();
	for (nAddress = 0; nAddress < cAddresses; nAddress++)
	{
		SYMBOL_INFO info;
		DWORD dwAddress = adwAddress[nAddress];

		char sz[20];
		wsprintfA(sz, "%8.8X: ", dwAddress);
		clipboardData.SendOut(sz);

		if (ResolveSymbol(hProcess, dwAddress, info))
		{
			clipboardData.SendOut(info.szModule);
			clipboardData.SendOut(info.szSymbol);
		}
		else
			clipboardData.SendOut("symbol not found");
		clipboardData.SendOut("\r\n");
	}

	clipboardData.SendOut("=== end AfxDumpStack() output ===\r\n");
}
