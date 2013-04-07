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

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

AFX_STATIC_DATA const TCHAR _afxShellOpenFmt[] = _T("%s\\shell\\open\\%s");
AFX_STATIC_DATA const TCHAR _afxShellPrintFmt[] = _T("%s\\shell\\print\\%s");
AFX_STATIC_DATA const TCHAR _afxShellPrintToFmt[] = _T("%s\\shell\\printto\\%s");
AFX_STATIC_DATA const TCHAR _afxDefaultIconFmt[] = _T("%s\\DefaultIcon");
AFX_STATIC_DATA const TCHAR _afxShellNewFmt[] = _T("%s\\ShellNew");

#define DEFAULT_ICON_INDEX 0

AFX_STATIC_DATA const TCHAR _afxIconIndexFmt[] = _T(",%d");
AFX_STATIC_DATA const TCHAR _afxCommand[] = _T("command");
AFX_STATIC_DATA const TCHAR _afxOpenArg[] = _T(" \"%1\"");
AFX_STATIC_DATA const TCHAR _afxPrintArg[] = _T(" /p \"%1\"");
AFX_STATIC_DATA const TCHAR _afxPrintToArg[] = _T(" /pt \"%1\" \"%2\" \"%3\" \"%4\"");
AFX_STATIC_DATA const TCHAR _afxDDEArg[] = _T(" /dde");

AFX_STATIC_DATA const TCHAR _afxDDEExec[] = _T("ddeexec");
AFX_STATIC_DATA const TCHAR _afxDDEOpen[] = _T("[open(\"%1\")]");
AFX_STATIC_DATA const TCHAR _afxDDEPrint[] = _T("[print(\"%1\")]");
AFX_STATIC_DATA const TCHAR _afxDDEPrintTo[] = _T("[printto(\"%1\",\"%2\",\"%3\",\"%4\")]");

AFX_STATIC_DATA const TCHAR _afxShellNewValueName[] = _T("NullFile");
AFX_STATIC_DATA const TCHAR _afxShellNewValue[] = _T("");

// recursively remove a registry key if and only if it has no subkeys

BOOL AFXAPI _AfxDeleteRegKey(LPCTSTR lpszKey)
{
	// copy the string
	LPTSTR lpszKeyCopy = _tcsdup(lpszKey);
	LPTSTR lpszLast = lpszKeyCopy + lstrlen(lpszKeyCopy);

	// work until the end of the string
	while (lpszLast != NULL)
	{
		*lpszLast = '\0';
		lpszLast = _tcsdec(lpszKeyCopy, lpszLast);

		// try to open that key
		HKEY hKey;
		if (::RegOpenKey(HKEY_CLASSES_ROOT, lpszKeyCopy, &hKey) != ERROR_SUCCESS)
			break;

		// enumerate the keys underneath
		TCHAR szScrap[_MAX_PATH+1];
		DWORD dwLen = _countof(szScrap);
		BOOL bItExists = FALSE;

		if (::RegEnumKey(hKey, 0, szScrap, dwLen) == ERROR_SUCCESS)
			bItExists = TRUE;
		::RegCloseKey(hKey);

		// found one?  quit looping
		if (bItExists)
			break;

		// otherwise, delete and find the previous backwhack
		::RegDeleteKey(HKEY_CLASSES_ROOT, lpszKeyCopy);
		lpszLast = _tcsrchr(lpszKeyCopy, '\\');
	}

	// release the string and return
	free(lpszKeyCopy);
	return TRUE;
}

AFX_STATIC BOOL AFXAPI
_AfxSetRegKey(LPCTSTR lpszKey, LPCTSTR lpszValue, LPCTSTR lpszValueName = NULL)
{
	if (lpszValueName == NULL)
	{
		if (::RegSetValue(HKEY_CLASSES_ROOT, lpszKey, REG_SZ,
			  lpszValue, lstrlen(lpszValue) * sizeof(TCHAR)) != ERROR_SUCCESS)
		{
			TRACE1("Warning: registration database update failed for key '%s'.\n",
				lpszKey);
			return FALSE;
		}
		return TRUE;
	}
	else
	{
		HKEY hKey;

		if(::RegCreateKey(HKEY_CLASSES_ROOT, lpszKey, &hKey) == ERROR_SUCCESS)
		{
			LONG lResult = ::RegSetValueEx(hKey, lpszValueName, 0, REG_SZ,
				(CONST BYTE*)lpszValue, (lstrlen(lpszValue) + 1) * sizeof(TCHAR));

			if(::RegCloseKey(hKey) == ERROR_SUCCESS && lResult == ERROR_SUCCESS)
				return TRUE;
		}
		TRACE1("Warning: registration database update failed for key '%s'.\n", lpszKey);
		return FALSE;
	}
}

CDocManager::CDocManager()
{
}

void CDocManager::UnregisterShellFileTypes()
{
	ASSERT(!m_templateList.IsEmpty());  // must have some doc templates

	CString strPathName, strTemp;

	AfxGetModuleShortFileName(AfxGetInstanceHandle(), strPathName);

	POSITION pos = m_templateList.GetHeadPosition();
	for (int nTemplateIndex = 1; pos != NULL; nTemplateIndex++)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);

		CString strFilterExt, strFileTypeId, strFileTypeName;
		if (pTemplate->GetDocString(strFileTypeId,
		   CDocTemplate::regFileTypeId) && !strFileTypeId.IsEmpty())
		{
			// enough info to register it
			if (!pTemplate->GetDocString(strFileTypeName,
			   CDocTemplate::regFileTypeName))
				strFileTypeName = strFileTypeId;    // use id name

			ASSERT(strFileTypeId.Find(' ') == -1);  // no spaces allowed

			strTemp.Format(_afxDefaultIconFmt, (LPCTSTR)strFileTypeId);
			_AfxDeleteRegKey(strTemp);

			// If MDI Application
			if (!pTemplate->GetDocString(strTemp, CDocTemplate::windowTitle) ||
				strTemp.IsEmpty())
			{
				// path\shell\open\ddeexec = [open("%1")]
				strTemp.Format(_afxShellOpenFmt, (LPCTSTR)strFileTypeId,
					(LPCTSTR)_afxDDEExec);
				_AfxDeleteRegKey(strTemp);

				// path\shell\print\ddeexec = [print("%1")]
				strTemp.Format(_afxShellPrintFmt, (LPCTSTR)strFileTypeId,
					(LPCTSTR)_afxDDEExec);
				_AfxDeleteRegKey(strTemp);

				// path\shell\printto\ddeexec = [printto("%1","%2","%3","%4")]
				strTemp.Format(_afxShellPrintToFmt, (LPCTSTR)strFileTypeId,
					(LPCTSTR)_afxDDEExec);
				_AfxDeleteRegKey(strTemp);
			}

			// path\shell\open\command = path filename
			strTemp.Format(_afxShellOpenFmt, (LPCTSTR)strFileTypeId,
				(LPCTSTR)_afxCommand);
			_AfxDeleteRegKey(strTemp);

			// path\shell\print\command = path /p filename
			strTemp.Format(_afxShellPrintFmt, (LPCTSTR)strFileTypeId,
				(LPCTSTR)_afxCommand);
			_AfxDeleteRegKey(strTemp);

			// path\shell\printto\command = path /pt filename printer driver port
			strTemp.Format(_afxShellPrintToFmt, (LPCTSTR)strFileTypeId,
				(LPCTSTR)_afxCommand);
			_AfxDeleteRegKey(strTemp);

			pTemplate->GetDocString(strFilterExt, CDocTemplate::filterExt);
			if (!strFilterExt.IsEmpty())
			{
				ASSERT(strFilterExt[0] == '.');

				LONG lSize = _MAX_PATH * 2;
				LONG lResult = ::RegQueryValue(HKEY_CLASSES_ROOT, strFilterExt,
					strTemp.GetBuffer(lSize), &lSize);
				strTemp.ReleaseBuffer();

				if (lResult != ERROR_SUCCESS || strTemp.IsEmpty() ||
					strTemp == strFileTypeId)
				{
					strTemp.Format(_afxShellNewFmt, (LPCTSTR)strFilterExt);
					_AfxDeleteRegKey(strTemp);

					// no association for that suffix
					_AfxDeleteRegKey(strFilterExt);
				}
			}
		}
	}
}


void CDocManager::RegisterShellFileTypes(BOOL bCompat)
{
	ASSERT(!m_templateList.IsEmpty());  // must have some doc templates

	CString strPathName, strTemp;

	AfxGetModuleShortFileName(AfxGetInstanceHandle(), strPathName);

	POSITION pos = m_templateList.GetHeadPosition();
	for (int nTemplateIndex = 1; pos != NULL; nTemplateIndex++)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);

		CString strOpenCommandLine = strPathName;
		CString strPrintCommandLine = strPathName;
		CString strPrintToCommandLine = strPathName;
		CString strDefaultIconCommandLine = strPathName;

		if (bCompat)
		{
			CString strIconIndex;
			HICON hIcon = ::ExtractIcon(AfxGetInstanceHandle(), strPathName, nTemplateIndex);
			if (hIcon != NULL)
			{
				strIconIndex.Format(_afxIconIndexFmt, nTemplateIndex);
				DestroyIcon(hIcon);
			}
			else
			{
				strIconIndex.Format(_afxIconIndexFmt, DEFAULT_ICON_INDEX);
			}
			strDefaultIconCommandLine += strIconIndex;
		}

		CString strFilterExt, strFileTypeId, strFileTypeName;
		if (pTemplate->GetDocString(strFileTypeId,
		   CDocTemplate::regFileTypeId) && !strFileTypeId.IsEmpty())
		{
			// enough info to register it
			if (!pTemplate->GetDocString(strFileTypeName,
			   CDocTemplate::regFileTypeName))
				strFileTypeName = strFileTypeId;    // use id name

			ASSERT(strFileTypeId.Find(' ') == -1);  // no spaces allowed

			// first register the type ID of our server
			if (!_AfxSetRegKey(strFileTypeId, strFileTypeName))
				continue;       // just skip it

			if (bCompat)
			{
				// path\DefaultIcon = path,1
				strTemp.Format(_afxDefaultIconFmt, (LPCTSTR)strFileTypeId);
				if (!_AfxSetRegKey(strTemp, strDefaultIconCommandLine))
					continue;       // just skip it
			}

			// If MDI Application
			if (!pTemplate->GetDocString(strTemp, CDocTemplate::windowTitle) ||
				strTemp.IsEmpty())
			{
				// path\shell\open\ddeexec = [open("%1")]
				strTemp.Format(_afxShellOpenFmt, (LPCTSTR)strFileTypeId,
					(LPCTSTR)_afxDDEExec);
				if (!_AfxSetRegKey(strTemp, _afxDDEOpen))
					continue;       // just skip it

				if (bCompat)
				{
					// path\shell\print\ddeexec = [print("%1")]
					strTemp.Format(_afxShellPrintFmt, (LPCTSTR)strFileTypeId,
						(LPCTSTR)_afxDDEExec);
					if (!_AfxSetRegKey(strTemp, _afxDDEPrint))
						continue;       // just skip it

					// path\shell\printto\ddeexec = [printto("%1","%2","%3","%4")]
					strTemp.Format(_afxShellPrintToFmt, (LPCTSTR)strFileTypeId,
						(LPCTSTR)_afxDDEExec);
					if (!_AfxSetRegKey(strTemp, _afxDDEPrintTo))
						continue;       // just skip it

					// path\shell\open\command = path /dde
					// path\shell\print\command = path /dde
					// path\shell\printto\command = path /dde
					strOpenCommandLine += _afxDDEArg;
					strPrintCommandLine += _afxDDEArg;
					strPrintToCommandLine += _afxDDEArg;
				}
				else
				{
					strOpenCommandLine += _afxOpenArg;
				}
			}
			else
			{
				// path\shell\open\command = path filename
				// path\shell\print\command = path /p filename
				// path\shell\printto\command = path /pt filename printer driver port
				strOpenCommandLine += _afxOpenArg;
				if (bCompat)
				{
					strPrintCommandLine += _afxPrintArg;
					strPrintToCommandLine += _afxPrintToArg;
				}
			}

			// path\shell\open\command = path filename
			strTemp.Format(_afxShellOpenFmt, (LPCTSTR)strFileTypeId,
				(LPCTSTR)_afxCommand);
			if (!_AfxSetRegKey(strTemp, strOpenCommandLine))
				continue;       // just skip it

			if (bCompat)
			{
				// path\shell\print\command = path /p filename
				strTemp.Format(_afxShellPrintFmt, (LPCTSTR)strFileTypeId,
					(LPCTSTR)_afxCommand);
				if (!_AfxSetRegKey(strTemp, strPrintCommandLine))
					continue;       // just skip it

				// path\shell\printto\command = path /pt filename printer driver port
				strTemp.Format(_afxShellPrintToFmt, (LPCTSTR)strFileTypeId,
					(LPCTSTR)_afxCommand);
				if (!_AfxSetRegKey(strTemp, strPrintToCommandLine))
					continue;       // just skip it
			}

			pTemplate->GetDocString(strFilterExt, CDocTemplate::filterExt);
			if (!strFilterExt.IsEmpty())
			{
				ASSERT(strFilterExt[0] == '.');

				LONG lSize = _MAX_PATH * 2;
				LONG lResult = ::RegQueryValue(HKEY_CLASSES_ROOT, strFilterExt,
					strTemp.GetBuffer(lSize), &lSize);
				strTemp.ReleaseBuffer();

				if (lResult != ERROR_SUCCESS || strTemp.IsEmpty() ||
					strTemp == strFileTypeId)
				{
					// no association for that suffix
					if (!_AfxSetRegKey(strFilterExt, strFileTypeId))
						continue;

					if (bCompat)
					{
						strTemp.Format(_afxShellNewFmt, (LPCTSTR)strFilterExt);
						(void)_AfxSetRegKey(strTemp, _afxShellNewValue, _afxShellNewValueName);
					}
				}
			}
		}
	}
}

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

AFX_STATIC void AFXAPI _AfxAppendFilterSuffix(CString& filter, OPENFILENAME& ofn,
	CDocTemplate* pTemplate, CString* pstrDefaultExt)
{
	ASSERT_VALID(pTemplate);
	ASSERT_KINDOF(CDocTemplate, pTemplate);

	CString strFilterExt, strFilterName;
	if (pTemplate->GetDocString(strFilterExt, CDocTemplate::filterExt) &&
	 !strFilterExt.IsEmpty() &&
	 pTemplate->GetDocString(strFilterName, CDocTemplate::filterName) &&
	 !strFilterName.IsEmpty())
	{
		// a file based document template - add to filter list
		ASSERT(strFilterExt[0] == '.');
		if (pstrDefaultExt != NULL)
		{
			// set the default extension
			*pstrDefaultExt = ((LPCTSTR)strFilterExt) + 1;  // skip the '.'
			ofn.lpstrDefExt = (LPTSTR)(LPCTSTR)(*pstrDefaultExt);
			ofn.nFilterIndex = ofn.nMaxCustFilter + 1;  // 1 based number
		}

		// add to filter
		filter += strFilterName;
		ASSERT(!filter.IsEmpty());  // must have a file type name
		filter += (TCHAR)'\0';  // next string please
		filter += (TCHAR)'*';
		filter += strFilterExt;
		filter += (TCHAR)'\0';  // next string please
		ofn.nMaxCustFilter++;
	}
}

// Get the best document template for the named file

class CNewTypeDlg : public CDialog
{
protected:
	CPtrList*   m_pList;        // actually a list of doc templates
public:
	CDocTemplate*   m_pSelectedTemplate;

public:
	//{{AFX_DATA(CNewTypeDlg)
	enum { IDD = AFX_IDD_NEWTYPEDLG };
	//}}AFX_DATA
	CNewTypeDlg(CPtrList* pList) : CDialog(CNewTypeDlg::IDD)
	{
		m_pList = pList;
		m_pSelectedTemplate = NULL;
	}
	~CNewTypeDlg() { }

protected:
	BOOL OnInitDialog();
	void OnOK();
	//{{AFX_MSG(CNewTypeDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

BEGIN_MESSAGE_MAP(CNewTypeDlg, CDialog)
	//{{AFX_MSG_MAP(CNewTypeDlg)
	ON_LBN_DBLCLK(AFX_IDC_LISTBOX, OnOK)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CNewTypeDlg::OnInitDialog()
{
	CListBox* pListBox = (CListBox*)GetDlgItem(AFX_IDC_LISTBOX);
	ASSERT(pListBox != NULL);

	// fill with document templates in list
	pListBox->ResetContent();

	POSITION pos = m_pList->GetHeadPosition();
	// add all the CDocTemplates in the list by name
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_pList->GetNext(pos);
		ASSERT_KINDOF(CDocTemplate, pTemplate);

		CString strTypeName;
		if (pTemplate->GetDocString(strTypeName, CDocTemplate::fileNewName) &&
		   !strTypeName.IsEmpty())
		{
			// add it to the listbox
			int nIndex = pListBox->AddString(strTypeName);
			if (nIndex == -1)
			{
				EndDialog(-1);
				return FALSE;
			}
			pListBox->SetItemDataPtr(nIndex, pTemplate);
		}
	}

	int nTemplates = pListBox->GetCount();
	if (nTemplates == 0)
	{
		TRACE0("Error: no document templates to select from!\n");
		EndDialog(-1); // abort
	}
	else if (nTemplates == 1)
	{
		// get the first/only item
		m_pSelectedTemplate = (CDocTemplate*)pListBox->GetItemDataPtr(0);
		ASSERT_VALID(m_pSelectedTemplate);
		ASSERT_KINDOF(CDocTemplate, m_pSelectedTemplate);
		EndDialog(IDOK);    // done
	}
	else
	{
		// set selection to the first one (NOT SORTED)
		pListBox->SetCurSel(0);
	}

	return CDialog::OnInitDialog();
}

void CNewTypeDlg::OnOK()
{
	CListBox* pListBox = (CListBox*)GetDlgItem(AFX_IDC_LISTBOX);
	ASSERT(pListBox != NULL);
	// if listbox has selection, set the selected template
	int nIndex;
	if ((nIndex = pListBox->GetCurSel()) == -1)
	{
		// no selection
		m_pSelectedTemplate = NULL;
	}
	else
	{
		m_pSelectedTemplate = (CDocTemplate*)pListBox->GetItemDataPtr(nIndex);
		ASSERT_VALID(m_pSelectedTemplate);
		ASSERT_KINDOF(CDocTemplate, m_pSelectedTemplate);
	}
	CDialog::OnOK();
}

/////////////////////////////////////////////////////////////////////////////
// CDocManager

void CDocManager::AddDocTemplate(CDocTemplate* pTemplate)
{
	if (pTemplate == NULL)
	{
		if (pStaticList != NULL)
		{
			POSITION pos = pStaticList->GetHeadPosition();
			while (pos != NULL)
			{
				CDocTemplate* pTemplate =
					(CDocTemplate*)pStaticList->GetNext(pos);
				AddDocTemplate(pTemplate);
			}
			delete pStaticList;
			pStaticList = NULL;
		}
		bStaticInit = FALSE;
	}
	else
	{
		ASSERT_VALID(pTemplate);
		ASSERT(m_templateList.Find(pTemplate, NULL) == NULL);// must not be in list
		pTemplate->LoadTemplate();
		m_templateList.AddTail(pTemplate);
	}
}

POSITION CDocManager::GetFirstDocTemplatePosition() const
{
	return m_templateList.GetHeadPosition();
}

CDocTemplate* CDocManager::GetNextDocTemplate(POSITION& pos) const
{
	return (CDocTemplate*)m_templateList.GetNext(pos);
}

BOOL CDocManager::SaveAllModified()
{
	POSITION pos = m_templateList.GetHeadPosition();
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		ASSERT_KINDOF(CDocTemplate, pTemplate);
		if (!pTemplate->SaveAllModified())
			return FALSE;
	}
	return TRUE;
}

void CDocManager::CloseAllDocuments(BOOL bEndSession)
{
	POSITION pos = m_templateList.GetHeadPosition();
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		ASSERT_KINDOF(CDocTemplate, pTemplate);
		pTemplate->CloseAllDocuments(bEndSession);
	}
}

BOOL CDocManager::DoPromptFileName(CString& fileName, UINT nIDSTitle, DWORD lFlags, BOOL bOpenFileDialog, CDocTemplate* pTemplate)
{
	CFileDialog dlgFile(bOpenFileDialog);

	CString title;
	VERIFY(title.LoadString(nIDSTitle));

	dlgFile.m_ofn.Flags |= lFlags;

	CString strFilter;
	CString strDefault;
	if (pTemplate != NULL)
	{
		ASSERT_VALID(pTemplate);
		_AfxAppendFilterSuffix(strFilter, dlgFile.m_ofn, pTemplate, &strDefault);
	}
	else
	{
		// do for all doc template
		POSITION pos = m_templateList.GetHeadPosition();
		BOOL bFirst = TRUE;
		while (pos != NULL)
		{
			CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
			_AfxAppendFilterSuffix(strFilter, dlgFile.m_ofn, pTemplate,
				bFirst ? &strDefault : NULL);
			bFirst = FALSE;
		}
	}

	// append the "*.*" all files filter
	CString allFilter;
	VERIFY(allFilter.LoadString(AFX_IDS_ALLFILTER));
	strFilter += allFilter;
	strFilter += (TCHAR)'\0';   // next string please
	strFilter += _T("*.*");
	strFilter += (TCHAR)'\0';   // last string
	dlgFile.m_ofn.nMaxCustFilter++;

	dlgFile.m_ofn.lpstrFilter = strFilter;
	dlgFile.m_ofn.lpstrTitle = title;
	dlgFile.m_ofn.lpstrFile = fileName.GetBuffer(_MAX_PATH);

	int nResult = dlgFile.DoModal();
	fileName.ReleaseBuffer();
	return nResult == IDOK;
}

int CDocManager::GetDocumentCount()
{
	// count all documents
	int nCount = 0;
	POSITION pos = m_templateList.GetHeadPosition();
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		POSITION pos2 = pTemplate->GetFirstDocPosition();
		while (pos2 != NULL)
		{
			pTemplate->GetNextDoc(pos2);
			++nCount;
		}
	}
	return nCount;
}

BOOL CDocManager::OnDDECommand(LPTSTR lpszCommand)
{
	CString strCommand = lpszCommand;
	CDocument* pDoc = NULL;

	// open format is "[open("%s")]" - no whitespace allowed, one per line
	// print format is "[print("%s")]" - no whitespace allowed, one per line
	// print to format is "[printto("%s","%s","%s","%s")]" - no whitespace allowed, one per line
	CCommandLineInfo cmdInfo;
	cmdInfo.m_nShellCommand = CCommandLineInfo::FileDDE;

	if (strCommand.Left(7) == _T("[open(\""))
	{
		cmdInfo.m_nShellCommand = CCommandLineInfo::FileOpen;
		strCommand = strCommand.Right(strCommand.GetLength() - 7);
	}
	else if (strCommand.Left(8) == _T("[print(\""))
	{
		cmdInfo.m_nShellCommand = CCommandLineInfo::FilePrint;
		strCommand = strCommand.Right(strCommand.GetLength() - 8);
	}
	else if (strCommand.Left(10) == _T("[printto(\""))
	{
		cmdInfo.m_nShellCommand = CCommandLineInfo::FilePrintTo;\
		strCommand = strCommand.Right(strCommand.GetLength() - 10);
	}
	else
		return FALSE; // not a command we handle

	int i = strCommand.Find('"');
	if (i == -1)
		return FALSE; // illegally terminated

	cmdInfo.m_strFileName = strCommand.Left(i);
	strCommand = strCommand.Right(strCommand.GetLength() - i);

	CCommandLineInfo* pOldInfo = NULL;
	BOOL bRetVal = TRUE;

	// If we were started up for DDE retrieve the Show state
	if (AfxGetApp()->m_pCmdInfo != NULL)
	{
		AfxGetApp()->m_nCmdShow = (int)AfxGetApp()->m_pCmdInfo;
		AfxGetApp()->m_pCmdInfo = &cmdInfo;
	}
	else
		pOldInfo = AfxGetApp()->m_pCmdInfo;

	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FileOpen)
	{
		// show the application window
		CWnd* pMainWnd = AfxGetApp()->m_pMainWnd;
		int nCmdShow = AfxGetApp()->m_nCmdShow;
		if (nCmdShow == -1 || nCmdShow == SW_SHOWNORMAL)
		{
			if (pMainWnd->IsIconic())
				nCmdShow = SW_RESTORE;
			else
				nCmdShow = SW_SHOW;
		}
		pMainWnd->ShowWindow(nCmdShow);
		if (nCmdShow != SW_MINIMIZE)
			pMainWnd->SetForegroundWindow();

		// then open the document
		AfxGetApp()->OpenDocumentFile(cmdInfo.m_strFileName);

		// user is now "in control" of the application
		if (!AfxOleGetUserCtrl())
			AfxOleSetUserCtrl(TRUE);

		// next time, show the window as default
		AfxGetApp()->m_nCmdShow = -1;
		goto RestoreAndReturn;
	}

	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FilePrintTo)
	{
		if (strCommand.Left(3) != _T("\",\""))
		{
			bRetVal = FALSE;
			goto RestoreAndReturn;
		}
		else
		{
			strCommand = strCommand.Right(strCommand.GetLength() - 3);
			i = strCommand.Find('"');
			if (i == -1)
			{
				bRetVal = FALSE;
				goto RestoreAndReturn;
			}
			else
			{
				cmdInfo.m_strPrinterName = strCommand.Left(i);
				strCommand = strCommand.Right(strCommand.GetLength() - i);
			}
		}

		if (strCommand.Left(3) != _T("\",\""))
		{
			bRetVal = FALSE;
			goto RestoreAndReturn;
		}
		else
		{
			strCommand = strCommand.Right(strCommand.GetLength() - 3);
			i = strCommand.Find('"');
			if (i == -1)
			{
				bRetVal = FALSE;
				goto RestoreAndReturn;
			}
			else
			{
				cmdInfo.m_strDriverName = strCommand.Left(i);
				strCommand = strCommand.Right(strCommand.GetLength() - i);
			}
		}

		if (strCommand.Left(3) != _T("\",\""))
		{
			bRetVal = FALSE;
			goto RestoreAndReturn;
		}
		else
		{
			strCommand = strCommand.Right(strCommand.GetLength() - 3);
			i = strCommand.Find('"');
			if (i == -1)
			{
				bRetVal = FALSE;
				goto RestoreAndReturn;
			}
			else
			{
				cmdInfo.m_strPortName = strCommand.Left(i);
				strCommand = strCommand.Right(strCommand.GetLength() - i);
			}
		}
	}

	// get document count before opening it
	int nOldCount; nOldCount = GetDocumentCount();

	// open the document, then print it.
	pDoc = AfxGetApp()->OpenDocumentFile(cmdInfo.m_strFileName);
	AfxGetApp()->m_pCmdInfo = &cmdInfo;
	AfxGetApp()->m_pMainWnd->SendMessage(WM_COMMAND, ID_FILE_PRINT_DIRECT);
	AfxGetApp()->m_pCmdInfo = NULL;

	// close the document if it wasn't open previously (based on doc count)
	if (GetDocumentCount() > nOldCount)
		pDoc->OnCloseDocument();

	 // if the app was only started to process this command then close
	 if (!AfxOleGetUserCtrl())
		AfxGetApp()->m_pMainWnd->PostMessage(WM_CLOSE);

RestoreAndReturn:
	AfxGetApp()->m_pCmdInfo = pOldInfo;
	return bRetVal;
}

void CDocManager::OnFileNew()
{
	if (m_templateList.IsEmpty())
	{
		TRACE0("Error: no document templates registered with CWinApp.\n");
		AfxMessageBox(AFX_IDP_FAILED_TO_CREATE_DOC);
		return;
	}

	CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetHead();
	if (m_templateList.GetCount() > 1)
	{
		// more than one document template to choose from
		// bring up dialog prompting user
		CNewTypeDlg dlg(&m_templateList);
		int nID = dlg.DoModal();
		if (nID == IDOK)
			pTemplate = dlg.m_pSelectedTemplate;
		else
			return;     // none - cancel operation
	}

	ASSERT(pTemplate != NULL);
	ASSERT_KINDOF(CDocTemplate, pTemplate);

	pTemplate->OpenDocumentFile(NULL);
		// if returns NULL, the user has already been alerted
}

void CDocManager::OnFileOpen()
{
	// prompt the user (with all document templates)
	CString newName;
	if (!DoPromptFileName(newName, AFX_IDS_OPENFILE,
	  OFN_HIDEREADONLY | OFN_FILEMUSTEXIST, TRUE, NULL))
		return; // open cancelled

	AfxGetApp()->OpenDocumentFile(newName);
		// if returns NULL, the user has already been alerted
}

#ifdef _DEBUG
void CDocManager::AssertValid() const
{
	CObject::AssertValid();

	POSITION pos = m_templateList.GetHeadPosition();
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		ASSERT_VALID(pTemplate);
	}
}

void CDocManager::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	if (dc.GetDepth() != 0)
	{
		dc << "\nm_templateList[] = {";
		POSITION pos = m_templateList.GetHeadPosition();
		while (pos != NULL)
		{
			CDocTemplate* pTemplate =
				(CDocTemplate*)m_templateList.GetNext(pos);
			dc << "\ntemplate " << pTemplate;
		}
		dc << "}";
	}

	dc << "\n";
}
#endif

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

CDocument* CDocManager::OpenDocumentFile(LPCTSTR lpszFileName)
{
	// find the highest confidence
	POSITION pos = m_templateList.GetHeadPosition();
	CDocTemplate::Confidence bestMatch = CDocTemplate::noAttempt;
	CDocTemplate* pBestTemplate = NULL;
	CDocument* pOpenDocument = NULL;

	TCHAR szPath[_MAX_PATH];
	ASSERT(lstrlen(lpszFileName) < _countof(szPath));
	TCHAR szTemp[_MAX_PATH];
	if (lpszFileName[0] == '\"')
		++lpszFileName;
	lstrcpyn(szTemp, lpszFileName, _MAX_PATH);
	LPTSTR lpszLast = _tcsrchr(szTemp, '\"');
	if (lpszLast != NULL)
		*lpszLast = 0;
	AfxFullPath(szPath, szTemp);
	TCHAR szLinkName[_MAX_PATH];
	if (AfxResolveShortcut(AfxGetMainWnd(), szPath, szLinkName, _MAX_PATH))
		lstrcpy(szPath, szLinkName);

	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		ASSERT_KINDOF(CDocTemplate, pTemplate);

		CDocTemplate::Confidence match;
		ASSERT(pOpenDocument == NULL);
		match = pTemplate->MatchDocType(szPath, pOpenDocument);
		if (match > bestMatch)
		{
			bestMatch = match;
			pBestTemplate = pTemplate;
		}
		if (match == CDocTemplate::yesAlreadyOpen)
			break;      // stop here
	}

	if (pOpenDocument != NULL)
	{
		POSITION pos = pOpenDocument->GetFirstViewPosition();
		if (pos != NULL)
		{
			CView* pView = pOpenDocument->GetNextView(pos); // get first one
			ASSERT_VALID(pView);
			CFrameWnd* pFrame = pView->GetParentFrame();
			if (pFrame != NULL)
				pFrame->ActivateFrame();
			else
				TRACE0("Error: Can not find a frame for document to activate.\n");
			CFrameWnd* pAppFrame;
			if (pFrame != (pAppFrame = (CFrameWnd*)AfxGetApp()->m_pMainWnd))
			{
				ASSERT_KINDOF(CFrameWnd, pAppFrame);
				pAppFrame->ActivateFrame();
			}
		}
		else
		{
			TRACE0("Error: Can not find a view for document to activate.\n");
		}
		return pOpenDocument;
	}

	if (pBestTemplate == NULL)
	{
		AfxMessageBox(AFX_IDP_FAILED_TO_OPEN_DOC);
		return NULL;
	}

	return pBestTemplate->OpenDocumentFile(szPath);
}

int CDocManager::GetOpenDocumentCount()
{
	int nOpen = 0;
	POSITION pos = m_templateList.GetHeadPosition();
	while (pos != NULL)
	{
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		POSITION pos2 = pTemplate->GetFirstDocPosition();
		while (pos2)
		{
			if (pTemplate->GetNextDoc(pos2) != NULL)
				nOpen++;
		}
	}
	return nOpen;
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

CDocManager::~CDocManager()
{
	// for cleanup - delete all document templates
	POSITION pos = m_templateList.GetHeadPosition();
	while (pos != NULL)
	{
		POSITION posTemplate = pos;
		CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
		if (pTemplate->m_bAutoDelete)
		{
			m_templateList.RemoveAt(posTemplate);
			delete (CDocTemplate*)pTemplate;
		}
	}
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDocManager, CObject)

/////////////////////////////////////////////////////////////////////////////
