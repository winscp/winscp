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
#include <SysUtils.hpp>
#include <StrUtils.hpp>

/////////////////////////////////////////////////////////////////////////////
// CFileException

void CFileException::ThrowOsError(LONG lOsError,
	LPCTSTR lpszFileName /* = NULL */)
{
	if (lOsError != 0)
		AfxThrowFileException(CFileException::OsErrorToException(lOsError),
			lOsError, lpszFileName);
}

UnicodeString CFileException::GetErrorMessage()
{
	UnicodeString strFileName = m_strFileName;
	if (strFileName.IsEmpty())
		strFileName = LoadStr(AFX_IDS_UNNAMED_FILE);
	UnicodeString strMessage = LoadStr(m_cause + AFX_IDP_FILE_NONE);
	strMessage = ReplaceStr(strMessage, L"%1", strFileName.c_str());
	return strMessage;
}

/////////////////////////////////////////////////////////////////////////////
// CFileException helpers

void AfxThrowFileException(int cause, LONG lOsError,
	LPCTSTR lpszFileName /* == NULL */)
{
	throw new CFileException(cause, lOsError, lpszFileName);
}

int CFileException::OsErrorToException(LONG lOsErr)
{
	// NT Error codes
	switch ((UINT)lOsErr)
	{
	case NO_ERROR:
		return CFileException::none;
	case ERROR_FILE_NOT_FOUND:
		return CFileException::fileNotFound;
	case ERROR_PATH_NOT_FOUND:
		return CFileException::badPath;
	case ERROR_TOO_MANY_OPEN_FILES:
		return CFileException::tooManyOpenFiles;
	case ERROR_ACCESS_DENIED:
		return CFileException::accessDenied;
	case ERROR_INVALID_HANDLE:
		return CFileException::fileNotFound;
	case ERROR_BAD_FORMAT:
		return CFileException::invalidFile;
	case ERROR_INVALID_ACCESS:
		return CFileException::accessDenied;
	case ERROR_INVALID_DRIVE:
		return CFileException::badPath;
	case ERROR_CURRENT_DIRECTORY:
		return CFileException::removeCurrentDir;
	case ERROR_NOT_SAME_DEVICE:
		return CFileException::badPath;
	case ERROR_NO_MORE_FILES:
		return CFileException::fileNotFound;
	case ERROR_WRITE_PROTECT:
		return CFileException::accessDenied;
	case ERROR_BAD_UNIT:
		return CFileException::hardIO;
	case ERROR_NOT_READY:
		return CFileException::hardIO;
	case ERROR_BAD_COMMAND:
		return CFileException::hardIO;
	case ERROR_CRC:
		return CFileException::hardIO;
	case ERROR_BAD_LENGTH:
		return CFileException::badSeek;
	case ERROR_SEEK:
		return CFileException::badSeek;
	case ERROR_NOT_DOS_DISK:
		return CFileException::invalidFile;
	case ERROR_SECTOR_NOT_FOUND:
		return CFileException::badSeek;
	case ERROR_WRITE_FAULT:
		return CFileException::accessDenied;
	case ERROR_READ_FAULT:
		return CFileException::badSeek;
	case ERROR_SHARING_VIOLATION:
		return CFileException::sharingViolation;
	case ERROR_LOCK_VIOLATION:
		return CFileException::lockViolation;
	case ERROR_WRONG_DISK:
		return CFileException::badPath;
	case ERROR_SHARING_BUFFER_EXCEEDED:
		return CFileException::tooManyOpenFiles;
	case ERROR_HANDLE_EOF:
		return CFileException::endOfFile;
	case ERROR_HANDLE_DISK_FULL:
		return CFileException::diskFull;
	case ERROR_DUP_NAME:
		return CFileException::badPath;
	case ERROR_BAD_NETPATH:
		return CFileException::badPath;
	case ERROR_NETWORK_BUSY:
		return CFileException::accessDenied;
	case ERROR_DEV_NOT_EXIST:
		return CFileException::badPath;
	case ERROR_ADAP_HDW_ERR:
		return CFileException::hardIO;
	case ERROR_BAD_NET_RESP:
		return CFileException::accessDenied;
	case ERROR_UNEXP_NET_ERR:
		return CFileException::hardIO;
	case ERROR_BAD_REM_ADAP:
		return CFileException::invalidFile;
	case ERROR_NO_SPOOL_SPACE:
		return CFileException::directoryFull;
	case ERROR_NETNAME_DELETED:
		return CFileException::accessDenied;
	case ERROR_NETWORK_ACCESS_DENIED:
		return CFileException::accessDenied;
	case ERROR_BAD_DEV_TYPE:
		return CFileException::invalidFile;
	case ERROR_BAD_NET_NAME:
		return CFileException::badPath;
	case ERROR_TOO_MANY_NAMES:
		return CFileException::tooManyOpenFiles;
	case ERROR_SHARING_PAUSED:
		return CFileException::badPath;
	case ERROR_REQ_NOT_ACCEP:
		return CFileException::accessDenied;
	case ERROR_FILE_EXISTS:
		return CFileException::accessDenied;
	case ERROR_CANNOT_MAKE:
		return CFileException::accessDenied;
	case ERROR_ALREADY_ASSIGNED:
		return CFileException::badPath;
	case ERROR_INVALID_PASSWORD:
		return CFileException::accessDenied;
	case ERROR_NET_WRITE_FAULT:
		return CFileException::hardIO;
	case ERROR_DISK_CHANGE:
		return CFileException::fileNotFound;
	case ERROR_DRIVE_LOCKED:
		return CFileException::lockViolation;
	case ERROR_BUFFER_OVERFLOW:
		return CFileException::badPath;
	case ERROR_DISK_FULL:
		return CFileException::diskFull;
	case ERROR_NO_MORE_SEARCH_HANDLES:
		return CFileException::tooManyOpenFiles;
	case ERROR_INVALID_TARGET_HANDLE:
		return CFileException::invalidFile;
	case ERROR_INVALID_CATEGORY:
		return CFileException::hardIO;
	case ERROR_INVALID_NAME:
		return CFileException::badPath;
	case ERROR_INVALID_LEVEL:
		return CFileException::badPath;
	case ERROR_NO_VOLUME_LABEL:
		return CFileException::badPath;
	case ERROR_NEGATIVE_SEEK:
		return CFileException::badSeek;
	case ERROR_SEEK_ON_DEVICE:
		return CFileException::badSeek;
	case ERROR_DIR_NOT_ROOT:
		return CFileException::badPath;
	case ERROR_DIR_NOT_EMPTY:
		return CFileException::removeCurrentDir;
	case ERROR_LABEL_TOO_LONG:
		return CFileException::badPath;
	case ERROR_BAD_PATHNAME:
		return CFileException::badPath;
	case ERROR_LOCK_FAILED:
		return CFileException::lockViolation;
	case ERROR_BUSY:
		return CFileException::accessDenied;
	case ERROR_INVALID_ORDINAL:
		return CFileException::invalidFile;
	case ERROR_ALREADY_EXISTS:
		return CFileException::accessDenied;
	case ERROR_INVALID_EXE_SIGNATURE:
		return CFileException::invalidFile;
	case ERROR_BAD_EXE_FORMAT:
		return CFileException::invalidFile;
	case ERROR_FILENAME_EXCED_RANGE:
		return CFileException::badPath;
	case ERROR_META_EXPANSION_TOO_LONG:
		return CFileException::badPath;
	case ERROR_DIRECTORY:
		return CFileException::badPath;
	case ERROR_OPERATION_ABORTED:
		return CFileException::hardIO;
	case ERROR_IO_INCOMPLETE:
		return CFileException::hardIO;
	case ERROR_IO_PENDING:
		return CFileException::hardIO;
	case ERROR_SWAPERROR:
		return CFileException::accessDenied;
	default:
		return CFileException::generic;
	}
}

/////////////////////////////////////////////////////////////////////////////
