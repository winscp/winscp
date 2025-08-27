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

/////////////////////////////////////////////////////////////////////////////
// CTime - absolute time

CTime::CTime(int nYear, int nMonth, int nDay, int nHour, int nMin, int nSec,
	int nDST)
{
	struct tm atm;
	atm.tm_sec = nSec;
	atm.tm_min = nMin;
	atm.tm_hour = nHour;
	ASSERT(nDay >= 1 && nDay <= 31);
	atm.tm_mday = nDay;
	ASSERT(nMonth >= 1 && nMonth <= 12);
	atm.tm_mon = nMonth - 1;        // tm_mon is 0 based
	ASSERT(nYear >= 1900);
	atm.tm_year = nYear - 1900;     // tm_year is 1900 based
	atm.tm_isdst = nDST;
	m_time = mktime(&atm);
	ASSERT(m_time != -1);       // indicates an illegal input time
}

CTime::CTime(const SYSTEMTIME& sysTime, int nDST)
{
	if (sysTime.wYear < 1900)
	{
		time_t time0 = 0L;
		CTime timeT(time0);
		*this = timeT;
	}
	else
	{
		CTime timeT(
			(int)sysTime.wYear, (int)sysTime.wMonth, (int)sysTime.wDay,
			(int)sysTime.wHour, (int)sysTime.wMinute, (int)sysTime.wSecond,
			nDST);
		*this = timeT;
	}
}

CTime::CTime(const FILETIME& fileTime, int nDST)
{
	// first convert file time (UTC time) to local time
	FILETIME localTime;
	if (!FileTimeToLocalFileTime(&fileTime, &localTime))
	{
		m_time = 0;
		return;
	}

	// then convert that time to system time
	SYSTEMTIME sysTime;
	if (!FileTimeToSystemTime(&localTime, &sysTime))
	{
		m_time = 0;
		return;
	}

	// then convert the system time to a time_t (C-runtime local time)
	CTime timeT(sysTime, nDST);
	*this = timeT;
}

CTime CTime::CreateForCurrentTime()
// return the current system time
{
	return CTime(::time(NULL));
}

struct tm* CTime::GetLocalTm(struct tm* ptm) const
{
	if (ptm != NULL)
	{
		struct tm* ptmTemp = localtime(&m_time);
		if (ptmTemp == NULL)
			return NULL;    // indicates the m_time was not initialized!

		*ptm = *ptmTemp;
		return ptm;
	}
	else
		return localtime(&m_time);
}

/////////////////////////////////////////////////////////////////////////////
