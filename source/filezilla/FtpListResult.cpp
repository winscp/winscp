//---------------------------------------------------------------------------
#include "stdafx.h"
#include "FtpListResult.h"
#include "FileZillaApi.h"
#include <WideStrUtils.hpp>

CFtpListResult::CFtpListResult(t_server server, bool mlst, bool *bUTF8, bool vmsAllRevisions, bool debugShowListing)
{
  m_mlst = mlst;
  m_server = server;
  m_bUTF8 = bUTF8;
  m_vmsAllRevisions = vmsAllRevisions;
  m_debugShowListing = debugShowListing;

  //Fill the month names map

  //English month names
  m_MonthNamesMap[L"jan"] = 1;
  m_MonthNamesMap[L"feb"] = 2;
  m_MonthNamesMap[L"mar"] = 3;
  m_MonthNamesMap[L"apr"] = 4;
  m_MonthNamesMap[L"may"] = 5;
  m_MonthNamesMap[L"jun"] = 6;
  m_MonthNamesMap[L"june"] = 6;
  m_MonthNamesMap[L"jul"] = 7;
  m_MonthNamesMap[L"july"] = 7;
  m_MonthNamesMap[L"aug"] = 8;
  m_MonthNamesMap[L"sep"] = 9;
  m_MonthNamesMap[L"sept"] = 9;
  m_MonthNamesMap[L"oct"] = 10;
  m_MonthNamesMap[L"nov"] = 11;
  m_MonthNamesMap[L"dec"] = 12;

  //Numerical values for the month
  m_MonthNamesMap[L"1"] = 1;
  m_MonthNamesMap[L"01"] = 1;
  m_MonthNamesMap[L"2"] = 2;
  m_MonthNamesMap[L"02"] = 2;
  m_MonthNamesMap[L"3"] = 3;
  m_MonthNamesMap[L"03"] = 3;
  m_MonthNamesMap[L"4"] = 4;
  m_MonthNamesMap[L"04"] = 4;
  m_MonthNamesMap[L"5"] = 5;
  m_MonthNamesMap[L"05"] = 5;
  m_MonthNamesMap[L"6"] = 6;
  m_MonthNamesMap[L"06"] = 6;
  m_MonthNamesMap[L"7"] = 7;
  m_MonthNamesMap[L"07"] = 7;
  m_MonthNamesMap[L"8"] = 8;
  m_MonthNamesMap[L"08"] = 8;
  m_MonthNamesMap[L"9"] = 9;
  m_MonthNamesMap[L"09"] = 9;
  m_MonthNamesMap[L"10"] = 10;
  m_MonthNamesMap[L"11"] = 11;
  m_MonthNamesMap[L"12"] = 12;

  //German month names
  m_MonthNamesMap[L"mrz"] = 3;
  m_MonthNamesMap[L"m\xE4r"] = 3;
  m_MonthNamesMap[L"m\xE4rz"] = 3;
  m_MonthNamesMap[L"mai"] = 5;
  m_MonthNamesMap[L"juni"] = 6;
  m_MonthNamesMap[L"m\xC3\xA4r"] = 3; // UTF-8
  m_MonthNamesMap[L"m\xC3\xA4rz"] = 3; // UTF-8
  m_MonthNamesMap[L"juli"] = 7;
  m_MonthNamesMap[L"okt"] = 10;
  m_MonthNamesMap[L"dez"] = 12;

  //Austrian month names
  m_MonthNamesMap[L"j\xE4n"] = 1;
  m_MonthNamesMap[L"j\xC3\xA4n"] = 1; // UTF-8

  //French month names
  m_MonthNamesMap[L"janv"] = 1;
  m_MonthNamesMap[L"f\xE9" L"b"] = 1;
  m_MonthNamesMap[L"f\xE9v"] = 2;
  m_MonthNamesMap[L"fev"] = 2;
  m_MonthNamesMap[L"f\xE9vr"] = 2;
  m_MonthNamesMap[L"fevr"] = 2;
  m_MonthNamesMap[L"mars"] = 3;
  m_MonthNamesMap[L"mrs"] = 3;
  m_MonthNamesMap[L"avr"] = 4;
  m_MonthNamesMap[L"juin"] = 6;
  m_MonthNamesMap[L"juil"] = 7;
  m_MonthNamesMap[L"jui"] = 7;
  m_MonthNamesMap[L"ao\xFB"] = 8;
  m_MonthNamesMap[L"ao\xFBt"] = 8;
  m_MonthNamesMap[L"aout"] = 8;
  m_MonthNamesMap[L"d\xE9" L"c"] = 12;
  m_MonthNamesMap[L"dec"] = 12;

  //Italian month names
  m_MonthNamesMap[L"gen"] = 1;
  m_MonthNamesMap[L"mag"] = 5;
  m_MonthNamesMap[L"giu"] = 6;
  m_MonthNamesMap[L"lug"] = 7;
  m_MonthNamesMap[L"ago"] = 8;
  m_MonthNamesMap[L"set"] = 9;
  m_MonthNamesMap[L"ott"] = 10;
  m_MonthNamesMap[L"dic"] = 12;

  //Spanish month names
  m_MonthNamesMap[L"ene"] = 1;
  m_MonthNamesMap[L"fbro"] = 2;
  m_MonthNamesMap[L"mzo"] = 3;
  m_MonthNamesMap[L"ab"] = 4;
  m_MonthNamesMap[L"abr"] = 4;
  m_MonthNamesMap[L"agto"] = 8;
  m_MonthNamesMap[L"sbre"] = 9;
  m_MonthNamesMap[L"obre"] = 9;
  m_MonthNamesMap[L"nbre"] = 9;
  m_MonthNamesMap[L"dbre"] = 9;

  //Polish month names
  m_MonthNamesMap[L"sty"] = 1;
  m_MonthNamesMap[L"lut"] = 2;
  m_MonthNamesMap[L"kwi"] = 4;
  m_MonthNamesMap[L"maj"] = 5;
  m_MonthNamesMap[L"cze"] = 6;
  m_MonthNamesMap[L"lip"] = 7;
  m_MonthNamesMap[L"sie"] = 8;
  m_MonthNamesMap[L"wrz"] = 9;
  m_MonthNamesMap[L"pa"] = 10;
  m_MonthNamesMap[L"lis"] = 11;
  m_MonthNamesMap[L"gru"] = 12;

  //Russian month names
  m_MonthNamesMap[L"\xFF\xED\xE2"] = 1;
  m_MonthNamesMap[L"\xF4\xE5\xE2"] = 2;
  m_MonthNamesMap[L"\xEC\xE0\xF0"] = 3;
  m_MonthNamesMap[L"\xE0\xEF\xF0"] = 4;
  m_MonthNamesMap[L"\xEC\xE0\xE9"] = 5;
  m_MonthNamesMap[L"\xE8\xFE\xED"] = 6;
  m_MonthNamesMap[L"\xE8\xFE\xEB"] = 7;
  m_MonthNamesMap[L"\xE0\xE2\xE3"] = 8;
  m_MonthNamesMap[L"\xF1\xE5\xED"] = 9;
  m_MonthNamesMap[L"\xEE\xEA\xF2"] = 10;
  m_MonthNamesMap[L"\xED\xEE\xFF"] = 11;
  m_MonthNamesMap[L"\xE4\xE5\xEA"] = 12;


  //Dutch month names
  m_MonthNamesMap[L"mrt"] = 3;
  m_MonthNamesMap[L"mei"] = 5;

  //Portuguese month names
  m_MonthNamesMap[L"out"] = 10;

  //Japanese month names
  m_MonthNamesMap[L"1\x8c\x8e"] = 1;
  m_MonthNamesMap[L"2\x8c\x8e"] = 2;
  m_MonthNamesMap[L"3\x8c\x8e"] = 3;
  m_MonthNamesMap[L"4\x8c\x8e"] = 4;
  m_MonthNamesMap[L"5\x8c\x8e"] = 5;
  m_MonthNamesMap[L"6\x8c\x8e"] = 6;
  m_MonthNamesMap[L"7\x8c\x8e"] = 7;
  m_MonthNamesMap[L"8\x8c\x8e"] = 8;
  m_MonthNamesMap[L"9\x8c\x8e"] = 9;
  m_MonthNamesMap[L"10\x8c\x8e"] = 10;
  m_MonthNamesMap[L"11\x8c\x8e"] = 11;
  m_MonthNamesMap[L"12\x8c\x8e"] = 12;

  //Korean (Unicode) month names
  m_MonthNamesMap[L"1\xC6\xD4"] = 1;
  m_MonthNamesMap[L"2\xC6\xD4"] = 2;
  m_MonthNamesMap[L"3\xC6\xD4"] = 3;
  m_MonthNamesMap[L"4\xC6\xD4"] = 4;
  m_MonthNamesMap[L"5\xC6\xD4"] = 5;
  m_MonthNamesMap[L"6\xC6\xD4"] = 6;
  m_MonthNamesMap[L"7\xC6\xD4"] = 7;
  m_MonthNamesMap[L"8\xC6\xD4"] = 8;
  m_MonthNamesMap[L"9\xC6\xD4"] = 9;
  m_MonthNamesMap[L"10\xC6\xD4"] = 10;
  m_MonthNamesMap[L"11\xC6\xD4"] = 11;
  m_MonthNamesMap[L"12\xC6\xD4"] = 12;

  //Korean (EUC-KR) month names
  m_MonthNamesMap[L"1\xBF\xF9"] = 1;
  m_MonthNamesMap[L"2\xBF\xF9"] = 2;
  m_MonthNamesMap[L"3\xBF\xF9"] = 3;
  m_MonthNamesMap[L"4\xBF\xF9"] = 4;
  m_MonthNamesMap[L"5\xBF\xF9"] = 5;
  m_MonthNamesMap[L"6\xBF\xF9"] = 6;
  m_MonthNamesMap[L"7\xBF\xF9"] = 7;
  m_MonthNamesMap[L"8\xBF\xF9"] = 8;
  m_MonthNamesMap[L"9\xBF\xF9"] = 9;
  m_MonthNamesMap[L"10\xBF\xF9"] = 10;
  m_MonthNamesMap[L"11\xBF\xF9"] = 11;
  m_MonthNamesMap[L"12\xBF\xF9"] = 12;

  //Finnish month names
  m_MonthNamesMap[L"tammi"] = 1;
  m_MonthNamesMap[L"helmi"] = 2;
  m_MonthNamesMap[L"maalis"] = 3;
  m_MonthNamesMap[L"huhti"] = 4;
  m_MonthNamesMap[L"touko"] = 5;
  m_MonthNamesMap[L"kes\xE4"] = 6;
  m_MonthNamesMap[L"hein\xE4"] = 7;
  m_MonthNamesMap[L"elo"] = 8;
  m_MonthNamesMap[L"syys"] = 9;
  m_MonthNamesMap[L"loka"] = 10;
  m_MonthNamesMap[L"marras"] = 11;
  m_MonthNamesMap[L"joulu"] = 12;

  //There are more languages and thus month
  //names, but as long nobody reports a
  //problem, I won't add them, there are way
  //too much languages

  //Some very strange combinations of names and numbers I've seen.
  //The developers of those ftp servers must have been dumb.
  m_MonthNamesMap[L"jan1"] = 1;
  m_MonthNamesMap[L"feb2"] = 2;
  m_MonthNamesMap[L"mar3"] = 3;
  m_MonthNamesMap[L"apr4"] = 4;
  m_MonthNamesMap[L"may5"] = 5;
  m_MonthNamesMap[L"jun6"] = 6;
  m_MonthNamesMap[L"jul7"] = 7;
  m_MonthNamesMap[L"aug8"] = 8;
  m_MonthNamesMap[L"sep9"] = 9;
  m_MonthNamesMap[L"sept9"] = 9;
  m_MonthNamesMap[L"oct0"] = 10;
  m_MonthNamesMap[L"nov1"] = 11;
  m_MonthNamesMap[L"dec2"] = 12;

  // Slovenian month names
  m_MonthNamesMap[L"avg"] = 8;
}

t_directory::t_direntry * CFtpListResult::getList(int & Num)
{
  if (!FBuffer.IsEmpty())
  {
    SendLineToMessageLog("Unparsed listing:");
    SendLineToMessageLog(FBuffer);
  }
  Num = m_EntryList.size();
  t_directory::t_direntry * Result;
  if (Num == 0)
  {
    SendLineToMessageLog("<Empty directory listing>");
    Result = NULL;
  }
  else
  {
    Result = new t_directory::t_direntry[Num];
    int I = 0;
    for (tEntryList::iterator Iter = m_EntryList.begin(); Iter != m_EntryList.end(); Iter++, I++)
    {
      Result[I] = *Iter;
    }
    m_EntryList.clear();
  }

  return Result;
}

BOOL CFtpListResult::parseLine(const char *lineToParse, const int linelen, t_directory::t_direntry &direntry)
{
  direntry.ownergroup = L"";
  direntry.owner = L"";
  direntry.group = L"";

  if (parseAsMlsd(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsUnix(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsDos(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsEPLF(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsVMS(lineToParse, linelen, direntry))
  {
    m_server.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
    return TRUE;
  }

  if (parseAsOther(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsIBMMVS(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsIBMMVSPDS(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsIBM(lineToParse, linelen, direntry))
    return TRUE;

  if (parseAsWfFtp(lineToParse, linelen, direntry))
    return TRUE;

  // Should be last
  if (parseAsIBMMVSPDS2(lineToParse, linelen, direntry))
    return TRUE;

  // name-only entries
  // (multiline VMS entries have only a name on the first line, so for VMS we have to skip this)
  if (FLAGCLEAR(m_server.nServerType, FZ_SERVERTYPE_SUB_FTP_VMS))
  {
    RawByteString Buf(lineToParse);
    // z/OS PDS members without ISPF statistics (name only) still have loads of spaces after them.
    Buf = Buf.TrimRight();
    if (Buf.Pos(' ') == 0)
    {
      direntry = t_directory::t_direntry();
      direntry.name = Buf.c_str();
      return TRUE;
    }
  }

  return FALSE;
}

bool CFtpListResult::IsNewLineChar(char C) const
{
  return
    (C == '\r') || (C == '\n') ||
    // Some of the parsing code cannot handle null characters, so if a malformed server sends some, treat is as a newline
    (C == '\0');
}

void CFtpListResult::AddData(const char * Data, int Size)
{
  FBuffer += RawByteString(Data, Size);

  // Just in case the previous buffer was terminated between CR and LF.
  while (!FBuffer.IsEmpty() && IsNewLineChar(FBuffer[1]))
  {
    FBuffer.Delete(1, 1);
  }

  bool Found;
  int Pos;
  int FirstLineEnd;
  int Count;
  bool Restart = true;

  do
  {
    if (Restart)
    {
      Pos = 1;
      FirstLineEnd = -1;
      Count = 0;
      Restart = false;
    }

    std::vector<RawByteString> Lines;
    while ((Pos <= FBuffer.Length()) && !IsNewLineChar(FBuffer[Pos]))
    {
      Pos++;
    }
    Found = (Pos <= FBuffer.Length());
    if (Found)
    {
      Count++;
      RawByteString Record = FBuffer.SubString(1, Pos - 1);
      while ((Pos <= FBuffer.Length()) && IsNewLineChar(FBuffer[Pos]))
      {
        Pos++;
      }
      if (FirstLineEnd < 0)
      {
        FirstLineEnd = Pos;
      }
      t_directory::t_direntry DirEntry;
      RawByteString Line = Record;
      for (int Index = 1; Index <= Line.Length(); Index++)
      {
        if (IsNewLineChar(Line[Index]))
        {
          Line[Index] = ' ';
        }
      }
      if (parseLine(Line.c_str(), Line.Length(), DirEntry))
      {
        if ((DirEntry.name != L".") && (DirEntry.name != L".."))
        {
          AddLine(DirEntry);
        }
        FBuffer.Delete(1, Pos - 1);
        Restart = true;
        SendLineToMessageLog(Record);
      }
      else
      {
        if (Count == 2)
        {
          RawByteString FirstLine = FBuffer.SubString(1, FirstLineEnd - 1).TrimRight();
          SendLineToMessageLog("Cannot parse line:");
          SendLineToMessageLog(FirstLine);
          FBuffer.Delete(1, FirstLineEnd - 1);
          Restart = true;
        }
      }
    }
  }
  while (Found);
}

void CFtpListResult::SendLineToMessageLog(const RawByteString & Line)
{
  if (m_debugShowListing)
  {
    t_ffam_statusmessage * Status = new t_ffam_statusmessage;
    Status->post = TRUE;
    Status->status = Line.c_str();
    Status->type = FZ_LOG_INFO;
    if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_STATUS, 0), (LPARAM)Status))
    {
      delete Status;
    }
  }
}

void CFtpListResult::AddLine(t_directory::t_direntry & direntry)
{
  if (m_server.nTimeZoneOffset &&
    direntry.date.hasdate && direntry.date.hastime && !direntry.date.utc)
  {
    SYSTEMTIME st = {0};
    st.wYear = direntry.date.year;
    st.wMonth = direntry.date.month;
    st.wDay = direntry.date.day;
    st.wHour = direntry.date.hour;
    st.wMinute = direntry.date.minute;
    st.wSecond = direntry.date.second;

    FILETIME ft;
    SystemTimeToFileTime(&st, &ft);
    _int64 nFt = ((_int64)ft.dwHighDateTime << 32) + ft.dwLowDateTime;
    _int64 nFt2 = nFt;
    nFt += ((_int64)m_server.nTimeZoneOffset) * 10000000 * 60;
    ft.dwHighDateTime = static_cast<unsigned long>(nFt >> 32);
    ft.dwLowDateTime = static_cast<unsigned long>(nFt & 0xFFFFFFFF);
    FileTimeToSystemTime(&ft, &st);
    direntry.date.year = st.wYear;
    direntry.date.month = st.wMonth;
    direntry.date.day = st.wDay;
    direntry.date.hour = st.wHour;
    direntry.date.minute = st.wMinute;
    direntry.date.second = st.wSecond;
  }

  if (m_server.nServerType&FZ_SERVERTYPE_SUB_FTP_VMS &&
    (!m_vmsAllRevisions || direntry.dir))
  { //Remove version information, only keep the latest file
    int pos=direntry.name.ReverseFind(L';');
    if (pos<=0 || pos>=(direntry.name.GetLength()-1))
      return;
    int version=_ttoi(direntry.name.Mid(pos+1));
    direntry.name=direntry.name.Left(pos);

    tEntryList::iterator entryiter=m_EntryList.begin();
    tTempData::iterator dataiter=m_TempData.begin();
    BOOL bContinue=FALSE;
    while (entryiter!=m_EntryList.end())
    {
      DebugAssert(dataiter!=m_TempData.end());
      t_directory::t_direntry dir=*entryiter;
      int oldversion=*dataiter;
      if (direntry.name==dir.name)
      {
        bContinue=TRUE;
        if (version>oldversion)
        {
          *entryiter=direntry;
          *dataiter=version;
        }
        break;
      }
      entryiter++;
      dataiter++;
    }
    if (bContinue)
      return;
    m_EntryList.push_back(direntry);
    m_TempData.push_back(version);
  }
  else
  {
    m_EntryList.push_back(direntry);
    m_TempData.push_back(0);
  }
}

bool CFtpListResult::IsNumeric(const char *str, int len) const
{
  if (!str)
    return false;
  if (!*str)
    return false;
  const char *p=str;
  while(*p)
  {
    if (len != -1)
      if ((p - str) >= len)
        return true;

    if (*p<'0' || *p>'9')
    {
      return false;
    }
    p++;
  }
  return true;
}

bool CFtpListResult::ParseShortDate(const char *str, int len, t_directory::t_direntry::t_date &date) const
{
  if (!str)
    return false;

  if (len <= 0)
    return false;

  int i=0;

  //Extract the date
  BOOL bGotYear = FALSE;
  BOOL bGotMonth = FALSE;
  BOOL bGotDay = FALSE;
  int value = 0;
  bool numeric = true;
  while (str[i] != '-' && str[i] != '.' && str[i] != '/')
  {
    if (!str[i])
      return false;

    // Left half of token not numeric, check if it's a month name
    if (str[i] < '0' || str[i] > '9')
      numeric = false;
    else
    {
      value *= 10;
      value += str[i] - '0';
    }
    i++;
    if (i == len)
      return false;
  }
  if (!i)
    return false;

  if (!numeric)
  {
    std::map<CString, int>::const_iterator iter;

    char *tmpstr = new char[i + 1];
    strncpy(tmpstr, str, i);
    tmpstr[i] = 0;
    strlwr(tmpstr);

    USES_CONVERSION;
    iter = m_MonthNamesMap.find(A2T(tmpstr));
    delete [] tmpstr;
    if (iter == m_MonthNamesMap.end())
      return false;

    date.month = iter->second;
    bGotMonth = true;
  }
  else if (i == 4)
  { //Seems to be yyyy-mm-dd
    if (value < 1900)
      return false;
    date.year = value;
    bGotYear = TRUE;
  }
  else if (i <= 2)
  {
    if (str[i] == '.')
    {
      // Maybe dd.mm.yyyy
      if (!value || value > 31)
        return false;
      date.day = value;
      bGotDay = TRUE;
    }
    else
    {
      // Seems to be mm-dd-yyyy or mm/dd/yyyy (stupid format, though)
      if (!value)
        return false;
      else if (value > 12) // sigh, guess dd/mm/yyyy instead
      {
        date.day = value;
        bGotDay = true;
      }
      else
      {
        date.month = value;
        bGotMonth = TRUE;
      }
    }
  }
  else
    return false;


  //Extract the second date field
  const char *p = str + i + 1;
  len -= i + 1;
  i=0;
  value=0;

  if (i >= len)
    return false;

  while (p[i]!='-' && p[i]!='.' && p[i]!='/')
  {
    value *= 10;
    value += p[i]-'0';
    i++;

    if (i >= len)
      return false;
  }
  if (bGotYear || bGotDay)
  {
    // Month field in yyyy-mm-dd or dd-mm-yyyy
    if (!value || value > 12)
      return false;
    date.month = value;
    bGotMonth = TRUE;

  }
  else
  {
    // Day field in mm-dd-yyyy
    if (!value || value > 31)
      return false;
    date.day = value;
    bGotDay = TRUE;
  }

  //Extract the last date field
  p += i+1;
  len -= i + 1;
  i=0;
  value=0;

  if (i >= len)
    return false;
  while (p[i]!='-' && p[i]!='.' && p[i]!='/')
  {
    value *= 10;
    value += p[i]-'0';
    i++;
    if (i >= len)
      break;
  }

  if (bGotYear)
  {
    // Day field in yyyy-mm-dd
    if (!value || value > 31)
      return false;
    date.day = value;
  }
  else
  {
    //Year in dd.mm.yyyy or mm-dd-yyyy
    date.year = value;
    if (date.year<50)
      date.year+=2000;
    else if (date.year<1000)
      date.year += 1900;
  }

  date.hasdate = TRUE;
  date.hasyear = TRUE;
  return true;
}

BOOL CFtpListResult::parseAsVMS(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int tokenlen = 0;
  int pos = 0;
  USES_CONVERSION;

  std::map<CString, int>::const_iterator iter;
  t_directory::t_direntry dir;

  dir.bUnsure = FALSE;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (!strnchr(str, tokenlen, ';'))
    return FALSE;

  dir.size = -2;
  const char *separator = strnchr(str, tokenlen, ';');
  if (!separator)
    return FALSE;

  dir.dir = FALSE;

  if ((separator - str) > 4)
    if (*(separator - 4) == '.')
      if (*(separator - 3) == 'D')
        if (*(separator - 2) == 'I')
          if (*(separator - 1) == 'R')
          {
            dir.dir = TRUE;
          }
  if (dir.dir)
  {
    int i;
    LPTSTR pBuffer = dir.name.GetBuffer(tokenlen - 4);
    for (i = 0; i < (separator - str - 4); i++)
      pBuffer[i] = str[i];
    for (i = 0; i < (tokenlen - (separator - str)); i++)
      pBuffer[i + (separator - str) - 4] = separator[i];
    dir.name.ReleaseBuffer(tokenlen - 4);
  }
  else
    copyStr(dir.name, 0, str, tokenlen);

  // This field is either the size or a username (???) enclosed in [].
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  bool gotSize = false;
  const char *p = strnchr(str, tokenlen, '/');
  if (!p && IsNumeric(str, tokenlen))
  {
    gotSize = true;
    dir.size = strntoi64(str, tokenlen) * 512;
  }
  else if (p && p > str && IsNumeric(str, p - str))
  {
    gotSize = true;
    dir.size = strntoi64(str, p - str) * 512;
  }
  else
  {
    if (tokenlen < 3 || str[0] != '[' || str[tokenlen - 1] != ']')
      return false;
    copyStr(dir.ownergroup, 0, str + 1, tokenlen - 2);
  }

  if (!gotSize)
  {
    //Size
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
      return FALSE;

    const char *p = strnchr(str, tokenlen, '/');
    int len;
    if (p)
      len = p - str;
    else
      len = tokenlen;

    if (!IsNumeric(str, len))
      return FALSE;

    dir.size = strntoi64(str, len) * 512;
  }

  //Get date
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  dir.date.hasdate = TRUE;

  //Day
  p = str;

  while (*p != '-')
    if ((++p - str) == tokenlen)
      return 0;

  dir.date.day = static_cast<int>(strntoi64(str, p-str));
  if (!dir.date.day || dir.date.day > 31)
    return FALSE;

  p++;
  const char *pMonth = p;
  //Month
  while (*p != '-')
    if ((++p - str) == tokenlen)
      return FALSE;
  if ((p - pMonth) >= 15)
    return FALSE;
  char buffer[15] = {0};
  memcpy(buffer, pMonth, p-pMonth);
  strlwr(buffer);
  iter = m_MonthNamesMap.find(A2T(buffer));
  if (iter == m_MonthNamesMap.end())
    return FALSE;
  dir.date.month = iter->second;
  p++;

  dir.date.year = static_cast<int>(strntoi64(p, tokenlen - (p - str)));
  dir.date.hasyear = TRUE;

  //Get time
  str = GetNextToken(line, linelen, tokenlen, pos, 0);

  if (str && strnchr(str + 1, tokenlen - 2, ':'))
  {
    dir.date.hastime = TRUE;

    p = str;
    //Hours
    while (*p != ':')
      if ((++p - str) == tokenlen)
        return 0;

    dir.date.hour = static_cast<int>(strntoi64(str, p - str));
    if (dir.date.hour < 0 || dir.date.hour > 23)
      return FALSE;
    p++;

    const char *pMinute = p;
    //Minutes
    while (*p && *p != ':' )
      p++;

    dir.date.minute = static_cast<int>(strntoi64(pMinute, p - pMinute));
    if (dir.date.minute < 0 || dir.date.minute > 59)
      return FALSE;

    str = GetNextToken(line, linelen, tokenlen, pos, 0);
  }
  else
  {
    dir.date.hastime = FALSE;
  }

  while (str)
  {
    if (tokenlen > 2 && str[0] == '(' && str[tokenlen - 1] == ')')
    {
      if (dir.permissionstr != L"")
        dir.permissionstr += L" ";
      CString tmp;
      copyStr(tmp, 0, str + 1, tokenlen - 2);
      dir.permissionstr += tmp;
    }
    else if (tokenlen > 2 && str[0] == '[' && str[tokenlen - 1] == ']')
    {
      if (dir.ownergroup != L"")
        dir.ownergroup += L" ";
      CString tmp;
      copyStr(tmp, 0, str + 1, tokenlen - 2);
      dir.ownergroup += tmp;
    }
    else
    {
      if (dir.permissionstr != L"")
        dir.permissionstr += L" ";
      CString tmp;
      copyStr(tmp, 0, str, tokenlen);
      dir.permissionstr += tmp;
    }

    str = GetNextToken(line, linelen, tokenlen, pos, 0);
  }

  direntry = dir;

  return TRUE;
}

BOOL CFtpListResult::parseAsEPLF(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  t_directory::t_direntry dir;
  const char *str = strstr(line, "\t");

  //Check if directory listing is an EPLF one
  if (*line=='+' && str)
  {
    str++;
    if (!*str)
      return FALSE;
    dir.bLink = FALSE;
    dir.bUnsure = FALSE;
    dir.date.hasdate = dir.date.hastime = FALSE;
    dir.dir = FALSE;
    dir.size = -2;
    dir.name = str;
    const char *fact = line + 1;
    const char *nextfact = fact;
    nextfact = strstr(nextfact, ",");
    //if (nextfact && nextfact < str)
    //  *nextfact=0;
    while (fact<=(str-2))
    {
      int len;
      if (!nextfact)
        len = str - fact - 1;
      else
        len = nextfact - fact;
      if (len == 1 && fact[0] == '/')
        dir.dir = TRUE;
      else if (*fact=='s')
        dir.size = strntoi64(fact+1, len-1);
      else if (*fact=='m')
      {
        time_t rawtime = (time_t)strntoi64(fact+1, len-1);
        TimeTToDate(rawtime, dir.date);
      }
      else if (len == 5 && *fact=='u' && *(fact+1)=='p')
      {
        char buffer[4] = {0};
        memcpy(buffer, fact+2, len-2);
        direntry.permissionstr = buffer;
      }
      if (!nextfact || nextfact>=(str-2))
        break;
      fact = nextfact+1;
      nextfact = strstr(nextfact+1, ",");
//      if (nextfact && nextfact<str)
//        *nextfact=0;
    }

    direntry = dir;

    return TRUE;
  }

  return FALSE;
}

BOOL CFtpListResult::parseAsMlsd(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  // MLSD format as described here: https://datatracker.ietf.org/doc/html/rfc3659
  // Parsing is done strict, abort on slightest error.

  // If we ever add some detection that entry is symlink,
  // make sure to add support for resolving the symlink
  // using MLST to TFTPFileSystem::ReadSymlink

  int pos = 0;
  int tokenlen = 0;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
  {
    return FALSE;
  }
  CString facts(str, tokenlen);
  if (facts.IsEmpty())
  {
    return FALSE;
  }
  direntry.name = L"";
  direntry.bUnsure = FALSE;
  direntry.dir = FALSE;
  direntry.bLink = FALSE;
  direntry.ownergroup = L"";
  direntry.owner = L"";
  direntry.group = L"";
  direntry.permissionstr = L"";

  CString owner, group, uid, gid, ownername, groupname;

  while (!facts.IsEmpty())
  {
    int delim = facts.Find(';');
    if (delim < 3)
    {
      if (delim != -1)
      {
        return 0;
      }
      else
        delim = facts.GetLength();
    }

    int pos = facts.Find('=');
    if (pos < 1 || pos > delim)
    {
      return FALSE;
    }

    CString factname = facts.Left(pos);
    factname.MakeLower();
    CString value = facts.Mid(pos + 1, delim - pos - 1);
    // When adding new facts, update filter in CFtpControlSocket::LogOnToServer
    // (CONNECT_FEAT state)
    if (factname == L"type")
    {
      if (!value.CompareNoCase(L"dir"))
      {
        direntry.dir = TRUE;
      }
      // This is syntax used by proftpd by default
      // http://www.proftpd.org/docs/modules/mod_facts.html
      // They claim it's the correct one.
      // See also
      // https://www.rfc-editor.org/errata_search.php?rfc=3659&eid=1500
      else if (!value.Left(15).CompareNoCase(L"OS.unix=symlink"))
      {
        direntry.dir = TRUE;
        direntry.bLink = TRUE;
        // actually symlink target should not be included in this syntax,
        // but just in case some servers do.
        if ((value.GetLength() > 16) && (value[15] == ':'))
          direntry.linkTarget = value.Mid(16);
      }
      // This is syntax shown in RFC 3659 section 7.7.4 "A More Complex Example"
      // Type=OS.unix=slink:/foobar;Perm=;Unique=keVO1+4G4; foobar
      // https://datatracker.ietf.org/doc/html/rfc3659
      else if (!value.Left(13).CompareNoCase(L"OS.unix=slink"))
      {
        direntry.dir = TRUE;
        direntry.bLink = TRUE;
        if ((value.GetLength() > 14) && (value[13] == ':'))
          direntry.linkTarget = value.Mid(14);
      }
      // For MLSD, these will be skipped in AddData.
      else if (!value.CompareNoCase(L"cdir"))
      {
        // ProFTPD up to 1.3.6rc1 and 1.3.5a incorrectly uses "cdir" for the current working directory.
        // So at least in MLST, where this would be the only entry, we treat it like "dir".
        if (!m_mlst)
        {
          direntry.name = L".";
        }
        direntry.dir = TRUE;
      }
      else if (!value.CompareNoCase(L"pdir"))
      {
        direntry.name = L"..";
        direntry.dir = TRUE;
      }
    }
    else if (factname == L"size")
    {
      direntry.size = 0;

      for (unsigned int i = 0; i < value.GetLength(); ++i)
      {
        if (value[i] < '0' || value[i] > '9')
        {
          return FALSE;
        }
        direntry.size *= 10;
        direntry.size += value[i] - '0';
      }
    }
    else if (factname == L"modify" ||
      (!direntry.date.hasdate && factname == L"create"))
    {
      if (!parseMlsdDateTime(value, direntry.date))
      {
        return FALSE;
      }
    }
    else if (factname == L"perm")
    {
      // there's no way we can convert Perm fact to unix-style permissions,
      // so we at least present Perm as-is to a user
      direntry.humanpermstr = value;
    }
    else if (factname == L"unix.mode")
    {
      direntry.permissionstr = value;
    }
    else if (factname == L"unix.owner" || factname == L"unix.user")
    {
      owner = value;
    }
    else if (factname == L"unix.group")
    {
      group = value;
    }
    else if (factname == L"unix.uid")
    {
      uid = value;
    }
    else if (factname == L"unix.gid")
    {
      gid = value;
    }
    else if (factname == L"unix.ownername")
    {
      ownername = value;
    }
    else if (factname == L"unix.groupname")
    {
      groupname = value;
    }

    facts = facts.Mid(delim + 1);
  }

  // The order of the facts is undefined
  if (!ownername.IsEmpty())
    direntry.owner = ownername;
  else if (!owner.IsEmpty())
    direntry.owner = owner;
  else if (!uid.IsEmpty())
    direntry.owner = uid;

  if (!groupname.IsEmpty())
    direntry.group = groupname;
  else if (!group.IsEmpty())
    direntry.group = group;
  else if (!gid.IsEmpty())
    direntry.group = gid;

  if (line[pos] != L' ')
  {
    return FALSE;
  }
  if (direntry.name.IsEmpty())
  {
    pos++;
    CString fileName;
    copyStr(fileName, 0, line + pos, linelen - pos, true);
    if (m_mlst)
    {
      // do not try to detect path type, assume a standard *nix syntax + do not trim
      CServerPath path(fileName, FZ_SERVERTYPE_FTP, false);
      direntry.name = path.GetLastSegment();
      if (direntry.name.IsEmpty())
      {
        direntry.name = fileName;
      }
    }
    else
    {
      direntry.name = fileName;
    }
  }
  return TRUE;
}

bool CFtpListResult::parseMlsdDateTime(const CString value, t_directory::t_direntry::t_date &date) const
{
  if (value.IsEmpty())
  {
    return FALSE;
  }

  bool result = FALSE;
  int Year, Month, Day, Hours, Minutes, Seconds;
  Year=Month=Day=Hours=Minutes=Seconds=0;
  // Time can include a fraction after a dot, this will ignore the fraction part.
  if (swscanf((LPCWSTR)value, L"%4d%2d%2d%2d%2d%2d", &Year, &Month, &Day, &Hours, &Minutes, &Seconds) == 6)
  {
    date.hasdate = TRUE;
    date.hastime = TRUE;
    date.hasseconds = TRUE;
    result = TRUE;
  }
  else if (swscanf((LPCWSTR)value, L"%4d%2d%2d", &Year, &Month, &Day) == 3)
  {
    date.hasdate = TRUE;
    date.hastime = FALSE;
    result = TRUE;
  }
  if (result)
  {
    date.year = Year;
    date.hasyear = TRUE;
    date.month = Month;
    date.day = Day;
    date.hour = Hours;
    date.minute = Minutes;
    date.second = Seconds;
    date.utc = TRUE;
  }
  return result;
}

void CFtpListResult::GuessYearIfUnknown(t_directory::t_direntry::t_date & Date)
{
  // Problem: Some servers use times only for files newer than 6 months,
  // others use one year as limit. IIS shows time for files from the current year (jan-dec).
  // So there is no support for files with time
  // dated in the near future. Under normal conditions there should not be such files.
  if (!Date.year) // might use direntry.date.hasyear now?
  {
    CTime curtime = CTime::GetCurrentTime();
    int curday = curtime.GetDay();
    int curmonth = curtime.GetMonth();
    int curyear = curtime.GetYear();
    int now = curmonth * 31 + curday;
    int file = Date.month * 31 + Date.day;
    if ((now + 1) >= file)
    {
      Date.year = curyear;
    }
    else
    {
      Date.year = curyear - 1;
    }
    // year is guessed, not setting hasyear
  }
}

BOOL CFtpListResult::parseAsUnix(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  USES_CONVERSION;
  int pos = 0;
  int tokenlen = 0;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
  {
    return FALSE;
  }
  //Check the first token
  if (str[0] != 'b' &&
    str[0] != 'c' &&
    str[0] != 'd' &&
    str[0] != 'l' &&
    str[0] != 'p' &&
    str[0] != 's' &&
    str[0] != '-')
  {
    return FALSE;
  }

  //First check if it is a netware server
  bool bNetWare = false;
  copyStr(direntry.permissionstr, 0, str, tokenlen);
  if (tokenlen == 1)
  {
    //Yes, it's most likely a netware server
    //Now get the full permission string
    bNetWare = true;
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
    {
      return FALSE;
    }
    direntry.permissionstr += L" ";
    copyStr(direntry.permissionstr, direntry.permissionstr.GetLength(), str, tokenlen);
  }

  //Set directory and link flags
  //Always assume links point directories
  //GUI frontend should try to figure out
  //to where the link really points
  if (direntry.permissionstr[0]==L'd' || direntry.permissionstr[0]==L'l')
    direntry.dir = true;
  else
    direntry.dir = false;

  if (direntry.permissionstr[0]==L'l')
    direntry.bLink = true;
  else
    direntry.bLink = false;

  bool bNetPresenz = false;
  if (!bNetWare) //On non-netware servers, expect at least two unused tokens
  {
    bool groupid = false;
    // Unused param
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
    {
      return FALSE;
    }
    if (tokenlen == 6 && !strncmp(str, "folder", tokenlen) && direntry.dir)
      bNetPresenz = true;  //Assume NetPresenz server
                //However, it's possible that we mark a non-NetPresenz
                //server if the fileowner is "folder"
    else if (!IsNumeric(str, tokenlen))
    {
      // Check for Connect:Enterprise server
      if (direntry.permissionstr.GetLength() > 3 &&
        (direntry.permissionstr.Right(3) == L"FTP" ||
         direntry.permissionstr.Right(3) == L"FTS" ||
         direntry.permissionstr.Right(3) == L"TCP" ||
         direntry.permissionstr.Right(3) == L"SSH"))
        groupid = TRUE;

      copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), str, tokenlen);
    }
    else
      groupid = TRUE;

    if (!bNetPresenz && groupid)
    {
      //Unused param
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
      {
        return FALSE;
      }

      if (direntry.ownergroup != L"")
        direntry.ownergroup += L" ";
      copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), str, tokenlen);
    }
  }

  //Skip param, may be used for size
  int skippedlen = 0;
  const char *skipped = GetNextToken(line, linelen,skippedlen, pos, 0);
  if (!skipped)
  {
    return FALSE;
  }

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
  {
    return FALSE;
  }

  // Keep parsing the information until we get a numerical string,
  // because some broken servers may send 3 tags: domain/network, group and user
  const char *prevstr = 0;
  int prevstrlen = 0;

  __int64 tmp = 0;
  std::map<CString, int>::const_iterator iter;
  while (str && !ParseSize(str, tokenlen, tmp) && !IsNumeric(skipped, skippedlen))
  {
    //Maybe the server has left no space between the group and the size
    //because of stupid alignment
    char *tmpstr = new char[tokenlen + 1];
    strncpy(tmpstr, str, tokenlen);
    tmpstr[tokenlen] = 0;
    strlwr(tmpstr);

    iter = m_MonthNamesMap.find(A2T(tmpstr));
    delete [] tmpstr;
    if (iter != m_MonthNamesMap.end())
    {
      BOOL bRightNumeric = true;
      if (skipped[skippedlen-1]<'0' || skipped[skippedlen-1]>'9')
        bRightNumeric = false;

      if (bRightNumeric)
        break;

      bRightNumeric = true;
      if (prevstr && prevstrlen)
      {
        if (prevstr[prevstrlen-1]<'0' || prevstr[prevstrlen-1]>'9')
          bRightNumeric = false;

        if (bRightNumeric)
        {
          if (direntry.ownergroup != L"")
            direntry.ownergroup += L" ";
          copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), str, tokenlen);
          skipped = prevstr;
          skippedlen = prevstrlen;
          break;
        }
      }
    }
    if (prevstr)
    {
      if (direntry.ownergroup != L"")
        direntry.ownergroup += L" ";

      copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), prevstr, prevstrlen);
    }
    prevstr = str;
    prevstrlen = tokenlen;
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
  }

  if (!str)
  {
    return FALSE;
  }

  const char *size = str;
  int sizelen = tokenlen;
  if (!ParseSize(str, tokenlen, direntry.size))
  {
    //Maybe we've skipped too much tokens
    if (!ParseSize(skipped, skippedlen, direntry.size))
    {
      //Maybe the server has left no space between the group and the size
      //because of stupid alignment
      bool bRightNumeric = true;
      const char *pos;
      for (pos=(str+tokenlen-1); pos > str; pos--)
      {
        if (*pos<'0' || *pos>'9')
        {
          if (pos==(str+tokenlen-1))
            bRightNumeric=false;
          break;
        }
      }
      if (bRightNumeric && pos>str)
      {
        size = pos + 1;
        sizelen = pos - str;
        direntry.ownergroup += L" ";

        if (direntry.ownergroup != L"")
          direntry.ownergroup += L" ";

        copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), str, pos-str);

      }
      else
      {
        for (pos=(skipped+skippedlen-1); pos > skipped; pos--)
        {
          if (*pos<'0' || *pos>'9')
          {
            if (pos==(skipped+skippedlen-1))
            {
              return false;
            }
            break;
          }
        }
        size = pos + 1;
        sizelen = skippedlen + skipped - size;

        if (direntry.ownergroup != L"")
          direntry.ownergroup += L" ";

        copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), skipped, skippedlen - sizelen);

      }
      direntry.size = strntoi64(size, sizelen);
    }
    else
    {
      //We should've not skipped the last token
      //This also fixes the problem with the NetPresenz detection
      if (bNetPresenz && direntry.dir && direntry.ownergroup != L"")
      {
        direntry.ownergroup = L"folder " + direntry.ownergroup;
      }
    }
  }
  else
  {
    if (direntry.ownergroup != L"")
      direntry.ownergroup += L" ";
    copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), skipped, skippedlen);
    if (prevstr)
    {
      direntry.ownergroup += L" ";
      copyStr(direntry.ownergroup, direntry.ownergroup.GetLength(), prevstr, prevstrlen);
    }
    str = 0;
  }

  //Month
  if (!str)
    str = GetNextToken(line, linelen, tokenlen, pos, 0);

  if (!str)
  {
    return FALSE;
  }

  const char *smonth = str;
  int smonthlen = tokenlen;
  direntry.date.year = 0;

  //Day
  const char *sday = 0;
  int sdaylen = 0;

  // Some VShell server send both the year and the time, try to detect them
  BOOL bCouldBeVShell = FALSE;

  //Some servers use the following date formats:
  // 26-09 2002, 2002-10-14, 01-jun-99
  const char *p = strnchr(smonth, smonthlen, '-');
  if (p)
  {
    int plen = smonthlen - (p - smonth);
    const char *pos2 = strnchr(p+1, plen - 1, '-');
    if (!pos2) //26-09 2002
    {
      sday = p + 1;
      sdaylen = plen - 1;
      smonthlen = p-smonth;
    }
    else if (p-smonth == 4) //2002-10-14
    {
      direntry.date.year = static_cast<int>(strntoi64(smonth, p-smonth));
      direntry.date.hasyear = TRUE;
      sday = pos2 + 1;
      sdaylen = smonthlen - (pos2 - smonth) - 1;
      smonthlen = pos2-smonth - (p-smonth) - 1;
      smonth = p + 1;
      /* Try to detect difference between yyyy/dd/mm and yyyy/mm/dd
       * Unfortunately we have to guess which one is the right if
       * the month is < 12
       */
      if (strntoi64(smonth, smonthlen) > 12)
      {
        const char *tmp = smonth;
        smonth = sday;
        sday = tmp;
        int tmplen = smonthlen;
        smonthlen = sdaylen;
        sdaylen = tmplen;
      }
    }
    else if (p-smonth) //14-10-2002 or 01-jun-99
    {
      direntry.date.year = static_cast<int>(strntoi64(pos2+1, tokenlen - (pos2-smonth) - 1));
      direntry.date.hasyear = TRUE;
      sday = smonth;
      sdaylen = p - smonth;
      smonthlen = pos2-smonth - (p-smonth) - 1;
      smonth = p + 1;
      /* Try to detect difference between yyyy/dd/mm and yyyy/mm/dd
       * Unfortunately we have to guess which one is the right if
       * the month is < 12
       */
      if (strntoi64(smonth, smonthlen) > 12)
      {
        const char *tmp = smonth;
        smonth = sday;
        sday = tmp;
        int tmplen = smonthlen;
        smonthlen = sdaylen;
        sdaylen = tmplen;
      }
    }
    else
    {
      return FALSE;
    }
  }
  /* Some servers use the following date formats:
   * yyyy/dd/mm, yyyy/mm/dd, dd/mm/yyyy, mm/dd/yyyy
   * try to detect them.
   */
  else if (strnchr(smonth, smonthlen, '/'))
  {
    const char *p = strnchr(smonth, smonthlen, '/');
    int plen = smonthlen - (p - smonth);
    const char *pos2 = strnchr(p+1, plen - 1, '/');
    if (!pos2) //Assume 26/09 2002
    {
      sday = p + 1;
      sdaylen = plen - 1;
      smonthlen = p-smonth;
    }
    else if (p-smonth==4)
    {
      direntry.date.year = static_cast<int>(strntoi64(smonth, p-smonth));
      direntry.date.hasyear = TRUE;
      sday = pos2 + 1;
      sdaylen = smonthlen - (pos2 - smonth) - 1;
      smonthlen = pos2-smonth - (p-smonth) - 1;
      smonth = p + 1;
      /* Try to detect difference between yyyy/dd/mm and yyyy/mm/dd
       * Unfortunately we have to guess which one is the right if
       * the month is < 12
       */
      if (strntoi64(smonth, smonthlen) > 12)
      {
        const char *tmp = smonth;
        smonth = sday;
        sday = tmp;
        int tmplen = smonthlen;
        smonthlen = sdaylen;
        sdaylen = tmplen;
      }
    }
    else if (p-smonth==2)
    {
      direntry.date.year = static_cast<int>(strntoi64(pos2+1, tokenlen - (pos2-smonth) - 1));
      direntry.date.hasyear = TRUE;
      sday = smonth;
      sdaylen = p - smonth;
      smonthlen = pos2-smonth - (p-smonth) - 1;
      smonth = p + 1;
      /* Try to detect difference between yyyy/dd/mm and yyyy/mm/dd
       * Unfortunately we have to guess which one is the right if
       * the month is < 12
       */
      if (strntoi64(smonth, smonthlen) > 12)
      {
        const char *tmp = smonth;
        smonth = sday;
        sday = tmp;
        int tmplen = smonthlen;
        smonthlen = sdaylen;
        sdaylen = tmplen;
      }
    }
    else
    {
      return FALSE;
    }
  }
  else
  {
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
    {
      return FALSE;
    }

    sday = str;
    sdaylen = tokenlen;

    if (sdaylen && sday[sdaylen-1] == ',')
    {
      sdaylen--;
      bCouldBeVShell = TRUE;
    }

    //Trim trailing characters
    while (sdaylen && (sday[sdaylen-1]=='.' || sday[sdaylen-1]==','))
    {
      bCouldBeVShell = FALSE;
      sdaylen--;
    }

    int i;
    for (i = 0; i < sdaylen; i++)
      if (sday[i] < '0' || sday[i] > '9')
        break;
    if (i && i < sdaylen)
    {
      if ((unsigned char)sday[i] > 127)
        sdaylen = i;
    }

    if (!sdaylen)
    {
      return FALSE;
    }
  }

  if (!strntoi64(sday, sdaylen)) //Day field invalid
  { //Maybe the server is sending a directory listing with localized date format.
    //Try to fix this really bad behavior
    bCouldBeVShell = FALSE;
    const char *tmp = smonth;
    smonth = sday;
    sday = tmp;
    int tmplen = smonthlen;
    smonthlen = sdaylen;
    sdaylen = tmplen;
  }

  //Time/Year
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
  {
    return FALSE;
  }

  //Trim trailing characters
  while (smonthlen && (smonth[smonthlen-1]=='.' || smonth[smonthlen-1]==','))
    smonthlen--;

  if (!smonthlen)
  {
    return FALSE;
  }

  char *lwr = new char[smonthlen + 1];
  memcpy(lwr, smonth, smonthlen);
  lwr[smonthlen] = 0;
  _strlwr(lwr);

  bool gotYear = false;
  int year = (int)strntoi64(smonth, smonthlen);
  if (year > 1000)
  {
    gotYear = true;
    direntry.date.year = year;
    direntry.date.hasyear = TRUE;
  }
  else
  {
    //Try if we can recognize the month name
    iter = m_MonthNamesMap.find(A2T(lwr));
    delete [] lwr;
    if (iter == m_MonthNamesMap.end())
    {
      int i;
      for (i = 0; i < smonthlen; i++)
        if (smonth[i] < '0' || smonth[i] > '9')
          break;
      if (!i || i == smonthlen)
      {
        return false;
      }
      if ((unsigned char)smonth[i] < 128)
      {
        return false;
      }

      smonthlen = i;
      direntry.date.month = (int)strntoi64(smonth, smonthlen);
      if (!direntry.date.month || direntry.date.month > 12)
      {
        return false;
      }
    }
    else
      direntry.date.month = iter->second;
  }

  if (!gotYear)
  {
    direntry.date.day = static_cast<int>(strntoi64(sday, sdaylen));
    if (direntry.date.day < 1 || direntry.date.day > 31)
    {
      return false;
    }
  }
  else
  {
    direntry.date.month = static_cast<int>(strntoi64(sday, sdaylen));
    if (direntry.date.month < 1 || direntry.date.month > 12)
    {
      return false;
    }
  }

  const char *stimeyear = str;
  int stimeyearlen = tokenlen;

  //Parse the time/year token
  const char *strpos = strnchr(stimeyear, stimeyearlen, ':');
  if (!strpos)
    strpos = strnchr(stimeyear, stimeyearlen, '.');
  if (!strpos)
    strpos = strnchr(stimeyear, stimeyearlen, '-');
  if (strpos)
  {
    //stimeyear has delimiter, so it's a time
    direntry.date.hour = static_cast<int>(strntoi64(stimeyear, strpos - stimeyear));
    int stimeyearrem = stimeyearlen - (strpos - stimeyear) - 1;
    const char *strpos2 = strnchr(strpos + 1, stimeyearrem, ':');
    if (strpos2 == NULL)
    {
      direntry.date.minute = static_cast<int>(strntoi64(strpos + 1, stimeyearrem));
    }
    else
    {
      direntry.date.minute = static_cast<int>(strntoi64(strpos + 1, strpos2 - strpos - 1));
      direntry.date.second = static_cast<int>(strntoi64(strpos2 + 1, stimeyearlen - (strpos2 - stimeyear) - 1));
      direntry.date.hasseconds = TRUE;
    }
    direntry.date.hastime = TRUE;

    GuessYearIfUnknown(direntry.date);
    bCouldBeVShell = FALSE;
  }
  else
  {
    if (gotYear)
    {
      direntry.date.day = static_cast<int>(strntoi64(stimeyear, stimeyearlen));
      if (direntry.date.day < 1 || direntry.date.day > 31)
      {
        return false;
      }
    }
    else if (!direntry.date.year) // might use direntry.date.hasyear now?
    {
      //No delimiters -> year

      direntry.date.hastime = FALSE;
      direntry.date.year = static_cast<int>(strntoi64(stimeyear, stimeyearlen));
      direntry.date.hasyear = TRUE;
    }
    else
    {
      // File has no time token and short date format
      pos -= stimeyearlen;
    }
  }

  if (!direntry.date.year) //Year 0? Really ancient file, this is invalid! might use direntry.date.hasyear now?
  {
    return FALSE;
  }

  // Check if server could still be one of the newer VShell servers
  if (bCouldBeVShell)
  {
    int oldpos = pos;

    //Get time
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
    {
      return FALSE;
    }

    const char *p = strnchr(str, tokenlen, ':');
    if (pos && (p - str) == 2 && IsNumeric(str, 2) && IsNumeric(str + 3, 2))
    {
      //stimeyear has delimiter, so it's a time
      int hour = static_cast<int>(strntoi64(str, 2));
      int minute = static_cast<int>(strntoi64(str + 3, 2));

      if (hour >= 0 && hour < 24 && minute >= 0 && minute < 60)
      {
        direntry.date.hour = hour;
        direntry.date.minute = minute;
        direntry.date.hastime = TRUE;
      }
      else
        pos = oldpos;
    }
    else
      pos = oldpos;
  }

  //Get filename
  str = GetNextToken(line, linelen, tokenlen, pos, 1);
  if (!str)
  {
    return FALSE;
  }

  //Trim link data from filename
  if (direntry.bLink)
  {
    const char *pos = strnstr(str, tokenlen, " -> ");
    if (pos)
    {
      copyStr(direntry.linkTarget, 0, pos + 4, tokenlen - (pos - str) - 4);
      tokenlen = pos - str;
    }

    if (!tokenlen)
    {
      return FALSE;
    }
  }

  //Trim indicators, some server add those to mark special files
  if (str[tokenlen - 1] == '*' ||
    str[tokenlen - 1] == '/' ||
//    str[tokenlen - 1] == '=' || //Don't trim this char, it would cause problems on certain servers
                  //This char just marks sockets, so it will never appear as indicator
                  //However it is valid as character for filenames on some systems
    str[tokenlen - 1] == '|')
    tokenlen--;

  copyStr(direntry.name, 0, str, tokenlen, true);

  direntry.bUnsure = FALSE;
  direntry.date.hasdate = TRUE;

  return TRUE;
}

BOOL CFtpListResult::parseAsDos(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int pos = 0;
  int tokenlen = 0;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  //Check the first token
  if (str[0]=='b' ||
    str[0]=='c' ||
    str[0]=='d' ||
    str[0]=='l' ||
    str[0]=='p' ||
    str[0]=='s' ||
    str[0]=='-')
    return FALSE;

  if (IsNumeric(str, tokenlen))
    return FALSE;

  //It's a NT server with MSDOS directory format

  if (!ParseShortDate(str, tokenlen, direntry.date))
    return FALSE;

  //Extract time
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (!parseTime(str, tokenlen, direntry.date))
    return FALSE;

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (tokenlen == 5 && !memcmp(str, "<DIR>", 5))
  {
    direntry.dir = TRUE;
  }
  else
  {
    char * buffer = new char[tokenlen];
    int i, j;
    for (i = 0, j = 0; i < tokenlen; i++)
    {
      if (str[i] != ',')
      buffer[j++] = str[i];
    }
    direntry.dir = FALSE;
    direntry.size = strntoi64(buffer, j);
    delete [] buffer;
  }

  str = GetNextToken(line, linelen, tokenlen, pos, 1);
  if (!str)
    return FALSE;
  copyStr(direntry.name, 0, str, tokenlen, true);

  direntry.bUnsure = FALSE;

  return TRUE;
}

void CFtpListResult::TimeTToDate(time_t TimeT, t_directory::t_direntry::t_date & Date) const
{
  tm * sTime = gmtime(&TimeT);
  Date.year = sTime->tm_year + 1900;
  Date.hasyear = TRUE;
  Date.month = sTime->tm_mon+1;
  Date.day = sTime->tm_mday;
  Date.hour = sTime->tm_hour;
  Date.minute = sTime->tm_min;
  Date.hasdate = Date.hastime = TRUE;
}

BOOL CFtpListResult::parseAsOther(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int pos = 0;
  int tokenlen = 0;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  //Check the first token
  if (str[0]=='b' ||
    str[0]=='c' ||
    str[0]=='d' ||
    str[0]=='l' ||
    str[0]=='p' ||
    str[0]=='s' ||
    str[0]=='-')
    return FALSE;

  if (!IsNumeric(str, tokenlen))
    return FALSE;

  //Could be numerical Unix style format or VShell format
  //or even an OS2 server

  const char *skipped = str;
  int skippedtokenlen = tokenlen;
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  //If next token is numerical, than it's the numerical Unix style format,
  //else it's the VShell or OS/2 format
  if (IsNumeric(str, tokenlen))
  {
    copyStr(direntry.permissionstr, 0, skipped, skippedtokenlen);

    if (skippedtokenlen >= 2 && skipped[1] == '4')
      direntry.dir = TRUE;
    else
      direntry.dir = FALSE;

    //Unused token
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
      return FALSE;

    //Size
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
      return FALSE;

    direntry.size = strntoi64(str, tokenlen);

    //Date/Time
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
      return FALSE;

    time_t secsSince1970 = static_cast<long>(strntoi64(str, tokenlen));
    TimeTToDate(secsSince1970, direntry.date);

    str = GetNextToken(line, linelen, tokenlen, pos, 1);
    if (!str)
      return FALSE;
    copyStr(direntry.name, 0, str, tokenlen, true);
  }
  else
  {
    std::map<CString, int>::const_iterator iter;

    //Get size
    direntry.size = strntoi64(skipped, skippedtokenlen);

    //Get date, month first
    if (tokenlen >= 15)
      return FALSE;

    char buffer[15] = {0};
    memcpy(buffer, str, tokenlen);
    strlwr(buffer);

    USES_CONVERSION;
    iter = m_MonthNamesMap.find(A2T(buffer));
    if (iter == m_MonthNamesMap.end())
    {
      direntry.dir = FALSE;
      while (str)
      {
        //Could be an OS/2 server
        if (tokenlen == 3 && !memcmp(str, "dir", 3))
          direntry.dir = TRUE;
        else if (tokenlen == 3 && !memcmp(str, "DIR", 3))
          direntry.dir = TRUE;
        else if (strnchr(str, tokenlen, '-'))
          break;
        str = GetNextToken(line, linelen, tokenlen, pos, 0);
      }
      if (!str)
        return FALSE;

      if (!ParseShortDate(str, tokenlen, direntry.date))
        return FALSE;

      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;

      //Parse the time token
      const char *strpos = strnchr(str, tokenlen, ':');
      if (!strpos)
        return FALSE;
      if (strpos)
      {
        //stimeyear has delimiter, so it's a time
        direntry.date.hour = static_cast<int>(strntoi64(str, strpos - str));
        direntry.date.minute = static_cast<int>(strntoi64(strpos+1, tokenlen - (strpos - str) - 1));
        direntry.date.hastime = TRUE;

        GuessYearIfUnknown(direntry.date);
      }

      str = GetNextToken(line, linelen, tokenlen, pos, 1);
      if (!str)
        return FALSE;

      if (tokenlen > 6)
      {
        if (!strnicmp(str + tokenlen - 5, "<DIR>", 5))
        {
          direntry.dir = TRUE;
          tokenlen -= 5;
          while (tokenlen && (str[tokenlen - 1] == ' ' || str[tokenlen - 1] == '\t'))
            tokenlen--;
        }
        if (!tokenlen)
          return FALSE;
      }


      copyStr(direntry.name, 0, str, tokenlen, true);
    }
    else
    {
      direntry.date.month = iter->second;

      //Day
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;

      if (str[tokenlen-1]==',')
        tokenlen--;
      if (!tokenlen)
        return FALSE;
      if (!IsNumeric(str, tokenlen))
        return FALSE;

      direntry.date.day = static_cast<int>(strntoi64(str, tokenlen));
      if (direntry.date.day < 1 || direntry.date.day > 31)
        return FALSE;

      //Year
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;

      if (!IsNumeric(str, tokenlen))
        return FALSE;

      direntry.date.year = static_cast<int>(strntoi64(str, tokenlen));
      direntry.date.hasyear = TRUE;
      if (direntry.date.year < 50)
        direntry.date.year += 2000;
      else if (direntry.date.year < 1000)
        direntry.date.year += 1900;

      direntry.date.hasdate = TRUE;

      //Now get the time
      str  = GetNextToken(line, linelen, tokenlen, pos, 0);
      const char *p = strnchr(str, tokenlen, ':');
      if (!p)
        return FALSE;

      if (p==str || !IsNumeric(str, p-str) || (p-str + 1) >= tokenlen || !IsNumeric(p+1, tokenlen - (p-str) - 1))
        return FALSE;
      direntry.date.hour = static_cast<int>(strntoi64(str, p-str));
      direntry.date.minute = static_cast<int>(strntoi64(p+1, tokenlen - (p-str) - 1));

      if (direntry.date.hour < 0 || direntry.date.hour > 24)
        return FALSE;
      if (direntry.date.minute < 0 || direntry.date.minute > 59)
        return FALSE;

      direntry.date.hastime = TRUE;

      str = GetNextToken(line, linelen, tokenlen, pos, 1);
      if (!str)
        return FALSE;

      if (tokenlen > 1 && (str[tokenlen-1] == '\\' || str[tokenlen-1] == '/'))
      {
        direntry.dir = TRUE;
        tokenlen--;
      }
      else
        direntry.dir = FALSE;

      if (!tokenlen)
        return FALSE;
      copyStr(direntry.name, 0, str, tokenlen, true);
    }
  }

  direntry.bUnsure = FALSE;
  direntry.date.hasdate = TRUE;

  return TRUE;
}

_int64 CFtpListResult::strntoi64(const char *str, int len) const
{
  _int64 res = 0;
  const char *p = str;
  while ((p-str) < len)
  {
    if (*p < '0' || *p > '9')
      break;
    res *= 10;
    res += *p++ - '0';
  }
  return res;
}

const char *CFtpListResult::GetNextToken(const char *line, const int linelen, int &len, int &pos, int type) const
{
  const char *p = line + pos;
  if ((p - line) >= linelen)
    return NULL;
  while ((p - line) < linelen && (!p || *p==' ' || *p=='\t'))
    p++;

  if ((p - line) >= linelen)
    return NULL;

  const char *res = p;

  if (type)
  {
    pos = linelen;
    len = linelen - (p - line);
  }
  else
  {
    while ((p - line) < linelen && *p && *p != ' ' && *p!='\t')
      p++;

    len = p - res;
    pos = p - line;
  }

  return res;
}

const char * CFtpListResult::strnchr(const char *str, int len, char c) const
{
  if (!str)
    return NULL;

  const char *p = str;
  while (len > 0)
  {
    if (!*p)
      return NULL;
    if (*p == c)
      return p;
    p++;
    len--;
  }
  return NULL;
}

const char * CFtpListResult::strnstr(const char *str, int len, const char *c) const
{
  if (!str)
    return NULL;
  if (!c)
    return NULL;
  int clen = strlen(c);

  const char *p = str;
  while (len > 0)
  {
    if (!*p)
      return NULL;
    if (*p == *c)
    {
      if (clen == 1)
        return p;
      else if (len >= clen)
      {
        if (!memcmp(p + 1, c+1, clen-1))
          return p;
      }
      else
        return NULL;
    }
    p++;
    len--;
  }
  return NULL;
}

void CFtpListResult::copyStr(CString &target, int pos, const char *source, int len, bool mayInvalidateUTF8 /*=false*/)
{
  USES_CONVERSION;

  char *p = new char[len + 1];
  memcpy(p, source, len);
  p[len] = '\0';
  if (m_bUTF8 && *m_bUTF8)
  {
    // convert from UTF-8 to ANSI
    if (DetectUTF8Encoding(RawByteString(p, len)) == etANSI)
    {
      if (mayInvalidateUTF8 && m_server.nUTF8 != 1)
      {
        LogMessage(FZ_LOG_WARNING, L"Server does not send proper UTF-8, falling back to local charset");
        *m_bUTF8 = false;
      }
      target = target.Left(pos) + A2CT(p);
    }
    else
    {
      // convert from UTF-8 to ANSI
      int len = MultiByteToWideChar(CP_UTF8, 0, (LPCSTR)p, -1, NULL, 0);
      if (len != 0)
      {
        LPWSTR p1 = new WCHAR[len + 1];
        MultiByteToWideChar(CP_UTF8, 0, (LPCSTR)p, -1 , (LPWSTR)p1, len + 1);
        target = target.Left(pos) + W2CT(p1);
        delete [] p1;
      }
      else
        target = target.Left(pos) + A2CT(p);
    }
  }
  else
    target = target.Left(pos) + A2CT(p);
  delete [] p;
}

BOOL CFtpListResult::parseAsIBM(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int pos = 0;
  int tokenlen = 0;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  copyStr(direntry.ownergroup , 0, str, tokenlen);

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (!IsNumeric(str, tokenlen))
    return FALSE;

  direntry.size = strntoi64(str, tokenlen);

  //Date
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!ParseShortDate(str, tokenlen, direntry.date))
    return FALSE;

  //Time
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!parseTime(str, tokenlen, direntry.date))
    return FALSE;

  //Unused Token
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  //Name
  str = GetNextToken(line, linelen, tokenlen, pos, 1);
  if (!str)
    return FALSE;

  if (str[tokenlen-1] == '/')
  {
    direntry.dir = TRUE;
    if (!--tokenlen)
      return FALSE;
  }
  else
    direntry.dir = FALSE;

  copyStr(direntry.name, 0, str, tokenlen, true);

  direntry.bUnsure = FALSE;

  return true;
}

BOOL CFtpListResult::parseAsIBMMVS(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int pos = 0;
  int tokenlen = 0;

  // volume
  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (strncmp(str, "Migrated", tokenlen) != 0)
  {
    // unit
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
      return FALSE;

    if (strncmp(str, "Tape", tokenlen) != 0)
    {
      //Referred Date
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;
      if (!ParseShortDate(str, tokenlen, direntry.date))
      {
        // Perhaps of the following type:
        // TSO004 3390 VSAM FOO.BAR
        if (tokenlen != 4 || strncmp(str, "VSAM", tokenlen))
          return FALSE;

        str = GetNextToken(line, linelen, tokenlen, pos, 1);
        if (!str)
          return FALSE;

        if (strnchr(str, tokenlen, ' '))
          return FALSE;

        copyStr(direntry.name, 0, str, tokenlen, true);
        direntry.dir = false;
        return true;
      }

      // ext
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;
      if (!IsNumeric(str, tokenlen))
        return FALSE;

      int prevLen = tokenlen;

      // used
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;
      if (IsNumeric(str, tokenlen))
      {
        // recfm
        str = GetNextToken(line, linelen, tokenlen, pos, 0);
        if (!str)
          return FALSE;

        if (IsNumeric(str, tokenlen))
          return false;
      }
      else
      {
        if (prevLen < 6)
          return false;
      }

      // lrecl
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;
      if (!IsNumeric(str, tokenlen))
        return FALSE;

      // blksize
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;
      if (!IsNumeric(str, tokenlen))
        return FALSE;

      // dsorg
      str = GetNextToken(line, linelen, tokenlen, pos, 0);
      if (!str)
        return FALSE;
      if (tokenlen == 2 && !memcmp(str, "PO", 2))
      {
        direntry.dir = TRUE;
      }
      else
      {
        direntry.dir = FALSE;
        direntry.size = 100;
      }
    }
  }

  // name of dataset or sequential name
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  copyStr(direntry.name, 0, str, tokenlen, true);

  direntry.bUnsure = FALSE;

  return true;
}

BOOL CFtpListResult::parseAsIBMMVSPDS(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int pos = 0;
  int tokenlen = 0;

  // pds member name
  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  copyStr(direntry.name, 0, str, tokenlen);

  // vv.mm
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  // creation date
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  // change date
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!ParseShortDate(str, tokenlen, direntry.date))
    return FALSE;

  // change time
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!parseTime(str, tokenlen, direntry.date))
    return FALSE;

  // size
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!IsNumeric(str, tokenlen))
    return FALSE;
  direntry.size = strntoi64(str, tokenlen);

  // init
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!IsNumeric(str, tokenlen))
    return FALSE;

  // mod
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;
  if (!IsNumeric(str, tokenlen))
    return FALSE;

  // id
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  direntry.dir = FALSE;
  direntry.bUnsure = FALSE;

  return true;
}

BOOL CFtpListResult::parseAsIBMMVSPDS2(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  // Use this one only on MVS servers, as it will cause problems on other servers

  if (!(m_server.nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000)))
    return false;

  int pos = 0;
  int tokenlen = 0;

  direntry.bUnsure = FALSE;
  direntry.dir = FALSE;
  direntry.bLink = FALSE;
  direntry.ownergroup = L"";
  direntry.permissionstr = L"";
  direntry.date.hasdate = direntry.date.hastime = FALSE;

  // pds member name
  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (m_server.nServerType & FZ_SERVERTYPE_SUB_FTP_BS2000 &&
    IsNumeric(str, tokenlen))
  {
    int prevlen = tokenlen;
    const char* prev = str;
    int oldpos = pos;

    str = GetNextToken(line, linelen, tokenlen, pos, 0);
    if (!str)
      return FALSE;
    if (str[0] != ':')
    {
      str = prev;
      tokenlen = prevlen;
      pos = oldpos;
    }
  }

  copyStr(direntry.name, 0, str, tokenlen);

  if (m_server.nServerType & FZ_SERVERTYPE_SUB_FTP_BS2000)
  {
    int pos = direntry.name.ReverseFind(L':');
    if (pos != -1)
      direntry.name = direntry.name.Mid(pos + 1);
    if (direntry.name[0] == L'$')
    {
      int pos = direntry.name.Find(L'.');
      if (pos != -1)
        direntry.name = direntry.name.Mid(pos + 1);
    }
    if (direntry.name == L"")
      return FALSE;
  }

  // Hexadecimal filesize
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    // We can return true since sometimes MVS servers send pure filenames
    return true;

  direntry.size = 0;
  const char *end = str + tokenlen;
  const char *p = str;
  while (p < end)
  {
    direntry.size *= 16;
    if (*p >= '0' && *p <= '9')
      direntry.size += *p - '0';
    else if (*p >= 'A' && *p <= 'F')
      direntry.size += *p - 'A' + 10;
    else
      return false;
    p++;
  }

  // Unused hexadecimal token
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return false;

  end = str + tokenlen;
  p = str;
  while (p < end)
  {
    if ((*p < '0' || *p > '9') && (*p < 'A' || *p > 'F'))
      return false;
    p++;
  }

  // Unused token
  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return false;
  if (!IsNumeric(str, tokenlen))
    return false;

  const char* prevprev = 0;
  const char* prev = 0;
  int prevprevlen = 0;
  int prevlen = 0;

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return false;
  while (str)
  {
    if (prevprev)
    {
      for (int i = 0; i < prevprevlen; i++)
        if (prevprev[i] < 'A' || prevprev[i] > 'Z')
          return false;
    }
    prevprev = prev;
    prevprevlen = prevlen;
    prev = str;
    prevlen = tokenlen;
    str = GetNextToken(line, linelen, tokenlen, pos, 0);
  }
  if (!prev || !prevprev)
    return false;

  if (!IsNumeric(prev, prevlen) && (prevlen != 3 || memcmp(prev, "ANY", 3)))
    return false;

  if (!IsNumeric(prevprev, prevprevlen) && (prevprevlen != 3 || memcmp(prevprev, "ANY", 3)))
    return false;

  return true;
}

bool CFtpListResult::parseTime(const char *str, int len, t_directory::t_direntry::t_date &date) const
{
  int i = 0;
  //Extract the hour
  date.hastime = TRUE;
  date.hour = 0;
  while (str[i] != ':')
  {
    if (str[i] < '0' || str[i] > '9')
      return false;
    date.hour *= 10;
    date.hour += str[i] - '0';
    if (date.hour > 24)
      return false;

    i++;
    if (i == len)
      return false;
  }
  // Make sure we did read at least one digit
  if (!i)
    return false;

  i++;

  if (i == len)
    return false;

  //Extract the minute
  date.minute = 0;
  while (str[i] >= '0' && str[i] <= '9')
  {
    date.minute *= 10;
    date.minute += str[i] - '0';
    if (date.minute > 59)
      return false;

    i++;
    if (i == len)
      break;
  }

  //Convert to 24h format
  //I really wish we would have the following system:
  //one year->ten months->ten days->ten hours->ten minutes->ten seconds and so on...
  //I should modifiy the earth rotation to force everyone to use this system *g*
  if (i != len)
  {
    if (str[i]=='P')
    {
      if (date.hour < 12)
        date.hour += 12;
    }
    else
      if (date.hour == 12)
        date.hour = 0;
  }

  date.hastime = TRUE;

  return true;
}

BOOL CFtpListResult::parseAsWfFtp(const char *line, const int linelen, t_directory::t_direntry &direntry)
{
  int pos = 0;
  int tokenlen = 0;

  const char *str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  copyStr(direntry.name, 0, str, tokenlen);

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (!IsNumeric(str, tokenlen))
    return FALSE;

  direntry.size = strntoi64(str, tokenlen);

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (!ParseShortDate(str, tokenlen, direntry.date))
    return FALSE;

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (str[tokenlen - 1] != '.')
    return FALSE;

  str = GetNextToken(line, linelen, tokenlen, pos, 0);
  if (!str)
    return FALSE;

  if (!parseTime(str, tokenlen, direntry.date))
    return FALSE;

  direntry.bUnsure = FALSE;
  direntry.dir = false;
  direntry.bLink = false;
  direntry.permissionstr = L"";
  direntry.ownergroup = L"";

  return TRUE;
}

bool CFtpListResult::ParseSize(const char* str, int len, __int64 &size) const
{
  if (len < 1)
    return false;

  if (IsNumeric(str, len))
  {
    size = strntoi64(str, len);
    return true;
  }

  size = 0;
  char last = str[--len];

  int delimiter = -1;
  for (int i = 0; i < len; i++)
  {
    char c = str[i];
    if (c >= '0' && c <= '9')
    {
      size *= 10;
      size += c - '0';
    }
    else if (c == '.')
      delimiter = len - i;
    else
      return false;
  }

  // Check for digit before or after delimiterr
  if (!delimiter || delimiter == len)
    return false;


  switch (last)
  {
  case 'k':
  case 'K':
    size *= 1 << 10;
    break;
  case 'm':
  case 'M':
    size *= 1 << 20;
    break;
  case 'g':
  case 'G':
    size *= 1 << 30;
    break;
  case 't':
  case 'T':
    size *= (__int64)1 << 40;
    break;
  default:
    return false;
  }

  if (delimiter == -1)
    return true;

  while (delimiter--)
    size /= 10;

  return true;
}
