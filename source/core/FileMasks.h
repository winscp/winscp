//---------------------------------------------------------------------------
#ifndef FileMasksH
#define FileMasksH
//---------------------------------------------------------------------------
#include <vector>
#include <Masks.hpp>
#include <Global.h>
//---------------------------------------------------------------------------
class EFileMasksException : public Exception
{
public:
  __fastcall EFileMasksException(UnicodeString Message, int ErrorStart, int ErrorLen);
  int ErrorStart;
  int ErrorLen;
};
//---------------------------------------------------------------------------
extern const wchar_t IncludeExcludeFileMasksDelimiter;
#define MASK_INDEX(DIRECTORY, INCLUDE) ((DIRECTORY ? 2 : 0) + (INCLUDE ? 0 : 1))
//---------------------------------------------------------------------------
class TFileMasks
{
public:
  struct TParams
  {
    TParams();
    __int64 Size;
    TDateTime Modification;

    UnicodeString ToString() const;
  };

  static bool __fastcall IsMask(const UnicodeString Mask);
  static UnicodeString EscapeMask(const UnicodeString & S);
  static UnicodeString __fastcall NormalizeMask(const UnicodeString & Mask, const UnicodeString & AnyMask = L"");
  static UnicodeString __fastcall ComposeMaskStr(
    TStrings * IncludeFileMasksStr, TStrings * ExcludeFileMasksStr,
    TStrings * IncludeDirectoryMasksStr, TStrings * ExcludeDirectoryMasksStr);
  static UnicodeString __fastcall ComposeMaskStr(TStrings * MasksStr, bool Directory);

  __fastcall TFileMasks();
  __fastcall TFileMasks(int ForceDirectoryMasks);
  __fastcall TFileMasks(const TFileMasks & Source);
  __fastcall TFileMasks(const UnicodeString & AMasks);
  __fastcall ~TFileMasks();
  TFileMasks & __fastcall operator =(const TFileMasks & rhm);
  TFileMasks & __fastcall operator =(const UnicodeString & rhs);
  bool __fastcall operator ==(const TFileMasks & rhm) const;
  bool __fastcall operator ==(const UnicodeString & rhs) const;

  void __fastcall SetMask(const UnicodeString & Mask);

  bool MatchesFileName(const UnicodeString & FileName, bool Directory = false, const TParams * Params = NULL) const;
  bool __fastcall Matches(const UnicodeString FileName, bool Local, bool Directory,
    const TParams * Params = NULL) const;
  bool __fastcall Matches(const UnicodeString FileName, bool Local, bool Directory,
    const TParams * Params, bool RecurseInclude, bool & ImplicitMatch) const;

  void SetRoots(const UnicodeString & LocalRoot, const UnicodeString & RemoteRoot);
  void SetRoots(TStrings * LocalFileList, const UnicodeString & RemoteRoot);
  void SetRoots(const UnicodeString & LocalRoot, TStrings * RemoteFileList);

  __property UnicodeString Masks = { read = FStr, write = SetMasks };
  __property bool NoImplicitMatchWithDirExcludeMask = { read = FNoImplicitMatchWithDirExcludeMask, write = FNoImplicitMatchWithDirExcludeMask };
  __property bool AllDirsAreImplicitlyIncluded = { read = FAllDirsAreImplicitlyIncluded, write = FAllDirsAreImplicitlyIncluded };

  __property TStrings * IncludeFileMasksStr = { read = GetMasksStr, index = MASK_INDEX(false, true) };
  __property TStrings * ExcludeFileMasksStr = { read = GetMasksStr, index = MASK_INDEX(false, false) };
  __property TStrings * IncludeDirectoryMasksStr = { read = GetMasksStr, index = MASK_INDEX(true, true) };
  __property TStrings * ExcludeDirectoryMasksStr = { read = GetMasksStr, index = MASK_INDEX(true, false) };

private:
  int FForceDirectoryMasks;
  UnicodeString FStr;
  bool FNoImplicitMatchWithDirExcludeMask;
  bool FAllDirsAreImplicitlyIncluded;
  bool FAnyRelative;
  UnicodeString FLocalRoot;
  UnicodeString FRemoteRoot;

  struct TMask
  {
    enum TKind { Any, NoExt, Regular };

    TKind FileNameMaskKind;
    Masks::TMask * FileNameMask;
    TKind DirectoryMaskKind;
    Masks::TMask * RemoteDirectoryMask;
    Masks::TMask * LocalDirectoryMask;

    enum TMaskBoundary { None, Open, Close };

    TMaskBoundary HighSizeMask;
    __int64 HighSize;
    TMaskBoundary LowSizeMask;
    __int64 LowSize;

    TMaskBoundary HighModificationMask;
    TDateTime HighModification;
    TMaskBoundary LowModificationMask;
    TDateTime LowModification;

    UnicodeString MaskStr;
    UnicodeString UserStr;
  };

  typedef std::vector<TMask> TMasks;
  TMasks FMasks[4];
  mutable TStrings * FMasksStr[4];

  void __fastcall SetStr(const UnicodeString value, bool SingleMask);
  void __fastcall SetMasks(const UnicodeString value);
  void __fastcall CreateMaskMask(
    const UnicodeString & Mask, int Start, int End, bool Ex, TMask::TKind & MaskKind, Masks::TMask *& MaskMask);
  void __fastcall CreateMask(const UnicodeString & MaskStr, int MaskStart,
    int MaskEnd, bool Include);
  TStrings * __fastcall GetMasksStr(int Index) const;
  static UnicodeString __fastcall MakeDirectoryMask(UnicodeString Str);
  inline void __fastcall Init();
  void __fastcall DoInit(bool Delete);
  void DoCopy(const TFileMasks & Source);
  void __fastcall Clear();
  static void __fastcall Clear(TMasks & Masks);
  static void __fastcall TrimEx(UnicodeString & Str, int & Start, int & End);
  static bool __fastcall MatchesMasks(
    const UnicodeString & FileName, bool Local, bool Directory,
    const UnicodeString & Path, const TParams * Params, const TMasks & Masks, bool Recurse);
  static inline bool MatchesMaskMask(TMask::TKind MaskKind, Masks::TMask * MaskMask, const UnicodeString & Str);
  static Masks::TMask * DoCreateMaskMask(const UnicodeString & Str);
  NORETURN void __fastcall ThrowError(int Start, int End);
  bool DoMatches(
    const UnicodeString & FileName, bool Local, bool Directory, const UnicodeString & Path, const TParams * Params,
    bool RecurseInclude, bool & ImplicitMatch) const;
};
//---------------------------------------------------------------------------
UnicodeString __fastcall MaskFileName(UnicodeString FileName, const UnicodeString Mask);
bool __fastcall IsFileNameMask(const UnicodeString & Mask);
bool __fastcall IsEffectiveFileNameMask(const UnicodeString & Mask);
UnicodeString __fastcall DelimitFileNameMask(UnicodeString Mask);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TCustomCommandPatternEvent)
  (int Index, const UnicodeString Pattern, void * Arg, UnicodeString & Replacement,
   bool & LastPass);
//---------------------------------------------------------------------------
class TCustomCommand
{
friend class TInteractiveCustomCommand;

public:
  TCustomCommand();
  // Needs an explicit virtual destructor, as is has virtual methods
  virtual ~TCustomCommand() {}

  UnicodeString __fastcall Complete(const UnicodeString & Command, bool LastPass);
  virtual void __fastcall Validate(const UnicodeString & Command);
  bool __fastcall HasAnyPatterns(const UnicodeString & Command);

  static UnicodeString __fastcall Escape(const UnicodeString & S);

protected:
  static const wchar_t NoQuote;
  static const UnicodeString Quotes;
  void __fastcall GetToken(const UnicodeString & Command,
    int Index, int & Len, wchar_t & PatternCmd);
  void __fastcall CustomValidate(const UnicodeString & Command, void * Arg);
  bool __fastcall FindPattern(const UnicodeString & Command, wchar_t PatternCmd);

  virtual void __fastcall ValidatePattern(const UnicodeString & Command,
    int Index, int Len, wchar_t PatternCmd, void * Arg);

  virtual int __fastcall PatternLen(const UnicodeString & Command, int Index) = 0;
  virtual void __fastcall PatternHint(int Index, const UnicodeString & Pattern);
  virtual bool __fastcall PatternReplacement(int Index, const UnicodeString & Pattern,
    UnicodeString & Replacement, bool & Delimit) = 0;
  virtual void __fastcall DelimitReplacement(UnicodeString & Replacement, wchar_t Quote);
};
//---------------------------------------------------------------------------
class TInteractiveCustomCommand : public TCustomCommand
{
public:
  TInteractiveCustomCommand(TCustomCommand * ChildCustomCommand);

protected:
  virtual void __fastcall Prompt(int Index, const UnicodeString & Prompt,
    UnicodeString & Value);
  virtual void __fastcall Execute(const UnicodeString & Command,
    UnicodeString & Value);
  virtual int __fastcall PatternLen(const UnicodeString & Command, int Index);
  virtual bool __fastcall PatternReplacement(int Index, const UnicodeString & Pattern,
    UnicodeString & Replacement, bool & Delimit);
  void __fastcall ParsePromptPattern(
    const UnicodeString & Pattern, UnicodeString & Prompt, UnicodeString & Default, bool & Delimit);
  bool __fastcall IsPromptPattern(const UnicodeString & Pattern);

private:
  TCustomCommand * FChildCustomCommand;
};
//---------------------------------------------------------------------------
class TTerminal;
class TSessionData;
struct TCustomCommandData
{
  __fastcall TCustomCommandData();
  __fastcall TCustomCommandData(TTerminal * Terminal);
  __fastcall TCustomCommandData(TSessionData * SessionData);
  __fastcall TCustomCommandData(
    TSessionData * SessionData, const UnicodeString & AUserName,
    const UnicodeString & Password);

  __property TSessionData * SessionData = { read = GetSessionData };

  void __fastcall operator=(const TCustomCommandData & Data);

private:
  std::unique_ptr<TSessionData> FSessionData;
  void __fastcall Init(TSessionData * ASessionData);
  void __fastcall Init(
    TSessionData * SessionData, const UnicodeString & AUserName,
    const UnicodeString & Password, const UnicodeString & HostKey);
  TSessionData * __fastcall GetSessionData() const;
};
//---------------------------------------------------------------------------
class TFileCustomCommand : public TCustomCommand
{
public:
  TFileCustomCommand();
  TFileCustomCommand(const TCustomCommandData & Data, const UnicodeString & Path);
  TFileCustomCommand(const TCustomCommandData & Data, const UnicodeString & Path,
    const UnicodeString & FileName, const UnicodeString & FileList);

  virtual void __fastcall Validate(const UnicodeString & Command);
  virtual void __fastcall ValidatePattern(const UnicodeString & Command,
    int Index, int Len, wchar_t PatternCmd, void * Arg);

  bool __fastcall IsFileListCommand(const UnicodeString & Command);
  virtual bool __fastcall IsFileCommand(const UnicodeString & Command);
  bool __fastcall IsRemoteFileCommand(const UnicodeString & Command);
  bool __fastcall IsSiteCommand(const UnicodeString & Command);
  bool __fastcall IsSessionCommand(const UnicodeString & Command);
  bool __fastcall IsPasswordCommand(const UnicodeString & Command);

protected:
  virtual int __fastcall PatternLen(const UnicodeString & Command, int Index);
  virtual bool __fastcall PatternReplacement(int Index, const UnicodeString & Pattern,
    UnicodeString & Replacement, bool & Delimit);

private:
  TCustomCommandData FData;
  UnicodeString FPath;
  UnicodeString FFileName;
  UnicodeString FFileList;
};
//---------------------------------------------------------------------------
typedef TFileCustomCommand TRemoteCustomCommand;
extern UnicodeString FileMasksDelimiters;
//---------------------------------------------------------------------------
#endif
