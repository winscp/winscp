//---------------------------------------------------------------------------
#ifndef FileMasksH
#define FileMasksH
//---------------------------------------------------------------------------
#include <vector>
#include <Masks.hpp>
//---------------------------------------------------------------------------
class EFileMasksException : public Exception
{
public:
  __fastcall EFileMasksException(AnsiString Message, int ErrorStart, int ErrorLen);
  int ErrorStart;
  int ErrorLen;
};
//---------------------------------------------------------------------------
class TFileMasks
{
public:
  struct TParams
  {
    TParams();
    __int64 Size;

    AnsiString ToString() const;
  };

  static bool __fastcall IsMask(const AnsiString Mask);
  static AnsiString __fastcall NormalizeMask(const AnsiString & Mask, const AnsiString & AnyMask = "");

  __fastcall TFileMasks();
  __fastcall TFileMasks(const TFileMasks & Source);
  __fastcall TFileMasks(const AnsiString & AMasks);
  __fastcall ~TFileMasks();
  TFileMasks & __fastcall operator =(const TFileMasks & rhm);
  TFileMasks & __fastcall operator =(const AnsiString & rhs);
  bool __fastcall operator ==(const TFileMasks & rhm) const;
  bool __fastcall operator ==(const AnsiString & rhs) const;

  void __fastcall SetMask(const AnsiString & Mask);
  void __fastcall Negate();

  bool __fastcall Matches(const AnsiString FileName, bool Directory = false,
    const AnsiString Path = "", const TParams * Params = NULL) const;
  bool __fastcall Matches(const AnsiString FileName, bool Local, bool Directory,
    const TParams * Params = NULL) const;

  __property AnsiString Masks = { read = FStr, write = SetMasks };

private:
  AnsiString FStr;

  struct TMaskMask
  {
    enum { Any, NoExt, Regular } Kind;
    TMask * Mask;
  };

  struct TMask
  {
    bool DirectoryOnly;
    TMaskMask FileNameMask;
    TMaskMask DirectoryMask;
    enum TSizeMask { None, Open, Close };
    TSizeMask HighSizeMask;
    __int64 HighSize;
    TSizeMask LowSizeMask;
    __int64 LowSize;
    AnsiString Str;
  };

  typedef std::vector<TMask> TMasks;
  TMasks FIncludeMasks;
  TMasks FExcludeMasks;

  void __fastcall SetStr(const AnsiString value, bool SingleMask);
  void __fastcall SetMasks(const AnsiString value);
  void __fastcall CreateMaskMask(const AnsiString & Mask, int Start, int End, bool Ex,
    TMaskMask & MaskMask);
  static inline void __fastcall ReleaseMaskMask(TMaskMask & MaskMask);
  inline void __fastcall Clear();
  static void __fastcall Clear(TMasks & Masks);
  static void __fastcall TrimEx(AnsiString & Str, int & Start, int & End);
  static bool __fastcall MatchesMasks(const AnsiString FileName, bool Directory,
    const AnsiString Path, const TParams * Params, const TMasks & Masks);
  static inline bool __fastcall MatchesMaskMask(const TMaskMask & MaskMask, const AnsiString & Str);
  static inline bool __fastcall IsAnyMask(const AnsiString & Mask);
  void __fastcall ThrowError(int Start, int End);
};
//---------------------------------------------------------------------------
AnsiString __fastcall MaskFileName(AnsiString FileName, const AnsiString Mask);
bool __fastcall IsFileNameMask(const AnsiString Mask);
AnsiString __fastcall DelimitFileNameMask(AnsiString Mask);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TCustomCommandPatternEvent)
  (int Index, const AnsiString Pattern, void * Arg, AnsiString & Replacement,
   bool & LastPass);
//---------------------------------------------------------------------------
class TCustomCommand
{
friend class TInteractiveCustomCommand;

public:
  TCustomCommand();

  AnsiString __fastcall Complete(const AnsiString & Command, bool LastPass);
  virtual void __fastcall Validate(const AnsiString & Command);

protected:
  static const char NoQuote;
  static const AnsiString Quotes;
  void __fastcall GetToken(const AnsiString & Command,
    int Index, int & Len, char & PatternCmd);
  void __fastcall CustomValidate(const AnsiString & Command, void * Arg);
  bool __fastcall FindPattern(const AnsiString & Command, char PatternCmd);

  virtual void __fastcall ValidatePattern(const AnsiString & Command,
    int Index, int Len, char PatternCmd, void * Arg);

  virtual int __fastcall PatternLen(int Index, char PatternCmd) = 0;
  virtual bool __fastcall PatternReplacement(int Index, const AnsiString & Pattern,
    AnsiString & Replacement, bool & Delimit) = 0;
  virtual void __fastcall DelimitReplacement(AnsiString & Replacement, char Quote);
};
//---------------------------------------------------------------------------
class TInteractiveCustomCommand : public TCustomCommand
{
public:
  TInteractiveCustomCommand(TCustomCommand * ChildCustomCommand);

protected:
  virtual void __fastcall Prompt(int Index, const AnsiString & Prompt,
    AnsiString & Value);
  virtual int __fastcall PatternLen(int Index, char PatternCmd);
  virtual bool __fastcall PatternReplacement(int Index, const AnsiString & Pattern,
    AnsiString & Replacement, bool & Delimit);

private:
  TCustomCommand * FChildCustomCommand;
};
//---------------------------------------------------------------------------
class TTerminal;
struct TCustomCommandData
{
  __fastcall TCustomCommandData();
  __fastcall TCustomCommandData(TTerminal * Terminal);

  AnsiString HostName;
  AnsiString UserName;
  AnsiString Password;
};
//---------------------------------------------------------------------------
class TFileCustomCommand : public TCustomCommand
{
public:
  TFileCustomCommand();
  TFileCustomCommand(const TCustomCommandData & Data, const AnsiString & Path);
  TFileCustomCommand(const TCustomCommandData & Data, const AnsiString & Path,
    const AnsiString & FileName, const AnsiString & FileList);

  virtual void __fastcall Validate(const AnsiString & Command);
  virtual void __fastcall ValidatePattern(const AnsiString & Command,
    int Index, int Len, char PatternCmd, void * Arg);

  bool __fastcall IsFileListCommand(const AnsiString & Command);
  virtual bool __fastcall IsFileCommand(const AnsiString & Command);

protected:
  virtual int __fastcall PatternLen(int Index, char PatternCmd);
  virtual bool __fastcall PatternReplacement(int Index, const AnsiString & Pattern,
    AnsiString & Replacement, bool & Delimit);

private:
  TCustomCommandData FData;
  AnsiString FPath;
  AnsiString FFileName;
  AnsiString FFileList;
};
//---------------------------------------------------------------------------
typedef TFileCustomCommand TRemoteCustomCommand;
//---------------------------------------------------------------------------
#endif
