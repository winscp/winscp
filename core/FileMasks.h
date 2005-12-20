//---------------------------------------------------------------------------
#ifndef FileMasksH
#define FileMasksH
//---------------------------------------------------------------------------
class TFileMasks
{
public:
  static bool __fastcall IsMask(const AnsiString Mask);
  static bool __fastcall SingleMaskMatch(const AnsiString Mask, const AnsiString FileName);

  __fastcall TFileMasks();
  __fastcall TFileMasks(const TFileMasks & Source);
  __fastcall TFileMasks(const AnsiString AMasks);
  TFileMasks & __fastcall operator =(const TFileMasks & rhm);
  TFileMasks & __fastcall operator =(const char * rhs);
  TFileMasks & __fastcall operator =(const AnsiString rhs);
  bool __fastcall operator ==(const TFileMasks & rhm) const;
  bool __fastcall operator ==(const AnsiString rhs) const;
  bool __fastcall Matches(AnsiString FileName, bool Directory, AnsiString Path = "") const;
  bool __fastcall Matches(AnsiString FileName, bool Local, bool Directory) const;
  bool __fastcall IsValid();
  bool __fastcall IsValid(int & Start, int & Length);

  __property AnsiString Masks = { read = FMasks, write = FMasks };

private:
  AnsiString FMasks;

  static inline bool __fastcall MatchesFileMask(const AnsiString & Filename, const AnsiString & Mask);
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
class TFileCustomCommand : public TCustomCommand
{
public:
  TFileCustomCommand();
  TFileCustomCommand(const AnsiString & FileName, const AnsiString & FileList);

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
  AnsiString FFileName;
  AnsiString FFileList;
};
//---------------------------------------------------------------------------
typedef TFileCustomCommand TRemoteCustomCommand;
//---------------------------------------------------------------------------
#endif
