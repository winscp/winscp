//---------------------------------------------------------------------------
#ifndef FileMasksH
#define FileMasksH
//---------------------------------------------------------------------------
class TFileMasks
{
public:
  __fastcall TFileMasks();
  __fastcall TFileMasks(const TFileMasks & Source);
  __fastcall TFileMasks(const AnsiString AMasks);
  TFileMasks & __fastcall operator =(const TFileMasks & rhm);
  TFileMasks & __fastcall operator =(const char * rhs);
  TFileMasks & __fastcall operator =(const AnsiString rhs);
  bool __fastcall operator ==(const TFileMasks & rhm) const;
  bool __fastcall operator ==(const AnsiString rhs) const;
  bool __fastcall Matches(AnsiString FileName) const;
  bool __fastcall IsValid();
  bool __fastcall IsValid(int & Start, int & Length);

  __property AnsiString Masks = { read = FMasks, write = FMasks };

private:
  AnsiString FMasks;
};
//---------------------------------------------------------------------------
AnsiString __fastcall MaskFileName(AnsiString FileName, const AnsiString Mask);
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
  void __fastcall GetToken(const AnsiString & Command,
    int Index, int & Len, char & PatternCmd);
  void __fastcall CustomValidate(const AnsiString & Command, void * Arg);
  bool __fastcall FindPattern(const AnsiString & Command, char PatternCmd);

  virtual void __fastcall ValidatePattern(const AnsiString & Command,
    int Index, int Len, char PatternCmd, void * Arg);

  virtual int __fastcall PatternLen(int Index, char PatternCmd) = 0;
  virtual bool __fastcall PatternReplacement(int Index, const AnsiString & Pattern,
    AnsiString & Replacement) = 0;
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
    AnsiString & Replacement);

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

protected:
  virtual int __fastcall PatternLen(int Index, char PatternCmd);
  virtual bool __fastcall PatternReplacement(int Index, const AnsiString & Pattern,
    AnsiString & Replacement);

private:
  AnsiString FFileName;
  AnsiString FFileList;
};
//---------------------------------------------------------------------------
typedef TFileCustomCommand TRemoteCustomCommand;
//---------------------------------------------------------------------------
#endif
