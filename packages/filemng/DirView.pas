unit DirView;
{===============================================================
 Component TDirView / Version 2.6, January 2000
 ===============================================================


    Description:
    ============
    Displays files of a single directory as listview with shell
    icons. Complete drag&Drop support for files and directories.


    Author:
    =======
    (c) Ingo Eckel 1998, 1999
    Sodener Weg 38
    65812 Bad Soden
    Germany

    Modifications (for WinSCP):
    ===========================
    (c) Martin Prikryl 2001- 2004

    V2.6:
    - Shows "shared"-symbol with directories
    - New property ShowSubDirSize. Displays subdirectories sizes.
    - Delphi5 compatible

    For detailed documentation and history see TDirView.htm.

 ===============================================================}

{Required compiler options for TDirView:}
{$A+,B-,X+,H+,P+}

interface

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$DEFINE USE_DRIVEVIEW}

uses
  Windows, ShlObj, ComCtrls, CompThread, CustomDirView, ListExt,
  ExtCtrls, Graphics, FileOperator, DiscMon, Classes, DirViewColProperties,
  DragDrop, Messages, ListViewColProperties, CommCtrl, DragDropFilesEx,
  FileCtrl, SysUtils, BaseUtils;

{$I ResStrings.pas }

type
  TVolumeDisplayStyle = (doPrettyName, doDisplayName, doLongPrettyName); {Diplaytext of drive node}

const
{$IFNDEF NO_THREADS}
  msThreadChangeDelay = 10; {TDiscMonitor: change delay}
  MaxWaitTimeOut = 10; {TFileDeleteThread: wait nn seconds for deleting files or directories}
{$ENDIF}
  FileAttr = SysUtils.faAnyFile and (not SysUtils.faVolumeID);
  SpecialExtensions = 'EXE,LNK,ICO,ANI,CUR,PIF,JOB,CPL';
  ExeExtension = 'EXE';
  MinDate = $21;    {01.01.1980}
  MaxDate = $EF9F;  {31.12.2099}
  MinTime = 0;      {00:00:00}
  MaxTime = $C000;  {24:00:00}

type
  {Exceptions:}
{$IFNDEF NO_THREADS}
  EIUThread = class(Exception);
{$ENDIF}
  EDragDrop = class(Exception);
  EInvalidFileName = class(Exception);
  ERenameFileFailed = class(Exception);

  TDriveLetter = 'A'..'Z';

  TClipboardOperation = (cboNone, cboCut, cboCopy);
  TFileNameDisplay = (fndStored, fndCap, fndNoCap, fndNice);

  {Record for each file item:}
  PFileRec = ^TFileRec;
  TFileRec = record
    Empty: Boolean;
    IconEmpty: Boolean;
    IsDirectory: Boolean;
    IsRecycleBin: Boolean;
    IsParentDir: Boolean;
    FileName: string;
    Displayname: string;
    FileExt: string;
    TypeName: string;
    ImageIndex: Integer;
    Size: Int64;
    Attr: LongWord;
    FileTime: TFileTime;
    PIDL: PItemIDList; {Fully qualified PIDL}
  end;

  {Record for fileinfo caching:}
  PInfoCache = ^TInfoCache;
  TInfoCache = record
    FileExt: string;
    TypeName: ShortString;
    ImageIndex: Integer;
  end;

{$IFDEF VER120}
type
  TWMContextMenu = packed record
    Msg: Cardinal;
    hWnd: HWND;
    case Integer of
      0: (XPos: Smallint;
          YPos: Smallint);
      1: (Pos: TSmallPoint;
          Result: Longint);
  end;
{$ENDIF}

{Additional events:}
type
  TDirViewAddFileEvent = procedure(Sender: TObject; var SearchRec: SysUtils.TSearchRec;
    var AddFile : Boolean) of object;
  TDirViewFileSizeChanged = procedure(Sender: TObject; Item: TListItem) of object;
  TDirViewFileIconForName = procedure(Sender: TObject; Item: TListItem; var FileName: string) of object;

type
  TDirView = class;

{$IFNDEF NO_THREADS}
  TSubDirScanner = class(TCompThread)
  private
    FOwner: TDirView;
    FStartPath: string;
    FDirName: string;
    FTotalSize: Int64;

    procedure ThreadTerminated(Sender: TObject);

  protected
    constructor Create(Owner: TDirView; Item: TListItem);
    procedure DoUpdateItem;
    procedure Execute; override;
  end;

  {  TIconUpdateThread (Fetch shell icons via thread) }
  TIconUpdateThread = class(TCompThread)
  private
    FOwner: TDirView;
    FIndex: Integer;
    FMaxIndex: Integer;
    FNewIcons: Boolean;
    FSyncIcon: Integer;
    CurrentIndex: Integer;
    CurrentFilePath: string;
    CurrentItemData: TFileRec;
    InvalidItem: Boolean;

    procedure SetIndex(Value: Integer);
    procedure SetMaxIndex(Value: Integer);

  protected
    constructor Create(Owner: TDirView);
    procedure DoFetchData;
    procedure DoUpdateIcon;
    procedure Execute; override;

    property Index: Integer read FIndex write SetIndex;
    property MaxIndex: Integer read FMaxIndex write SetMaxIndex;

  public
    procedure Terminate; override;
  end;
{$ENDIF}

  { TDirView }
  TDirView = class(TCustomDirView)
  private
    FConfirmDelete: Boolean;
    FConfirmOverwrite: Boolean;
    FUseIconCache: Boolean;
    FInfoCacheList: TListExt;
    {$IFDEF USE_DRIVEVIEW}
    FDriveView: TObject;
    {$ENDIF}
    FChangeTimer: TTimer;
    FChangeInterval: Cardinal;
    FUseIconUpdateThread: Boolean;
    {$IFNDEF NO_THREADS}
    FIUThreadFinished: Boolean;
    {$ENDIF}
    FDriveType: Integer;
    FAttrSpace: string;
    FNoCheckDrives: string;
    FSortAfterUpdate: Boolean;
    FCompressedColor: TColor;
    FFileNameDisplay: TFileNameDisplay;
    FParentFolder: IShellFolder;
    FDesktopFolder: IShellFolder;
    FDirOK: Boolean;
    FPath: string;
    FDrawLinkOverlay: Boolean;
    SelectNewFiles: Boolean;
    FSelfDropDuplicates: Boolean;
    FHiddenCount: Integer;
    FFilteredCount: Integer;

    {File selection properties:}
    FSelArchive: TSelAttr;
    FSelHidden: TSelAttr;
    FSelSysFile: TSelAttr;
    FSelReadOnly: TSelAttr;
    FSelFileSizeFrom: Int64;
    FSelFileSizeTo: Int64;
    FSelFileDateFrom: Word;
    FSelFileDateTo: Word;
    FSelFileTimeFrom: Word;
    FSelFileTimeTo: Word;

    {shFileOperation-shell component TFileOperator:}
    FFileOperator: TFileOperator;
    {Additional thread components:}
    {$IFNDEF NO_THREADS}
    FIconUpdateThread: TIconUpdateThread;
    {$ENDIF}
    FDiscMonitor: TDiscMonitor;
    FHomeDirectory: string;
    FSubDirScanner: TList;

    {Additional events:}
    FOnAddFile: TDirViewAddFileEvent;
    FOnFileSizeChanged: TDirViewFileSizeChanged;
    FOnFileIconForName: TDirViewFileIconForName;
    FOnChangeDetected: TNotifyEvent;
    FOnChangeInvalid: TNotifyEvent;

    iRecycleFolder: iShellFolder;
    PIDLRecycle: PItemIDList;

    FLastPath: array[TDriveLetter] of string;

    {Drag&Drop:}
    function GetDirColProperties: TDirViewColProperties;
    function GetHomeDirectory: string;

    {Drag&drop helper functions:}
    {$IFNDEF NO_THREADS}
    procedure SignalFileDelete(Sender: TObject; Files: TStringList);
    {$ENDIF}
    procedure PerformDragDropFileOperation(TargetPath: string; dwEffect: Integer;
      RenameOnCollision: Boolean);
    procedure SetDirColProperties(Value: TDirViewColProperties);

  protected
    function NewColProperties: TCustomListViewColProperties; override;
    function SortAscendingByDefault(Index: Integer): Boolean; override;
    procedure SetShowSubDirSize(Value: Boolean); override;
    {$IFDEF USE_DRIVEVIEW}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ENDIF}

    procedure Delete(Item: TListItem); override;
    procedure DDError(ErrorNo: TDDError);
    function GetCanUndoCopyMove: Boolean; virtual;
    {Shell namespace functions:}
    function GetShellFolder(Dir: string): iShellFolder;
    function GetDirOK: Boolean; override;
    procedure GetDisplayInfo(ListItem: TListItem; var DispInfo: TLVItemA); override;

    procedure DDDragDetect(grfKeyState: Longint; DetectStart, Point: TPoint;
      DragStatus: TDragDetectStatus); override;
    procedure DDMenuDone(Sender: TObject; AMenu: HMenu); override;
    procedure DDDropHandlerSucceeded(Sender: TObject; grfKeyState: Longint;
      Point: TPoint; dwEffect: Longint); override;
    procedure DDChooseEffect(grfKeyState: Integer; var dwEffect: Integer); override;

    function GetPathName: string; override;
    procedure SetChangeInterval(Value: Cardinal); virtual;
    procedure LoadFromRecycleBin(Dir: string); virtual;
    procedure SetLoadEnabled(Value: Boolean); override;
    function GetPath: string; override;
    procedure SetPath(Value: string); override;
    procedure PathChanged; override;
    procedure SetItemImageIndex(Item: TListItem; Index: Integer); override;
    procedure SetCompressedColor(Value: TColor);
    procedure ChangeDetected(Sender: TObject; const Directory: string;
      var SubdirsChanged: Boolean);
    procedure ChangeInvalid(Sender: TObject; const Directory: string; const ErrorStr: string);
    procedure TimerOnTimer(Sender: TObject);
    procedure ResetItemImage(Index: Integer);
    procedure SetAttrSpace(Value: string);
    procedure SetNoCheckDrives(Value: string);
    procedure SetWatchForChanges(Value: Boolean); override;
    procedure AddParentDirItem;
    procedure AddToDragFileList(FileList: TFileList; Item: TListItem); override;
    procedure SetFileNameDisplay(Value: TFileNameDisplay); virtual;
    procedure DisplayContextMenu(Where: TPoint); override;
    function DragCompleteFileList: Boolean; override;
    procedure ExecuteFile(Item: TListItem); override;
    function GetIsRoot: Boolean; override;
    procedure InternalEdit(const HItem: TLVItem); override;
    function ItemColor(Item: TListItem): TColor; override;
    function ItemDisplayName(FileName: string): string; virtual;
    function ItemFileExt(Item: TListItem): string;
    function ItemFileNameOnly(Item: TListItem): string;
    function ItemFileTime(Item: TListItem; var Precision: TDateTimePrecision): TDateTime; override;
    function ItemImageIndex(Item: TListItem; Cache: Boolean): Integer; override;
    function ItemIsFile(Item: TListItem): Boolean; override;
    function ItemIsRecycleBin(Item: TListItem): Boolean; override;
    function ItemMatchesFilter(Item: TListItem; const Filter: TFileFilter): Boolean; override;
    function ItemOverlayIndexes(Item: TListItem): Word; override;
    procedure LoadFiles; override;
    function MinimizePath(Path: string; Len: Integer): string; override;
    procedure PerformItemDragDropOperation(Item: TListItem; Effect: Integer); override;
    procedure SortItems; override;
    {$IFNDEF NO_THREADS}
    procedure StartFileDeleteThread;
    {$ENDIF}
    procedure SetShowHiddenFiles(Value: Boolean); override;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    function SecondaryColumnHeader(Index: Integer; var AliasOnly: Boolean): Integer; override;
    function HiddenCount: Integer; override;
    function FilteredCount: Integer; override;

  public
    {Runtime, readonly properties:}
    property DriveType: Integer read FDriveType;
    {$IFDEF USE_DRIVEVIEW}
    {Linked component TDriveView:}
    property DriveView: TObject read FDriveView write FDriveView;
    {$ENDIF}
    {It is not required to store the items edited at designtime:}
    property Items stored False;
    { required, otherwise AV generated, when dragging columns}
    property Columns stored False;
    property ParentFolder: IShellFolder read FParentFolder;
    {Drag&Drop runtime, readonly properties:}
    property CanUndoCopyMove: Boolean read GetCanUndoCopyMove;
    property DDFileOperator: TFileOperator read FFileOperator;
    {Drag&Drop fileoperation methods:}
    function UndoCopyMove: Boolean; dynamic;
    {Clipboard fileoperation methods (requires drag&drop enabled):}
    procedure EmptyClipboard; dynamic;
    function CopyToClipBoard: Boolean; dynamic;
    function CutToClipBoard: Boolean; dynamic;
    function PasteFromClipBoard(TargetPath: string = ''): Boolean; override;
    function DuplicateSelectedFiles: Boolean; dynamic;
    procedure DisplayPropertiesMenu; override;

    procedure ExecuteParentDirectory; override;
    procedure ExecuteRootDirectory; override;
    function ItemIsDirectory(Item: TListItem): Boolean; override;
    function ItemFullFileName(Item: TListItem): string; override;
    function ItemIsParentDirectory(Item: TListItem): Boolean; override;
    function ItemFileName(Item: TListItem): string; override;
    function ItemFileSize(Item: TListItem): Int64; override;

    {$IFNDEF NO_THREADS}
    {Thread handling: }
    procedure StartWatchThread;
    procedure StopWatchThread;
    function WatchThreadActive: Boolean;
    procedure StartIconUpdateThread;
    procedure StopIconUpdateThread;
    procedure StartSubDirScanner;
    procedure StopSubDirScanner;
    procedure TerminateThreads;
    {$ENDIF}

    {Other additional functions: }
    procedure Syncronize;
    procedure ClearIconCache;

    {Create a new file:}
    function CreateFile(NewName: string): TListItem; dynamic;
    {Create a new subdirectory:}
    procedure CreateDirectory(DirName: string); override;
    {Delete all selected files:}
    function DeleteSelectedFiles(AllowUndo: Boolean): Boolean; dynamic;

    {Check, if file or files still exists:}
    procedure ValidateFile(Item: TListItem); overload;
    procedure ValidateFile(FileName:TFileName); overload;
    procedure ValidateSelectedFiles; dynamic;

    {Access the internal data-structures:}
    function AddItem(SRec: SysUtils.TSearchRec): TListItem; reintroduce;
    procedure GetDisplayData(Item: TListItem; FetchIcon: Boolean);
    function GetFileRec(Index: Integer): PFileRec;

    {Populate / repopulate the filelist:}
    procedure Load; override;
    procedure ReLoad(CacheIcons : Boolean); override;
    procedure Reload2;

    function FormatFileTime(FileTime: TFileTime): string; virtual;
    function GetAttrString(Attr: Integer): string; virtual;
    procedure FetchAllDisplayData;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteHomeDirectory; override;
    procedure ReloadDirectory; override;
    procedure ExecuteDrive(Drive: TDriveLetter);
    property HomeDirectory: string read GetHomeDirectory write FHomeDirectory;

    {Redefined functions: }
    {Properties for filtering files:}

    property SelArchive: TSelAttr
      read FSelArchive write FSelArchive default selDontCare;
    property SelHidden: TSelAttr
      read FSelHidden write FSelHidden default selDontCare;
    property SelSysFile: TSelAttr
      read FSelSysFile write FSelSysFile default selDontCare;
    property SelReadOnly: TSelAttr
      read FSelReadOnly write FSelReadOnly default selDontCare;
    property SelFileSizeFrom: Int64
      read FSelFileSizeFrom write FSelFileSizeFrom;
    property SelFileSizeTo: Int64
      read FSelFileSizeTo write FSelFileSizeTo default 0;
    property SelFileDateFrom: Word
      read FSelFileDateFrom write FSelFileDateFrom default MinDate; {01.01.1980}
    property SelFileDateTo: Word
      read FSelFileDateTo write FSelFileDateTo default MaxDate; {31.12.2099}
    property SelFileTimeFrom: Word
      read FSelFileTimeFrom write FSelFileTimeFrom;
    property SelFileTimeTo: Word
      read FSelFileTimeTo write FSelFileTimeTo default MaxTime;

  published
    property DirColProperties: TDirViewColProperties read GetDirColProperties write SetDirColProperties;
    property PathLabel;
    property OnUpdateStatusBar;
    property OnGetSelectFilter;
    property HeaderImages;

    property LoadAnimation;
    property DimmHiddenFiles;
    property ShowDirectories;
    property ShowHiddenFiles;
    property DirsOnTop;
    property ShowSubDirSize;
    property SingleClickToExec;
    property WantUseDragImages;
    property TargetPopupMenu;
    property AddParentDir;
    property OnSelectItem;
    property OnStartLoading;
    property OnLoaded;
    property OnDDDragEnter;
    property OnDDDragLeave;
    property OnDDDragOver;
    property OnDDDrop;
    property OnDDQueryContinueDrag;
    property OnDDGiveFeedback;
    property OnDDDragDetect;
    property OnDDCreateDragFileList;
    property OnDDEnd;
    property OnDDCreateDataObject;
    property OnDDTargetHasDropHandler;
    {Drag&Drop:}
    property DDLinkOnExeDrag default True;
    property OnDDProcessDropped;
    property OnDDError;
    property OnDDExecuted;
    property OnDDFileOperation;
    property OnDDFileOperationExecuted;
    property OnDDMenuPopup;

    property OnExecFile;
    property OnMatchMask;
    property OnGetOverlay;

    property CompressedColor: TColor
      read FCompressedColor write SetCompressedColor default clBlue;
    {Confirm deleting files}
    property ConfirmDelete: Boolean
      read FConfirmDelete write FConfirmDelete default True;
    {Confirm overwriting files}
    property ConfirmOverwrite: Boolean
      read FConfirmOverwrite write fConfirmOverwrite default True;
    property SortAfterUpdate: Boolean
      read FSortAfterUpdate write FSortAfterUpdate default True;
    {Reload the directory after only the interval:}
    property ChangeInterval: Cardinal
      read FChangeInterval write SetChangeInterval default 1000;
    {Fetch shell icons by thread:}
    property UseIconUpdateThread: Boolean
      read FUseIconUpdateThread write FUseIconUpdateThread default False;
    {Enables or disables icon caching for registered file extensions. Caching enabled
     enhances the performance but does not take care about installed icon handlers, wich
     may modify the display icon for registered files. Only the iconindex is cached not the
     icon itself:}
    property UseIconCache: Boolean
      read FUseIconCache write FUseIconCache default False;
    property FileNameDisplay: TFileNameDisplay
      read FFileNameDisplay write SetFileNameDisplay default fndStored;
    {Use this string as whitespace in the attribute column:}
    property AttrSpace: string read FAttrSpace write SetAttrSpace;
    {Don't watch these drives for changes:}
    property NoCheckDrives: string read FNoCheckDrives write SetNoCheckDrives;
    {Watch current directory for filename changes (create, rename, delete files)}
    property WatchForChanges;
    property SelfDropDuplicates: Boolean
      read FSelfDropDuplicates write FSelfDropDuplicates default False;

    {Additional events:}
    {The watchthread has detected new, renamed or deleted files}
{$IFNDEF NO_THREADS}
    property OnChangeDetected: TNotifyEvent
      read FOnChangeDetected write FOnChangeDetected;
    {The watchthread can't watch the current directory. Occurs on novell
     network drives.}
    property OnChangeInvalid: TNotifyEvent
      read FOnChangeInvalid write FOnChangeInvalid;
{$ENDIF}
    {Set AddFile to false, if actual file should not be added to the filelist:}
    property OnAddFile: TDirViewAddFileEvent
      read FOnAddFile write FOnAddFile;
    property OnFileSizeChanged: TDirViewFileSizeChanged
      read FOnFileSizeChanged write FOnFileSizeChanged;
    property OnFileIconForName: TDirViewFileIconForName
      read FOnFileIconForName write FOnFileIconForName;
    property UseSystemContextMenu;
    property OnContextPopup;
    property OnBeginRename;
    property OnEndRename;
    property OnHistoryChange;
    property OnPathChange;

    property ColumnClick;
    property MultiSelect;
    property ReadOnly;
  end; {Type TDirView}

procedure Register;

{Returns True, if the specified extension matches one of the extensions in ExtList:}
function MatchesFileExt(Ext: string; const FileExtList: string): Boolean;

var
  LastClipBoardOperation: TClipBoardOperation;
  LastIOResult: DWORD;

implementation

uses
{$IFDEF USE_DRIVEVIEW}
  DriveView,
{$ENDIF}
  PIDL, Forms, Dialogs, Controls,
  ShellAPI, ComObj,
  ActiveX, ImgList,
  ShellDialogs, IEDriveInfo,
  FileChanges, Math;

var
  DaylightHack: Boolean;

procedure Register;
begin
  RegisterComponents('DriveDir', [TDirView]);
end; {Register}

function CompareInfoCacheItems(I1, I2: Pointer): Integer;
begin
  if PInfoCache(I1)^.FileExt < PInfoCache(I2)^.FileExt then Result := fLess
    else
  if PInfoCache(I1)^.FileExt > PInfoCache(I2)^.FileExt then Result := fGreater
    else Result := fEqual;
end; {CompareInfoCacheItems}

function MatchesFileExt(Ext: string; const FileExtList: string): Boolean;
begin
  Result := (Length(Ext) = 3) and (Pos(Ext, FileExtList) <> 0);
end; {MatchesFileExt}

function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
var
  SysTime: TSystemTime;
  UniverzalSysTime: TSystemTime;
  LocalFileTime: TFileTime;
begin
  // duplicated in Common.cpp
  if not DaylightHack then
  begin
    FileTimeToSystemTime(FileTime, UniverzalSysTime);
    SystemTimeToTzSpecificLocalTime(nil, UniverzalSysTime, SysTime);
  end
    else
  begin
    FileTimeToLocalFileTime(FileTime, LocalFileTime);
    FileTimeToSystemTime(LocalFileTime, SysTime);
  end;
  Result := SystemTimeToDateTime(SysTime);
end;

function SizeFromSRec(const SRec: SysUtils.TSearchRec): Int64;
begin
  with SRec do
  begin
    // Hopefuly TSearchRec.FindData is available with all Windows versions
    {if Size >= 0 then Result := Size
      else}
{$WARNINGS OFF}
    Result := Int64(FindData.nFileSizeHigh) shl 32 + FindData.nFileSizeLow;
{$WARNINGS ON}
  end;
end;

{$IFNDEF NO_THREADS}

{ TSubDirScanner }

constructor TSubDirScanner.Create(Owner: TDirView; Item: TListItem);
begin
  inherited Create(True);
  FOwner := Owner;
  FTotalSize := 0;
  FStartPath := FOwner.ItemFullFileName(Item);
  FDirName := Item.Caption;
  FreeOnTerminate := False;
  OnTerminate := ThreadTerminated;
  Priority := tpLower;
  Resume;
end; {Create}

procedure TSubDirScanner.Execute;

  function ScanSubDir(Path: string): Boolean;
  var
    SRec: SysUtils.TSearchRec;
    DosError: Integer;
    SubDirs: TStringList;
    Index: Integer;
    FSize: Int64;
  begin
    Result := True;
    DosError := FindFirst(Path + '*.*', faAnyFile, SRec);
    if DosError = 0 then
    begin
      SubDirs := TStringList.Create;
      try
        while DosError = 0 do
        begin
          if Terminated then
            Break;

          if (SRec.Name <> '.') and (SRec.name <> '..') then
          begin
            FSize := SizeFromSRec(SRec);
            if FSize > 0 then
              Inc(FTotalSize, FSize);

            if SRec.Attr and faDirectory <> 0 then
              SubDirs.Add(IncludeTrailingPathDelimiter(Path + Srec.Name));
          end;
          if not Terminated then DosError := FindNext(SRec)
            else Break;
        end; {While}
        FindClose(SRec);
      finally
        try
          for Index := 0 to SubDirs.Count - 1 do
          begin
            Result := ScanSubDir(SubDirs[Index]);
            if not Result then Break;
          end;
        finally
          SubDirs.Free;
          if Result then
            Result := (DosError = ERROR_NO_MORE_FILES);
        end;
      end;
    end;
  end; {ScanSubDir}

begin {Execute}
  if ScanSubDir(IncludeTrailingPathDelimiter(FStartPath)) and not Terminated then
    Synchronize(DoUpdateItem);
end; {Execute}

procedure TSubDirScanner.DoUpdateItem;
var
  Item: TListItem;
  StartPos: Integer;
begin
  if not Terminated then
  begin
    StartPos := 0;
    Item := nil;
    while StartPos < FOwner.Items.Count do
    begin
      Item := FOwner.FindCaption(StartPos, FDirName, False, True, False);
      if Assigned(Item) and (FOwner.ItemFullFileName(Item) = FStartPath) then
        Break
        else
      if not Assigned(Item) then Break
        else StartPos := Item.Index + 1;
    end;

    if Assigned(Item) and not Terminated then
    begin
      PFileRec(Item.Data)^.Size := FTotalSize;
      Inc(FOwner.FFilesSize, FTotalSize);
      if Item.Selected then
        Inc(FOwner.FFilesSelSize, FTotalSize);
      FOwner.UpdateItems(Item.Index, Item.Index);
      if Assigned(FOwner.OnFileSizeChanged) then
        FOwner.OnFileSizeChanged(FOwner, Item);
    end;
  end;
end; {DoUpdateItem}

procedure TSubDirScanner.ThreadTerminated(Sender: TObject);
var
  Index: Integer;
begin
  Assert(Assigned(FOwner));
  with FOwner do
    for Index := 0 to FSubDirScanner.Count - 1 do
      if FSubDirScanner[Index] = Self then
      begin
        try
          FSubDirScanner.Delete(Index);
          if (FSubDirScanner.Count = 0) and
             (FOwner.SortColumn = Integer(dvSize)) and
             not Loading then FOwner.SortItems;
        finally
          inherited Destroy;
        end;
        Exit;
      end;

  Assert(False, 'TSubDirScanner failed: ' + FStartPath);
  inherited Destroy;
end; {ThreadTerminated}

{ TIconUpdateThread }

constructor TIconUpdateThread.Create(Owner: TDirView);
begin
  inherited Create(True);
  FOwner := Owner;
  FIndex := 0;
  FNewIcons := False;
  if (FOwner.ViewStyle = vsReport) or (FOwner.ViewStyle = vsList) then
    FMaxIndex := FOwner.VisibleRowCount
      else FMaxIndex := 0;
  FOwner.FIUThreadFinished := False;
end; {TIconUpdateThread.Create}

procedure TIconUpdateThread.SetMaxIndex(Value: Integer);
var
  Point: TPoint;
  Item: TListItem;
begin
  if Value <> MaxIndex then
  begin
    FNewIcons := True;
    if Value < FMaxIndex then
    begin
      if Suspended then FIndex := Value
        else
      begin
        Point.X := 0;
        Point.X := 0;
        Item := FOwner.GetNearestItem(Point, TSearchDirection(sdAbove));
        if Assigned(Item) then FIndex := Item.Index
          else FIndex := Value;
      end;
    end
      else FMaxIndex := Value;
  end;
end; {SetMaxIndex}

procedure TIconUpdateThread.SetIndex(Value: Integer);
var
  PageSize: Integer;
begin
  if Value <> Index then
  begin
    PageSize := FOwner.VisibleRowCount;
    FIndex := Value;
    FNewIcons := True;
    if FOwner.ViewStyle = vsList then FMaxIndex := Value + 2 * PageSize
      else FMaxIndex := Value + PageSize;
  end;
end; {SetIndex}

procedure TIconUpdateThread.Execute;
var
  FileInfo: TShFileInfo;
  Count: Integer;
  WStr: WideString;
  Eaten: ULONG;
  ShAttr: ULONG;
  FileIconForName: string;
  ForceByName: Boolean;
begin
  if Assigned(FOwner.TopItem) then FIndex := FOwner.TopItem.Index
    else FIndex := 0;

  FNewIcons := (FIndex > 0);

  while not Terminated do
  begin
    if FIndex > FMaxIndex then Suspend;
    Count := FOwner.Items.Count;
    if not Terminated and ((FIndex >= Count) or (Count = 0)) then
      Suspend;
    InvalidItem := True;
    if Terminated then Break;
    Synchronize(DoFetchData);
    if (not InvalidItem) and (not Terminated) and
      CurrentItemData.IconEmpty then
    begin
      try
        ForceByName := False;
        FileIconForName := CurrentFilePath;
        if Assigned(FOwner.FOnFileIconForName) then
        begin
          FOwner.FOnFileIconForName(FOwner, nil, FileIconForName);
          ForceByName := (FileIconForName <> CurrentFilePath);
        end;

        if not Assigned(CurrentItemData.PIDL) then
        begin
          WStr := CurrentFilePath;
          FOwner.FDesktopFolder.ParseDisplayName(FOwner.ParentForm.Handle, nil,
            PWideChar(WStr), Eaten, CurrentItemData.PIDL, ShAttr);
        end;

        if (not ForceByName) and Assigned(CurrentItemData.PIDL) then
          shGetFileInfo(PChar(CurrentItemData.PIDL), 0, FileInfo, SizeOf(FileInfo),
            SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_PIDL)
        else
          shGetFileInfo(PChar(FileIconForName), 0, FileInfo, SizeOf(FileInfo),
            SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX);

      except
        {Capture exceptions generated by the shell}
        FSyncIcon := UnKnownFileIcon;
      end;
      if Terminated then
      begin
        FreePIDL(CurrentItemData.PIDL);
        Break;
      end;
      FSyncIcon := FileInfo.iIcon;
      if FSyncIcon <> CurrentItemData.ImageIndex then
        FNewIcons := True;
      if not Terminated then
        Synchronize(DoUpdateIcon);

      FreePIDL(CurrentItemData.PIDL);
    end;
    SetLength(CurrentFilePath, 0);
    if CurrentIndex = FIndex then Inc(FIndex);
    SetLength(CurrentFilePath, 0);
  end;
end; {TIconUpdateThread.Execute}

procedure TIconUpdateThread.DoFetchData;
begin
  CurrentIndex := fIndex;
  if not Terminated and
     (Pred(FOwner.Items.Count) >= CurrentIndex) and
     Assigned(FOwner.Items[CurrentIndex]) and
     Assigned(FOwner.Items[CurrentIndex].Data) then
  begin
    CurrentFilePath := FOwner.ItemFullFileName(FOwner.Items[CurrentIndex]);
    CurrentItemData := PFileRec(FOwner.Items[CurrentIndex].Data)^;
    InvalidItem := False;
  end
    else InvalidItem := True;
end; {TIconUpdateThread.DoFetchData}

procedure TIconUpdateThread.DoUpdateIcon;
var
  LVI: TLVItem;
begin
  if (FOwner.Items.Count > CurrentIndex) and
     not fOwner.Loading and not Terminated and
     Assigned(FOwner.Items[CurrentIndex]) and
     Assigned(FOwner.Items[CurrentIndex].Data) then
    with FOwner.Items[CurrentIndex] do
    begin
      if (FSyncIcon >= 0) and (PFileRec(Data)^.ImageIndex <> FSyncIcon) then
      begin
        with PFileRec(Data)^ do
          ImageIndex := FSyncIcon;

        {To avoid flickering of the display use Listview_SetItem
        instead of using the property ImageIndex:}
        LVI.mask := LVIF_IMAGE;
        LVI.iItem := CurrentIndex;
        LVI.iSubItem := 0;
        LVI.iImage := I_IMAGECALLBACK;
        if not Terminated then
          ListView_SetItem(FOwner.Handle, LVI);
        FNewIcons := True;
      end;
      PFileRec(Data)^.IconEmpty := False;
    end;
end; {TIconUpdateThread.DoUpdateIcon}

procedure TIconUpdateThread.Terminate;
begin
  FOwner.FIUThreadFinished := True;
  inherited;
end; {TIconUpdateThread.Terminate}

{$ENDIF} // NO_THREADS

{ TDirView }

constructor TDirView.Create(AOwner: TComponent);
var
  D: TDriveLetter;
begin
  inherited Create(AOwner);

  FInfoCacheList := TListExt.Create(SizeOf(TInfoCache));
  FDriveType := DRIVE_UNKNOWN;
  FUseIconCache := False;
  FConfirmDelete := True;
  FAttrSpace := EmptyStr;
  FSortAfterUpdate := True;
  FCompressedColor := clBlue;
  FFileNameDisplay := fndStored;
  FParentFolder := nil;
  FDesktopFolder := nil;
  SelectNewFiles := False;
  FDrawLinkOverlay := True;
  DragOnDriveIsMove := True;
  FSelfDropDuplicates := False;
  FHiddenCount := 0;
  FFilteredCount := 0;

  FFileOperator := TFileOperator.Create(Self);
  FFileOperator.ProgressTitle := coFileOperatorTitle;
  FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];
  FDirOK := True;
  FPath := '';

  FDiscMonitor := nil;
  FSubDirScanner := TList.Create;

  {ChangeTimer: }
  if FChangeInterval = 0 then FChangeInterval := 1000;
  FChangeTimer := TTimer.Create(Self);
  FChangeTimer.Interval := FChangeInterval;
  FChangeTimer.Enabled := False;
  FChangeTimer.OnTimer := TimerOnTimer;

  FSelArchive := selDontCare;
  FSelHidden := selDontCare;
  FSelReadOnly := selDontCare;
  FSelSysFile := selDontCare;
  FSelFileSizeTo := 0;
  FSelFileDateFrom := MinDate;
  FSelFileDateTo := MaxDate;
  FSelFileTimeTo := MaxTime;

  {Drag&drop:}
  FConfirmOverwrite := True;
  DDLinkOnExeDrag := True;

  with DragDropFilesEx do
  begin
    SourceEffects := DragSourceEffects;
    TargetEffects := [deCopy, deMove, deLink];

    ShellExtensions.DragDropHandler := True;
    ShellExtensions.DropHandler := True;
  end;

  for D := Low(FLastPath) to High(FLastPath) do
    FLastPath[D] := '';
end; {Create}

destructor TDirView.Destroy;
begin
  FSubDirScanner.Free;

  if Assigned(PIDLRecycle) then FreePIDL(PIDLRecycle);

  FInfoCacheList.Free;
  FFileOperator.Free;
  FChangeTimer.Free;

  inherited Destroy;
end; {Destroy}

procedure TDirView.WMDestroy(var Msg: TWMDestroy);
begin
  Selected := nil;
  ClearItems;
{$IFNDEF NO_THREADS}
  TerminateThreads;
{$ENDIF}
  inherited;
end; {WMDestroy}

{$IFNDEF NO_THREADS}

procedure TDirView.TerminateThreads;
begin
  StopSubDirScanner;
  StopIconUpdateThread;
  StopWatchThread;
end; {TerminateThreads}

{$ENDIF}

function TDirView.GetHomeDirectory: string;
begin
  if FHomeDirectory <> '' then Result := FHomeDirectory
    else
  begin
    Result := UserDocumentDirectory;
    if IsUNCPath(Result) then
      Result := AnyValidPath;
  end;
end; { GetHomeDirectory }

function TDirView.GetIsRoot: Boolean;
begin
  Result := (Length(Path) = 2) and (Path[2] = ':');
end;

function TDirView.GetPath: string;
begin
  Result := FPath;
end;

procedure TDirView.PathChanged;
var
  Expanded: string;
begin
  inherited;

  // make sure to use PathName as Path maybe just X: what
  // ExpandFileName resolves to current working directory
  // on the drive, not to root path
  Expanded := ExpandFileName(PathName);
  Assert(Pos(':', Expanded) = 2);
  FLastPath[UpCase(Expanded[1])] := Expanded;
end;

procedure TDirView.SetPath(Value: string);
begin
  // do checks before passing directory to drive view, because
  // it would truncate non-existing directory to first superior existing
  Value := StringReplace(Value, '/', '\', [rfReplaceAll]);

  if IsUncPath(Value) then
    raise Exception.CreateFmt(SUcpPathsNotSupported, [Value]);
  if not DirectoryExists(Value) then
    raise Exception.CreateFmt(SDirNotExists, [Value]);

  if Assigned(FDriveView) and
     (TDriveView(FDriveView).Directory <> Value) then
  begin
    TDriveView(FDriveView).Directory := Value;
  end
    else
  if FPath <> Value then
  try
    while (Length(Value) > 0) and (Value[Length(Value)] = '\') do
      SetLength(Value, Length(Value) - 1);
    PathChanging(True);
    FPath := Value;
    Load;
  finally
    PathChanged;
  end;
end;

procedure TDirView.SetLoadEnabled(Value: Boolean);
begin
  if Value <> LoadEnabled then
  begin
    FLoadEnabled := Enabled;
    if LoadEnabled and Dirty then
    begin
      if Items.Count > 100 then Reload2
        else Reload(True);
    end;
  end;
end; {SetLoadEnabled}

procedure TDirView.SetShowHiddenFiles(Value: Boolean);
begin
  if Value <> ShowHiddenFiles then
  begin
    if Value then FSelHidden := selDontCare
      else FSelHidden := selNo;
    inherited;
  end;
end;

procedure TDirView.SetCompressedColor(Value: TColor);
begin
  if Value <> CompressedColor then
  begin
    FCompressedColor := Value;
    Invalidate;
  end;
end; {SetCompressedColor}

function TDirView.GetPathName: string;
begin
  if (Length(Path) = 2) and (Path[2] = ':') then Result := Path + '\'
    else Result := Path;
end; {GetPathName}

function TDirView.GetFileRec(Index: Integer): PFileRec;
begin
  if Index > Pred(Items.Count) then Result := nil
    else Result := Items[index].Data;
end; {GetFileRec}

function TDirView.ItemDisplayName(FileName: string): string;
begin
  case FFileNameDisplay of
    fndCap: Result := UpperCase(FileName);
    fndNoCap: Result := LowerCase(FileName);
    fndNice:
      if (Length(FileName) > 12) or (Pos(' ', FileName) <> 0) then
          Result := FileName
        else
      begin
        Result := LowerCase(FileName);
        Result[1] := Upcase(Result[1]);
      end;
    else
      Result := FileName;
  end; {Case}
end; {ItemDisplayName}

function TDirView.SecondaryColumnHeader(Index: Integer; var AliasOnly: Boolean): Integer;
begin
  if Index = Integer(dvName) then Result := Integer(dvExt)
    else Result := -1;
  AliasOnly := False;
end;

function TDirView.HiddenCount: Integer;
begin
  Result := FHiddenCount;
end;

function TDirView.FilteredCount: Integer;
begin
  Result := FFilteredCount;
end;

function TDirView.AddItem(SRec: SysUtils.TSearchRec): TListItem;
var
  PItem: PFileRec;
  Item: TListItem;
begin
  Item := Items.Add;
  New(PItem);
  with PItem^ do
  begin
    // must be set as soon as possible, at least before Caption is set,
    // because if come column is "autosized" setting Caption invokes some callbacks
    Item.Data := PItem;

    FileName := SRec.Name;
    FileExt := UpperCase(ExtractFileExt(Srec.Name));
    FileExt := Copy(FileExt, 2, Length(FileExt) - 1);
    DisplayName := ItemDisplayName(FileName);
{$WARNINGS OFF}
    Attr := SRec.FindData.dwFileAttributes;
{$WARNINGS ON}
    IsParentDir := False;
    IsDirectory := ((Attr and SysUtils.faDirectory) <> 0);
    IsRecycleBin := IsDirectory and (Length(Path) = 2) and
      Bool(Attr and SysUtils.faSysFile) and
      ((UpperCase(FileName) = 'RECYCLED') or (UpperCase(FileName) = 'RECYCLER'));

    if not IsDirectory then Size := SizeFromSRec(SRec)
      else Size := -1;

    if not Self.IsRecycleBin then Item.Caption := SRec.Name;

{$WARNINGS OFF}
    FileTime := SRec.FindData.ftLastWriteTime;
{$WARNINGS ON}
    Empty := True;
    IconEmpty := True;
    if Size > 0 then Inc(FFilesSize, Size);
    PIDL := nil;

    if FileExt = 'LNK' then Item.OverlayIndex := 1;
  end;
  if SelectNewFiles then Item.Selected := True;

  Result := Item;
end; {AddItem}

procedure TDirView.AddParentDirItem;
var
  PItem: PFileRec;
  Item: TListItem;
  SRec: SysUtils.TSearchRec;
begin
  FHasParentDir := True;
  Item := Items.Add;
  New(PItem);
  if FindFirst(FPath, faAnyFile, SRec) = 0 then
    FindClose(SRec);
  with PItem^ do
  begin
    Item.Data := PItem;
    FileName := '..';
    FileExt := '';
    DisplayName := '..';
    Attr := SRec.Attr;
    IsDirectory := True;
    IsRecycleBin := False;
    IsParentDir := True;
    Size := -1;

    Item.Caption := '..';

{$WARNINGS OFF}
    FileTime := SRec.FindData.ftLastWriteTime;
{$WARNINGS ON}
    Empty := True;
    IconEmpty := False;
    PIDL := nil;

    if HasExtendedCOMCTL32 then ImageIndex := StdDirIcon
      else ImageIndex  := StdDirSelIcon;

    TypeName := SParentDir;
    Empty := False;
  end;
end; {AddParentDirItem}

procedure TDirView.LoadFromRecycleBin(Dir: string);
var
  PIDLRecycleLocal: PItemIDList;
  PCurrList: PItemIDList;
  FQPIDL: PItemIDList;
  EnumList: IEnumIDList;
  Fetched: ULONG;
  SRec: SysUtils.TSearchRec;
  DisplayName: string;
  FullPath: string;
  NewItem: TListItem;
  FileRec: PFileRec;
  FileInfo: TSHFileInfo;
  FileSel: Boolean;
  DosError: Integer;
  AttrIncludeMask: Integer;
  AttrExcludeMask: Integer;
  FileTimeFrom: LongWord;
  FileTimeTo: LongWord;
  FSize: Int64;

  procedure AddToMasks(Attr: TSelAttr; Mask: Word);
  begin
    case Attr of
      selYes: AttrIncludeMask := AttrIncludeMask or Mask;
      selNo:  AttrExcludeMask := AttrExcludeMask or Mask;
    end;
  end;

begin
  if not Assigned(iRecycleFolder) then
  begin
    PIDLRecycleLocal := nil;
    try
      OLECheck(shGetSpecialFolderLocation(Self.Handle,
        CSIDL_BITBUCKET, PIDLRecycleLocal));
      PIDLRecycle := PIDL_Concatenate(nil, PIDLRecycleLocal);

      if not SUCCEEDED(FDesktopFolder.BindToObject(PIDLRecycle, nil,
        IID_IShellFolder, Pointer(iRecycleFolder))) then Exit;
    finally
      if Assigned(PIDLRecycleLocal) then
        FreePIDL(PIDLRecycleLocal);
    end;
  end;
  FParentFolder := iRecycleFolder;

  if AddParentDir then AddParentDirItem;

  FHiddenCount := 0;
  FFilteredCount := 0;

  AttrIncludeMask := 0;
  AttrExcludeMask := 0;
  AddToMasks(FSelArchive, SysUtils.faArchive);
  AddToMasks(FSelHidden, SysUtils.faHidden);
  AddToMasks(FSelReadOnly, SysUtils.faReadOnly);
  AddToMasks(FSelSysFile, SysUtils.faSysFile);

  FileTimeFrom := LongWord(FSelFileDateFrom) shl 16 or FSelFileTimeFrom;
  FileTimeTo   := LongWord(FSelFileDateTo)   shl 16 or FSelFileTimeTo;

  if SUCCEEDED(iRecycleFolder.EnumObjects(Self.Handle,
    SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, EnumList)) then
  begin
     while (EnumList.Next(1, PCurrList, Fetched) = S_OK) and not AbortLoading do
     begin
       if Assigned(PCurrList) then
       try
         FQPIDL := PIDL_Concatenate(PIDLRecycle, PCurrList);

         {Physical filename:}
         SetLength(FullPath, MAX_PATH);
         if shGetPathFromIDList(FQPIDL, PChar(FullPath)) then
           SetLength(FullPath, StrLen(PChar(FullPath)));

         {Filesize, attributes and -date:}
         DosError := FindFirst(FullPath, faAnyFile, SRec);
         FindClose(Srec);
         SRec.Name := ExtractFilePath(FullPath) + SRec.Name;

         {Displayname:}
         GetShellDisplayName(iRecycleFolder, PCurrList, SHGDN_FORPARSING, DisplayName);

         FileSel := (DosError = 0);
         if FileSel and not (Bool(SRec.Attr and faDirectory)) then
         begin
           if (AttrIncludeMask <> 0) then
             FileSel := Srec.Attr and AttrIncludeMask >= AttrIncludeMask;
           if FileSel and (AttrExcludeMask <> 0) then
              FileSel := AttrExcludeMask and Srec.Attr = 0;

           FSize := SizeFromSRec(SRec);
           FileSel :=
            FileSel and
            ((Mask = '') or FileNameMatchesMasks(DisplayName, False, FSize, Mask)) and
            (FSize >= FSelFileSizeFrom) and
            ((FSelFileSizeTo = 0) or
             (FSize <= FSelFileSizeTo)) and
            (LongWord(SRec.Time) >= FileTimeFrom) and
            (LongWord(SRec.Time) <= FileTimeTo);
         end;

         if Assigned(FOnAddFile) then
           FOnAddFile(Self, SRec, FileSel);

         if FileSel then
         begin
           {Filetype and icon:}
           SHGetFileInfo(PChar(FQPIDL), 0, FileInfo, SizeOf(FileInfo),
            SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_SYSICONINDEX);

           NewItem := AddItem(Srec);
           NewItem.Caption := DisplayName;
           FileRec := NewItem.Data;
           FileRec^.Empty := False;
           FileRec^.IconEmpty := False;
           FileRec^.DisplayName := DisplayName;
           FileRec^.PIDL := FQPIDL;
           FileRec^.TypeName := FileInfo.szTypeName;
           if FileRec^.Typename = EmptyStr then
             FileRec^.TypeName := Format(STextFileExt, [FileRec.FileExt]);
           FileRec^.ImageIndex := FileInfo.iIcon;

{$IFNDEF NO_THREADS}
           if ShowSubDirSize and FileRec^.isDirectory then
             FSubDirScanner.Add(TSubDirScanner.Create(Self, NewItem));
{$ENDIF}
         end
           else
         begin
           if (AttrExcludeMask and Srec.Attr and SysUtils.faHidden) <> 0 then
             Inc(FHiddenCount)
           else
             Inc(FFilteredCount);
           FreePIDL(FQPIDL);
         end;

         FreePIDL(PCurrList);
       except
         if Assigned(PCurrList) then
         try
           FreePIDL(PCurrList);
         except
         end;
       end;
     end; {While EnumList ...}
  end;
end; {LoadFromRecycleBin}

function TDirView.GetShellFolder(Dir: string): iShellFolder;
var
  WDir: WideString;
  Eaten: ULONG;
  Attr: ULONG;
  NewPIDL: PItemIDList;
begin
  Result := nil;
  if not Assigned(FDesktopFolder) then
    ShGetDesktopFolder(FDesktopFolder);

  WDir := Dir;
  if Assigned(FDesktopFolder) then
  begin
    if Succeeded(FDesktopFolder.ParseDisplayName(
         ParentForm.Handle, nil, PWideChar(WDir), Eaten, NewPIDL, Attr)) then
    begin
      try
        assert(Assigned(NewPIDL));
        FDesktopFolder.BindToObject(NewPIDL, nil, IID_IShellFolder, Pointer(Result));
        Assert(Assigned(Result));
      finally
        FreePIDL(NewPIDL);
      end;
    end;
  end;
end; {GetShellFolder}

function TDirView.ItemIsDirectory(Item: TListItem): Boolean;
begin
  Result :=
    (Assigned(Item) and Assigned(Item.Data) and
    PFileRec(Item.Data)^.IsDirectory);
end;

function TDirView.ItemIsFile(Item: TListItem): Boolean;
begin
  Result :=
    (Assigned(Item) and Assigned(Item.Data) and
     (not PFileRec(Item.Data)^.IsParentDir));
end;

function TDirView.ItemIsParentDirectory(Item: TListItem): Boolean;
begin
  Result :=
    (Assigned(Item) and Assigned(Item.Data) and
    PFileRec(Item.Data)^.IsParentDir);
end;

function TDirView.ItemIsRecycleBin(Item: TListItem): Boolean;
begin
  Result := (Assigned(Item) and Assigned(Item.Data) and
    PFileRec(Item)^.IsRecycleBin);
end;

function TDirView.ItemMatchesFilter(Item: TListItem; const Filter: TFileFilter): Boolean;
var
  FileRec: PFileRec;
  Modification: TDateTime;
begin
  Assert(Assigned(Item) and Assigned(Item.Data));
  FileRec := PFileRec(Item.Data);
  if (Filter.ModificationFrom > 0) or (Filter.ModificationTo > 0) then
    Modification := FileTimeToDateTime(FileRec^.FileTime)
  else
    Modification := 0;

  Result :=
    ((FileRec^.Attr and Filter.IncludeAttr) = Filter.IncludeAttr) and
    ((FileRec^.Attr and Filter.ExcludeAttr) = 0) and
    ((not FileRec^.IsDirectory) or Filter.Directories) and
    ((Filter.FileSizeFrom = 0) or (FileRec^.Size >= Filter.FileSizeFrom)) and
    ((Filter.FileSizeTo = 0) or (FileRec^.Size <= Filter.FileSizeTo)) and
    ((Filter.ModificationFrom = 0) or (Modification >= Filter.ModificationFrom)) and
    ((Filter.ModificationTo = 0) or (Modification <= Filter.ModificationTo)) and
    ((Filter.Masks = '') or
     FileNameMatchesMasks(FileRec^.FileName, FileRec^.IsDirectory,
       FileRec^.Size, Filter.Masks));
end;

function TDirView.ItemOverlayIndexes(Item: TListItem): Word;
begin
  Result := inherited ItemOverlayIndexes(Item);
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    if PFileRec(Item.Data)^.IsParentDir then
      Inc(Result, oiDirUp);
    if FDrawLinkOverlay and
       (UpperCase(ItemFileExt(Item)) = '.LNK') then
        Inc(Result, oiLink);
  end;
end;

procedure TDirView.Load;
begin
  try
{$IFNDEF NO_THREADS}
    StopSubDirScanner;
    StopIconUpdateThread;
    StopWatchThread;
{$ENDIF}

    FChangeTimer.Enabled  := False;
    FChangeTimer.Interval := 0;

    inherited;
  finally
    if DirOK and not AbortLoading then
    begin
{$IFNDEF NO_THREADS}
      if FUseIconUpdateThread and (not IsRecycleBin) then
        StartIconUpdateThread;
      StartWatchThread;
{$ENDIF}
    end;
  end;
end;

procedure TDirView.LoadFiles;
var
  SRec: SysUtils.TSearchRec;
  DosError: Integer;
  FileSel: Boolean;
  FSize: Int64;
  {$IFNDEF NO_THREADS}
  NewItem: TListItem;
  {$ENDIF}
  AttrIncludeMask: Integer;
  AttrExcludeMask: Integer;
  FileTimeFrom: LongWord;
  FileTimeTo: LongWord;
  {$IFDEF USE_DRIVEVIEW}
  DirsCount: Integer;
  SelTreeNode: TTreeNode;
  Node: TTreeNode;
  {$ENDIF}

  procedure AddToMasks(Attr: TSelAttr; Mask: Word);
  begin
    case Attr of
      selYes: AttrIncludeMask := AttrIncludeMask or Mask;
      selNo:  AttrExcludeMask := AttrExcludeMask or Mask;
    end;
  end;

begin
  FHiddenCount := 0;
  FFilteredCount := 0;

  AttrIncludeMask := 0;
  AttrExcludeMask := 0;
  AddToMasks(FSelArchive, SysUtils.faArchive);
  AddToMasks(FSelHidden, SysUtils.faHidden);
  AddToMasks(FSelReadOnly, SysUtils.faReadOnly);
  AddToMasks(FSelSysFile, SysUtils.faSysFile);

  FileTimeFrom := LongWord(fSelFileDateFrom) shl 16 or fSelFileTimeFrom;
  FileTimeTo   := LongWord(fSelFileDateTo)   shl 16 or fSelFileTimeTo;

  try
    if Length(FPath) > 0 then
    begin
      DriveInfo.ReadDriveStatus(FPath[1], dsSize);
      FDriveType := DriveInfo[FPath[1]].DriveType;
    end
      else FDriveType := DRIVE_UNKNOWN;

    FDirOK := (Length(FPath) > 0) and
      DriveInfo[FPath[1]].DriveReady and DirExists(FPath);

    if DirOK then
    begin
      {$IFDEF USE_DRIVEVIEW}
      if Assigned(FDriveView) then
        SelTreeNode := TDriveView(FDriveView).FindNodeToPath(FPath)
        else SelTreeNode := nil;
      {$ENDIF}

      {$IFDEF USE_DRIVEVIEW}
      if Assigned(FDriveView) and Assigned(SelTreeNode) then
          FIsRecycleBin := TNodeData(SelTreeNode.Data).IsRecycleBin
        else
      {$ENDIF}
      FIsRecycleBin :=
        (Uppercase(Copy(FPath, 2, 10)) = ':\RECYCLED') or
        (Uppercase(Copy(FPath, 2, 10)) = ':\RECYCLER');

      if not Assigned(FDesktopFolder) then
        shGetDesktopFolder(FDesktopFolder);

      {$IFNDEF PHYSICALRECYCLEBIN}
      if IsRecycleBin then LoadFromRecycleBin(Path)
        else
      {$ENDIF}
      begin
        FParentFolder := GetShellFolder(PathName);

        DosError := SysUtils.FindFirst(IncludeTrailingPathDelimiter(FPath) + '*.*',
          FileAttr, SRec);
        while (DosError = 0) and (not AbortLoading) do
        begin
          if (SRec.Attr and faDirectory) = 0 then
          begin
            FileSel := True;
            FSize := SizeFromSRec(SRec);
            if AttrIncludeMask <> 0 then
              FileSel := (SRec.Attr and AttrIncludeMask) >= AttrIncludeMask;
            if FileSel and (AttrExcludeMask <> 0) then
              FileSel := ((AttrExcludeMask and Srec.Attr) = 0);

            if FileSel and
              ((Mask = '') or FileNameMatchesMasks(SRec.Name, False, FSize, Mask)) and
              (FSize >= FSelFileSizeFrom) and
              ((FSelFileSizeTo = 0) or (FSize <= FSelFileSizeTo)) and
              (LongWord(SRec.Time) >= FileTimeFrom) and
              (LongWord(SRec.Time) <= FileTimeTo) then
            begin
              if Assigned(OnAddFile) then
                FOnAddFile(Self, SRec, FileSel);

              if FileSel then
              begin
                AddItem(SRec);
              end;
            end
              else
            begin
             if (AttrExcludeMask and Srec.Attr and SysUtils.faHidden) <> 0 then
               Inc(FHiddenCount)
             else
               Inc(FFilteredCount);
            end;
          end;
          DosError := FindNext(SRec);
        end;
        SysUtils.FindClose(SRec);

        if AddParentDir and (Length(FPath) > 2) then
        begin
          AddParentDirItem;
        end;

        {Search for directories:}
        {$IFDEF USE_DRIVEVIEW}
        DirsCount := 0;
        {$ENDIF}
        if ShowDirectories then
        begin
          DosError := SysUtils.FindFirst(IncludeTrailingPathDelimiter(FPath) + '*.*',
            DirAttrMask, SRec);
          while (DosError = 0) and (not AbortLoading) do
          begin
            FileSel := True;
            if AttrIncludeMask <> 0 then
              FileSel := ((SRec.Attr and AttrIncludeMask) = AttrIncludeMask);
            if FileSel and (AttrExcludeMask <> 0) then
              FileSel := ((AttrExcludeMask and SRec.Attr) = 0);

            if (SRec.Name <> '.') and (SRec.Name <> '..') and
               ((Srec.Attr and faDirectory) <> 0) then
            begin
              {$IFDEF USE_DRIVEVIEW}
              Inc(DirsCount);
              {$ENDIF}
              if Assigned(OnAddFile) then
                OnAddFile(Self, SRec, FileSel);

              if FileSel then
              begin
                {$IFNDEF NO_THREADS}
                NewItem :=
                {$ENDIF}
                AddItem(Srec);
                {$IFNDEF NO_THREADS}
                if ShowSubDirSize then
                  FSubDirScanner.Add(TSubDirScanner.Create(Self, NewItem));
                {$ENDIF}
              end
                else
              begin
                if (AttrExcludeMask and Srec.Attr and SysUtils.faHidden) <> 0 then
                  Inc(FHiddenCount);
              end;
            end;
            DosError := FindNext(SRec);
          end;
          SysUtils.FindClose(SRec);

          {$IFDEF USE_DRIVEVIEW}
          {Update TDriveView's subdir indicator:}
          if Assigned(FDriveView) and (FDriveType = DRIVE_REMOTE) then
            with FDriveView as TDriveView do
            begin
              Node := FindNodeToPath(PathName);
              if Assigned(Node) and Assigned(Node.Data) and
                 not TNodeData(Node.Data).Scanned then
              begin
                if DirsCount = 0 then
                begin
                  Node.HasChildren := False;
                  TNodeData(Node.Data).Scanned := True;
                end;
              end;
            end;
          {$ENDIF}
        end; {If FShowDirectories}
      end; {not isRecycleBin}
    end
      else FIsRecycleBin := False;

  finally
    //if Assigned(Animate) then Animate.Free;
    FInfoCacheList.Sort(CompareInfoCacheItems);
  end; {Finally}
end;

procedure TDirView.Reload2;
type
  PEFileRec = ^TEFileRec;
  TEFileRec = record
    iSize: Int64;
    iAttr: Integer;
    iFileTime: TFileTime;
    iIndex: Integer;
  end;
var
  Index: Integer;
  EItems: TStringList;
  FItems: TStringList;
  NewItems: TStringList;
  {$IFNDEF NO_THREADS}
  NewItem: TListItem;
  {$ENDIF}
  Srec: SysUtils.TSearchRec;
  DosError: Integer;
  PSrec: ^SysUtils.TSearchRec;
  Dummy: Integer;
  ItemIndex: Integer;
  AnyUpdate: Boolean;
  PUpdate: Boolean;
  PEFile: PEFileRec;
  SaveCursor: TCursor;
  FileTimeFrom: LongWord;
  FileTimeTo: LongWord;
  AttrIncludeMask: Integer;
  AttrExcludeMask: Integer;
  FileSel: Boolean;
  FSize: Int64;
  FocusedIsVisible: Boolean;
  R: TRect;

  procedure AddToMasks(Attr: TSelAttr; Mask: Word);
  begin
    case Attr of
      selYes: AttrIncludeMask := AttrIncludeMask or Mask;
      selNo:  AttrExcludeMask := AttrExcludeMask or Mask;
    end;
  end;

begin
  if (not Loading) and LoadEnabled then
  begin
    if IsRecycleBin then Reload(True)
      else
    begin
      if not DirExists(Path) then
      begin
        ClearItems;
        FDirOK := False;
        FDirty := False;
      end
        else
      begin
        if Assigned(ItemFocused) then
        begin
          R := ItemFocused.DisplayRect(drBounds);
          // btw, we use vsReport only, nothing else was tested
          Assert(ViewStyle = vsReport);
          case ViewStyle of
            vsReport:
              FocusedIsVisible := (TopItem.Index <= ItemFocused.Index) and
                (ItemFocused.Index < TopItem.Index + VisibleRowCount);

            vsList:
              // do not know how to implement that
              FocusedIsVisible := False;

            else // vsIcon and vsSmallIcon
              FocusedIsVisible :=
                IntersectRect(R,
                  Rect(ViewOrigin, Point(ViewOrigin.X + ClientWidth, ViewOrigin.Y + ClientHeight)),
                  ItemFocused.DisplayRect(drBounds));
          end;
        end
          else FocusedIsVisible := False; // shut up

        SaveCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        FChangeTimer.Enabled  := False;
        FChangeTimer.Interval := 0;

        EItems := TStringlist.Create;
        FItems := TStringlist.Create;
        NewItems := TStringlist.Create;

        PUpdate := False;
        AnyUpdate := False;

        FHiddenCount := 0;
        FFilteredCount := 0;

        AttrIncludeMask := 0;
        AttrExcludeMask := 0;
        AddToMasks(FSelArchive, SysUtils.faArchive);
        AddToMasks(FSelHidden, SysUtils.faHidden);
        AddToMasks(FSelReadOnly, SysUtils.faReadOnly);
        AddToMasks(FSelSysFile, SysUtils.faSysFile);

        FileTimeFrom := LongWord(fSelFileDateFrom) shl 16 or fSelFileTimeFrom;
        FileTimeTo   := LongWord(fSelFileDateTo)   shl 16 or fSelFileTimeTo;

        try
          {Store existing files and directories:}
          for Index := 0 to Items.Count - 1 do
          begin
            New(PEFile);
            with PFileRec(Items[Index].Data)^ do
            begin
              PEFile^.iSize := Size;
              PEFile^.iAttr := Attr;
              PEFile^.iFileTime := FileTime;
              PEFile^.iIndex := Index;
            end;
            EItems.AddObject(PFileRec(Items[Index].Data)^.FileName, Pointer(PEFile));
          end;
          EItems.Sort;

          DosError := SysUtils.FindFirst(IncludeTrailingPathDelimiter(FPath) + '*.*',
            FileAttr, SRec);
          while DosError = 0 do
          begin
            if (SRec.Attr and faDirectory) = 0 then
            begin
              FileSel := True;
              if (AttrIncludeMask <> 0) then
                FileSel := ((SRec.Attr and AttrIncludeMask) = AttrIncludeMask);
              if FileSel and (AttrExcludeMask <> 0) then
                FileSel := ((AttrExcludeMask and Srec.Attr) = 0);

              FSize := SizeFromSRec(SRec);
              FileSel :=
                FileSel and
                ((Mask = '') or FileNameMatchesMasks(SRec.Name, False, FSize, Mask)) and
                (FSize >= FSelFileSizeFrom) and
                ((FSelFileSizeTo = 0) or (FSize <= FSelFileSizeTo)) and
                (LongWord(SRec.Time) >= FileTimeFrom) and
                (LongWord(SRec.Time) <= FileTimeTo);

              if FileSel then
              begin
                ItemIndex := -1;
                if not EItems.Find(SRec.Name, ItemIndex) then
                begin
                  if Assigned(OnAddFile) then
                    FOnAddFile(Self, Srec, FileSel);
                  if FileSel then
                  begin
                    New(PSrec);
                    PSRec^ := SRec;
                    NewItems.AddObject(SRec.Name, Pointer(PSrec));
                    FItems.Add(Srec.Name);
                  end
                end
                  else
                begin
                  with PEFileRec(EItems.Objects[ItemIndex])^ do
{$WARNINGS OFF}
                  if (iSize <> FSize) or (iAttr <> SRec.Attr) or
                     not CompareMem(@iFileTime, @SRec.FindData.ftLastWriteTime,
                        SizeOf(iFileTime)) Then
{$WARNINGS ON}
                  begin
                    with PFileRec(Items[iIndex].Data)^ do
                    begin
                      Dec(FFilesSize, Size);
                      Inc(FFilesSize, FSize);
                      if Items[iIndex].Selected then
                      begin
                        Dec(FFilesSelSize, Size);
                        Inc(FFilesSelSize, FSize);
                      end;

                      Size := FSize;
                      Attr := SRec.Attr;
{$WARNINGS OFF}
                      FileTime := SRec.FindData.ftLastWriteTime;
{$WARNINGS ON}
                      if (iSize <> FSize) and Assigned(OnFileSizeChanged) then
                        OnFileSizeChanged(Self, Items[iIndex]);
                    end;
                    // alternative to TListItem.Update (which causes flicker)
                    R := Items[iIndex].DisplayRect(drBounds);
                    InvalidateRect(Handle, @R, True);
                    AnyUpdate := True;
                  end;
                  FItems.Add(Srec.Name);
                end;
              end;

              if not FileSel then
              begin
                if ((AttrExcludeMask and Srec.Attr and SysUtils.faHidden) <> 0) then
                  Inc(FHiddenCount)
                else
                  Inc(FFilteredCount);
              end;
            end;
            DosError := FindNext(Srec);
          end;
          SysUtils.FindClose(Srec);

          {Search new directories:}
          if ShowDirectories then
          begin
            DosError := SysUtils.FindFirst(FPath + '\*.*', DirAttrMask, SRec);
            while DosError = 0 do
            begin
              if (Srec.Attr and faDirectory) <> 0 then
              begin
                FileSel := True;
                if AttrIncludeMask <> 0 then
                  FileSel := ((SRec.Attr and AttrIncludeMask) = AttrIncludeMask);
                if FileSel and (AttrExcludeMask <> 0) then
                  FileSel := ((AttrExcludeMask and SRec.Attr) = 0);

                if (SRec.Name <> '.') and (SRec.Name <> '..') then
                begin
                  if not EItems.Find(SRec.Name, ItemIndex) then
                  begin
                    if Assigned(FOnAddFile) then
                      FOnAddFile(Self, SRec, FileSel);

                    if FileSel then
                    begin
                      New(PSrec);
                      PSrec^ := SRec;
                      NewItems.AddObject(Srec.Name, Pointer(PSrec));
                      FItems.Add(SRec.Name);
                    end
                      else
                    begin
                      if (AttrExcludeMask and Srec.Attr and SysUtils.faHidden) <> 0 then
                        Inc(FHiddenCount);
                    end;
                  end
                    else
                  begin
                    FileSel := true;
                    FItems.Add(SRec.Name);
                  end;
                end
                  else
                begin
                  FItems.Add(SRec.Name);
                end;
              end;
              DosError := FindNext(SRec);
            end;
            SysUtils.FindClose(SRec);
          end; {If FShowDirectories}


          {Check wether displayed Items still exists:}
          FItems.Sort;
          for Index := Items.Count - 1 downto 0 do
          begin
            if not FItems.Find(PFileRec(Items[Index].Data)^.FileName, Dummy) then
            begin
              if not PUpdate then
              begin
                PUpdate := True;
                Items.BeginUpdate;
              end;
              AnyUpdate := True;

              with PFileRec(Items[Index].Data)^ do
              begin
                Dec(FFilesSize, Size);
                if Items[Index].Selected then
                  Dec(FFilesSelSize, Size);
              end;

              Items[Index].Delete;
            end;
          end;

        finally
          try
            for Index := 0 to EItems.Count - 1  do
              Dispose(PEFileRec(EItems.Objects[Index]));
            EItems.Free;
            FItems.Free;
            for Index := 0 to NewItems.Count - 1 do
            begin
              if not PUpdate then
              begin
                PUpdate := True;
                Items.BeginUpdate;
              end;
              AnyUpdate := True;
              PSrec := Pointer(NewItems.Objects[Index]);
              {$IFNDEF NO_THREADS}
              NewItem :=
              {$ENDIF}
              AddItem(PSrec^);
              {$IFNDEF NO_THREADS}
              if ShowSubDirSize and ((PSrec^.Attr and faDirectory) <> 0) then
                FSubDirScanner.Add(TSubDirScanner.Create(Self, NewItem));
              {$ENDIF}
              Dispose(PSrec);
            end;
            NewItems.Free;
            // if we are sorted by name and there were only updates to existing
            // items, there is no need for sorting
            if SortAfterUpdate and
               (PUpdate or
                (AnyUpdate and (DirColProperties.SortDirColumn <> dvName))) then
            begin
              SortItems;
            end;
            if PUpdate then
              Items.EndUpdate;
          finally
            FDirOK := True;
            FDirty := false;
{$IFNDEF NO_THREADS}
            if FUseIconUpdateThread and (not FisRecycleBin) then
              StartIconUpdateThread;
            StartWatchThread;
{$ENDIF}
            // make focused item visible, only if it was before
            if FocusedIsVisible and Assigned(ItemFocused) then
              ItemFocused.MakeVisible(False);
            if AnyUpdate and Assigned(OnDirUpdated) then
              OnDirUpdated(Self);

            UpdateStatusBar;

            Screen.Cursor := SaveCursor;
          end;
        end; {Finally}
      end;
    end;
  end;
end; {Reload2}

procedure TDirView.PerformItemDragDropOperation(Item: TListItem; Effect: Integer);
begin
  if Assigned(Item) then
  begin
    if Assigned(Item.Data) then
    begin
      if ItemIsParentDirectory(Item) then
        PerformDragDropFileOperation(ExcludeTrailingPathDelimiter(ExtractFilePath(Path)),
          Effect, False)
      else
        PerformDragDropFileOperation(IncludeTrailingPathDelimiter(PathName) +
          ItemFileName(Item), Effect, False);
    end;
  end
    else
  PerformDragDropFileOperation(PathName, Effect,
    DDOwnerIsSource and (Effect = DropEffect_Copy));
end;

procedure TDirView.ReLoad(CacheIcons: Boolean);
begin
  if not FLoadEnabled then FDirty := True
    else inherited;
end; {ReLoad}

procedure TDirView.ClearIconCache;
begin
  if Assigned(FInfoCacheList) then
    FInfoCacheList.Clear;
end; {ClearIconCache}

function TDirView.FormatFileTime(FileTime: TFileTime): string;
begin
  Result := FormatDateTime(DateTimeFormatStr,
    FileTimeToDateTime(FileTime));
end; {FormatFileTime}

function TDirView.GetAttrString(Attr: Integer): string;
const
  Attrs: array[1..5] of Integer =
    (FILE_ATTRIBUTE_COMPRESSED, FILE_ATTRIBUTE_ARCHIVE,
     FILE_ATTRIBUTE_SYSTEM, FILE_ATTRIBUTE_HIDDEN,
     FILE_ATTRIBUTE_READONLY);
  AttrChars: array[1..5] of Char = ('c', 'a', 's', 'h', 'r');
var
  Index: Integer;
  LowBound: Integer;
begin
  Result := '';
  if Attr <> 0 then
  begin
    LowBound := Low(Attrs);
    if Win32PlatForm <> VER_PLATFORM_WIN32_NT then
      Inc(LowBound);

    for Index := LowBound to High(Attrs) do
      if (Attr and Attrs[Index] <> 0) then
        Result := Result + AttrChars[Index]
      else
        Result := Result + FAttrSpace;
  end;
end; {GetAttrString}

procedure TDirView.GetDisplayData(Item: TListItem; FetchIcon: Boolean);
var
  FileInfo: TShFileInfo;
  Index: Integer;
  PExtItem: PInfoCache;
  CacheItem: TInfoCache;
  IsSpecialExt: Boolean;
  ForceByName: Boolean;
  WStr: WideString;
  Eaten: ULONG;
  shAttr: ULONG;
  FileIconForName, FullName: string;
begin
  Assert(Assigned(Item) and Assigned(Item.Data));
  with PFileRec(Item.Data)^ do
  begin
    IsSpecialExt := MatchesFileExt(FileExt, SpecialExtensions);
    if FUseIconCache and not IsSpecialExt and not IsDirectory then
    begin
      CacheItem.FileExt := FileExt;
      Index := FInfoCacheList.FindSequential(Addr(CacheItem), CompareInfoCacheItems);
      if Index >= 0 then
      begin
        TypeName := PInfoCache(FInfoCacheList[Index])^.TypeName;
        ImageIndex  := PInfoCache(FInfoCacheList[Index])^.ImageIndex;
        Empty := False;
        IconEmpty := False;
      end;
    end;

    FetchIcon := IconEmpty and (FetchIcon or not IsSpecialExt);

    if Empty or FetchIcon then
    begin
      if FetchIcon then
      begin
        {Fetch the Item FQ-PIDL:}
        if not Assigned(PIDL) and IsSpecialExt then
        begin
          try
            WStr := FPath + '\' + FileName;
            FDesktopFolder.ParseDisplayName(ParentForm.Handle, nil,
              PWideChar(WStr), Eaten, PIDL, ShAttr);

            {Retrieve the shell display attributes for directories:}
            if IsDirectory and Assigned(PIDL) then
            begin
              shAttr := SFGAO_DISPLAYATTRMASK;
              try
                if Assigned(ParentFolder) and
                   Succeeded(ParentFolder.GetAttributesOf(1, PIDL, shAttr)) then
                begin
                  if (shAttr and SFGAO_SHARE) <> 0 then
                    Item.OverlayIndex := 0;
                end;
              except end;
            end;
          except end;
        end;

        if IsDirectory then
        begin
          if FDriveType = DRIVE_FIXED then
          begin
            try
              {Retrieve icon and typename for the directory}
              if Assigned(PIDL) then
                  SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo),
                    SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_PIDL)
              else
                SHGetFileInfo(PChar(FPath + '\' + FileName), 0, FileInfo, SizeOf(FileInfo),
                  SHGFI_TYPENAME or SHGFI_SYSICONINDEX);

              if (FileInfo.iIcon <= 0) or (FileInfo.iIcon > SmallImages.Count) then
                {Invalid icon returned: retry with access file attribute flag:}
                SHGetFileInfo(PChar(fPath + '\' + FileName), FILE_ATTRIBUTE_DIRECTORY,
                  FileInfo, SizeOf(FileInfo),
                  SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
              TypeName := FileInfo.szTypeName;
              if FetchIcon then
              begin
                ImageIndex := FileInfo.iIcon;
                IconEmpty := False;
              end;
            {Capture exceptions generated by the shell}
            except
              ImageIndex := StdDirIcon;
              IconEmpty := False;
            end; {Except}
          end
            else
          begin
            TypeName := StdDirTypeName;
            ImageIndex := StdDirIcon;
            IconEmpty  := False;
          end;
        end
          else
        begin
          {Retrieve icon and typename for the file}
          try
            ForceByName := False;
            FullName := FPath + '\' + FileName;
            FileIconForName := FullName;
            if Assigned(OnFileIconForName) then
            begin
              OnFileIconForName(Self, Item, FileIconForName);
              ForceByName := (FileIconForName <> FullName);
            end;
            if (not ForceByName) and Assigned(PIDL) then
              SHGetFileInfo(PChar(PIDL), FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo),
                SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX or SHGFI_PIDL)
            else
              SHGetFileInfo(PChar(FileIconForName), FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo),
                SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or SHGFI_SYSICONINDEX);

            TypeName := FileInfo.szTypeName;
            ImageIndex := FileInfo.iIcon;
            IconEmpty := False;

          {Capture exceptions generated by the shell}
          except
            ImageIndex := UnKnownFileIcon;
            IconEmpty := False;
          end; {Except}
        end;

        if (Length(TypeName) > 0) then
        begin
          {Fill FileInfoCache:}
          if FUseIconCache and not IsSpecialExt and not IconEmpty and not IsDirectory then
          begin
            GetMem(PExtItem, SizeOf(TInfoCache));
            PExtItem.FileExt := FileExt;
            PExtItem.TypeName := TypeName;
            PExtItem.ImageIndex := ImageIndex;
            FInfoCacheList.Add(PExtItem);
          end;
        end
          else TypeName := Format(STextFileExt, [FileExt]);
      end {If FetchIcon}
        else
      begin
        try
          if IsDirectory then
            shGetFileInfo(PChar(fPath), FILE_ATTRIBUTE_DIRECTORY, FileInfo, SizeOf(FileInfo),
            SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES)
            else
          shGetFileInfo(PChar(fPath + '\' + FileName), FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo),
            SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES);
          TypeName := FileInfo.szTypeName;
        except
          {Capture exceptions generated by the shell}
          TypeName := '';
        end;

        if IconEmpty then
        begin
          if FileExt = ExeExtension then ImageIndex := DefaultExeIcon
            else ImageIndex  := UnKnownFileIcon;
        end;
      end;
      Empty := False;
    end;
  end;
end; {GetDisplayData}

function TDirView.GetDirOK: Boolean;
begin
  Result := FDirOK;
end;

function TDirView.ItemFullFileName(Item: TListItem): string;
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    if not IsRecycleBin then
    begin
      if PFileRec(Item.Data)^.IsParentDir then
      begin
        Result := ExcludeTrailingBackslash(ExtractFilePath(FPath));
      end
        else
      begin
        Result := FPath + '\' + PFileRec(Item.Data)^.FileName;
      end;
    end
      else
    Result := PFileRec(Item.Data)^.FileName;
  end
    else
  Result := EmptyStr;
end; {ItemFullFileName}

function TDirView.ItemFileNameOnly(Item: TListItem): string;
begin
  Assert(Assigned(Item) and Assigned(Item.Data));
  Result := PFileRec(Item.Data)^.FileName;
  SetLength(Result, Length(Result) - Length(ItemFileExt(Item)));
end; {ItemFileNameOnly}

function TDirView.ItemFileExt(Item: TListItem): string;
begin
  Assert(Assigned(Item) and Assigned(Item.Data));
  Result := ExtractFileExt(PFileRec(Item.Data)^.FileName);
end; {ItemFileExt}

function TDirView.DeleteSelectedFiles(AllowUndo: Boolean): Boolean;
const
  MaxSel = 10;
var
  ItemIndex: Integer;
  Item, NextItem: TListItem;
  FileOperator: TFileOperator;
  UpdateEnabled: Boolean;
  WatchDir: Boolean;
  Updating: Boolean;
  {$IFDEF USE_DRIVEVIEW}
  DirDeleted: Boolean;
  {$ENDIF}
begin
  AllowUndo := AllowUndo and (not IsRecycleBin);
  {$IFDEF USE_DRIVEVIEW}
  DirDeleted := False;
{$IFNDEF NO_THREADS}
  if Assigned(FDriveView) then
    TDriveView(FDriveView).StopWatchThread;
{$ENDIF}
  {$ENDIF}
  WatchDir := WatchForChanges;
  WatchForChanges := False;
  UpdateEnabled := (SelCount < MaxSel);
  if not UpdateEnabled then Items.BeginUpdate;

  FileOperator := TFileOperator.Create(Self);
  try
    ItemIndex := Selected.Index;
    FileOperator.Operation := foDelete;
    FileOperator.Flags := [foNoConfirmMkDir];
    FileOperator.ProgressTitle := coFileOperatorTitle;
    CreateFileList(False, True, FileOperator.OperandFrom);

    if not ConfirmDelete then
      FileOperator.Flags := FileOperator.Flags + [foNoConfirmation];

    if AllowUndo then
      FileOperator.Flags := FileOperator.Flags + [foAllowUndo];

{$IFNDEF NO_THREADS}
    StopIconUpdateThread;
    StopSubDirScanner;
{$ENDIF}
    Result := FileOperator.Execute;
    Result := Result and (not FileOperator.OperationAborted);
    Sleep(0);

    Updating := False;
    Item := GetNextItem(nil, sdAll, [isSelected]);
    while Assigned(Item) do
    begin
      NextItem := GetNextItem(Item, sdAll, [isSelected]);
      case PFileRec(Item.Data)^.IsDirectory of
        True:
          if not DirExists(ItemFullFileName(Item)) then
          begin
            {$IFDEF USE_DRIVEVIEW}
            DirDeleted := True;
            {$ENDIF}
            Item.Delete;
          end;
        False:
          if not CheckFileExists(ItemFullFileName(Item)) then
          begin
            if (SelCount > 3) and (not Updating) then
            begin
              Items.BeginUpdate;
              Updating := True;
            end;
            Item.Delete;
          end;
      end;
      Item := NextItem;
    end;
    if Updating then
      Items.EndUpdate;

  finally
    if not UpdateEnabled then
      Items.EndUpdate;
    FileOperator.Free;
    if Assigned(OnDirUpdated) then
      OnDirUpdated(Self);
  end;

  {$IFDEF USE_DRIVEVIEW}
  if Assigned(DriveView) then
    with TDriveView(DriveView) do
    begin
      if DirDeleted and Assigned(Selected) then
        ValidateDirectory(Selected);
{$IFNDEF NO_THREADS}
      TDriveView(fDriveView).StartWatchThread;
{$ENDIF}
    end;
  {$ENDIF}

{$IFNDEF NO_THREADS}
  if UseIconUpdateThread then StartIconUpdateThread;
  if ShowSubDirSize then StartSubDirScanner;
{$ENDIF}

  WatchForChanges := WatchDir;

  if (not Assigned(Selected)) and (Items.Count > 0) then
    Selected := Items[Min(ItemIndex, Pred(Items.Count))];
end; {DeleteSelectedFiles}

function CompareFileName(I1, I2: TListItem; AOwner: TDirView): Integer; stdcall;
var
  P1, P2: PFileRec;
begin
  if I1 = I2 then  Result := fEqual
    else
  if I1 = nil then Result := fLess
    else
  if I2 = nil then Result := fGreater
    else
  begin
    P1 := PFileRec(I1.Data);
    P2 := PFileRec(I2.Data);
    if P1.isParentDir then
    begin
      Result := fLess;
      Exit;
    end
      else
    if P2.isParentDir then
    begin
      Result := fGreater;
      Exit;
    end;

    {Directories allways should appear "grouped":}
    if P1.isDirectory <> P2.isDirectory then
    begin
      if P1.isDirectory then
      begin
        Result := fLess;
        if AOwner.DirsOnTop then
          Exit;
      end
        else
      begin
        Result := fGreater;
        if AOwner.DirsOnTop then
          Exit;
      end;
    end
      else Result := lstrcmpi(PChar(P1.DisplayName), PChar(P2.DisplayName));
  end;

  if not AOwner.SortAscending then
    Result := -Result;
end; {CompareFileName}

function CompareFileSize(I1, I2: TListItem; AOwner : TDirView): Integer; stdcall;
var
  P1, P2: PFileRec;
begin
  if I1 = I2 then Result := fEqual
    else
  if I1 = nil then Result := fLess
    else
  if I2 = nil then Result := fGreater
    else
  begin
    P1 := PFileRec(I1.Data);
    P2 := PFileRec(I2.Data);
    if P1.isParentDir then
    begin
      Result := fLess;
      Exit;
    end
      else
    if P2.isParentDir then
    begin
      Result := fGreater;
      Exit;
    end;
    {Directories always should appear "grouped":}
    if P1.isDirectory <> P2.isDirectory then
    begin
      if P1.isDirectory then
      begin
        Result := fLess;
        if AOwner.DirsOnTop then Exit;
      end
        else
      begin
        Result := fGreater;
        if AOwner.DirsOnTop then Exit;
      end;
    end
      else
    begin
      if P1.Size < P2.Size then Result := fLess
        else
      if P1.Size > P2.Size then Result := fGreater
        else
      Result := lstrcmpi(PChar(P1.DisplayName), PChar(P2.DisplayName));
    end;
  end;
  if not AOwner.SortAscending then
    Result := -Result;
end; {CompareFileSize}

function CompareFileType(I1, I2: TListItem; AOwner: TDirView): Integer; stdcall;
var
  P1, P2: PFileRec;
begin
  if I1 = I2 then  Result := fEqual
    else
  if I1 = nil then Result := fLess
    else
  if I2 = nil then Result := fGreater
    else
  begin
    P1 := PFileRec(I1.Data);
    P2 := PFileRec(I2.Data);
    if P1.isParentDir then
    begin
      Result := fLess;
      Exit;
    end
      else
    if P2.isParentDir then
    begin
      Result := fGreater;
      Exit;
    end;

    {Directories allways should appear "grouped":}
    if P1.isDirectory <> P2.isDirectory then
    begin
      if P1.isDirectory then
      begin
        Result := fLess;
        if AOwner.DirsOnTop then
          Exit;
      end
        else
      begin
        Result := fGreater;
        if AOwner.DirsOnTop then
          Exit;
      end;
    end
      else
    begin
      if P1.Empty then TDirView(I1.ListView).GetDisplayData(I1, False);
      if P2.Empty then TDirView(I2.ListView).GetDisplayData(I2, False);
      Result := lstrcmpi(PChar(P1.TypeName + ' ' + P1.FileExt + ' ' + P1.DisplayName),
                         PChar(P2.TypeName + ' ' + P2.FileExt + ' ' + P2.DisplayName));
      if Result = 0 then
        Result := lstrcmpi(PChar(P1.DisplayName), PChar(P2.DisplayName));
    end;
  end;
  if not AOwner.SortAscending then
    Result := -Result;
end; {CompareFileType}

function CompareFileExt(I1, I2: TListItem; AOwner: TDirView): Integer; stdcall;
var
  P1, P2: PFileRec;
begin
  if I1 = I2 then Result := fEqual
    else
  if I1 = nil then Result := fLess
    else
  if I2 = nil then Result := fGreater
    else
  begin
    P1 := PFileRec(I1.Data);
    P2 := PFileRec(I2.Data);
    if P1.isParentDir then
    begin
      Result := fLess;
      Exit;
    end
      else
    if P2.isParentDir then
    begin
      Result := fGreater;
      Exit;
    end;
    {Directories allways should appear "grouped":}
    if P1.isDirectory <> P2.isDirectory then
    begin
      if P1.isDirectory then
      begin
        Result := fLess;
        if AOwner.DirsOnTop then
          Exit;
      end
        else
      begin
        Result := fGreater;
        if AOwner.DirsOnTop then
          Exit;
      end;
    end
      else
    Result := lstrcmpi(PChar(P1.FileExt + ' ' + P1.DisplayName),
                       PChar(P2.FileExt + ' ' + P2.DisplayName));
  end;
  if not AOwner.SortAscending then
    Result := -Result;
end; {CompareFileExt}

function CompareFileAttr(I1, I2: TListItem; AOwner: TDirView): Integer; stdcall;
var
  P1, P2: PFileRec;
begin
  if I1 = I2 then Result := 0
    else
  if I1 = nil then Result := -1
    else
  if I2 = nil then Result := 1
    else
  begin
    P1 := PFileRec(I1.Data);
    P2 := PFileRec(I2.Data);
    if P1.isParentDir then
    begin
      Result := fLess;
      Exit;
    end
      else
    if P2.isParentDir then
    begin
      Result := fGreater;
      Exit;
    end;
    {Directories allways should appear "grouped":}
    if P1.isDirectory <> P2.isDirectory then
    begin
      if P1.isDirectory then
      begin
        Result := fLess;
        if AOwner.DirsOnTop then
          Exit;
      end
        else
      begin
        Result := fGreater;
        if AOwner.DirsOnTop then
          Exit;
      end;
    end
      else
    begin
      if P1.Attr < P2.Attr then Result := fLess
        else
      if P1.Attr > P2.Attr then Result := fGreater
        else
      Result := lstrcmpi(PChar(P1.DisplayName), PChar(P2.DisplayName));
    end;
  end;
  if not AOwner.SortAscending then
    Result := -Result;
end; {CompareFileAttr}

function CompareFileTime(I1, I2: TListItem; AOwner: TDirView): Integer; stdcall;
var
  Time1, Time2: Int64;
  P1, P2: PFileRec;
begin
  if I1 = I2 then Result := fEqual
    else
  if I1 = nil then Result := fLess
    else
  if I2 = nil then Result := fGreater
    else
  begin
    P1 := PFileRec(I1.Data);
    P2 := PFileRec(I2.Data);
    if P1.isParentDir then
    begin
      Result := fLess;
      Exit;
    end
      else
    if P2.isParentDir then
    begin
      Result := fGreater;
      Exit;
    end;
    {Directories allways should appear "grouped":}
    if P1.isDirectory <> P2.isDirectory then
    begin
      if P1.isDirectory then
      begin
        Result := fLess;
        if AOwner.DirsOnTop then
          Exit;
      end
        else
      begin
        Result := fGreater;
        if AOwner.DirsOnTop then
          Exit;
      end;
    end
      else
    begin
      Time1 := Int64(P1.FileTime.dwHighDateTime) shl 32 + P1.FileTime.dwLowDateTime;
      Time2 := Int64(P2.FileTime.dwHighDateTime) shl 32 + P2.FileTime.dwLowDateTime;
      if Time1 < Time2 then Result := fLess
        else
      if Time1 > Time2 then Result := fGreater
        else
      Result := CompareFileName(I1, I2, AOwner);
    end;
  end;
  if not AOwner.SortAscending then
    Result := -Result;
end; {CompareFileTime}

procedure TDirView.SortItems;
var
  SortProc: TLVCompare;
begin
  if HandleAllocated then
  begin
{$IFNDEF NO_THREADS}
    StopIconUpdateThread;
{$ENDIF}
    try
      case DirColProperties.SortDirColumn of
        dvName: SortProc := @CompareFilename;
        dvSize: SortProc := @CompareFileSize;
        dvType: if not SortByExtension then SortProc := @CompareFileType
             else SortProc := @CompareFileExt;
        dvChanged: SortProc := @CompareFileTime;
        dvAttr: SortProc := @CompareFileAttr;
        dvExt: SortProc := @CompareFileExt;
        else SortProc := @CompareFilename;
      end;
      CustomSortItems(Pointer(@SortProc));
    finally
{$IFNDEF NO_THREADS}
      if (not Loading) and FUseIconUpdateThread then
        StartIconUpdateThread;
{$ENDIF}
    end;
  end
end;

procedure TDirView.ValidateFile(Item : TListItem);
var
  Index: Integer;
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    Index := Item.Index;
    if not FileExists(ItemFullFileName(Items[Index])) then
    begin
      Item.Delete;
      if Assigned(OnDirUpdated) then
        OnDirUpdated(Self);
    end;
  end;
end; {ValidateFile}

procedure TDirView.ValidateFile(FileName: TFileName);
var
  FilePath: string;
begin
  FilePath := ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
  if IsRecycleBin then ValidateFile(FindFileItem(FileName))
    else
  if FilePath = Path then
    ValidateFile(FindFileItem(ExtractFileName(FileName)));
end; {ValidateFile}

procedure TDirView.ValidateSelectedFiles;
var
  FileList: TStrings;
  i: Integer;
  ToDelete: Boolean;
  Updating: Boolean;
  Deleted: Boolean;
  Item: TListItem;
begin
  if SelCount > 50 then Reload2
    else
  begin
    Updating := False;
    Deleted := False;
    FileList := CustomCreateFileList(True, False, True, nil, True);
    try
      for i := 0 to FileList.Count - 1 do
      begin
        Item := TListItem(FileList.Objects[i]);
        if ItemIsDirectory(Item) then
          ToDelete := not DirectoryExists(FileList[i])
        else
          ToDelete := not FileExists(FileList[i]);

        if ToDelete then
        begin
          if (SelCount > 10) and (not Updating) then
          begin
            Items.BeginUpdate;
            Updating := True;
          end;
          Item.Delete;
          Deleted := True;
        end;
      end;
    finally
      if Updating then
        Items.EndUpdate;
      FileList.Free;
      if Deleted and Assigned(OnDirUpdated) then
        OnDirUpdated(Self);
    end;
  end;
end; {ValidateSelectedFiles}

function TDirView.CreateFile(NewName: string): TListItem;
var
  F: file;
  SRec: SysUtils.TSearchRec;
begin
  Result := nil;
  {Neue Datei anlegen:}
  NewName := Path + '\' + NewName;

  {Ermitteln des neuen Dateinamens:}
  if not FileExists(NewName) then
  begin
{$IFNDEF NO_THREADS}
    if FWatchForChanges then
      StopWatchThread;
    StopIconUpdateThread;
{$ENDIF}

    try
      {Create the desired file as empty file:}
      AssignFile(F, NewName);
      Rewrite(F);
      LastIOResult := IOResult;
      if LastIOResult = 0 then
      begin
        CloseFile(F);

        {Anlegen der Datei als TListItem:}
        if FindFirst(NewName, faAnyFile, SRec) = 0 then
        begin
          Result := AddItem(SRec);
          ItemFocused := FindFileItem(GetFileRec(Result.Index)^.FileName);
          if Assigned(ItemFocused) then
            ItemFocused.MakeVisible(False);
          if Assigned(OnDirUpdated) then
            OnDirUpdated(Self);
        end;
        FindClose(Srec);
      end;
    finally
{$IFNDEF NO_THREADS}
      if FUseIconUpdateThread then
        StartIconUpdateThread;
      if WatchForChanges then
        StartWatchThread;
{$ENDIF}
    end;
  end
    else LastIOResult := 183;
end; {CreateFile}

procedure TDirView.CreateDirectory(DirName: string);
var
  SRec: SysUtils.TSearchRec;
  Item: TListItem;
begin
  // keep absolute path as is
  if Copy(DirName, 2, 1) <> ':' then
    DirName := Path + '\' + DirName;

{$IFNDEF NO_THREADS}
  if WatchForChanges then StopWatchThread;

  {$IFDEF USE_DRIVEVIEW}
  if Assigned(FDriveView) then
    TDriveView(FDriveView).StopWatchThread;
  {$ENDIF}

  StopIconUpdateThread;
{$ENDIF}
  try
    {create the phyical directory:}
    Win32Check(Windows.CreateDirectory(PChar(DirName), nil));

    if IncludeTrailingBackslash(ExtractFilePath(ExpandFileName(DirName))) =
         IncludeTrailingBackslash(Path) then
    begin
      {Create the TListItem:}
      if FindFirst(DirName, faAnyFile, SRec) = 0 then
      begin
        Item := AddItem(SRec);
        ItemFocused := FindFileItem(GetFileRec(Item.Index)^.FileName);
        SortItems;
        if Assigned(ItemFocused) then
          ItemFocused.MakeVisible(False);
        if Assigned(OnDirUpdated) then
          OnDirUpdated(Self);
      end;
      FindClose(SRec);
    end;

  finally
{$IFNDEF NO_THREADS}
    if FUseIconUpdateThread then
      StartIconUpdateThread;

    if WatchForChanges then StartWatchThread;
{$ENDIF}
    {$IFDEF USE_DRIVEVIEW}
    if Assigned(FDriveView) then
      with FDriveView as TDriveView do
        if not WatchThreadActive and Assigned(Selected) then
          ValidateDirectory(Selected);
    {$ENDIF}
  end;
end; {CreateDirectory}

procedure TDirView.DisplayContextMenu(Where: TPoint);
var
  FileList : TStringList;
  Index: Integer;
  Item: TListItem;
  DefDir: string;
  Verb: string;
  PIDLArray: PPIDLArray;
  Count: Integer;
  DiffSelectedPath: Boolean;
  WithEdit: Boolean;
  PIDLRel: PItemIDList;
  PIDLPath: PItemIDList;
  Handled: Boolean;
begin
  GetDir(0, DefDir);
  ChDir(PathName);
  Verb := EmptyStr;
{$IFNDEF NO_THREADS}
  StopWatchThread;
{$ENDIF}
  try
    if Assigned(OnContextPopup) then
    begin
      Handled := False;
      OnContextPopup(Self, ScreenToClient(Where), Handled);
      if Handled then Abort;
    end;
    if (MarkedCount > 1) and
       ((not Assigned(ItemFocused)) or ItemFocused.Selected) then
    begin
      if FIsRecycleBin then
      begin
        Count := 0;
        GetMem(PIDLArray, SizeOf(PItemIDList) * SelCount);
        try
          FillChar(PIDLArray^, Sizeof(PItemIDList) * SelCount, #0);
          for Index := Selected.Index to Items.Count - 1 do
            if Items[Index].Selected then
            begin
              PIDL_GetRelative(PFileRec(Items[Index].Data)^.PIDL, PIDLPath, PIDLRel);
              FreePIDL(PIDLPath);
              PIDLArray^[Count] := PIDLRel;
              Inc(Count);
            end;

          try
            ShellDisplayContextMenu(ParentForm.Handle, Where, iRecycleFolder, Count,
              PidlArray^[0], False, Verb, False);
          finally
            for Index := 0 to Count - 1 do
              FreePIDL(PIDLArray[Index]);
          end;
        finally
          FreeMem(PIDLArray, Count);
        end;
      end
        else
      begin
        FileList := TStringList.Create;
        CreateFileList(False, True, FileList);
        for Index := 0 to FileList.Count - 1 do
          FileList[Index] := ExtractFileName(FileList[Index]);
        ShellDisplayContextMenu(ParentForm.Handle, Where, PathName,
          FileList, Verb, False);
        FileList.Destroy;
      end;

      {------------ Cut -----------}
      if Verb = shcCut then
      begin
        LastClipBoardOperation := cboCut;
        {Clear items previous marked as cut:}
        Item := GetNextItem(nil, sdAll, [isCut]);
        while Assigned(Item) do
        begin
          Item.Cut := False;
          Item := GetNextItem(Item, sdAll, [isCut]);
        end;
        {Set property cut to TRUE for all selected items:}
        Item := GetNextItem(nil, sdAll, [isSelected]);
        while Assigned(Item) do
        begin
          Item.Cut := True;
          Item := GetNextItem(Item, sdAll, [isSelected]);
        end;
      end
        else
      {----------- Copy -----------}
      if Verb = shcCopy then LastClipBoardOperation := cboCopy
        else
      {----------- Paste ----------}
      if Verb = shcPaste then
          PasteFromClipBoard(ItemFullFileName(Selected))
        else
      if not FIsRecycleBin then Reload2;
    end
      else
    if Assigned(ItemFocused) and Assigned(ItemFocused.Data) then
    begin
      Verb := EmptyStr;
      WithEdit := not FisRecycleBin and CanEdit(ItemFocused);
      LoadEnabled := True;

      if FIsRecycleBin then
      begin
        PIDL_GetRelative(PFileRec(ItemFocused.Data)^.PIDL, PIDLPath, PIDLRel);
        ShellDisplayContextMenu(ParentForm.Handle, Where,
          iRecycleFolder, 1, PIDLRel, False, Verb, False);
        FreePIDL(PIDLRel);
        FreePIDL(PIDLPath);
      end
        else
      begin
        ShellDisplayContextMenu(ParentForm.Handle, Where,
          ItemFullFileName(ItemFocused), WithEdit, Verb,
          not PFileRec(ItemFocused.Data)^.isDirectory);
        LoadEnabled := True;
      end; {not FisRecycleBin}

      {---------- Rename ----------}
      if Verb = shcRename then ItemFocused.EditCaption
        else
      {------------ Cut -----------}
      if Verb = shcCut then
      begin
        LastClipBoardOperation := cboCut;

        Item := GetNextItem(nil, sdAll, [isCut]);
        while Assigned(Item) do
        begin
          Item.Cut := False;
          Item := GetNextItem(ITem, sdAll, [isCut]);
        end;
        ItemFocused.Cut := True;
      end
        else
      {----------- Copy -----------}
      if Verb = shcCopy then LastClipBoardOperation := cboCopy
        else
      {----------- Paste ----------}
      if Verb = shcPaste then
      begin
        if PFileRec(ItemFocused.Data)^.IsDirectory then
        PasteFromClipBoard(ItemFullFileName(ItemFocused));
      end
        else
      if not FIsRecycleBin then Reload2;
    end;
    ChDir(DefDir);

    if IsRecycleBin and (Verb <> shcCut) and (Verb <> shcProperties) and (SelCount > 0) then
    begin
      DiffSelectedPath := False;
      for Index := Selected.Index to Items.Count - 1 do
        if ExtractFilePath(PFileRec(Items[Index].Data)^.FileName) <> FPath + '\' then
        begin
          DiffSelectedPath := True;
          Break;
        end;

      if DiffSelectedPath then
      begin
{$IFNDEF NO_THREADS}
        StartFileDeleteThread;
{$ENDIF}
        Exit;
      end;
    end;

    if Win32PlatForm = VER_PLATFORM_WIN32_NT then Sleep(250);
    ValidateSelectedFiles;

  finally
{$IFNDEF NO_THREADS}
    StartWatchThread;
{$ENDIF}
  end;
end;

procedure TDirView.GetDisplayInfo(ListItem: TListItem;
  var DispInfo: TLVItem);
begin
  Assert(Assigned(ListItem) and Assigned(ListItem.Data));
  with PFileRec(ListItem.Data)^, DispInfo  do
  begin
    {Fetch display data of current file:}
    if Empty then
      GetDisplayData(ListItem, IconEmpty and
        (not FUseIconUpdateThread or
         ((ViewStyle <> vsReport) and (Win32PlatForm = VER_PLATFORM_WIN32_NT))));

    if IconEmpty and
       (not FUseIconUpdateThread or
        ((ViewStyle <> vsReport) and (Win32PlatForm = VER_PLATFORM_WIN32_NT))) and
       ((DispInfo.Mask and LVIF_IMAGE) <> 0) then
      GetDisplayData(ListItem, True);

    {Set IconUpdatethread :}
{$IFNDEF NO_THREADS}
    if IconEmpty and Assigned(FIconUpdateThread) then
    begin
      if Assigned(TopItem) then
      {Viewstyle is vsReport or vsList:}
        FIconUpdateThread.Index := Self.TopItem.Index
      else
        {Viewstyle is vsIcon or vsSmallIcon:}
        FIconUpdateThread.MaxIndex := ListItem.Index;

      if FIconUpdateThread.Suspended and not FIsRecycleBin then
        FIconUpdateThread.Resume;
    end;
{$ENDIF}

    if (DispInfo.Mask and LVIF_TEXT) <> 0 then
    begin
      if iSubItem = 0 then StrPLCopy(pszText, DisplayName, cchTextMax)
        else
      if iSubItem < DirViewColumns then
      begin
        case TDirViewCol(iSubItem) of
          dvSize: {Size:     }
            if not IsDirectory or
               (IsDirectory and ShowSubDirSize and (Size >= 0)) then
                 StrPLCopy(pszText, FormatSize(Size), cchTextMax);
          dvType: {FileType: }
            if SortByExtension and (not IsDirectory) then
            begin
              case FFileNameDisplay of
                fndNoCap, fndNice: StrPLCopy(pszText, LowerCase(FileExt), cchTextMax);
                else StrPLCopy(pszText, FileExt, cchTextMax);
              end; {Case}
            end
              else StrPLCopy(pszText, TypeName, cchTextMax);
          dvChanged: {Date}
            StrPLCopy(pszText, FormatFileTime(FileTime), cchTextMax);
          dvAttr: {Attrs:}
            if FFileNameDisplay = fndCap then
              StrPLCopy(pszText, UpperCase(GetAttrString(Attr)), cchTextMax)
            else
              StrPLCopy(pszText, GetAttrString(Attr), cchTextMax);
          dvExt:
            StrPLCopy(pszText, FileExt, cchTextMax);
        end {Case}
      end {SubItem}
        else pszText[0] := #0;
    end;

    {Set display icon of current file:}
    if (iSubItem = 0) and ((DispInfo.Mask and LVIF_IMAGE) <> 0) then
    begin
      iImage := PFileRec(ListItem.Data).ImageIndex;
      Mask := Mask or LVIF_DI_SETITEM;
    end;
  end; {With PFileRec Do}
  {Mask := Mask Or LVIF_DI_SETITEM; {<== causes flickering display and icons not to be updated on renaming the item}
end;

function TDirView.ItemColor(Item: TListItem): TColor;
begin
  if PFileRec(Item.Data).Attr and FILE_ATTRIBUTE_COMPRESSED <> 0 then
      Result := FCompressedColor
    else
  if DimmHiddenFiles and not Item.Selected and
     (PFileRec(Item.Data).Attr and FILE_ATTRIBUTE_HIDDEN <> 0) then
      Result := clGrayText
    else
  Result := clDefaultItemColor;
end;

{$IFNDEF NO_THREADS}

procedure TDirView.StartFileDeleteThread;
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    CreateFileList(False, True, Files);
    TFileDeleteThread.Create(Files, MaxWaitTimeOut, SignalFileDelete);
  finally
    Files.Free;
  end;
end;

procedure TDirView.StartIconUpdateThread;
begin
  if DirOK then
  begin
    if not Assigned(FIconUpdateThread) then
    begin
      if Items.Count > 0 then
        FIconUpdateThread := TIconUpdateThread.Create(Self);
    end
      else
    begin
      Assert(not FIconUpdateThread.Terminated);
      FIconUpdateThread.Index := 0;
      if ViewStyle = vsReport then
        FIconUpdateThread.Resume;
    end;
  end;
end; {StartIconUpdateThread}

procedure TDirView.StartSubDirScanner;
var
  Index: Integer;
begin
  if not (csDesigning in ComponentState) and
     DirOk and ShowDirectories and ShowSubDirSize then
  for Index := 0 to Items.Count - 1 do
    with PFileRec(Items[Index].Data)^ do
      if IsDirectory and not isParentDir then
        FSubDirScanner.Add(TSubDirScanner.Create(Self, Items[Index]));
end; {StartSubDirScanner}

procedure TDirView.StopSubDirScanner;
var
  Index: Integer;
begin
  for Index := 0 To FSubDirScanner.Count - 1 do
    if Assigned(FSubDirScanner[Index]) then
      with TSubDirScanner(FSubDirScanner[Index]) do
      begin
        Priority := tpHigher;
        Resume;
        Terminate;
      end;
  Application.ProcessMessages;
end; {StopSubDirScanner}

procedure TDirView.StopIconUpdateThread;
var
  Counter: Integer;
begin
  if Assigned(FIconUpdateThread) then
  begin
    Counter := 0;
    FIconUpdateThread.Terminate;
    FIconUpdateThread.Priority := tpHigher;
    if fIconUpdateThread.Suspended then
      FIconUpdateThread.Resume;
    Sleep(0);

    try
      {Wait until the thread has teminated to prevent AVs:}
      while not FIUThreadFinished do
      begin
        Sleep(10);
        Application.ProcessMessages;
        Inc(Counter);
        {Raise an exception after 2 second, if the thread has not terminated:}
        if Counter = 200 then
        begin
          {MP}raise EIUThread.Create(SIconUpdateThreadTerminationError);
          Break;
        end;
      end;
    finally
      FIconUpdateThread.Destroy;
      FIconUpdateThread := nil;
    end;
  end;
end; {StopIconUpdateThread}

procedure TDirView.StopWatchThread;
begin
  if Assigned(FDiscMonitor) then
  begin
    FDiscMonitor.Free;
    FDiscMonitor := nil;
  end;
end; {StopWatchThread}

procedure TDirView.StartWatchThread;
begin
  if (Length(Path) > 0) and WatchForChanges and DirOK and
     (Pos(Path[1], NoCheckDrives) = 0) then
  begin
    if not Assigned(FDiscMonitor) then
    begin
      FDiscMonitor := TDiscMonitor.Create(Self);
      with FDiscMonitor do
      begin
        ChangeDelay := msThreadChangeDelay;
        SubTree := False;
        Filters := [moDirName, moFileName, moSize, moAttributes, moLastWrite];
        SetDirectory(PathName);
        OnChange := ChangeDetected;
        OnInvalid := ChangeInvalid;
        Open;
      end;
    end
      else
    begin
      FDiscMonitor.SetDirectory(PathName);
      FDiscMonitor.Open;
    end;
  end
end; {StartWatchThread}

{$ENDIF}

procedure TDirView.TimerOnTimer(Sender: TObject);
begin
  if not Loading then
  begin
    // fix by MP: disable timer and reload directory before call to event
    FChangeTimer.Enabled := False;
    FChangeTimer.Interval := 0;
    Reload2;
    if Assigned(FOnChangeDetected) then
      FOnChangeDetected(Self);
  end
end; {TimerOnTimer}

procedure TDirView.ChangeDetected(Sender: TObject; const Directory: string;
  var SubdirsChanged: Boolean);
begin
  // avoid prolonging the actual update with each change, as if continous change
  // is occuring in current directory, the panel will never be updated
  if not FChangeTimer.Enabled then
  begin
    FDirty := True;
    FChangeTimer.Interval := FChangeInterval;
    FChangeTimer.Enabled := True;
  end;
end; {ChangeDetected}

procedure TDirView.ChangeInvalid(Sender: TObject; const Directory: string;
  const ErrorStr: string);
begin
  FDiscMonitor.Close;
  if Assigned(FOnChangeInvalid) then
    FOnChangeInvalid(Self);
end; {ChangeInvalid}

procedure TDirView.Syncronize;
begin
  Application.ProcessMessages;
  FChangeTimer.Enabled := False;
  FChangeTimer.Interval := 0;
  LoadEnabled := True;
  if Dirty then Reload2;
end; {Syncronize}

{$IFNDEF NO_THREADS}

function TDirView.WatchThreadActive: Boolean;
begin
  Result := WatchForChanges and Assigned(FDiscMonitor) and
    FDiscMonitor.Active;
end; {WatchThreadActive}

{$ENDIF}

procedure TDirView.SetChangeInterval(Value: Cardinal);
begin
  if Value > 0 then
  begin
    FChangeInterval := Value;
    FChangeTimer.Interval := Value;
  end;
end; {SetChangeInterval}

procedure TDirView.SetFileNameDisplay(Value: TFileNameDisplay);
begin
  if Value <> FileNameDisplay then
  begin
    FFileNameDisplay := Value;
    if DirOK then Reload(True);
  end;
end; {SetFileNameDisplay}

procedure TDirView.SetDirColProperties(Value: TDirViewColProperties);
begin
  if Value <> ColProperties then
    ColProperties := Value;
end;

function TDirView.GetDirColProperties: TDirViewColProperties;
begin
  Result := TDirViewColProperties(ColProperties);
end;

procedure TDirView.SetShowSubDirSize(Value: Boolean);
begin
  if Value <> ShowSubDirSize then
  begin
    inherited;
    if Value then
    begin
      {$IFNDEF NO_THREADS}
      if ShowDirectories then
        StartSubDirScanner;
      {$ENDIF}
    end
      else
    begin
      {$IFNDEF NO_THREADS}
      StopSubDirScanner;
      {$ENDIF}
      Invalidate;
    end;
  end;
end; {SetShowSubDirSize}

procedure TDirView.SetWatchForChanges(Value: Boolean);
begin
  if WatchForChanges <> Value then
  begin
    FWatchForChanges := Value;
    if not (csDesigning in ComponentState) then
    begin
{$IFNDEF NO_THREADS}
      if Value then StartWatchThread
        else StopWatchThread;
{$ENDIF}
    end;
  end;
end; {SetWatchForChanges}

procedure TDirView.DisplayPropertiesMenu;
var
  FileList: TStringList;
  Index: Integer;
  PIDLRel: PItemIDList;
  PIDLPath: PItemIDList;
begin
  if not Assigned(ItemFocused) then
      ShellExecuteContextCommand(ParentForm.Handle, shcProperties, PathName)
    else
  if (not IsRecycleBin) and (MarkedCount > 1) and ItemFocused.Selected then
  begin
    FileList := TStringList.Create;
    try
      CreateFileList(False, True, FileList);
      for Index := 0 to Pred(FileList.Count) do
        FileList[Index] := ExtractFileName(FileList[Index]);
      ShellExecuteContextCommand(ParentForm.Handle, shcProperties,
        PathName, FileList);
    finally
      FileList.Free;
    end;
  end
    else
  if Assigned(ItemFocused.Data) then
  begin
    if IsRecycleBin then
    begin
      if Assigned(PFileRec(ItemFocused.Data)^.PIDL) then
      begin
        PIDL_GetRelative(PFileRec(ItemFocused.Data)^.PIDL, PIDLPath, PIDLRel);
        ShellExecuteContextCommand(ParentForm.Handle, shcProperties, iRecycleFolder, 1, PIDLRel);
        FreePIDL(PIDLRel);
        FreePIDL(PIDLPath);
      end;
    end
      else
    ShellExecuteContextCommand(ParentForm.Handle, shcProperties,
      ItemFullFileName(ItemFocused));
  end;
end;

procedure TDirView.ExecuteFile(Item: TListItem);
var
  DefDir: string;
  FileName: string;
  {$IFDEF USE_DRIVEVIEW}
  Node: TTreeNode;
  {$ENDIF}
begin
  if (UpperCase(PFileRec(Item.Data)^.FileExt) = 'LNK') or
     PFileRec(Item.Data)^.IsDirectory then
  begin
    if PFileRec(Item.Data)^.IsDirectory then
    begin
      FileName := ItemFullFileName(Item);
      if not DirExists(FileName) then
      begin
        Reload2;
        {$IFDEF USE_DRIVEVIEW}
        if Assigned(FDriveView) and Assigned(TDriveView(FDriveView).Selected) then
          with FDriveView as TDriveView do
            ValidateDirectory(Selected);
        {$ENDIF}
        Exit;
      end;
    end
      else
    FileName := ResolveFileShortCut(ItemFullFileName(Item), True);

    if DirExists(FileName) then
    begin
      {$IFDEF USE_DRIVEVIEW}
      if Assigned(FDriveView) then
        with (FDriveView as TDriveView) do
        begin
          Node := FindNodeToPath(FileName);
          if not Assigned(Node) then
          begin
            ValidateDirectory(GetDriveStatus(FileName[1]).RootNode);
            Node := FindNodeToPath(FileName);
          end;
          if Assigned(Node) then
          begin
            Directory := FileName;
            CenterNode(Selected);
          end;
          Exit;
        end
      else
      {$ENDIF}
      begin
        Path := FileName;
        Exit;
      end;
    end
      else
    if not FileExists(FileName) then Exit;
  end;

  GetDir(0, DefDir);
  ChDir(PathName);

  try
    ShellExecuteContextCommand(ParentForm.Handle, shcDefault,
      ItemFullFileName(Item));
  finally
    ChDir(DefDir);
  end;
end;

procedure TDirView.ExecuteDrive(Drive: TDriveLetter);
var
  APath: string;
begin
  if FLastPath[Drive] <> '' then
  begin
    APath := FLastPath[Drive];
    if not DirectoryExists(APath) then
      APath := Format('%s:', [Drive]);
  end
    else
  begin
    GetDir(Integer(Drive) - Integer('A') + 1, APath);
    APath := ExcludeTrailingPathDelimiter(APath);
  end;

  if Path <> APath then
    Path := APath;
end;

procedure TDirView.ExecuteHomeDirectory;
begin
  Path := HomeDirectory;
end;

procedure TDirView.ExecuteParentDirectory;
begin
  if Valid then
  begin
    {$IFDEF USE_DRIVEVIEW}
    if Assigned(DriveView) and Assigned(TDriveView(DriveView).Selected) then
      TDriveView(DriveView).Selected := TDriveView(DriveView).Selected.Parent
    else
    {$ENDIF}
    Path := ExtractFilePath(Path);
  end;
end;

procedure TDirView.ExecuteRootDirectory;
begin
  if Valid then
  try
    PathChanging(False);
    FPath := ExtractFileDrive(Path);
    Load;
  finally
    PathChanged;
  end;
end;

procedure TDirView.Delete(Item: TListItem);
begin
  if Assigned(Item) and Assigned(Item.Data) then
    with PFileRec(Item.Data)^ do
    begin
      SetLength(FileName, 0);
      SetLength(TypeName, 0);
      SetLength(DisplayName, 0);
      if Assigned(PIDL) then FreePIDL(PIDL);
      Dispose(PFileRec(Item.Data));
      Item.Data := nil;
    end;
  inherited Delete(Item);
end; {Delete}

procedure TDirView.InternalEdit(const HItem: TLVItem);
var
  Item: TListItem;
  Info: string;
  NewCaption: string;
  {$IFDEF USE_DRIVEVIEW}
  IsDirectory: Boolean;
  {$ENDIF}
begin
  Item := GetItemFromHItem(HItem);
  {$IFDEF USE_DRIVEVIEW}
  IsDirectory := DirExists(ItemFullFileName(Item));
  {$ENDIF}
  NewCaption := HItem.pszText;

{$IFNDEF NO_THREADS}
  StopWatchThread;
  {$IFDEF USE_DRIVEVIEW}
  if IsDirectory and Assigned(FDriveView) then
    TDriveView(FDriveView).StopWatchThread;
  {$ENDIF}
{$ENDIF}

  with FFileOperator do
  begin
    Flags := [foAllowUndo, foNoConfirmation];
    Operation := foRename;
    OperandFrom.Clear;
    OperandTo.Clear;
    OperandFrom.Add(ItemFullFileName(Item));
    OperandTo.Add(fPath + '\' + HItem.pszText);
  end;

  try
    if FFileOperator.Execute then
    begin
      {$IFDEF USE_DRIVEVIEW}
      if IsDirectory and Assigned(FDriveView) then
        with (FDriveView as TDriveView) do
          if Assigned(Selected) then
            ValidateDirectory(Selected);
      {$ENDIF}

      with GetFileRec(Item.Index)^ do
      begin
        Empty := True;
        IconEmpty := True;
        FileName := NewCaption;
        DisplayName := FileName;
        FileExt := UpperCase(ExtractFileExt(HItem.pszText));
        FileExt := Copy(FileExt, 2, Length(FileExt) - 1);
        TypeName := EmptyStr;
        if Assigned(PIDL) then
          FreePIDL(PIDL);
      end;
      GetDisplayData(Item, True);
      ResetItemImage(Item.Index);
      UpdateItems(Item.Index, Item.Index);
      if Assigned(OnEdited) then OnEdited(Self, Item, NewCaption);
      if Item <> nil then Item.Caption := NewCaption;
      SortItems;
      if Assigned(ItemFocused) then ItemFocused.MakeVisible(False);
    end
      else
    begin
      Item.Caption := GetFileRec(Item.Index)^.FileName;
      Item.Update;

      if FileOrDirExists(IncludeTrailingPathDelimiter(FPath) + HItem.pszText) then
        Info := SErrorRenameFileExists + HItem.pszText
      else
        Info := SErrorRenameFile + HItem.pszText;

      MessageBeep(MB_ICONHAND);
      if MessageDlg(FormatLastOSError(Info), mtError, [mbOK, mbAbort], 0) = mrOK then
        RetryRename(HItem.pszText);
    end;
  finally
    Sleep(0);
    LoadEnabled := True;
{$IFNDEF NO_THREADS}
    if FWatchForChanges and (not WatchThreadActive) then
      StartWatchThread;
    {$IFDEF USE_DRIVEVIEW}
    if Assigned(FDriveView) then
      TDriveView(FDriveView).StartWatchThread;
    {$ENDIF}
{$ENDIF}
  end;
end;

function TDirView.ItemFileName(Item: TListItem): string;
begin
  if Assigned(Item) and Assigned(Item.Data) then
    Result := ExtractFileName(PFileRec(Item.Data)^.FileName)
  else
    Result := '';
end;

function TDirView.ItemFileSize(Item: TListItem): Int64;
begin
  Result := 0;
  if Assigned(Item) and Assigned(Item.Data) then
    with PFileRec(Item.Data)^ do
      if Size >= 0 then Result := Size;
end;

function TDirView.ItemFileTime(Item: TListItem;
  var Precision: TDateTimePrecision): TDateTime;
begin
  Result := FileTimeToDateTime(PFileRec(Item.Data)^.FileTime);
  Precision := tpMillisecond;
end;

function TDirView.ItemImageIndex(Item: TListItem;
  Cache: Boolean): Integer;
begin
  if Assigned(Item) and Assigned(Item.Data) then
  begin
    if PFileRec(Item.Data)^.IconEmpty then
    begin
      if Cache then Result := -1
        else Result := UnknownFileIcon;
    end
      else
    begin
      if (not Cache) or MatchesFileExt(PFileRec(Item.Data)^.FileExt, SpecialExtensions) then
        Result := PFileRec(Item.Data)^.ImageIndex
      else
        Result := -1
    end;
  end
    else Result := -1;
end;

{$IFDEF USE_DRIVEVIEW}
procedure TDirView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDriveView) then
    FDriveView := nil;
end; {Notification}
{$ENDIF}

procedure TDirView.ReloadDirectory;
begin
  Reload(True);
end;

procedure TDirView.ResetItemImage(Index: Integer);
var
  LVI: TLVItem;
begin
  with PFileRec(Items[Index].Data)^, LVI do
  begin
    {Update imageindex:}
    Mask := LVIF_STATE or LVIF_DI_SETITEM or LVIF_IMAGE;
    iItem := Index;
    iSubItem := 0;
    if ListView_GetItem(Handle, LVI) then
    begin
      iImage := I_IMAGECALLBACK;
      Mask := Mask and (not LVIF_DI_SETITEM);
      ListView_SetItem(Handle, LVI);
    end;
  end; {With}
end; {ResetItemImage}

procedure TDirView.SetAttrSpace(Value: string);
begin
  if Value <> FAttrSpace then
  begin
    FAttrSpace := Value;
    Invalidate;
  end;
end; {SetAttrSpace}

procedure TDirView.SetNoCheckDrives(Value: string);
begin
  FNoCheckDrives := UpperCase(Value);
end; {SetNoCheckDrives}

{ Drag&Drop handling }

{$IFNDEF NO_THREADS}
procedure TDirView.SignalFileDelete(Sender: TObject; Files: TStringList);
{Called by TFileDeleteThread, when a file was deleted by the Drag&Drop target window:}
var
  Index: Integer;
begin
  if Files.Count > 0 then
    for Index := 0 to Files.Count - 1 do
      ValidateFile(Files[Index]);
end;
{$ENDIF}

procedure TDirView.DDMenuDone(Sender: TObject; AMenu: HMenu);
begin
{$IFNDEF NO_THREADS}
  if not WatchThreadActive then
{$ENDIF}
  begin
    FChangeTimer.Interval := Min(FChangeInterval * 2, 3000);
    FChangeTimer.Enabled  := True;
  end;
  inherited;
end;

procedure TDirView.DDDropHandlerSucceeded(Sender: TObject; grfKeyState: Longint;
  Point: TPoint; dwEffect: Longint);
begin
{$IFNDEF NO_THREADS}
  if not WatchThreadActive then
{$ENDIF}
  begin
    FChangeTimer.Interval := FChangeInterval;
    FChangeTimer.Enabled  := True;
  end;
  inherited;
end;

procedure TDirView.AddToDragFileList(FileList: TFileList; Item: TListItem);
begin
  Assert(Assigned(Item));
  if IsRecycleBin then
  begin
    if Assigned(Item.Data) then
    begin
      if UpperCase(ExtractFileExt(PFileRec(Item.Data)^.DisplayName)) =
        ('.' + PFileRec(Item.Data)^.FileExt) then
          FileList.AddItemEx(PFileRec(Item.Data)^.PIDL,
            ItemFullFileName(Item), PFileRec(Item.Data)^.DisplayName)
      else
        FileList.AddItemEx(PFileRec(Item.Data)^.PIDL,
          ItemFullFileName(Item), PFileRec(Item.Data)^.DisplayName +
            ExtractFileExt(PFileRec(Item.Data)^.FileName));
    end;
  end
    else inherited;
end;

procedure TDirView.DDDragDetect(grfKeyState: Longint; DetectStart, Point: TPoint;
  DragStatus:TDragDetectStatus);
{$IFNDEF NO_THREADS}
var
  WasWatchThreadActive: Boolean;
{$ENDIF}
begin
  if (DragStatus = ddsDrag) and (MarkedCount > 0) then
  begin
{$IFNDEF NO_THREADS}
    WasWatchThreadActive := WatchThreadActive;
{$ENDIF}
    inherited;

{$IFNDEF NO_THREADS}
    if (LastDDResult = drMove) and (not WasWatchThreadActive) then
      StartFileDeleteThread;
{$ENDIF}
  end;
end; {DDDragDetect}

procedure TDirView.DDChooseEffect(grfKeyState: Integer;
  var dwEffect: Integer);
begin
  if (not SelfDropDuplicates) and DragDropFilesEx.OwnerIsSource and
     (dwEffect = DropEffect_Copy) and (not Assigned(DropTarget)) then
        dwEffect := DropEffect_None
    else
  if (grfKeyState and (MK_CONTROL or MK_SHIFT) = 0) then
  begin
    if ExeDrag and (Path[1] >= FirstFixedDrive) and
      (DragDrive >= FirstFixedDrive) then dwEffect := DropEffect_Link
      else
    if DragOnDriveIsMove and
       (not DDOwnerIsSource or Assigned(DropTarget)) and
       (((DragDrive = Upcase(Path[1])) and (dwEffect = DropEffect_Copy) and
       (DragDropFilesEx.AvailableDropEffects and DropEffect_Move <> 0))
         or IsRecycleBin) then dwEffect := DropEffect_Move;
  end;

  inherited;
end;

procedure TDirView.PerformDragDropFileOperation(TargetPath: string;
  dwEffect: Integer; RenameOnCollision: Boolean);
var
  Index: Integer;
  SourcePath: string;
  SourceFile: string;
  OldCursor: TCursor;
  OldWatchForChanges: Boolean;
  DoFileOperation: Boolean;
  IsRecycleBin: Boolean;
  {$IFDEF USE_DRIVEVIEW}
  SourceIsDirectory: Boolean;
  Node: TTreeNode;
  {$ENDIF}
begin
  if DragDropFilesEx.FileList.Count > 0 then
  begin
    if not DirExists(TargetPath) then
    begin
      Reload(True);
      DDError(DDPathNotFoundError);
    end
      else
    begin
      IsRecycleBin := Self.IsRecycleBin or
        ((DropTarget <> nil) and ItemIsRecycleBin(DropTarget));
      if not (DragDropFilesEx.FileNamesAreMapped and IsRecycleBin) then
      begin
        OldCursor := Screen.Cursor;
        OldWatchForChanges := WatchForChanges;
        {$IFDEF USE_DRIVEVIEW}
        SourceIsDirectory := True;
        {$ENDIF}
        SourcePath := EmptyStr;

        try
          Screen.Cursor := crHourGlass;
          WatchForChanges := False;

          if (dwEffect in [DropEffect_Copy, DropEffect_Move]) then
          begin
{$IFNDEF NO_THREADS}
            StopWatchThread;

            {$IFDEF USE_DRIVEVIEW}
            if Assigned(DriveView) then
              TDriveView(DriveView).StopWatchThread;
            {$ENDIF}

            if (DropSourceControl <> Self) and
               (DropSourceControl is TDirView) then
                TDirView(DropSourceControl).StopWatchThread;
{$ENDIF}

            SourcePath := '';

            {Set the source filenames:}
            for Index := 0 to DragDropFilesEx.FileList.Count - 1 do
            begin
              FFileOperator.OperandFrom.Add(
                TFDDListItem(DragDropFilesEx.FileList[Index]^).Name);
              if DragDropFilesEx.FileNamesAreMapped then
                FFileOperator.OperandTo.Add(IncludeTrailingPathDelimiter(TargetPath) +
                  TFDDListItem(DragDropFilesEx.FileList[Index]^).MappedName);

              if SourcePath = '' then
              begin
                if DirExists(TFDDListItem(DragDropFilesEx.FileList[Index]^).Name) then
                begin
                  SourcePath := TFDDListItem(DragDropFilesEx.FileList[Index]^).Name;
                  {$IFDEF USE_DRIVEVIEW}
                  SourceIsDirectory := True;
                  {$ENDIF}
                end
                  else
                begin
                  SourcePath := ExtractFilePath(TFDDListItem(DragDropFilesEx.FileList[Index]^).Name);
                  {$IFDEF USE_DRIVEVIEW}
                  SourceIsDirectory := False;
                  {$ENDIF}
                end;
              end;
            end;

            FFileOperator.Flags := [foAllowUndo, foNoConfirmMkDir];
            if RenameOnCollision then
            Begin
              FFileOperator.Flags := FFileOperator.Flags + [foRenameOnCollision];
              FFileOperator.WantMappingHandle := True;
            end
              else FFileOperator.WantMappingHandle := False;

            {Set the target directory or the target filenames:}
            if DragDropFilesEx.FileNamesAreMapped and (not IsRecycleBin) then
              FFileOperator.Flags := FFileOperator.Flags + [foMultiDestFiles]
              else
            begin
              FFileOperator.Flags := FFileOperator.Flags - [foMultiDestFiles];
              FFileOperator.OperandTo.Clear;
              FFileOperator.OperandTo.Add(TargetPath);
            end;

            {if the target directory is the recycle bin, then delete the selected files:}
            if IsRecycleBin then FFileOperator.Operation := foDelete
              else
            case dwEffect of
              DropEffect_Copy: FFileOperator.Operation := foCopy;
              DropEffect_Move: FFileOperator.Operation := foMove;
            end;

            if IsRecycleBin then
            begin
              if not ConfirmDelete then
                FFileOperator.Flags := FFileOperator.Flags + [foNoConfirmation];
            end
              else
            if not ConfirmOverwrite then
              FFileOperator.Flags := FFileOperator.Flags + [foNoConfirmation];

            DoFileOperation := True;
            if Assigned(OnDDFileOperation) then
              OnDDFileOperation(Self, dwEffect, SourcePath, TargetPath,
                DoFileOperation);

            if DoFileOperation and (FFileOperator.OperandFrom.Count > 0) then
            begin
              FFileOperator.Execute;
              ReLoad2;
              if DragDropFilesEx.FileNamesAreMapped then
                FFileOperator.ClearUndo;
              if Assigned(OnDDFileOperationExecuted) then
                OnDDFileOperationExecuted(Self, dwEffect, SourcePath, TargetPath);
            end;
          end
            else
          if dwEffect = DropEffect_Link then
          (* Create Link requested: *)
          begin
{$IFNDEF NO_THREADS}
            StopWatchThread;
{$ENDIF}
            for Index := 0 to DragDropFilesEx.FileList.Count - 1 do
            begin
              SourceFile := TFDDListItem(DragDropFilesEx.FileList[Index]^).Name;

              if Length(SourceFile) = 3 then
                {Create a link to a drive:}
                SourcePath := Copy(DriveInfo[SourceFile[1]].PrettyName, 4, 255) + '(' + SourceFile[1] + ')'
              else
                {Create a link to a file or directory:}
                SourcePath := ExtractFileName(SourceFile);

              if not CreateFileShortCut(SourceFile, IncludeTrailingPathDelimiter(TargetPath) +
                ChangeFileExt(SourcePath,'.lnk'),
                ExtractFileNameOnly(SourceFile)) then
                  DDError(DDCreateShortCutError);
            end;
            ReLoad2;
          end;

          if Assigned(DropSourceControl) and
             (DropSourceControl is TDirView) and
             (DropSourceControl <> Self) and
             (dwEffect = DropEffect_Move) then
                TDirView(DropSourceControl).ValidateSelectedFiles;

          {$IFDEF USE_DRIVEVIEW}
          if Assigned(FDriveView) and SourceIsDirectory then
            with TDriveView(FDriveView) do
            begin
              try
                ValidateDirectory(FindNodeToPath(TargetPath));
              except
              end;

              if (dwEffect = DropEffect_Move) or IsRecycleBin then
              try
                Node := FindNodeToPath(SourcePath);
                if Assigned(Node) and Assigned(Node.Parent) then
                  Node := Node.Parent;
                ValidateDirectory(Node);
              except
              end;
            end;
          {$ENDIF}
        finally
          FFileOperator.OperandFrom.Clear;
          FFileOperator.OperandTo.Clear;
          {$IFDEF USE_DRIVEVIEW}
{$IFNDEF NO_THREADS}
          if Assigned(FDriveView) then
            TDriveView(FDriveView).StartWatchThread;
{$ENDIF}
          {$ENDIF}
          Sleep(0);
          WatchForChanges := OldWatchForChanges;
{$IFNDEF NO_THREADS}
          if (DropSourceControl <> Self) and (DropSourceControl is TDirView) then
            TDirView(DropSourceControl).StartWatchThread;
{$ENDIF}
          Screen.Cursor := OldCursor;
        end;
      end;
    end;
  end;
end; {PerformDragDropFileOperation}

procedure TDirView.DDError(ErrorNo: TDDError);
begin
  if Assigned(OnDDError) then OnDDError(Self, ErrorNo)
    else
  raise EDragDrop.Create(Format(SDragDropError, [Ord(ErrorNo)]));
end; {DDError}

function TDirView.GetCanUndoCopyMove: Boolean;
begin
  Result := Assigned(FFileOperator) and FFileOperator.CanUndo;
end; {CanUndoCopyMove}

function TDirView.UndoCopyMove : Boolean;
var
  LastTarget: string;
  LastSource: string;
begin
  Result := False;
  if FFileOperator.CanUndo then
  begin
    Lasttarget := FFileOperator.LastOperandTo[0];
    LastSource := FFileOperator.LastOperandFrom[0];
{$IFNDEF NO_THREADS}
    {$IFDEF USE_DRIVEVIEW}
    if Assigned(FDriveView) then
      TDriveView(FDriveView).StopAllWatchThreads;
    {$ENDIF}
{$ENDIF}

    Result := FFileOperator.UndoExecute;

{$IFNDEF NO_THREADS}
    if not WatchthreadActive then
{$ENDIF}
      Reload2;

    {$IFDEF USE_DRIVEVIEW}
    if Assigned(FDriveView) then
      with TDriveView(FDriveView) do
      begin
        ValidateDirectory(FindNodeToPath(ExtractFilePath(LastTarget)));
        ValidateDirectory(FindNodeToPath(ExtractFilePath(LastSource)));
{$IFNDEF NO_THREADS}
        StartAllWatchThreads;
{$ENDIF}
      end;
    {$ENDIF}
  end;
end; {UndoCopyMove}

procedure TDirView.EmptyClipboard;
var
  Item: TListItem;
begin
  if Windows.OpenClipBoard(0) then
  begin
    Windows.EmptyClipBoard;
    Windows.CloseClipBoard;
    if LastClipBoardOperation <> cboNone then
    begin
      Item := GetNextItem(nil, sdAll, [isCut]);
      while Assigned(Item) do
      begin
        Item.Cut := False;
        Item := GetNextItem(Item, sdAll, [isCut]);
      end;
    end;
    LastClipBoardOperation := cboNone;
    {$IFDEF USE_DRIVEVIEW}
    if Assigned(FDriveView) then
      TDriveView(FDriveView).LastPathCut := '';
    {$ENDIF}
  end;
end; {EmptyClipBoard}

function TDirView.CopyToClipBoard : Boolean;
var
  Item: TListItem;
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Result := False;
    EmptyClipBoard;
    DragDropFilesEx.FileList.Clear;
    if SelCount > 0 then
    begin
      Item := GetNextItem(nil, sdAll, [isSelected]);
      while Assigned(Item) do
      begin
        DragDropFilesEx.FileList.AddItem(nil, ItemFullFileName(Item));
        Item := GetNextItem(Item, sdAll, [isSelected]);
      end;

      Result := DragDropFilesEx.CopyToClipBoard;
      LastClipBoardOperation := cboCopy;
    end;
  finally
    Screen.Cursor := SaveCursor;
  end;
end; {CopyToClipBoard}

function TDirView.CutToClipBoard : Boolean;
var
  Item: TListItem;
begin
  Result := False;
  EmptyClipBoard;
  DragDropFilesEx.FileList.Clear;
  if SelCount > 0 then
  begin
    Item := GetNextItem(nil, sdAll, [isSelected]);
    while Assigned(Item) do
    begin
      DragDropFilesEx.FileList.AddItem(nil, ItemFullFileName(Item));
      Item.Cut := True;
      Item := GetNextItem(Item, sdAll, [isSelected]);
    end;

    Result := DragDropFilesEx.CopyToClipBoard;
    LastClipBoardOperation := cboCut;
  end;
end; {CutToClipBoard}

function TDirView.PasteFromClipBoard(TargetPath: string): Boolean;
begin
  DragDropFilesEx.FileList.Clear;
  Result := False;
  if CanPasteFromClipBoard and
    {MP}{$IFDEF OLD_DND} DragDropFilesEx.GetFromClipBoard {$ELSE} DragDropFilesEx.PasteFromClipboard {$ENDIF}{/MP}
    then
  begin
    if TargetPath = '' then
      TargetPath := PathName;
    case LastClipBoardOperation of
      cboNone:
        begin
          PerformDragDropFileOperation(TargetPath, DropEffect_Copy, False);
          if Assigned(OnDDExecuted) then OnDDExecuted(Self, DropEffect_Copy);
        end;
      cboCopy:
        begin
          PerformDragDropFileOperation(TargetPath, DropEffect_Copy,
            ExcludeTrailingPathDelimiter(ExtractFilePath(TFDDListItem(DragDropFilesEx.FileList[0]^).Name)) = Path);
          if Assigned(OnDDExecuted) then OnDDExecuted(Self, DropEffect_Copy);
        end;
      cboCut:
        begin
          PerformDragDropFileOperation(TargetPath, DropEffect_Move, False);
          if Assigned(OnDDExecuted) then OnDDExecuted(Self, DropEffect_Move);
          EmptyClipBoard;
        end;
    end;
    Result := True;
  end;
end; {PasteFromClipBoard}

function TDirView.DragCompleteFileList: Boolean;
begin
  Result := inherited DragCompleteFileList and
    (FDriveType <> DRIVE_REMOVABLE);
end;

function TDirView.DuplicateSelectedFiles: Boolean;
begin
  Result := False;
  if SelCount > 0 then
  begin
    Result := CopyToClipBoard;
    if Result then
      try
        SelectNewFiles := True;
        Selected := nil;
        Result := PasteFromClipBoard();
      finally
        SelectNewFiles := False;
        if Assigned(Selected) then
        begin
          ItemFocused := Selected;
          Selected.MakeVisible(False);
          if SelCount = 1 then
            Selected.EditCaption;
        end;
      end;

  end;
  EmptyClipBoard;
end; {DuplicateFiles}

procedure TDirView.FetchAllDisplayData;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    if Assigned(Items[Index]) and Assigned(Items[Index].Data) then
      if PFileRec(Items[Index].Data)^.Empty then
        GetDisplayData(Items[Index], False);
end; {FetchAllDisplayData}

function TDirView.MinimizePath(Path: string; Len: Integer): string;
begin
  Result := MinimizeName(Path, Canvas, Len);
end; { MinimizePath }

function TDirView.NewColProperties: TCustomListViewColProperties;
begin
  Result := TDirViewColProperties.Create(Self);
end;

function TDirView.SortAscendingByDefault(Index: Integer): Boolean;
begin
  Result := not (TDirViewCol(Index) in [dvSize, dvChanged]);
end;

procedure TDirView.SetItemImageIndex(Item: TListItem; Index: Integer);
begin
  Assert(Assigned(Item));
  if Assigned(Item.Data) then
    with PFileRec(Item.Data)^ do
    begin
      ImageIndex := Index;
      IconEmpty := (ImageIndex < 0);
    end;
end;

{=================================================================}

initialization
  LastClipBoardOperation := cboNone;
  LastIOResult := 0;
  DaylightHack := not
    ((Win32MajorVersion > 6) or
     ((Win32MajorVersion = 6) and (Win32MinorVersion >= 1)));
end.
