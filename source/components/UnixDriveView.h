//---------------------------------------------------------------------------
#ifndef UnixDriveViewH
#define UnixDriveViewH
//---------------------------------------------------------------------------
#include "CustomDriveView.hpp"
#include "UnixDirView.h"
//---------------------------------------------------------------------------
struct TNodeData;
class TTerminal;
class TRemoteFileList;
class TRemoteFile;
//---------------------------------------------------------------------------
class TCustomUnixDriveView : public TCustomDriveView
{
friend class TUnixDirView;
public:
  __fastcall TCustomUnixDriveView(TComponent * Owner);
  virtual __fastcall ~TCustomUnixDriveView();

  virtual UnicodeString __fastcall NodePathName(TTreeNode * Node);
  TStrings * __fastcall DragFileList();
  void __fastcall UpdateDropTarget();
  void __fastcall UpdateDropSource();

  __property TUnixDirView * DirView = { read = FDirView, write = SetDirView };
  __property UnicodeString RootName = { read = FRootName, write = FRootName, stored = IsRootNameStored };
  __property bool DDAllowMove = { read = FDDAllowMove, write = FDDAllowMove, default = false };
  __property TDDDragFileName OnDDDragFileName = { read = FOnDDDragFileName, write = FOnDDDragFileName};
  __property bool ShowInaccesibleDirectories = { read=FShowInaccesibleDirectories, write=SetShowInaccesibleDirectories, default=true  };

protected:
  DYNAMIC void __fastcall Delete(TTreeNode * Node);
  DYNAMIC void __fastcall Change(TTreeNode * Node);
  virtual void __fastcall CreateWnd();
  virtual void __fastcall DestroyWnd();
  DYNAMIC bool __fastcall CanChange(TTreeNode * Node);

  void __fastcall LoadDirectory();
  TTreeNode * __fastcall LoadPath(UnicodeString Path);
  TTreeNode * __fastcall LoadPathEasy(TTreeNode * Parent,
    UnicodeString Path, TRemoteFile * File);
  void __fastcall UpdatePath(TTreeNode * Node, bool Force, bool CanLoad = false);
  void __fastcall CheckPendingDeletes();

  void __fastcall SetTerminal(TTerminal * value);

  inline TNodeData * __fastcall NodeData(const TTreeNode * Node);
  inline TRemoteFileList * __fastcall NodeFileList(const TTreeNode * Node);
  inline TRemoteFile * __fastcall NodeFile(const TTreeNode * Node);
  inline TRemoteFile * __fastcall NodeFileForce(TTreeNode * Node);
  inline bool __fastcall NodeIsHidden(const TTreeNode * Node);
  inline bool __fastcall NodeTryDelete(TTreeNode * Node, bool RememberIfFails);

  virtual TCustomDirView * __fastcall GetCustomDirView();
  virtual void __fastcall SetCustomDirView(TCustomDirView * Value);
  void __fastcall SetDirView(TUnixDirView * Value);

  virtual void __fastcall PerformDragDropFileOperation(TTreeNode * Node, int Effect);
  virtual void __fastcall DDChooseEffect(int KeyState, int & Effect, int PreferredEffect);
  virtual bool __fastcall DragCompleteFileList();
  virtual TDropEffectSet __fastcall DDSourceEffects();

  TTreeNode * __fastcall FindNodeToPath(UnicodeString Path);
  virtual UnicodeString __fastcall NodePath(TTreeNode * Node);
  virtual bool __fastcall NodeIsRecycleBin(TTreeNode * Node);
  virtual bool __fastcall NodePathExists(TTreeNode * Node);
  virtual TColor __fastcall NodeColor(TTreeNode * Node);
  virtual TTreeNode * __fastcall FindPathNode(UnicodeString Path);
  virtual void __fastcall GetImageIndex(TTreeNode * Node);
  virtual Word __fastcall NodeOverlayIndexes(TTreeNode * Node);
  virtual void __fastcall ClearDragFileList(TFileList * FileList);
  virtual void __fastcall AddToDragFileList(TFileList * FileList, TTreeNode * Node);

  virtual void __fastcall ValidateDirectoryEx(TTreeNode * Node,
    TRecursiveScan Recurse, bool NewDirs);
  void LoadNodeState(TTreeNode * Node, const UnicodeString & Path);

  virtual void __fastcall RebuildTree();
  virtual void __fastcall DisplayContextMenu(TTreeNode * Node, const TPoint & ScreenPos);
  virtual void __fastcall DisplayPropertiesMenu(TTreeNode * Node);

  TObject * SaveState();

  #pragma warn -inl
  BEGIN_MESSAGE_MAP
    VCL_MESSAGE_HANDLER(CM_SHOWINGCHANGED, TMessage, CMShowingChanged)
  END_MESSAGE_MAP(TCustomDriveView)
  #pragma warn +inl

  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };

private:
  TTerminal * FTerminal;
  TUnixDirView * FDirView;
  UnicodeString FRootName;
  bool FDirectoryLoaded;
  bool FIgnoreChange;
  TTreeNode * FPrevSelected;
  bool FDDAllowMove;
  TDDDragFileName FOnDDDragFileName;
  bool FShowInaccesibleDirectories;
  TRemoteFile * FDummyDragFile;
  TList * FPendingDelete;
  bool FChangingDirectory;

  bool __fastcall IsRootNameStored();
  void __fastcall SetShowInaccesibleDirectories(bool value);
  void __fastcall CMShowingChanged(TMessage & Message);
};
//---------------------------------------------------------------------------
class TUnixDriveView : public TCustomUnixDriveView
{
__published:
  __property DirView;
  __property RootName;
  __property OnDDDragFileName;
  __property OnDDEnd;

  __property TargetPopUpMenu;
  __property UseSystemContextMenu;

  __property OnDDDragEnter;
  __property OnDDDragLeave;
  __property OnDDDragOver;
  __property OnDDDrop;
  __property OnDDQueryContinueDrag;
  __property OnDDChooseEffect;
  __property OnDDGiveFeedback;
  __property OnDDDragDetect;
  __property OnDDProcessDropped;
  __property OnDDError;
  __property OnDDExecuted;
  __property OnDDFileOperation;
  __property OnDDFileOperationExecuted;
  __property OnDDCreateDragFileList;
  __property OnDDCreateDataObject;

  __property Align;
  __property Anchors;
  __property AutoExpand;
  __property BiDiMode;
  __property BorderStyle;
  __property BorderWidth;
  __property ChangeDelay;
  __property Color;
  __property Ctl3D;
  __property Constraints;
  __property DoubleBuffered;
  __property DragKind;
  __property DragCursor;
  __property DragMode = { default = dmAutomatic };
  __property OnDragDrop;
  __property OnDragOver;
  __property Enabled;
  __property Font;
  __property HideSelection;
  __property HotTrack;
  __property Indent;
  __property ParentBiDiMode;
  __property ParentColor;
  __property ParentCtl3D;
  __property ParentDoubleBuffered;
  __property ParentFont;
  __property ParentShowHint;
  __property PopupMenu;
  __property ReadOnly;
  __property RightClickSelect;
  __property RowSelect;
  __property ShowButtons;
  __property ShowHint;
  __property ShowLines;
  __property TabOrder;
  __property TabStop = { default = true };
  __property ToolTips;
  __property Visible;
  __property OnChange;
  __property OnChanging;
  __property OnClick;
  __property OnCollapsing;
  __property OnCollapsed;
  __property OnCompare;
  __property OnDblClick;
  __property OnDeletion;
  __property OnEdited;
  __property OnEditing;
  __property OnEndDock;
  __property OnEndDrag;
  __property OnEnter;
  __property OnExit;
  __property OnExpanding;
  __property OnExpanded;
  __property OnGetImageIndex;
  __property OnGetSelectedIndex;
  __property OnKeyDown;
  __property OnKeyPress;
  __property OnKeyUp;
  __property OnMouseDown;
  __property OnMouseMove;
  __property OnMouseUp;
  __property OnStartDock;
  __property OnStartDrag;
  __property OnBusy;

public:
  __fastcall TUnixDriveView(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
