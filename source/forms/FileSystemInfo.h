//----------------------------------------------------------------------------
#ifndef FileSystemInfoH
#define FileSystemInfoH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.StdActns.hpp>
//----------------------------------------------------------------------------
#include <GUITools.h>
//----------------------------------------------------------------------------
typedef void __fastcall (__closure *TFeedFileSystemData)
  (TControl * Control, int Label, UnicodeString Value);
//----------------------------------------------------------------------------
class TFileSystemInfoDialog : public TForm
{
__published:
  TButton *CloseButton;
  TButton *HelpButton;
  TPageControl *PageControl;
  TTabSheet *ProtocolSheet;
  TGroupBox *HostKeyGroup;
  TEdit *HostKeyFingerprintSHA256Edit;
  TTabSheet *CapabilitiesSheet;
  TGroupBox *InfoGroup;
  TMemo *InfoMemo;
  TListView *ServerView;
  TListView *ProtocolView;
  TButton *ClipboardButton;
  TPopupMenu *ListViewMenu;
  TMenuItem *Copy;
  TTabSheet *SpaceAvailableSheet;
  TListView *SpaceAvailableView;
  TLabel *Label1;
  TEdit *SpaceAvailablePathEdit;
  TButton *SpaceAvailableButton;
  TGroupBox *CertificateGroup;
  TEdit *CertificateFingerprintEdit;
  TButton *CertificateViewButton;
  TLabel *Label2;
  TEdit *HostKeyAlgorithmEdit;
  TLabel *Label3;
  TLabel *Label4;
  TEdit *HostKeyFingerprintMD5Edit;
  TPopupMenu *FingerprintPopupMenu;
  TActionList *FingerprintActionList;
  TEditCopy *EditCopyAction;
  TEditSelectAll *EditSelectAllAction;
  TMenuItem *Copy1;
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ClipboardButtonClick(TObject *Sender);
  void __fastcall CopyClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall SpaceAvailableButtonClick(TObject *Sender);
  void __fastcall PageControlChange(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall SpaceAvailablePathEditEnter(TObject *Sender);
  void __fastcall SpaceAvailablePathEditExit(TObject *Sender);
  void __fastcall ControlContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
  void __fastcall CertificateViewButtonClick(TObject *Sender);
  void __fastcall SpaceAvailableViewCustomDrawItem(TCustomListView *Sender, TListItem *Item,
          TCustomDrawState State, bool &DefaultDraw);
  void __fastcall EditCopyActionExecute(TObject *Sender);
  void __fastcall HostKeyFingerprintSHA256EditContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall EditCopyActionUpdate(TObject *Sender);
public:
  virtual __fastcall TFileSystemInfoDialog(TComponent * AOwner,
    TGetSpaceAvailable OnGetSpaceAvailable);

  void __fastcall Execute(const TSessionInfo & SessionInfo,
    const TFileSystemInfo & FileSystemInfo, UnicodeString SpaceAvailablePath);

private:
  TControl * FLastFeededControl;
  UnicodeString FClipboard;
  TGetSpaceAvailable FOnGetSpaceAvailable;
  bool FSpaceAvailableLoaded;
  TSpaceAvailable FSpaceAvailable;
  int FLastListItem;
  TSessionInfo FSessionInfo;
  TFileSystemInfo FFileSystemInfo;

  void __fastcall Feed(TFeedFileSystemData AddItem);
  void __fastcall UpdateControls();
  UnicodeString __fastcall CapabilityStr(TFSCapability Capability);
  UnicodeString __fastcall CapabilityStr(TFSCapability Capability1,
    TFSCapability Capability2);
  UnicodeString __fastcall SpaceStr(__int64 Bytes);
  void __fastcall ControlsAddItem(TControl * Control, int Label, UnicodeString Value);
  void __fastcall ClipboardAddItem(TControl * Control, int Label, UnicodeString Value);
  void __fastcall CheckSpaceAvailable();
  void __fastcall NeedSpaceAvailable();
  bool __fastcall SpaceAvailableSupported();
  void __fastcall FeedControls();

  INTERFACE_HOOK;
};
//----------------------------------------------------------------------------
#endif
