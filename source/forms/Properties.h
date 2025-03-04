//----------------------------------------------------------------------------
#ifndef PropertiesH
#define PropertiesH
//----------------------------------------------------------------------------
#include "PathLabel.hpp"
#include "Rights.h"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Menus.hpp>
//----------------------------------------------------------------------------
#include <GUITools.h>
//----------------------------------------------------------------------------
struct TCalculateSizeStats;
//----------------------------------------------------------------------------
class TPropertiesDialog : public TForm
{
__published:
  TButton *OkButton;
  TButton *CancelButton;
  TPageControl *PageControl;
  TTabSheet *CommonSheet;
  TBevel *Bevel1;
  TEdit *FileLabel;
  TLabel *Label1;
  TEdit *LocationLabel;
  TLabel *Label2;
  TEdit *SizeLabel;
  TLabel *LinksToLabelLabel;
  TEdit *LinksToLabel;
  TBevel *Bevel2;
  TLabel *RightsLabel;
  TBevel *GroupOwnerRightsBevel;
  TLabel *GroupLabel;
  TComboBox *GroupComboBox;
  TLabel *OwnerLabel;
  TComboBox *OwnerComboBox;
  TImage *FileIconImage;
  TBevel *RecursiveBevel;
  TCheckBox *RecursiveCheck2;
  TButton *CalculateSizeButton;
  TRightsFrame *RightsFrame;
  TButton *HelpButton;
  TTabSheet *ChecksumSheet;
  TListView *ChecksumView;
  TLabel *Label6;
  TComboBox *ChecksumAlgEdit;
  TButton *ChecksumButton;
  TGroupBox *ChecksumGroup;
  TEdit *ChecksumEdit;
  TPopupMenu *ListViewMenu;
  TMenuItem *Copy;
  TLabel *ChecksumUnknownLabel;
  TEdit *OwnerView;
  TEdit *GroupView;
  TTabSheet *TagsSheet;
  TListView *TagsView;
  TButton *AddTagButton;
  TButton *RemoveTagButton;
  TButton *EditTagButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall CalculateSizeButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ChecksumButtonClick(TObject *Sender);
  void __fastcall PageControlChange(TObject *Sender);
  void __fastcall ChecksumAlgEditChange(TObject *Sender);
  void __fastcall CopyClick(TObject *Sender);
  void __fastcall ListViewContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall GroupComboBoxExit(TObject *Sender);
  void __fastcall OwnerComboBoxExit(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall TagsViewKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
  void __fastcall AddTagButtonClick(TObject *Sender);
  void __fastcall TagsViewSelectItem(TObject *Sender, TListItem *Item, bool Selected);
  void __fastcall EditTagButtonClick(TObject *Sender);
  void __fastcall RemoveTagButtonClick(TObject *Sender);
  void __fastcall TagsViewDblClick(TObject *Sender);

private:
  int FAllowedChanges;
  int FOptions;
  TStrings * FFileList;
  const TRemoteTokenList * FGroupList;
  const TRemoteTokenList * FUserList;
  TStrings * FChecksumAlgs;
  TRemoteProperties FOrigProperties;
  bool FMultiple;
  bool FAnyDirectories;
  bool FAllowCalculateStats;
  bool FStatsNotCalculated;
  TCalculateSizeEvent FOnCalculateSize;
  TCalculateChecksumEvent FOnCalculateChecksum;
  bool FChecksumLoaded;
  UnicodeString FAlgUsed;
  bool FMultipleChecksum;

  void __fastcall CalculateChecksum();
  void __fastcall NeedChecksum();
  bool __fastcall ChecksumSupported();
  void __fastcall ResetChecksum();
  void __fastcall CalculatedChecksum(
    const UnicodeString & FileName, const UnicodeString & Alg, const UnicodeString & Hash);
  void __fastcall SetFileProperties(const TRemoteProperties & value);
  TRemoteProperties __fastcall GetFileProperties();
  TModalResult __fastcall DefaultResult();
  void __fastcall CMDpiChanged(TMessage & Message);

protected:
  void __fastcall LoadInfo();
  void __fastcall LoadRemoteTokens(TComboBox * ComboBox, const TRemoteTokenList * List);
  UnicodeString __fastcall LoadRemoteToken(const TRemoteToken & Token);
  void __fastcall LoadRemoteToken(
    TComboBox * ComboBox, TEdit * View, TLabel * Label, bool Valid, const TRemoteToken & Token, int Change);
  TRemoteToken __fastcall StoreRemoteToken(const TRemoteToken & Orig,
    UnicodeString Text, int Message, const TRemoteTokenList * List);
  void __fastcall StoreRemoteToken(TComboBox * ComboBox,
    int ChangeFlag, TValidProperty PropertyFlag, const TRemoteToken & Orig,
    TRemoteToken & Token, int Message, const TRemoteTokenList * List,
    TRemoteProperties & Properties);
  void __fastcall StoreRemoteToken(unsigned int ID, const UnicodeString & Text,
    const TRemoteTokenList * List, TRemoteToken & Result);
  void __fastcall ValidateRemoteToken(
    const TRemoteToken & Orig, int Message, TComboBox * ComboBox,
    const TRemoteTokenList * List);
  void __fastcall UpdateControls();
  void __fastcall LoadStats(__int64 FilesSize, const TCalculateSizeStats & Stats);
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall UpdateFileImage();
  TListItem * AddTag(const UnicodeString & Key, const UnicodeString & Value);
  void AutoSizeTagsView();
  void AddEditTag(bool Add);

  INTERFACE_HOOK;

public:
  virtual __fastcall TPropertiesDialog(TComponent * AOwner,
    TStrings * FileList, const UnicodeString Directory,
    const TRemoteTokenList * GroupList, const TRemoteTokenList * UserList,
    TStrings * ChecksumAlgs,
    int AllowedChanges, int Options, TCalculateSizeEvent OnCalculateSize,
    TCalculateChecksumEvent OnCalculateChecksum);

  virtual __fastcall ~TPropertiesDialog();
  bool __fastcall Execute(TRemoteProperties & Properties);
};
//----------------------------------------------------------------------------
#endif
