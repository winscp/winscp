//----------------------------------------------------------------------------
#ifndef PropertiesH
#define PropertiesH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <PathLabel.hpp>

#include "Rights.h"
#include "RightsExt.h"
#include <Menus.hpp>
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
  TImage *FilesIconImage;
  TBevel *Bevel1;
  TLabel *FileLabel;
  TLabel *Label1;
  TPathLabel *LocationLabel;
  TLabel *Label2;
  TLabel *SizeLabel;
  TLabel *LinksToLabelLabel;
  TPathLabel *LinksToLabel;
  TBevel *Bevel2;
  TLabel *Label3;
  TBevel *Bevel3;
  TLabel *Label4;
  TComboBox *GroupComboBox;
  TLabel *Label5;
  TComboBox *OwnerComboBox;
  TImage *FileIconImage;
  TBevel *RecursiveBevel;
  TCheckBox *RecursiveCheck;
  TButton *CalculateSizeButton;
  TRightsExtFrame *RightsFrame;
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
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall CalculateSizeButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ChecksumButtonClick(TObject *Sender);
  void __fastcall PageControlChange(TObject *Sender);
  void __fastcall ChecksumAlgEditChange(TObject *Sender);
  void __fastcall CopyClick(TObject *Sender);
  void __fastcall ChecksumViewContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);

private:
  int FAllowedChanges;
  TStrings * FFileList;
  TRemoteProperties FOrigProperties;
  bool FGroupsSet;
  bool FUsersSet;
  TImageList * FShellImageList;
  bool FAllowCalculateStats;
  bool FStatsNotCalculated;
  TCalculateSizeEvent FOnCalculateSize;
  TCalculateChecksumEvent FOnCalculateChecksum;
  bool FChecksumLoaded;
  bool FMultipleChecksum;

  void __fastcall SetDirectory(const AnsiString value);
  AnsiString __fastcall GetDirectory();
  TRemoteProperties __fastcall GetFileProperties();
  TStrings * __fastcall GetGroupList();
  TStrings * __fastcall GetUserList();
  bool __fastcall GetMultiple();
  void __fastcall SetAllowedChanges(int value);
  void __fastcall SetFileList(TStrings * value);
  void __fastcall SetFileProperties(TRemoteProperties value);
  void __fastcall SetGroupList(TStrings * value);
  void __fastcall SetUserList(TStrings * value);

  void __fastcall CalculateChecksum();
  void __fastcall NeedChecksum();
  bool __fastcall ChecksumSupported();
  void __fastcall ResetChecksum();
  void __fastcall CalculatedChecksum(
    const AnsiString & FileName, const AnsiString & Alg, const AnsiString & Hash);

protected:
  void __fastcall LoadInfo();
  void __fastcall UpdateControls();
  void __fastcall LoadStats(__int64 FilesSize, const TCalculateSizeStats & Stats);

  __property bool Multiple = { read = GetMultiple };

public:
  virtual __fastcall ~TPropertiesDialog();
  bool __fastcall Execute();
  virtual __fastcall TPropertiesDialog(TComponent * AOwner,
    TCalculateSizeEvent OnCalculateSize,
    TCalculateChecksumEvent OnCalculateChecksum);

  __property int AllowedChanges = { read = FAllowedChanges, write = SetAllowedChanges };
  __property AnsiString Directory = { read = GetDirectory, write = SetDirectory };
  __property TStrings * FileList = { read = FFileList, write = SetFileList };
  __property TRemoteProperties FileProperties = { read = GetFileProperties, write = SetFileProperties };
  __property TStrings * GroupList = { read = GetGroupList, write = SetGroupList };
  __property TStrings * UserList = { read = GetUserList, write = SetUserList };
};
//----------------------------------------------------------------------------
#endif
