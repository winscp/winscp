object CleanupDialog: TCleanupDialog
  Left = 356
  Top = 218
  HelpType = htKeyword
  HelpKeyword = 'ui_cleanup'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Cleanup application data'
  ClientHeight = 299
  ClientWidth = 489
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    489
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 475
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Following list contains all application data possibly stored on ' +
      'this computer. Check those you want to remove.'#13#10#13#10'If another int' +
      'ance of application is running, please close it before cleaning ' +
      'data.'#13#10#13#10'Notice: Opening session and/or next execution of applic' +
      'ation will recreate some of data.'
    WordWrap = True
  end
  object OKButton: TButton
    Left = 249
    Top = 268
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 329
    Top = 268
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
  end
  object DataListView: TListView
    Left = 8
    Top = 104
    Width = 475
    Height = 156
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Data'
        Tag = 1
        Width = 160
      end
      item
        Caption = 'Location'
        Width = 500
      end>
    ColumnClick = False
    HideSelection = False
    Items.ItemData = {
      037E0100000600000001000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      0016470065006E006500720061006C00200063006F006E006600690067007500
      72006100740069006F006E00580002000000FFFFFFFFFFFFFFFF00000000FFFF
      FFFF0000000010530074006F007200650064002000730065007300730069006F
      006E007300580003000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000000011
      430061006300680065006400200068006F007300740020006B00650079007300
      580004000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000001743006F006E
      00660069006700750072006100740069006F006E00200049004E004900200066
      0069006C006500580005000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      0011520061006E0064006F006D00200073006500650064002000660069006C00
      6500580006000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000000012540065
      006D0070006F007200610072007900200066006F006C00640065007200730058
      00}
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnInfoTip = DataListViewInfoTip
    OnKeyUp = DataListViewKeyUp
    OnMouseDown = DataListViewMouseDown
  end
  object CheckAllButton: TButton
    Left = 8
    Top = 266
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Un/check &all'
    TabOrder = 1
    OnClick = CheckAllButtonClick
  end
  object HelpButton: TButton
    Left = 409
    Top = 268
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
end
