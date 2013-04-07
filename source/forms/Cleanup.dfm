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
    DoubleBuffered = True
    HideSelection = False
    Items.ItemData = {
      056A0100000600000001000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      0016470065006E006500720061006C00200063006F006E006600690067007500
      72006100740069006F006E00580002000000FFFFFFFFFFFFFFFF00000000FFFF
      FFFF000000000653006900740065007300580003000000FFFFFFFFFFFFFFFF00
      000000FFFFFFFF0000000011430061006300680065006400200068006F007300
      740020006B00650079007300580004000000FFFFFFFFFFFFFFFF00000000FFFF
      FFFF000000001743006F006E00660069006700750072006100740069006F006E
      00200049004E0049002000660069006C006500580005000000FFFFFFFFFFFFFF
      FF00000000FFFFFFFF0000000011520061006E0064006F006D00200073006500
      650064002000660069006C006500580006000000FFFFFFFFFFFFFFFF00000000
      FFFFFFFF0000000012540065006D0070006F007200610072007900200066006F
      006C0064006500720073005800}
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
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
