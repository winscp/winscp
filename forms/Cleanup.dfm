object CleanupDialog: TCleanupDialog
  Left = 356
  Top = 218
  BorderStyle = bsDialog
  Caption = 'Cleanup application data'
  ClientHeight = 288
  ClientWidth = 489
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    489
    288)
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
    Left = 329
    Top = 257
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 409
    Top = 257
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object DataListView: TListView
    Left = 8
    Top = 104
    Width = 475
    Height = 145
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
    Items.Data = {
      CB0000000500000001000000FFFFFFFFFFFFFFFF00000000000000001547656E
      6572616C20636F6E66696775726174696F6E02000000FFFFFFFFFFFFFFFF0000
      0000000000000F53746F7265642073657373696F6E7303000000FFFFFFFFFFFF
      FFFF00000000000000001043616368656420686F7374206B65797304000000FF
      FFFFFFFFFFFFFF000000000000000016436F6E66696775726174696F6E20494E
      492066696C6505000000FFFFFFFFFFFFFFFF00000000000000001052616E646F
      6D20736565642066696C65}
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
    Top = 255
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Check &all'
    TabOrder = 1
    OnClick = CheckAllButtonClick
  end
end
