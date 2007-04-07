object ImportSessionsDialog: TImportSessionsDialog
  Left = 362
  Top = 186
  HelpType = htKeyword
  HelpKeyword = 'ui_import'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Import sessions from PuTTY'
  ClientHeight = 273
  ClientWidth = 375
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    375
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 361
    Height = 45
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Following list contains sessions stored in PuTTY SSH client. Che' +
      'ck sessions you want to import and press OK button.'
    WordWrap = True
  end
  object OKButton: TButton
    Left = 135
    Top = 242
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 215
    Top = 242
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object SessionListView: TListView
    Left = 8
    Top = 52
    Width = 361
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Session'
        Width = 240
      end
      item
        Caption = 'Protocol'
        Width = 60
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnInfoTip = SessionListViewInfoTip
    OnKeyUp = SessionListViewKeyUp
    OnMouseDown = SessionListViewMouseDown
  end
  object CheckAllButton: TButton
    Left = 8
    Top = 242
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Un/check &all'
    TabOrder = 2
    OnClick = CheckAllButtonClick
  end
  object ImportKeysCheck: TCheckBox
    Left = 16
    Top = 218
    Width = 345
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Import cached host &keys for checked sessions'
    TabOrder = 1
  end
  object HelpButton: TButton
    Left = 294
    Top = 242
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 5
    OnClick = HelpButtonClick
  end
end
