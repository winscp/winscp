object ImportSessionsDialog: TImportSessionsDialog
  Left = 362
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Import sessions from PuTTY'
  ClientHeight = 275
  ClientWidth = 351
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    351
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 337
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption =
      'Following list contains sessions stored in PuTTY SSH client. Che' +
      'ck sessions you want to import and press OK button. '#13#10 +
      #13#10'Notice: This application always connects using SSH protocol.'
    WordWrap = True
  end
  object OKButton: TButton
    Left = 191
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 271
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object SessionListView: TListView
    Left = 8
    Top = 72
    Width = 337
    Height = 164
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
end
