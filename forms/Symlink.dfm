object SymlinkDialog: TSymlinkDialog
  Left = 384
  Top = 214
  HelpType = htKeyword
  HelpKeyword = 'ui_symlink'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'SymlinkDialog'
  ClientHeight = 183
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    396
    183)
  PixelsPerInch = 96
  TextHeight = 13
  object SymlinkGroup: TXPGroupBox
    Left = 8
    Top = 6
    Width = 380
    Height = 135
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      380
      135)
    object FileNameLabel: TLabel
      Left = 11
      Top = 16
      Width = 82
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Link/shortcut file:'
      FocusControl = FileNameEdit
    end
    object Label1: TLabel
      Left = 11
      Top = 64
      Width = 101
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Point link/shortcut to:'
      FocusControl = PointToEdit
    end
    object FileNameEdit: TEdit
      Left = 11
      Top = 32
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
      OnKeyDown = PathEditsKeyDown
    end
    object PointToEdit: TEdit
      Left = 11
      Top = 80
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
      OnKeyDown = PathEditsKeyDown
    end
    object SymbolicCheck: TCheckBox
      Left = 17
      Top = 109
      Width = 238
      Height = 17
      Caption = 'Sy&mbolic link'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object OkButton: TButton
    Left = 228
    Top = 150
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 312
    Top = 150
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
