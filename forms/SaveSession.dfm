object SaveSessionDialog: TSaveSessionDialog
  Left = 403
  Top = 281
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Save session as'
  ClientHeight = 108
  ClientWidth = 326
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    326
    108)
  PixelsPerInch = 96
  TextHeight = 13
  object InputLabel: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = '&Save session as:'
    FocusControl = InputCombo
  end
  object OKButton: TButton
    Left = 69
    Top = 76
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 157
    Top = 76
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object InputCombo: TComboBox
    Left = 8
    Top = 24
    Width = 312
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    MaxLength = 255
    TabOrder = 0
    OnChange = InputComboChange
  end
  object HelpButton: TButton
    Left = 244
    Top = 76
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object SavePasswordCheck: TCheckBox
    Left = 14
    Top = 51
    Width = 299
    Height = 17
    Caption = 'Save &password (not recommended)'
    TabOrder = 1
  end
end
