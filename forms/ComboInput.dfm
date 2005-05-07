object ComboInputDialog: TComboInputDialog
  Left = 403
  Top = 281
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Save session as'
  ClientHeight = 86
  ClientWidth = 326
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    326
    86)
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
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 157
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
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
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
