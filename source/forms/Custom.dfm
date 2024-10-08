object CustomDialog: TCustomDialog
  Left = 401
  Top = 228
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Save session as siteX'
  ClientHeight = 41
  ClientWidth = 326
  Color = clBtnFace
  ParentFont = True
  Position = poOwnerFormCenter
  DesignSize = (
    326
    41)
  TextHeight = 13
  object OKButton: TButton
    Left = 68
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 156
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object HelpButton: TButton
    Left = 243
    Top = 9
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 1
    OnClick = HelpButtonClick
  end
end
