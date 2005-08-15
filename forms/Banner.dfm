object BannerDialog: TBannerDialog
  Left = 413
  Top = 230
  HelpType = htKeyword
  HelpKeyword = 'ui_banner'
  ActiveControl = CloseButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Authentication Banner'
  ClientHeight = 291
  ClientWidth = 440
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    440
    291)
  PixelsPerInch = 96
  TextHeight = 13
  object CloseButton: TButton
    Left = 273
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BannerMemo: TMemo
    Left = 8
    Top = 8
    Width = 425
    Height = 244
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    WantReturns = False
  end
  object NeverShowAgainCheck: TCheckBox
    Left = 15
    Top = 262
    Width = 250
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&Never show this banner again'
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 357
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 1
    OnClick = HelpButtonClick
  end
end
