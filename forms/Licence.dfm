object LicenceDialog: TLicenceDialog
  Left = 413
  Top = 230
  ActiveControl = CloseButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Licence'
  ClientHeight = 320
  ClientWidth = 312
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    312
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object CloseButton: TButton
    Left = 229
    Top = 285
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object LicenceMemo: TMemo
    Left = 8
    Top = 8
    Width = 297
    Height = 273
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WantReturns = False
  end
end
