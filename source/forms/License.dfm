object LicenseDialog: TLicenseDialog
  Left = 413
  Top = 230
  ActiveControl = CloseButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'License'
  ClientHeight = 320
  ClientWidth = 504
  Color = clBtnFace
  ParentFont = True
  Position = poOwnerFormCenter
  DesignSize = (
    504
    320)
  TextHeight = 13
  object CloseButton: TButton
    Left = 421
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
  object LicenseMemo: TMemo
    Left = 8
    Top = 8
    Width = 489
    Height = 273
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WantReturns = False
  end
end
