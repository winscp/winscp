object LicenseDialog: TLicenseDialog
  Left = 413
  Top = 230
  ActiveControl = CloseButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'License'
  ClientHeight = 355
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  DesignSize = (
    559
    355)
  TextHeight = 15
  object CloseButton: TButton
    Left = 471
    Top = 322
    Width = 80
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
    Width = 543
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WantReturns = False
  end
end
