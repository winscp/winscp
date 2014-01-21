object CopyParamCustomDialog: TCopyParamCustomDialog
  Left = 374
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_transfer_custom'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Transfer settings'
  ClientHeight = 468
  ClientWidth = 420
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    420
    468)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 168
    Top = 435
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 252
    Top = 435
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  inline CopyParamsFrame: TCopyParamsFrame
    Left = 0
    Top = 0
    Width = 420
    Height = 431
    HelpType = htKeyword
    TabOrder = 0
  end
  object HelpButton: TButton
    Left = 336
    Top = 435
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
