object CopyDialog: TCopyDialog
  Left = 363
  Top = 198
  BorderStyle = bsDialog
  Caption = 'CopyDialog'
  ClientHeight = 254
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    511
    254)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoryLabel: TLabel
    Left = 8
    Top = 8
    Width = 187
    Height = 13
    Caption = 'Copy 2 selected files to remote directory'
  end
  object LocalDirectoryEdit: TDirectoryEdit
    Left = 8
    Top = 25
    Width = 496
    Height = 21
    AcceptFiles = True
    DialogText = 'Select target local directory.'
    ClickKey = 16397
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'LocalDirectoryEdit'
  end
  object RemoteDirectoryEdit: TEdit
    Left = 8
    Top = 25
    Width = 494
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 1000
    TabOrder = 1
    Text = 'RemoteDirectoryEdit'
  end
  object MoreButton: TMoreButton
    Left = 251
    Top = 224
    Width = 75
    Height = 25
    Panel = MorePanel
    RepositionForm = True
    Anchors = [akRight, akBottom]
    TabOrder = 3
  end
  object CopyButton: TButton
    Left = 339
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Copy'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 427
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object MorePanel: TPanel
    Left = 0
    Top = 52
    Width = 511
    Height = 170
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    object SaveSettingsCheck: TCheckBox
      Left = 8
      Top = 152
      Width = 217
      Height = 17
      Caption = 'Use &same settings next time'
      TabOrder = 1
    end
    inline CopyParamsFrame: TCopyParamsFrame
      Left = 2
      Top = 0
      Width = 506
      Height = 149
      TabOrder = 0
      inherited RemotePropertiesGroup: TXPGroupBox
        inherited PreserveRightsCheck: TCheckBox
          Left = 12
        end
      end
    end
  end
end
