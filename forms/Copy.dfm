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
  object LocalDirectoryEdit: THistoryComboBox
    Left = 8
    Top = 25
    Width = 410
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    Text = 'LocalDirectoryEdit'
  end
  object RemoteDirectoryEdit: THistoryComboBox
    Left = 8
    Top = 25
    Width = 494
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    MaxLength = 1000
    TabOrder = 2
    Text = 'RemoteDirectoryEdit'
  end
  object MoreButton: TMoreButton
    Left = 251
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '<< &Less'
    TabOrder = 4
    Panel = MorePanel
    RepositionForm = True
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
    TabOrder = 5
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
    TabOrder = 6
  end
  object MorePanel: TPanel
    Left = 0
    Top = 52
    Width = 511
    Height = 170
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    object SaveSettingsCheck: TCheckBox
      Left = 8
      Top = 152
      Width = 377
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
        inherited RightsFrame: TRightsFrame
          PopupMenu = CopyParamsFrame.RightsFrame.RightsPopup
        end
        inherited PreserveRightsCheck: TCheckBox
          Left = 12
        end
      end
    end
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 427
    Top = 23
    Width = 75
    Height = 25
    Caption = 'B&rowse...'
    TabOrder = 1
    OnClick = LocalDirectoryBrowseButtonClick
  end
end
