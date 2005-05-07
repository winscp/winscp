object CopyDialog: TCopyDialog
  Left = 363
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_copy'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyDialog'
  ClientHeight = 359
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
    359)
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
    Left = 176
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '<< &Less'
    TabOrder = 7
    OnChange = ControlChange
    Panel = MorePanel
    RepositionForm = True
  end
  object CopyButton: TButton
    Left = 260
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Copy'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object CancelButton: TButton
    Left = 343
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object MorePanel: TPanel
    Left = 0
    Top = 52
    Width = 511
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    object SaveSettingsCheck: TCheckBox
      Left = 8
      Top = 235
      Width = 377
      Height = 17
      Caption = 'Use &same settings next time'
      TabOrder = 2
    end
    inline CopyParamsFrame: TCopyParamsFrame
      Left = 2
      Top = 0
      Width = 508
      Height = 213
      TabOrder = 0
    end
    object NewerOnlyCheck: TCheckBox
      Left = 8
      Top = 215
      Width = 377
      Height = 17
      Caption = 'N&ew and updated file(s) only'
      TabOrder = 1
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
  object QueueCheck: TCheckBox
    Left = 8
    Top = 307
    Width = 301
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Transfer on background (add to transfer &queue)'
    TabOrder = 4
    OnClick = ControlChange
  end
  object QueueNoConfirmationCheck: TCheckBox
    Left = 312
    Top = 307
    Width = 193
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'No &confirmations'
    TabOrder = 5
    OnClick = ControlChange
  end
  object PresetsButton: TButton
    Left = 8
    Top = 329
    Width = 97
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Presets...'
    TabOrder = 6
    OnClick = PresetsButtonClick
  end
  object HelpButton: TButton
    Left = 427
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 10
    OnClick = HelpButtonClick
  end
end
