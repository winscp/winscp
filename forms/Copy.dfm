object CopyDialog: TCopyDialog
  Left = 363
  Top = 198
  BorderStyle = bsDialog
  Caption = 'CopyDialog'
  ClientHeight = 276
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
    276)
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
    OnKeyDown = DirectoryEditKeyDown
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
    OnKeyDown = DirectoryEditKeyDown
  end
  object MoreButton: TMoreButton
    Left = 251
    Top = 246
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '<< &Less'
    TabOrder = 4
    OnChange = ControlChange
    Panel = MorePanel
    RepositionForm = True
  end
  object CopyButton: TButton
    Left = 339
    Top = 246
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
    Top = 246
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
      Width = 508
      Height = 150
      TabOrder = 0
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
    Top = 224
    Width = 301
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Transfer on background (add to transfer &queue)'
    TabOrder = 7
    OnClick = ControlChange
  end
  object QueueNoConfirmationCheck: TCheckBox
    Left = 312
    Top = 224
    Width = 193
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'No &confirmations'
    TabOrder = 8
    OnClick = ControlChange
  end
end
