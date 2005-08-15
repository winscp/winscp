object FullSynchronizeDialog: TFullSynchronizeDialog
  Left = 365
  Top = 185
  HelpType = htKeyword
  HelpKeyword = 'ui_synchronize'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Synchronize'
  ClientHeight = 399
  ClientWidth = 433
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
    433
    399)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TXPGroupBox
    Left = 8
    Top = 6
    Width = 417
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      417
      119)
    object LocalDirectoryLabel: TLabel
      Left = 11
      Top = 19
      Width = 72
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Lo&cal directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 11
      Top = 68
      Width = 83
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Remo&te directory:'
      FocusControl = RemoteDirectoryEdit
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 84
      Width = 395
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 35
      Width = 312
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 330
      Top = 33
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Bro&wse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object OkButton: TButton
    Left = 185
    Top = 366
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object CancelButton: TButton
    Left = 267
    Top = 366
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object OptionsGroup: TXPGroupBox
    Left = 8
    Top = 184
    Width = 268
    Height = 97
    Caption = 'Synchronize options'
    TabOrder = 2
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 126
      Height = 17
      Caption = '&Delete files'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 139
      Top = 68
      Width = 123
      Height = 17
      Caption = 'S&elected files only'
      TabOrder = 4
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 139
      Top = 44
      Width = 123
      Height = 17
      Caption = '&Existing files only'
      TabOrder = 2
      OnClick = ControlChange
    end
    object SynchronizePreviewChangesCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 126
      Height = 17
      Caption = 'Pre&view changes'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SynchronizeTimestampCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 246
      Height = 17
      Caption = 'Synchronize timestamps &only, not files'
      TabOrder = 0
      OnClick = ControlChange
    end
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 366
    Width = 129
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Transfer settin&gs...'
    TabOrder = 6
    OnClick = TransferSettingsButtonClick
  end
  object DirectionGroup: TXPGroupBox
    Left = 8
    Top = 130
    Width = 417
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Direction/Target directory'
    TabOrder = 1
    object SynchronizeBothButton: TRadioButton
      Left = 11
      Top = 20
      Width = 102
      Height = 17
      Caption = '&Both'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeRemoteButton: TRadioButton
      Left = 139
      Top = 20
      Width = 102
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeLocalButton: TRadioButton
      Left = 288
      Top = 20
      Width = 110
      Height = 17
      Caption = '&Local'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object CompareCriterionsGroup: TXPGroupBox
    Left = 285
    Top = 184
    Width = 140
    Height = 97
    Caption = 'Compare criterions'
    TabOrder = 3
    object SynchronizeByTimeCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 121
      Height = 17
      Caption = '&Modification time'
      TabOrder = 0
      OnClick = SynchronizeByTimeSizeCheckClick
    end
    object SynchronizeBySizeCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 121
      Height = 17
      Caption = 'File si&ze'
      TabOrder = 1
      OnClick = SynchronizeByTimeSizeCheckClick
    end
  end
  object SaveSettingsCheck: TCheckBox
    Left = 19
    Top = 342
    Width = 246
    Height = 17
    Caption = 'Use &same options next time'
    TabOrder = 5
  end
  object CopyParamGroup: TXPGroupBox
    Left = 8
    Top = 286
    Width = 417
    Height = 50
    Caption = 'Transfer settings'
    TabOrder = 4
    OnContextPopup = CopyParamGroupContextPopup
    OnDblClick = CopyParamGroupDblClick
    DesignSize = (
      417
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 403
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      WordWrap = True
      OnDblClick = CopyParamGroupDblClick
    end
  end
  object HelpButton: TButton
    Left = 349
    Top = 366
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 9
    OnClick = HelpButtonClick
  end
end
