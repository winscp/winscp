object SynchronizeDialog: TSynchronizeDialog
  Left = 367
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_keepuptodate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Keep remote directory up to date X'
  ClientHeight = 322
  ClientWidth = 396
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
    396
    322)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TXPGroupBox
    Left = 8
    Top = 6
    Width = 381
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      381
      119)
    object LocalDirectoryLabel: TLabel
      Left = 11
      Top = 19
      Width = 200
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Watch for changes in the local directory ...'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 11
      Top = 68
      Width = 263
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '... &and automatically reflect them on the remote directory'
      FocusControl = RemoteDirectoryEdit
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 84
      Width = 359
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
      Width = 276
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
      Left = 293
      Top = 33
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'B&rowse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object StopButton: TButton
    Left = 152
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Stop'
    TabOrder = 4
    OnClick = StopButtonClick
  end
  object CancelButton: TButton
    Left = 232
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 6
  end
  object OptionsGroup: TXPGroupBox
    Left = 8
    Top = 130
    Width = 381
    Height = 95
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Synchronize options'
    TabOrder = 1
    DesignSize = (
      381
      95)
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 182
      Height = 17
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeNoConfirmationCheck: TCheckBox
      Left = 203
      Top = 44
      Width = 166
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&No confirmations'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SaveSettingsCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 182
      Height = 17
      Caption = 'Use same &options next time'
      TabOrder = 4
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 203
      Top = 20
      Width = 166
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Existing files only'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeRecursiveCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 182
      Height = 17
      Caption = 'Update s&ubdirectories'
      TabOrder = 2
      OnClick = ControlChange
    end
    object SynchronizeSynchronizeCheck: TGrayedCheckBox
      Left = 203
      Top = 68
      Width = 166
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Synchronize on s&tart'
      TabOrder = 5
      OnClick = ControlChange
    end
  end
  object StartButton: TButton
    Left = 152
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Start'
    Default = True
    TabOrder = 3
    OnClick = StartButtonClick
  end
  object MinimizeButton: TButton
    Left = 233
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Minimize'
    TabOrder = 5
    OnClick = MinimizeButtonClick
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 288
    Width = 137
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Transfer settin&gs...'
    TabOrder = 2
    OnClick = TransferSettingsButtonClick
  end
  object CopyParamGroup: TXPGroupBox
    Left = 8
    Top = 230
    Width = 381
    Height = 50
    Caption = 'Transfer settings'
    TabOrder = 7
    OnContextPopup = CopyParamGroupContextPopup
    OnDblClick = CopyParamGroupDblClick
    DesignSize = (
      381
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 367
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      WordWrap = True
      OnDblClick = CopyParamGroupDblClick
    end
  end
  object HelpButton: TButton
    Left = 313
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 8
    OnClick = HelpButtonClick
  end
end
