object FullSynchronizeDialog: TFullSynchronizeDialog
  Left = 365
  Top = 185
  HelpType = htKeyword
  HelpKeyword = 'ui_synchronize'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Synchronize'
  ClientHeight = 299
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    396
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TXPGroupBox
    Left = 8
    Top = 6
    Width = 380
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      380
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
      Width = 358
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
      OnKeyDown = DirectoryEditKeyDown
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 35
      Width = 275
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
      OnKeyDown = DirectoryEditKeyDown
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 293
      Top = 33
      Width = 75
      Height = 25
      Caption = '&Browse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object OkButton: TButton
    Left = 228
    Top = 266
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 312
    Top = 266
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object OptionsGroup: TXPGroupBox
    Left = 8
    Top = 184
    Width = 380
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Synchronize options'
    TabOrder = 1
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 97
      Height = 17
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeNoConfirmationCheck: TCheckBox
      Left = 235
      Top = 20
      Width = 135
      Height = 17
      Caption = '&No confirmations'
      TabOrder = 2
      OnClick = ControlChange
    end
    object SaveSettingsCheck: TCheckBox
      Left = 123
      Top = 44
      Width = 249
      Height = 17
      Caption = 'Use &same options next time'
      TabOrder = 4
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 123
      Top = 20
      Width = 107
      Height = 17
      Caption = '&Existing files only'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizePreviewChangesCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 112
      Height = 17
      Caption = 'Pre&view changes'
      TabOrder = 3
      OnClick = ControlChange
    end
  end
  object TransferPreferencesButton: TButton
    Left = 8
    Top = 266
    Width = 137
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Transfer &preferences...'
    TabOrder = 2
    OnClick = TransferPreferencesButtonClick
  end
  object DirectionGroup: TXPGroupBox
    Left = 8
    Top = 130
    Width = 380
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Direction'
    TabOrder = 5
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
      Left = 123
      Top = 20
      Width = 102
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeLocalButton: TRadioButton
      Left = 235
      Top = 20
      Width = 110
      Height = 17
      Caption = '&Local'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
end
