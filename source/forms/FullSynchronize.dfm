object FullSynchronizeDialog: TFullSynchronizeDialog
  Left = 365
  Top = 185
  HelpType = htKeyword
  HelpKeyword = 'ui_synchronize'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Synchronize'
  ClientHeight = 477
  ClientWidth = 481
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    481
    477)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 465
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      465
      119)
    object LocalDirectoryLabel: TLabel
      Left = 49
      Top = 19
      Width = 74
      Height = 13
      Caption = 'Lo&cal directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 49
      Top = 68
      Width = 87
      Height = 13
      Caption = 'R&emote directory:'
      FocusControl = RemoteDirectoryEdit
    end
    object Image: TImage
      Left = 11
      Top = 22
      Width = 32
      Height = 32
      AutoSize = True
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 84
      Width = 405
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 35
      Width = 322
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 378
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
    Left = 220
    Top = 444
    Width = 88
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = OkButtonClick
    OnDropDownClick = OkButtonDropDownClick
  end
  object CancelButton: TButton
    Left = 315
    Top = 444
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 238
    Width = 227
    Height = 121
    Caption = 'Synchronize options'
    TabOrder = 3
    DesignSize = (
      227
      121)
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 11
      Top = 92
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Selected files o&nly'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'E&xisting files only'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizePreviewChangesCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Pre&view changes'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 444
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Transfer settin&gs...'
    TabOrder = 7
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object DirectionGroup: TGroupBox
    Left = 8
    Top = 130
    Width = 465
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Direction/Target directory'
    TabOrder = 1
    object SynchronizeBothButton: TRadioButton
      Left = 11
      Top = 20
      Width = 138
      Height = 17
      Caption = '&Both'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ControlChange
    end
    object SynchronizeRemoteButton: TRadioButton
      Left = 155
      Top = 20
      Width = 143
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeLocalButton: TRadioButton
      Left = 304
      Top = 20
      Width = 154
      Height = 17
      Caption = '&Local'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object CompareCriterionsGroup: TGroupBox
    Left = 245
    Top = 238
    Width = 228
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Comparison criteria'
    TabOrder = 4
    DesignSize = (
      228
      121)
    object SynchronizeByTimeCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'M&odification time'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeBySizeCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File si&ze'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeCaseSensitiveCheck: TCheckBox
      Left = 11
      Top = 92
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'C&ase sensitive'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SynchronizeByChecksumCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 210
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'C&hecksum'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object SaveSettingsCheck: TCheckBox
    Left = 19
    Top = 365
    Width = 454
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &same options next time'
    TabOrder = 5
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 386
    Width = 465
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 6
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      465
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 451
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      ShowAccelChar = False
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object HelpButton: TButton
    Left = 397
    Top = 444
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 10
    OnClick = HelpButtonClick
  end
  object ModeGroup: TGroupBox
    Left = 8
    Top = 184
    Width = 465
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mode'
    TabOrder = 2
    object SynchronizeFilesButton: TRadioButton
      Left = 11
      Top = 20
      Width = 135
      Height = 17
      Caption = 'Synchronize &files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object MirrorFilesButton: TRadioButton
      Left = 155
      Top = 20
      Width = 143
      Height = 17
      Caption = '&Mirror files'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeTimestampsButton: TRadioButton
      Left = 304
      Top = 20
      Width = 154
      Height = 17
      Caption = 'Synchronize &timestamps'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object OkMenu: TPopupMenu
    Left = 416
    Top = 384
    object Start1: TMenuItem
      Caption = '&Start'
      Default = True
      OnClick = Start1Click
    end
    object StartInNewWindowItem: TMenuItem
      Caption = 'Start in &New Window'
      OnClick = StartInNewWindowItemClick
    end
  end
end
