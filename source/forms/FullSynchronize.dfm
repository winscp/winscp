object FullSynchronizeDialog: TFullSynchronizeDialog
  Left = 365
  Top = 185
  HelpType = htKeyword
  HelpKeyword = 'ui_synchronize'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Synchronize'
  ClientHeight = 492
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    534
    492)
  TextHeight = 15
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 518
    Height = 120
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      518
      120)
    object LocalDirectoryLabel: TLabel
      Left = 49
      Top = 22
      Width = 81
      Height = 15
      Caption = 'Lo&cal directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 49
      Top = 69
      Width = 94
      Height = 15
      Caption = 'R&emote directory:'
      FocusControl = RemoteDirectoryEdit
    end
    object Image: TImage
      Left = 11
      Top = 24
      Width = 32
      Height = 32
      AutoSize = True
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 87
      Width = 460
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 40
      Width = 374
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 429
      Top = 39
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Bro&wse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object OkButton: TButton
    Left = 260
    Top = 459
    Width = 94
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
    Left = 360
    Top = 459
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 244
    Width = 252
    Height = 121
    Caption = 'Synchronize options'
    TabOrder = 3
    DesignSize = (
      252
      121)
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 22
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 11
      Top = 91
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Selected files o&nly'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'E&xisting files only'
      TabOrder = 2
      OnClick = ControlChange
    end
    object SynchronizePreviewChangesCheck: TCheckBox
      Left = 11
      Top = 45
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Pre&view changes'
      TabOrder = 1
      OnClick = ControlChange
    end
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 459
    Width = 175
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Transfer settin&gs...'
    TabOrder = 7
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object DirectionGroup: TGroupBox
    Left = 8
    Top = 134
    Width = 518
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Direction/Target directory'
    TabOrder = 1
    object SynchronizeBothButton: TRadioButton
      Left = 11
      Top = 22
      Width = 158
      Height = 17
      Caption = '&Both'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ControlChange
    end
    object SynchronizeRemoteButton: TRadioButton
      Left = 175
      Top = 22
      Width = 158
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeLocalButton: TRadioButton
      Left = 339
      Top = 22
      Width = 170
      Height = 17
      Caption = '&Local'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object CompareCriterionsGroup: TGroupBox
    Left = 274
    Top = 244
    Width = 252
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Comparison criteria'
    TabOrder = 4
    DesignSize = (
      252
      121)
    object SynchronizeByTimeCheck: TCheckBox
      Left = 11
      Top = 22
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'M&odification time'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeBySizeCheck: TCheckBox
      Left = 11
      Top = 45
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File si&ze'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeCaseSensitiveCheck: TCheckBox
      Left = 11
      Top = 91
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'C&ase sensitive'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SynchronizeByChecksumCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 232
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'C&hecksum'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object SaveSettingsCheck: TCheckBox
    Left = 19
    Top = 371
    Width = 507
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &same options next time'
    TabOrder = 5
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 394
    Width = 518
    Height = 59
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 6
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      518
      59)
    object CopyParamLabel: TLabel
      Left = 9
      Top = 20
      Width = 500
      Height = 35
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      ShowAccelChar = False
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object HelpButton: TButton
    Left = 446
    Top = 459
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 10
    OnClick = HelpButtonClick
  end
  object ModeGroup: TGroupBox
    Left = 8
    Top = 189
    Width = 518
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mode'
    TabOrder = 2
    object SynchronizeFilesButton: TRadioButton
      Left = 11
      Top = 20
      Width = 158
      Height = 17
      Caption = 'Synchronize &files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object MirrorFilesButton: TRadioButton
      Left = 175
      Top = 20
      Width = 158
      Height = 17
      Caption = '&Mirror files'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeTimestampsButton: TRadioButton
      Left = 339
      Top = 20
      Width = 170
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
