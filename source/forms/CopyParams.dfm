object CopyParamsFrame: TCopyParamsFrame
  Left = 0
  Top = 0
  Width = 456
  Height = 471
  HelpType = htKeyword
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object CommonPropertiesGroup: TGroupBox
    Left = 232
    Top = 151
    Width = 221
    Height = 124
    Caption = 'Common options'
    TabOrder = 3
    DesignSize = (
      221
      124)
    object SpeedLabel3: TLabel
      Left = 9
      Top = 94
      Width = 70
      Height = 15
      Caption = '&Speed (KB/s):'
      FocusControl = SpeedCombo
    end
    object PreserveTimeCheck: TCheckBox
      Left = 11
      Top = 22
      Width = 201
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Preserve timestamp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ControlChange
    end
    object CommonCalculateSizeCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 201
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Calculate total size'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ControlChange
    end
    object SpeedCombo: THistoryComboBox
      Left = 111
      Top = 91
      Width = 101
      Height = 23
      AutoComplete = False
      TabOrder = 3
      Text = 'SpeedCombo'
      OnExit = SpeedComboExit
      Items.Strings = (
        'Unlimited'
        '1024'
        '512'
        '256'
        '128'
        '64'
        '32'
        '16'
        '8')
    end
    object PreserveTimeDirsCheck: TCheckBox
      Left = 27
      Top = 45
      Width = 185
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Including directories'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = ControlChange
    end
  end
  object LocalPropertiesGroup: TGroupBox
    Left = 232
    Top = 281
    Width = 221
    Height = 49
    Caption = 'Download options'
    TabOrder = 4
    DesignSize = (
      221
      49)
    object PreserveReadOnlyCheck: TCheckBox
      Left = 11
      Top = 22
      Width = 201
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Preserve rea&d-only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object RemotePropertiesGroup: TGroupBox
    Left = 3
    Top = 151
    Width = 215
    Height = 179
    Caption = 'Upload options'
    TabOrder = 2
    object PreserveRightsCheck: TCheckBox
      Left = 11
      Top = 22
      Width = 173
      Height = 17
      Caption = 'Set pe&rmissions:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ControlChange
    end
    object RightsEdit: TComboEdit
      Left = 26
      Top = 45
      Width = 123
      Height = 21
      ButtonHint = 'Configure permissions'
      ClickKey = 16397
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'RightsEdit'
      OnButtonClick = RightsEditButtonClick
      OnExit = RightsEditExit
      OnContextPopup = RightsEditContextPopup
    end
    object IgnorePermErrorsCheck: TCheckBox
      Left = 11
      Top = 72
      Width = 173
      Height = 17
      Caption = 'Ign&ore permission errors'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ControlChange
    end
    object ClearArchiveCheck: TCheckBox
      Left = 11
      Top = 95
      Width = 173
      Height = 17
      Caption = 'Clear '#39'Arc&hive'#39' attribute'
      TabOrder = 3
    end
    object RemoveCtrlZAndBOMCheck: TCheckBox
      Left = 11
      Top = 118
      Width = 173
      Height = 17
      Caption = 'Remo&ve BOM and EOF marks X'
      TabOrder = 4
      OnClick = ControlChange
    end
    object EncryptNewFilesCheck: TCheckBox
      Left = 11
      Top = 141
      Width = 173
      Height = 17
      Caption = '&Encrypt new files'
      TabOrder = 5
      OnClick = ControlChange
    end
  end
  object ChangeCaseGroup: TGroupBox
    Left = 290
    Top = 3
    Width = 163
    Height = 142
    Caption = 'Filename modification'
    TabOrder = 1
    DesignSize = (
      163
      142)
    object CCLowerCaseShortButton: TRadioButton
      Left = 11
      Top = 91
      Width = 143
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Lower case &8.3'
      TabOrder = 3
    end
    object CCNoChangeButton: TRadioButton
      Left = 11
      Top = 22
      Width = 143
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'No chan&ge'
      TabOrder = 0
    end
    object CCUpperCaseButton: TRadioButton
      Left = 11
      Top = 45
      Width = 143
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Upper case'
      TabOrder = 1
    end
    object CCLowerCaseButton: TRadioButton
      Left = 11
      Top = 68
      Width = 143
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Lo&wer case'
      TabOrder = 2
    end
    object ReplaceInvalidCharsCheck: TCheckBox
      Left = 11
      Top = 114
      Width = 143
      Height = 17
      Caption = 'Rep&lace '#39'\:*?'#39'...'
      TabOrder = 4
      OnClick = ControlChange
    end
  end
  object TransferModeGroup: TGroupBox
    Left = 3
    Top = 3
    Width = 273
    Height = 142
    Caption = 'Transfer mode'
    TabOrder = 0
    DesignSize = (
      273
      142)
    object AsciiFileMaskLabel: TLabel
      Left = 9
      Top = 91
      Width = 191
      Height = 15
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Transfer following &files in text mode:'
      FocusControl = AsciiFileMaskCombo
    end
    object TMTextButton: TRadioButton
      Left = 11
      Top = 22
      Width = 253
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Text (plain text, html, scripts, ...)'
      TabOrder = 0
      OnClick = ControlChange
    end
    object TMBinaryButton: TRadioButton
      Left = 11
      Top = 45
      Width = 253
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Binary (archives, doc, ...)'
      TabOrder = 1
      OnClick = ControlChange
    end
    object TMAutomaticButton: TRadioButton
      Left = 11
      Top = 68
      Width = 253
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Automatic'
      TabOrder = 2
      OnClick = ControlChange
    end
    object AsciiFileMaskCombo: THistoryComboBox
      Left = 9
      Top = 109
      Width = 255
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 3
      Text = 'AsciiFileMaskCombo'
      OnExit = ValidateMaskComboExit
    end
  end
  object OtherGroup: TGroupBox
    Left = 3
    Top = 336
    Width = 450
    Height = 131
    Caption = 'Other'
    TabOrder = 5
    DesignSize = (
      450
      131)
    object IncludeFileMaskLabel: TLabel
      Left = 9
      Top = 22
      Width = 52
      Height = 15
      Caption = 'File &mask:'
      FocusControl = IncludeFileMaskCombo
    end
    object IncludeFileMaskCombo: THistoryComboBox
      Left = 9
      Top = 40
      Width = 346
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 3000
      TabOrder = 0
      Text = 'IncludeFileMaskCombo'
      OnExit = ValidateMaskComboExit
    end
    object IncludeFileMaskButton: TButton
      Left = 361
      Top = 39
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit...'
      TabOrder = 1
      OnClick = IncludeFileMaskButtonClick
    end
    object NewerOnlyCheck: TCheckBox
      Left = 11
      Top = 81
      Width = 213
      Height = 17
      Caption = '&New and updated files only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = ControlChange
    end
    object IncludeFileMaskHintText: TStaticText
      Left = 230
      Top = 63
      Width = 125
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'mask hints'
      TabOrder = 2
      TabStop = True
    end
    object ExcludeHiddenFilesCheck: TCheckBox
      Left = 240
      Top = 81
      Width = 201
      Height = 17
      Caption = 'Exclude h&idden files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = ControlChange
    end
    object ExcludeEmptyDirectoriesCheck: TCheckBox
      Left = 11
      Top = 104
      Width = 213
      Height = 17
      Caption = 'E&xclude empty directories'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = ControlChange
    end
  end
end
