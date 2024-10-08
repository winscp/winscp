object CopyParamsFrame: TCopyParamsFrame
  Left = 0
  Top = 0
  Width = 420
  Height = 477
  HelpType = htKeyword
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object CommonPropertiesGroup: TGroupBox
    Left = 212
    Top = 155
    Width = 201
    Height = 121
    Caption = 'Common options'
    TabOrder = 3
    DesignSize = (
      201
      121)
    object SpeedLabel3: TLabel
      Left = 15
      Top = 93
      Width = 66
      Height = 13
      Caption = '&Speed (KB/s):'
      FocusControl = SpeedCombo
    end
    object PreserveTimeCheck: TCheckBox
      Left = 16
      Top = 21
      Width = 175
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Preserve timestamp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ControlChange
    end
    object CommonCalculateSizeCheck: TCheckBox
      Left = 16
      Top = 67
      Width = 175
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Calculate total size'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ControlChange
    end
    object SpeedCombo: THistoryComboBox
      Left = 106
      Top = 90
      Width = 85
      Height = 21
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
      Left = 32
      Top = 44
      Width = 159
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
    Left = 212
    Top = 282
    Width = 201
    Height = 50
    Caption = 'Download options'
    TabOrder = 4
    DesignSize = (
      201
      50)
    object PreserveReadOnlyCheck: TCheckBox
      Left = 16
      Top = 21
      Width = 179
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Preserve rea&d-only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object RemotePropertiesGroup: TGroupBox
    Left = 8
    Top = 155
    Width = 194
    Height = 177
    Caption = 'Upload options'
    TabOrder = 2
    object PreserveRightsCheck: TCheckBox
      Left = 16
      Top = 21
      Width = 173
      Height = 17
      Caption = 'Set pe&rmissions:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ControlChange
    end
    object RightsEdit: TComboEdit
      Left = 34
      Top = 44
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
      Left = 16
      Top = 73
      Width = 173
      Height = 17
      Caption = 'Ign&ore permission errors'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ControlChange
    end
    object ClearArchiveCheck: TCheckBox
      Left = 16
      Top = 96
      Width = 173
      Height = 17
      Caption = 'Clear '#39'Arc&hive'#39' attribute'
      TabOrder = 3
    end
    object RemoveCtrlZAndBOMCheck: TCheckBox
      Left = 16
      Top = 119
      Width = 173
      Height = 17
      Caption = 'Remo&ve BOM and EOF marks X'
      TabOrder = 4
      OnClick = ControlChange
    end
    object EncryptNewFilesCheck: TCheckBox
      Left = 16
      Top = 142
      Width = 173
      Height = 17
      Caption = '&Encrypt new files'
      TabOrder = 5
      OnClick = ControlChange
    end
  end
  object ChangeCaseGroup: TGroupBox
    Left = 267
    Top = 8
    Width = 146
    Height = 141
    Caption = 'Filename modification'
    TabOrder = 1
    DesignSize = (
      146
      141)
    object CCLowerCaseShortButton: TRadioButton
      Left = 16
      Top = 90
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Lower case &8.3'
      TabOrder = 3
    end
    object CCNoChangeButton: TRadioButton
      Left = 16
      Top = 21
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'No chan&ge'
      TabOrder = 0
    end
    object CCUpperCaseButton: TRadioButton
      Left = 16
      Top = 44
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Upper case'
      TabOrder = 1
    end
    object CCLowerCaseButton: TRadioButton
      Left = 16
      Top = 67
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Lo&wer case'
      TabOrder = 2
    end
    object ReplaceInvalidCharsCheck: TCheckBox
      Left = 16
      Top = 113
      Width = 125
      Height = 17
      Caption = 'Rep&lace '#39'\:*?'#39'...'
      TabOrder = 4
      OnClick = ControlChange
    end
  end
  object TransferModeGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 141
    Caption = 'Transfer mode'
    TabOrder = 0
    DesignSize = (
      249
      141)
    object AsciiFileMaskLabel: TLabel
      Left = 16
      Top = 90
      Width = 175
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Transfer following &files in text mode:'
      FocusControl = AsciiFileMaskCombo
    end
    object TMTextButton: TRadioButton
      Left = 16
      Top = 21
      Width = 225
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Text (plain text, html, scripts, ...)'
      TabOrder = 0
      OnClick = ControlChange
    end
    object TMBinaryButton: TRadioButton
      Left = 16
      Top = 44
      Width = 225
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Binary (archives, doc, ...)'
      TabOrder = 1
      OnClick = ControlChange
    end
    object TMAutomaticButton: TRadioButton
      Left = 16
      Top = 67
      Width = 225
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Automatic'
      TabOrder = 2
      OnClick = ControlChange
    end
    object AsciiFileMaskCombo: THistoryComboBox
      Left = 15
      Top = 106
      Width = 219
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 3
      Text = 'AsciiFileMaskCombo'
      OnExit = ValidateMaskComboExit
    end
  end
  object OtherGroup: TGroupBox
    Left = 8
    Top = 338
    Width = 405
    Height = 132
    Caption = 'Other'
    TabOrder = 5
    DesignSize = (
      405
      132)
    object IncludeFileMaskLabel: TLabel
      Left = 16
      Top = 20
      Width = 47
      Height = 13
      Caption = 'File &mask:'
      FocusControl = IncludeFileMaskCombo
    end
    object IncludeFileMaskCombo: THistoryComboBox
      Left = 15
      Top = 36
      Width = 294
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 3000
      TabOrder = 0
      Text = 'IncludeFileMaskCombo'
      OnExit = ValidateMaskComboExit
    end
    object IncludeFileMaskButton: TButton
      Left = 315
      Top = 33
      Width = 80
      Height = 25
      Caption = '&Edit...'
      TabOrder = 1
      OnClick = IncludeFileMaskButtonClick
    end
    object NewerOnlyCheck: TCheckBox
      Left = 16
      Top = 79
      Width = 197
      Height = 17
      Caption = '&New and updated files only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = ControlChange
    end
    object IncludeFileMaskHintText: TStaticText
      Left = 184
      Top = 58
      Width = 125
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'mask hints'
      TabOrder = 2
      TabStop = True
    end
    object ExcludeHiddenFilesCheck: TCheckBox
      Left = 219
      Top = 79
      Width = 176
      Height = 17
      Caption = 'Exclude h&idden files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = ControlChange
    end
    object ExcludeEmptyDirectoriesCheck: TCheckBox
      Left = 16
      Top = 102
      Width = 197
      Height = 17
      Caption = 'E&xclude empty directories'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = ControlChange
    end
  end
end
