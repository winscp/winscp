object CopyParamsFrame: TCopyParamsFrame
  Left = 0
  Top = 0
  Width = 420
  Height = 456
  HelpType = htKeyword
  TabOrder = 0
  object CommonPropertiesGroup: TGroupBox
    Left = 212
    Top = 165
    Width = 201
    Height = 123
    Caption = 'Common options'
    TabOrder = 3
    DesignSize = (
      201
      123)
    object SpeedLabel3: TLabel
      Left = 15
      Top = 96
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
      Top = 71
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
      Top = 92
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
      Top = 45
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
    Top = 293
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
    Top = 165
    Width = 194
    Height = 178
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
      Top = 42
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
      Left = 16
      Top = 98
      Width = 173
      Height = 17
      Caption = 'Clear '#39'Arc&hive'#39' attribute'
      TabOrder = 3
    end
    object RemoveCtrlZAndBOMCheck: TCheckBox
      Left = 16
      Top = 124
      Width = 173
      Height = 17
      Caption = 'Remo&ve BOM and EOF marks X'
      TabOrder = 4
      OnClick = ControlChange
    end
  end
  object ChangeCaseGroup: TGroupBox
    Left = 267
    Top = 8
    Width = 146
    Height = 151
    Caption = 'Filename modification'
    TabOrder = 1
    DesignSize = (
      146
      151)
    object CCLowerCaseShortButton: TRadioButton
      Left = 16
      Top = 96
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
      Top = 46
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Upper case'
      TabOrder = 1
    end
    object CCLowerCaseButton: TRadioButton
      Left = 16
      Top = 71
      Width = 125
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Lo&wer case'
      TabOrder = 2
    end
    object ReplaceInvalidCharsCheck: TCheckBox
      Left = 16
      Top = 122
      Width = 125
      Height = 17
      Caption = 'Rep&lace '#39'\:*?'#39' ...'
      TabOrder = 4
      OnClick = ControlChange
    end
  end
  object TransferModeGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 151
    Caption = 'Transfer mode'
    TabOrder = 0
    DesignSize = (
      249
      151)
    object AsciiFileMaskLabel: TLabel
      Left = 16
      Top = 100
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
      Top = 47
      Width = 225
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Binary (archives, doc, ...)'
      TabOrder = 1
      OnClick = ControlChange
    end
    object TMAutomaticButton: TRadioButton
      Left = 16
      Top = 73
      Width = 225
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Automatic'
      TabOrder = 2
      OnClick = ControlChange
    end
    object AsciiFileMaskCombo: THistoryComboBox
      Left = 15
      Top = 116
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
    Top = 347
    Width = 405
    Height = 102
    Caption = 'Other'
    TabOrder = 5
    DesignSize = (
      405
      102)
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
      Top = 72
      Width = 293
      Height = 17
      Caption = '&New and updated files only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = ControlChange
    end
    object IncludeFileMaskHintText: TStaticText
      Left = 204
      Top = 58
      Width = 105
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'mask hints'
      TabOrder = 2
      TabStop = True
    end
  end
end
