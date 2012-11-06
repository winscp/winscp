object CopyParamsFrame: TCopyParamsFrame
  Left = 0
  Top = 0
  Width = 420
  Height = 409
  HelpType = htKeyword
  TabOrder = 0
  object CommonPropertiesGroup: TGroupBox
    Left = 212
    Top = 165
    Width = 201
    Height = 98
    Caption = 'Common options'
    TabOrder = 3
    DesignSize = (
      201
      98)
    object SpeedLabel2: TLabel
      Left = 15
      Top = 71
      Width = 68
      Height = 13
      Caption = '&Speed (KiB/s):'
      FocusControl = SpeedCombo
    end
    object PreserveTimeCheck: TCheckBox
      Left = 16
      Top = 21
      Width = 175
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Preserve timesta&mp'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ControlChange
    end
    object CommonCalculateSizeCheck: TCheckBox
      Left = 16
      Top = 46
      Width = 175
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Calculate total size'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = ControlChange
    end
    object SpeedCombo: THistoryComboBox
      Left = 106
      Top = 67
      Width = 85
      Height = 21
      AutoComplete = False
      TabOrder = 2
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
  end
  object LocalPropertiesGroup: TGroupBox
    Left = 212
    Top = 268
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
    Height = 153
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
      Top = 100
      Width = 173
      Height = 17
      Caption = 'Clear '#39'Archi&ve'#39' attribute'
      TabOrder = 3
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
      Caption = '&No change'
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
      Caption = 'Replace '#39'\:*?'#39' ...'
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
      Width = 181
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
    Top = 322
    Width = 405
    Height = 80
    Caption = 'Other'
    TabOrder = 5
    DesignSize = (
      405
      80)
    object IncludeFileMaskLabel: TLabel
      Left = 16
      Top = 20
      Width = 47
      Height = 13
      Caption = 'File mas&k:'
      FocusControl = IncludeFileMaskCombo
    end
    object IncludeFileMaskCombo: THistoryComboBox
      Left = 15
      Top = 36
      Width = 302
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 3000
      TabOrder = 0
      Text = 'IncludeFileMaskCombo'
      OnExit = ValidateMaskComboExit
    end
    object IncludeFileMaskButton: TButton
      Left = 323
      Top = 33
      Width = 72
      Height = 25
      Caption = '&Edit...'
      TabOrder = 1
      OnClick = IncludeFileMaskButtonClick
    end
  end
  object IncludeFileMaskHintText: TStaticText
    Left = 271
    Top = 380
    Width = 54
    Height = 17
    Alignment = taCenter
    Caption = 'mask hints'
    TabOrder = 6
    TabStop = True
  end
end
