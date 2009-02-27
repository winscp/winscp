object LoggingFrame: TLoggingFrame
  Left = 0
  Top = 0
  Width = 314
  Height = 259
  TabOrder = 0
  DesignSize = (
    314
    259)
  object LoggingGroup: TGroupBox
    Left = 3
    Top = 59
    Width = 309
    Height = 198
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Logging options'
    TabOrder = 0
    DesignSize = (
      309
      198)
    object LogWindowLinesText: TLabel
      Left = 256
      Top = 166
      Width = 21
      Height = 13
      Caption = 'lines'
    end
    object LogProtocolLabel: TLabel
      Left = 16
      Top = 21
      Width = 66
      Height = 13
      Caption = 'Logging &level:'
      FocusControl = LogProtocolCombo
    end
    object LogToFileCheck: TCheckBox
      Left = 16
      Top = 45
      Width = 193
      Height = 17
      Caption = 'Log to &file:'
      TabOrder = 1
      OnClick = LogToFileCheckChange
    end
    object LogFileNameEdit2: TFilenameEdit
      Left = 40
      Top = 68
      Width = 255
      Height = 21
      AcceptFiles = True
      OnBeforeDialog = LogFileNameEdit2BeforeDialog
      OnAfterDialog = LogFileNameEdit2AfterDialog
      DefaultExt = 'log'
      Filter = 'Log files (*.log; *.xml)|*.log;*.xml|All files (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofPathMustExist]
      DialogTitle = 'Select file for session log.'
      OnCreateEditDialog = LogFileNameEdit2CreateEditDialog
      ClickKey = 16397
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'LogFileNameEdit2'
      OnChange = DataChange
    end
    object LogShowWindowCheck: TCheckBox
      Left = 16
      Top = 121
      Width = 129
      Height = 17
      Caption = 'Show log &window:'
      TabOrder = 5
      OnClick = DataChange
    end
    object LogWindowCompleteButton: TRadioButton
      Left = 40
      Top = 142
      Width = 233
      Height = 17
      Caption = 'Display &complete session'
      TabOrder = 6
      OnClick = DataChange
    end
    object LogWindowLinesButton: TRadioButton
      Left = 40
      Top = 166
      Width = 136
      Height = 17
      Caption = 'Display only &last '
      TabOrder = 7
      OnClick = DataChange
    end
    object LogWindowLinesEdit: TUpDownEdit
      Left = 176
      Top = 162
      Width = 73
      Height = 21
      Alignment = taRightJustify
      Increment = 50
      MaxValue = 10000
      MinValue = 50
      TabOrder = 8
      OnChange = DataChange
    end
    object LogFilePanel: TPanel
      Left = 40
      Top = 93
      Width = 185
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 4
      object LogFileAppendButton: TRadioButton
        Left = 0
        Top = 4
        Width = 97
        Height = 17
        Caption = 'Appen&d'
        TabOrder = 0
        OnClick = DataChange
      end
      object LogFileOverwriteButton: TRadioButton
        Left = 96
        Top = 4
        Width = 89
        Height = 17
        Caption = '&Overwrite'
        TabOrder = 1
        OnClick = DataChange
      end
    end
    object LogProtocolCombo: TComboBox
      Left = 112
      Top = 17
      Width = 113
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Normal'
        'Debug 1'
        'Debug 2')
    end
    object LogFileNameHintText: TStaticText
      Left = 224
      Top = 91
      Width = 71
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&patterns'
      TabOrder = 3
      TabStop = True
    end
  end
  object LogGroup: TGroupBox
    Left = 3
    Top = 3
    Width = 309
    Height = 51
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Log'
    TabOrder = 1
    object LoggingOffButton: TRadioButton
      Left = 16
      Top = 21
      Width = 89
      Height = 17
      Caption = '&No log'
      TabOrder = 0
      OnClick = LoggingButtonClick
    end
    object LoggingOnButton: TRadioButton
      Left = 112
      Top = 21
      Width = 89
      Height = 17
      Caption = '&Text log'
      TabOrder = 1
      OnClick = LoggingButtonClick
    end
    object LoggingActionsButton: TRadioButton
      Left = 208
      Top = 21
      Width = 89
      Height = 17
      Caption = '&XML log'
      TabOrder = 2
      OnClick = LoggingButtonClick
    end
  end
end
