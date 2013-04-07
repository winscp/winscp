object LoggingFrame: TLoggingFrame
  Left = 0
  Top = 0
  Width = 356
  Height = 300
  TabOrder = 0
  DesignSize = (
    356
    300)
  object LoggingGroup: TGroupBox
    Left = 3
    Top = 3
    Width = 351
    Height = 190
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Session log'
    TabOrder = 0
    DesignSize = (
      351
      190)
    object LogWindowLinesText: TLabel
      Left = 256
      Top = 163
      Width = 21
      Height = 13
      Caption = 'lines'
    end
    object LogToFileCheck: TCheckBox
      Left = 16
      Top = 47
      Width = 217
      Height = 17
      Caption = 'Log to &file:'
      TabOrder = 2
      OnClick = DataChange
    end
    object LogFileNameEdit3: TFilenameEdit
      Left = 40
      Top = 69
      Width = 297
      Height = 21
      AcceptFiles = True
      OnBeforeDialog = LogFileNameEditBeforeDialog
      OnAfterDialog = LogFileNameEditAfterDialog
      DefaultExt = 'log'
      Filter = 'Session log files (*.log)|*.log|All files (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofPathMustExist]
      DialogTitle = 'Select file for session log.'
      OnCreateEditDialog = LogFileNameEditCreateEditDialog
      ClickKey = 16397
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = 'LogFileNameEdit3'
      OnChange = DataChange
    end
    object LogShowWindowCheck: TCheckBox
      Left = 16
      Top = 120
      Width = 217
      Height = 17
      Caption = 'Show log &window:'
      TabOrder = 6
      OnClick = DataChange
    end
    object LogWindowCompleteButton: TRadioButton
      Left = 40
      Top = 140
      Width = 233
      Height = 17
      Caption = 'Display &complete session'
      TabOrder = 7
      OnClick = DataChange
    end
    object LogWindowLinesButton: TRadioButton
      Left = 40
      Top = 163
      Width = 136
      Height = 17
      Caption = 'Display only &last '
      TabOrder = 8
      OnClick = DataChange
    end
    object LogWindowLinesEdit: TUpDownEdit
      Left = 176
      Top = 159
      Width = 73
      Height = 21
      Alignment = taRightJustify
      Increment = 50.000000000000000000
      MaxValue = 10000.000000000000000000
      MinValue = 50.000000000000000000
      Value = 50.000000000000000000
      TabOrder = 9
      OnChange = DataChange
    end
    object LogFilePanel: TPanel
      Left = 40
      Top = 93
      Width = 227
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 5
      object LogFileAppendButton: TRadioButton
        Left = 0
        Top = 4
        Width = 97
        Height = 17
        Caption = 'Appe&nd'
        TabOrder = 0
        OnClick = DataChange
      end
      object LogFileOverwriteButton: TRadioButton
        Left = 112
        Top = 4
        Width = 97
        Height = 17
        Caption = '&Overwrite'
        TabOrder = 1
        OnClick = DataChange
      end
    end
    object LogProtocolCombo: TComboBox
      Left = 224
      Top = 21
      Width = 113
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Items.Strings = (
        'Normal'
        'Debug 1'
        'Debug 2')
    end
    object LogFileNameHintText: TStaticText
      Left = 255
      Top = 91
      Width = 82
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&patterns'
      TabOrder = 4
      TabStop = True
    end
    object EnableLoggingCheck: TCheckBox
      Left = 16
      Top = 23
      Width = 202
      Height = 17
      Caption = 'Enable &session logging on level:'
      TabOrder = 0
      OnClick = DataChange
    end
  end
  object ActionsLoggingGroup: TGroupBox
    Left = 3
    Top = 198
    Width = 351
    Height = 86
    Anchors = [akLeft, akTop, akRight]
    Caption = 'XML log'
    TabOrder = 1
    DesignSize = (
      351
      86)
    object ActionsLogFileNameEdit: TFilenameEdit
      Left = 40
      Top = 43
      Width = 296
      Height = 21
      AcceptFiles = True
      OnBeforeDialog = LogFileNameEditBeforeDialog
      OnAfterDialog = LogFileNameEditAfterDialog
      DefaultExt = 'xml'
      Filter = 'XML log files (*.xml)|*.xml|All files (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofPathMustExist]
      DialogTitle = 'Select file for XML log.'
      OnCreateEditDialog = LogFileNameEditCreateEditDialog
      ClickKey = 16397
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'ActionsLogFileNameEdit'
      OnChange = DataChange
    end
    object ActionsLogFileNameHintText: TStaticText
      Left = 255
      Top = 65
      Width = 82
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'pa&tterns'
      TabOrder = 2
      TabStop = True
    end
    object EnableActionsLoggingCheck: TCheckBox
      Left = 16
      Top = 21
      Width = 241
      Height = 17
      Caption = 'Enable &XML logging to file:'
      TabOrder = 0
      OnClick = DataChange
    end
  end
end
