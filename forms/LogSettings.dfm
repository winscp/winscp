object LoggingFrame: TLoggingFrame
  Left = 0
  Top = 0
  Width = 314
  Height = 212
  TabOrder = 0
  DesignSize = (
    314
    212)
  object LoggingCheck: TCheckBox
    Left = 14
    Top = 8
    Width = 203
    Height = 17
    Caption = '&Enable logging'
    TabOrder = 0
    OnClick = DataChange
  end
  object LoggingGroup: TXPGroupBox
    Left = 3
    Top = 32
    Width = 309
    Height = 177
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Logging options'
    TabOrder = 1
    DesignSize = (
      309
      177)
    object LogWindowLinesText: TLabel
      Left = 256
      Top = 145
      Width = 21
      Height = 13
      Caption = 'lines'
    end
    object LogToFileCheck: TCheckBox
      Left = 16
      Top = 24
      Width = 193
      Height = 17
      Caption = 'Log to &file:'
      TabOrder = 0
      OnClick = LogToFileCheckChange
    end
    object LogFileNameEdit: TFilenameEdit
      Left = 40
      Top = 47
      Width = 255
      Height = 21
      AcceptFiles = True
      DefaultExt = 'log'
      Filter = 'Log files (*.log)|*.log|All files (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofPathMustExist]
      DialogTitle = 'Select file for session log.'
      ClickKey = 16397
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'LogFileNameEdit'
      OnChange = DataChange
      OnKeyDown = LogFileNameEditKeyDown
    end
    object LogShowWindowCheck: TCheckBox
      Left = 16
      Top = 100
      Width = 129
      Height = 17
      Caption = 'Show log &window:'
      TabOrder = 3
      OnClick = DataChange
    end
    object LogWindowCompleteButton: TRadioButton
      Left = 40
      Top = 121
      Width = 233
      Height = 17
      Caption = 'Display &complete session'
      TabOrder = 4
      OnClick = DataChange
    end
    object LogWindowLinesButton: TRadioButton
      Left = 40
      Top = 145
      Width = 136
      Height = 17
      Caption = 'Display only &last '
      TabOrder = 5
      OnClick = DataChange
    end
    object LogWindowLinesEdit: TUpDownEdit
      Left = 176
      Top = 141
      Width = 73
      Height = 21
      Alignment = taRightJustify
      Increment = 50
      MaxValue = 10000
      MinValue = 50
      TabOrder = 6
      OnChange = DataChange
    end
    object LogFilePanel: TPanel
      Left = 40
      Top = 72
      Width = 255
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 2
      object LogFileAppendButton: TRadioButton
        Left = 0
        Top = 4
        Width = 129
        Height = 17
        Caption = '&Append'
        TabOrder = 0
        OnClick = DataChange
      end
      object LogFileOverwriteButton: TRadioButton
        Left = 136
        Top = 4
        Width = 113
        Height = 17
        Caption = '&Overwrite'
        TabOrder = 1
        OnClick = DataChange
      end
    end
  end
end
