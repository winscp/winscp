object ProgressForm: TProgressForm
  Left = 445
  Top = 291
  HelpType = htKeyword
  HelpKeyword = 'ui_progress'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Operation'
  ClientHeight = 224
  ClientWidth = 394
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    394
    224)
  PixelsPerInch = 96
  TextHeight = 13
  object Animate: TAnimate
    Left = 8
    Top = 5
    Width = 299
    Height = 60
    Active = False
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    StopFrame = 23
  end
  object CancelButton: TButton
    Left = 314
    Top = 8
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object MinimizeButton: TButton
    Left = 314
    Top = 40
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 1
    OnClick = MinimizeButtonClick
  end
  object MainPanel: TPanel
    Left = 8
    Top = 65
    Width = 299
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      299
      68)
    object Label1: TLabel
      Left = 0
      Top = 7
      Width = 19
      Height = 13
      Caption = 'File:'
    end
    object FileLabel: TPathLabel
      Left = 56
      Top = 7
      Width = 243
      Height = 13
      IndentHorizontal = 0
      IndentVertical = 0
      Align = alNone
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object TargetLabel: TLabel
      Left = 0
      Top = 23
      Width = 34
      Height = 13
      Caption = 'Target:'
    end
    object TargetPathLabel: TPathLabel
      Left = 56
      Top = 23
      Width = 243
      Height = 13
      IndentHorizontal = 0
      IndentVertical = 0
      Align = alNone
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object OperationProgress: TProgressBar
      Left = 0
      Top = 42
      Width = 299
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Min = 0
      Max = 100
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object TransferPanel: TPanel
    Left = 8
    Top = 133
    Width = 299
    Height = 63
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      299
      63)
    object StartTimeLabel: TLabel
      Left = 88
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00:00:00'
    end
    object TimeLeftLabel: TLabel
      Left = 88
      Top = 2
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00:00:00'
    end
    object TimeLeftLabelLabel: TLabel
      Left = 0
      Top = 2
      Width = 43
      Height = 13
      Caption = 'Time left:'
    end
    object CPSLabel: TLabel
      Left = 234
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '0 KiB/s'
    end
    object TimeElapsedLabel: TLabel
      Left = 234
      Top = 2
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00:00:00'
    end
    object BytesTransferedLabel: TLabel
      Left = 88
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0 KiB'
    end
    object Label3: TLabel
      Left = 162
      Top = 2
      Width = 66
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Time elapsed:'
    end
    object StartTimeLabelLabel: TLabel
      Left = 0
      Top = 2
      Width = 47
      Height = 13
      Caption = 'Start time:'
    end
    object Label4: TLabel
      Left = 0
      Top = 18
      Width = 82
      Height = 13
      Caption = 'Bytes transferred:'
    end
    object Label12: TLabel
      Left = 162
      Top = 18
      Width = 34
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Speed:'
    end
    object FileProgress: TProgressBar
      Left = 0
      Top = 37
      Width = 299
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      Min = 0
      Max = 100
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object DisconnectWhenCompleteCheck: TCheckBox
    Left = 16
    Top = 198
    Width = 289
    Height = 17
    Caption = '&Disconnect when operation finishes'
    TabOrder = 6
  end
  object SpeedPanel: TPanel
    Left = 308
    Top = 89
    Width = 85
    Height = 73
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 5
    object SpeedLabel2: TLabel
      Left = 8
      Top = 0
      Width = 66
      Height = 13
      Caption = '&Speed (KiB/s)'
    end
    object SpeedCombo: THistoryComboBox
      Left = 6
      Top = 16
      Width = 73
      Height = 21
      AutoComplete = False
      ItemHeight = 13
      TabOrder = 0
      Text = 'SpeedCombo'
      OnExit = SpeedComboExit
      OnKeyPress = SpeedComboKeyPress
      OnSelect = SpeedComboSelect
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
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 336
    Top = 176
  end
end
