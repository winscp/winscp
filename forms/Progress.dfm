object ProgressForm: TProgressForm
  Left = 356
  Top = 204
  HelpType = htKeyword
  HelpKeyword = 'ui_progress'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Operation'
  ClientHeight = 240
  ClientWidth = 394
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    394
    240)
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
    Height = 79
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      299
      79)
    object StartTimeLabel: TLabel
      Left = 88
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00:00:00'
    end
    object TimeEstimatedLabel: TLabel
      Left = 88
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '00:00:00'
    end
    object TimeEstimatedLabelLabel: TLabel
      Left = 0
      Top = 18
      Width = 74
      Height = 13
      Caption = 'Time estimated:'
    end
    object CPSLabel: TLabel
      Left = 234
      Top = 34
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '0 KB/s'
    end
    object TimeElapsedLabel: TLabel
      Left = 234
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '00:00:00'
    end
    object ResumeLabel: TLabel
      Left = 234
      Top = 2
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'Enabled'
    end
    object TransferModeLabel: TLabel
      Left = 88
      Top = 2
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Binary'
    end
    object BytesTransferedLabel: TLabel
      Left = 88
      Top = 34
      Width = 65
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '0 KB'
    end
    object Label3: TLabel
      Left = 162
      Top = 18
      Width = 66
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Time elapsed:'
    end
    object StartTimeLabelLabel: TLabel
      Left = 0
      Top = 18
      Width = 47
      Height = 13
      Caption = 'Start time:'
    end
    object Label4: TLabel
      Left = 0
      Top = 34
      Width = 82
      Height = 13
      Caption = 'Bytes transferred:'
    end
    object Label7: TLabel
      Left = 162
      Top = 34
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'CPS:'
    end
    object Label10: TLabel
      Left = 0
      Top = 2
      Width = 71
      Height = 13
      Caption = 'Transfer mode:'
    end
    object Label11: TLabel
      Left = 162
      Top = 2
      Width = 42
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Resume:'
    end
    object FileProgress: TProgressBar
      Left = 0
      Top = 53
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
    Top = 214
    Width = 289
    Height = 17
    Caption = '&Disconnect when operation finishes'
    TabOrder = 5
  end
  object SpeedPanel: TPanel
    Left = 308
    Top = 89
    Width = 85
    Height = 73
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 6
    object SpeedLabel: TLabel
      Left = 8
      Top = 0
      Width = 31
      Height = 13
      Caption = '&Speed'
      FocusControl = SpeedBar
    end
    object SpeedLowLabel: TLabel
      Left = 7
      Top = 48
      Width = 26
      Height = 13
      AutoSize = False
      Caption = '5%'
    end
    object SpeedHighLabel: TLabel
      Left = 46
      Top = 48
      Width = 32
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '100%'
    end
    object SpeedBar: TTrackBar
      Left = 0
      Top = 15
      Width = 85
      Height = 33
      LineSize = 5
      Max = 100
      Min = 5
      Orientation = trHorizontal
      PageSize = 25
      Frequency = 25
      Position = 100
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      TickMarks = tmBottomRight
      TickStyle = tsManual
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 336
    Top = 176
  end
end
