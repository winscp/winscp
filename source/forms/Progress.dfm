object ProgressForm: TProgressForm
  Left = 445
  Top = 291
  HelpType = htKeyword
  HelpKeyword = 'ui_progress'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Operation'
  ClientHeight = 196
  ClientWidth = 425
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    425
    196)
  PixelsPerInch = 96
  TextHeight = 13
  object OnceDoneOperationLabel: TLabel
    Left = 322
    Top = 88
    Width = 69
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Once &finished:'
    FocusControl = OnceDoneOperationCombo
  end
  object Animate: TAnimate
    Left = 10
    Top = 5
    Width = 301
    Height = 60
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    StopFrame = 23
  end
  object CancelButton: TButton
    Left = 320
    Top = 8
    Width = 100
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object MinimizeButton: TButton
    Left = 320
    Top = 40
    Width = 100
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 1
    OnClick = MinimizeButtonClick
  end
  object MainPanel: TPanel
    Left = 10
    Top = 65
    Width = 301
    Height = 68
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      301
      68)
    object Label1: TLabel
      Left = 0
      Top = 7
      Width = 20
      Height = 13
      Caption = 'File:'
    end
    object FileLabel: TPathLabel
      Left = 56
      Top = 7
      Width = 245
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
      Width = 36
      Height = 13
      Caption = 'Target:'
    end
    object TargetPathLabel: TPathLabel
      Left = 56
      Top = 23
      Width = 245
      Height = 13
      IndentHorizontal = 0
      IndentVertical = 0
      Align = alNone
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object TopProgress: TProgressBar
      Left = 0
      Top = 42
      Width = 301
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object TransferPanel: TPanel
    Left = 10
    Top = 133
    Width = 321
    Height = 63
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      321
      63)
    object StartTimeLabel: TLabel
      Left = 88
      Top = 2
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
      Width = 45
      Height = 13
      Caption = 'Time left:'
    end
    object CPSLabel: TLabel
      Left = 236
      Top = 18
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '0 KB/s'
    end
    object TimeElapsedLabel: TLabel
      Left = 236
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
      Caption = '0 KB'
    end
    object Label3: TLabel
      Left = 164
      Top = 2
      Width = 66
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Time elapsed:'
    end
    object StartTimeLabelLabel: TLabel
      Left = 0
      Top = 2
      Width = 51
      Height = 13
      Caption = 'Start time:'
    end
    object Label4: TLabel
      Left = 0
      Top = 18
      Width = 89
      Height = 13
      Caption = 'Bytes transferred:'
    end
    object Label12: TLabel
      Left = 164
      Top = 18
      Width = 34
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Speed:'
    end
    object BottomProgress: TProgressBar
      Left = 0
      Top = 37
      Width = 301
      Height = 16
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object SpeedPanel: TPanel
    Left = 314
    Top = 151
    Width = 112
    Height = 41
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 6
    DesignSize = (
      112
      41)
    object SpeedLabel3: TLabel
      Left = 8
      Top = 0
      Width = 66
      Height = 13
      Caption = '&Speed (KB/s):'
      FocusControl = SpeedCombo
    end
    object SpeedCombo: THistoryComboBox
      Left = 6
      Top = 16
      Width = 100
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
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
  object OnceDoneOperationCombo: TComboBox
    Left = 320
    Top = 104
    Width = 100
    Height = 21
    AutoComplete = False
    Style = csDropDownList
    Anchors = [akTop, akRight]
    TabOrder = 5
    OnCloseUp = OnceDoneOperationComboCloseUp
    OnSelect = OnceDoneOperationComboSelect
    Items.Strings = (
      'Stay idle'
      'Disconnect'
      'Shut down')
  end
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 336
    Top = 168
  end
  object MinimizeMenu: TPopupMenu
    Left = 384
    Top = 48
    object MoveToQueueMenuItem: TMenuItem
      Caption = 'Proceed in &Background'
      OnClick = MoveToQueueMenuItemClick
    end
    object MinimizeMenuItem: TMenuItem
      Caption = '&Minimize'
      OnClick = MinimizeMenuItemClick
    end
  end
end
