object SynchronizeProgressForm: TSynchronizeProgressForm
  Left = 335
  Top = 260
  BorderStyle = bsDialog
  Caption = 'Synchronization'
  ClientHeight = 127
  ClientWidth = 370
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    370
    127)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 29
    Height = 13
    Caption = 'Local:'
  end
  object Label2: TLabel
    Left = 8
    Top = 29
    Width = 40
    Height = 13
    Caption = 'Remote:'
  end
  object RemoteDirectoryLabel: TPathLabel
    Left = 88
    Top = 29
    Width = 273
    Height = 13
    UnixPath = True
    IndentHorizontal = 0
    IndentVertical = 0
    Align = alNone
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object LocalDirectoryLabel: TPathLabel
    Left = 88
    Top = 9
    Width = 273
    Height = 13
    IndentHorizontal = 0
    IndentVertical = 0
    Align = alNone
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object StartTimeLabel: TLabel
    Left = 88
    Top = 49
    Width = 81
    Height = 13
    AutoSize = False
    Caption = '00:00:00'
  end
  object Label4: TLabel
    Left = 8
    Top = 49
    Width = 47
    Height = 13
    Caption = 'Start time:'
  end
  object Label3: TLabel
    Left = 8
    Top = 69
    Width = 66
    Height = 13
    Caption = 'Time elapsed:'
  end
  object TimeElapsedLabel: TLabel
    Left = 88
    Top = 69
    Width = 79
    Height = 13
    AutoSize = False
    Caption = '00:00:00'
  end
  object CancelButton: TButton
    Left = 105
    Top = 93
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object MinimizeButton: TButton
    Left = 190
    Top = 93
    Width = 73
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 1
    OnClick = MinimizeButtonClick
  end
  object UpdateTimer: TTimer
    Enabled = False
    OnTimer = UpdateTimerTimer
    Left = 41
    Top = 89
  end
end
