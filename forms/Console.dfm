object ConsoleDialog: TConsoleDialog
  Left = 349
  Top = 169
  Width = 567
  Height = 431
  HelpType = htKeyword
  HelpKeyword = 'ui_console'
  BorderIcons = [biSystemMenu, biMaximize, biHelp]
  Caption = 'Console'
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 380
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  DesignSize = (
    559
    397)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 559
    Height = 78
    Align = alTop
    Shape = bsBottomLine
  end
  object Label1: TLabel
    Left = 13
    Top = 13
    Width = 74
    Height = 13
    Caption = 'Enter &command'
    FocusControl = CommandEdit
  end
  object Label2: TLabel
    Left = 13
    Top = 56
    Width = 80
    Height = 13
    Caption = 'Current directory:'
  end
  object Label4: TLabel
    Left = 13
    Top = 34
    Width = 286
    Height = 13
    Caption = 'Warning: Do NOT execute commands that require user-input'
  end
  object DirectoryLabel: TPathLabel
    Left = 120
    Top = 56
    Width = 337
    Height = 13
    IndentHorizontal = 0
    IndentVertical = 0
    Align = alNone
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
  end
  object OutputMemo: TMemo
    Left = 0
    Top = 78
    Width = 559
    Height = 319
    TabStop = False
    Align = alClient
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WantReturns = False
  end
  object CancelBtn: TButton
    Left = 472
    Top = 7
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
  end
  object CommandEdit: THistoryComboBox
    Left = 120
    Top = 9
    Width = 251
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    MaxLength = 250
    TabOrder = 0
    OnChange = CommandEditChange
  end
  object ExecuteButton: TButton
    Left = 384
    Top = 7
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Execute'
    Default = True
    TabOrder = 1
    OnClick = ExecuteButtonClick
  end
  object HelpButton: TButton
    Left = 472
    Top = 42
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
