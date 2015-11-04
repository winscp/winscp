object GenerateUrlDialog: TGenerateUrlDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_generateurl'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Generate URL X'
  ClientHeight = 362
  ClientWidth = 484
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    484
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 61
    Width = 468
    Height = 95
    Anchors = [akLeft, akTop, akRight]
    Caption = 'URL options'
    TabOrder = 1
    object UserNameCheck: TCheckBox
      Tag = 1
      Left = 11
      Top = 20
      Width = 216
      Height = 17
      Caption = '&User name'
      TabOrder = 0
      OnClick = ControlChange
    end
    object PasswordCheck: TCheckBox
      Tag = 2
      Left = 235
      Top = 20
      Width = 216
      Height = 17
      HelpType = htKeyword
      Caption = '&Password'
      TabOrder = 1
      OnClick = ControlChange
    end
    object HostKeyCheck: TCheckBox
      Tag = 4
      Left = 11
      Top = 43
      Width = 216
      Height = 17
      Caption = 'SSH &host Key'
      TabOrder = 2
      OnClick = ControlChange
    end
    object RemoteDirectoryCheck: TCheckBox
      Tag = 8
      Left = 235
      Top = 43
      Width = 216
      Height = 17
      Caption = 'Initial &directory'
      TabOrder = 3
      OnClick = ControlChange
    end
    object WinSCPSpecificCheck: TCheckBox
      Tag = 16
      Left = 11
      Top = 66
      Width = 216
      Height = 17
      Caption = '&WinSCP-specific'
      TabOrder = 4
      OnClick = ControlChange
    end
    object SaveExtensionCheck: TCheckBox
      Tag = 32
      Left = 235
      Top = 66
      Width = 216
      Height = 17
      Caption = '&Save extension'
      TabOrder = 5
      OnClick = ControlChange
    end
  end
  object ResultGroup: TGroupBox
    Left = 8
    Top = 272
    Width = 468
    Height = 51
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'ResultX'
    TabOrder = 4
    DesignSize = (
      468
      51)
    object ResultMemo: TMemo
      Left = 7
      Top = 15
      Width = 454
      Height = 27
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Lines.Strings = (
        'ResultMemo')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object CancelBtn: TButton
    Left = 318
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 6
  end
  object HelpButton: TButton
    Left = 401
    Top = 329
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpButtonClick
  end
  object ClipboardButton: TButton
    Left = 8
    Top = 329
    Width = 145
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Copy to Clipboard'
    TabOrder = 5
    OnClick = ClipboardButtonClick
  end
  object GenerateGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 468
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Generate'
    TabOrder = 0
    object UrlButton: TRadioButton
      Tag = 1
      Left = 11
      Top = 20
      Width = 102
      Height = 17
      Caption = '&URL'
      TabOrder = 0
      OnClick = ControlChange
    end
    object ScriptButton: TRadioButton
      Tag = 2
      Left = 124
      Top = 20
      Width = 102
      Height = 17
      HelpType = htKeyword
      Caption = '&Script'
      TabOrder = 1
      OnClick = ControlChange
    end
    object AssemblyButton: TRadioButton
      Tag = 4
      Left = 235
      Top = 20
      Width = 216
      Height = 17
      Caption = '.NET &assembly code'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object AssemblyOptionsGroup: TGroupBox
    Left = 8
    Top = 217
    Width = 468
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Code options'
    TabOrder = 3
    object Label1: TLabel
      Left = 11
      Top = 21
      Width = 51
      Height = 13
      Caption = '&Language:'
      FocusControl = AssemblyLanguageCombo
    end
    object AssemblyLanguageCombo: TComboBox
      Left = 124
      Top = 17
      Width = 103
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ControlChange
      Items.Strings = (
        'C#'
        'VB.NET'
        'PowerShell')
    end
  end
  object ScriptOptionsGroup: TGroupBox
    Left = 8
    Top = 162
    Width = 468
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Script options'
    TabOrder = 2
    object Label2: TLabel
      Left = 11
      Top = 21
      Width = 38
      Height = 13
      Caption = '&Format:'
      FocusControl = ScriptFormatCombo
    end
    object ScriptFormatCombo: TComboBox
      Left = 124
      Top = 17
      Width = 103
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = ControlChange
      Items.Strings = (
        'Script file'
        'Batch file'
        'Command-line')
    end
  end
end
