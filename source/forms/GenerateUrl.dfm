object GenerateUrlDialog: TGenerateUrlDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_generateurl'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Generate URL X'
  ClientHeight = 381
  ClientWidth = 539
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    539
    381)
  TextHeight = 15
  object OptionsPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 539
    Height = 104
    ActivePage = UrlSheet
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ControlChange
    object UrlSheet: TTabSheet
      Caption = 'URL'
      object UserNameCheck: TCheckBox
        Tag = 1
        Left = 7
        Top = 5
        Width = 151
        Height = 17
        Caption = '&User name'
        TabOrder = 0
        OnClick = ControlChange
      end
      object HostKeyCheck: TCheckBox
        Tag = 4
        Left = 164
        Top = 5
        Width = 151
        Height = 17
        Caption = 'SSH &host Key'
        TabOrder = 3
        OnClick = ControlChange
      end
      object WinSCPSpecificCheck: TCheckBox
        Tag = 16
        Left = 164
        Top = 28
        Width = 151
        Height = 17
        Caption = '&WinSCP-specific'
        TabOrder = 4
        OnClick = ControlChange
      end
      object SaveExtensionCheck: TCheckBox
        Tag = 32
        Left = 164
        Top = 51
        Width = 151
        Height = 17
        Caption = '&Save extension'
        TabOrder = 5
        OnClick = ControlChange
      end
      object RemoteDirectoryCheck: TCheckBox
        Tag = 8
        Left = 7
        Top = 51
        Width = 151
        Height = 17
        Caption = 'Initial &directory'
        TabOrder = 2
        OnClick = ControlChange
      end
      object PasswordCheck: TCheckBox
        Tag = 2
        Left = 7
        Top = 28
        Width = 151
        Height = 17
        HelpType = htKeyword
        Caption = '&Password'
        TabOrder = 1
        OnClick = ControlChange
      end
      object RawSettingsCheck: TCheckBox
        Tag = 64
        Left = 321
        Top = 5
        Width = 147
        Height = 17
        Caption = '&Advanced settings'
        TabOrder = 6
        OnClick = ControlChange
      end
    end
    object ScriptSheet: TTabSheet
      Caption = 'Script'
      ImageIndex = 1
      DesignSize = (
        531
        74)
      object Label2: TLabel
        Left = 5
        Top = 8
        Width = 41
        Height = 15
        Caption = '&Format:'
        FocusControl = ScriptFormatCombo
      end
      object ScriptDescriptionLabel: TLabel
        Left = 5
        Top = 32
        Width = 518
        Height = 42
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = 'ScriptDescriptionLabel'
        ShowAccelChar = False
        WordWrap = True
      end
      object ScriptFormatCombo: TComboBox
        Left = 106
        Top = 5
        Width = 134
        Height = 23
        Style = csDropDownList
        TabOrder = 0
        OnChange = ControlChange
        Items.Strings = (
          'Script file'
          'Batch file'
          'Command-line'
          'PowerShell script')
      end
    end
    object AssemblySheet: TTabSheet
      Caption = '.NET assembly code'
      ImageIndex = 2
      DesignSize = (
        531
        74)
      object Label1: TLabel
        Left = 5
        Top = 8
        Width = 55
        Height = 15
        Caption = '&Language:'
        FocusControl = AssemblyLanguageCombo
      end
      object AssemblyDescriptionLabel: TLabel
        Left = 5
        Top = 32
        Width = 518
        Height = 42
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = 'AssemblyDescriptionLabel'
        ShowAccelChar = False
        WordWrap = True
      end
      object AssemblyLanguageCombo: TComboBox
        Left = 106
        Top = 5
        Width = 134
        Height = 23
        Style = csDropDownList
        TabOrder = 0
        OnChange = ControlChange
        Items.Strings = (
          'C#'
          'VB.NET'
          'PowerShell')
      end
    end
  end
  object ResultGroup: TGroupBox
    Left = 8
    Top = 110
    Width = 523
    Height = 232
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'ResultX'
    TabOrder = 1
    DesignSize = (
      523
      232)
    object ResultMemo: TMemo
      Left = 7
      Top = 15
      Width = 509
      Height = 208
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object CancelBtn: TButton
    Left = 365
    Top = 348
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 451
    Top = 348
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object ClipboardButton: TButton
    Left = 8
    Top = 348
    Width = 183
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Copy to Clipboard'
    TabOrder = 2
    OnClick = ClipboardButtonClick
  end
end
