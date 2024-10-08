object GenerateUrlDialog: TGenerateUrlDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_generateurl'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Generate URL X'
  ClientHeight = 338
  ClientWidth = 484
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 484
  ParentFont = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    484
    338)
  TextHeight = 13
  object OptionsPageControl: TPageControl
    Left = 5
    Top = 5
    Width = 475
    Height = 111
    ActivePage = UrlSheet
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ControlChange
    object UrlSheet: TTabSheet
      Caption = 'URL'
      object UserNameCheck: TCheckBox
        Tag = 1
        Left = 11
        Top = 8
        Width = 144
        Height = 17
        Caption = '&User name'
        TabOrder = 0
        OnClick = ControlChange
      end
      object HostKeyCheck: TCheckBox
        Tag = 4
        Left = 161
        Top = 8
        Width = 144
        Height = 17
        Caption = 'SSH &host Key'
        TabOrder = 3
        OnClick = ControlChange
      end
      object WinSCPSpecificCheck: TCheckBox
        Tag = 16
        Left = 161
        Top = 31
        Width = 144
        Height = 17
        Caption = '&WinSCP-specific'
        TabOrder = 4
        OnClick = ControlChange
      end
      object SaveExtensionCheck: TCheckBox
        Tag = 32
        Left = 161
        Top = 54
        Width = 144
        Height = 17
        Caption = '&Save extension'
        TabOrder = 5
        OnClick = ControlChange
      end
      object RemoteDirectoryCheck: TCheckBox
        Tag = 8
        Left = 11
        Top = 54
        Width = 144
        Height = 17
        Caption = 'Initial &directory'
        TabOrder = 2
        OnClick = ControlChange
      end
      object PasswordCheck: TCheckBox
        Tag = 2
        Left = 11
        Top = 31
        Width = 144
        Height = 17
        HelpType = htKeyword
        Caption = '&Password'
        TabOrder = 1
        OnClick = ControlChange
      end
      object RawSettingsCheck: TCheckBox
        Tag = 64
        Left = 311
        Top = 8
        Width = 144
        Height = 17
        Caption = '&Advanced settings'
        TabOrder = 6
        OnClick = ControlChange
      end
    end
    object ScriptSheet: TTabSheet
      Caption = 'Script'
      ImageIndex = 1
      object Label2: TLabel
        Left = 11
        Top = 8
        Width = 38
        Height = 13
        Caption = '&Format:'
        FocusControl = ScriptFormatCombo
      end
      object ScriptDescriptionLabel: TLabel
        Left = 11
        Top = 32
        Width = 446
        Height = 42
        AutoSize = False
        Caption = 'ScriptDescriptionLabel'
        ShowAccelChar = False
        WordWrap = True
      end
      object ScriptFormatCombo: TComboBox
        Left = 112
        Top = 5
        Width = 121
        Height = 21
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
      object Label1: TLabel
        Left = 11
        Top = 8
        Width = 51
        Height = 13
        Caption = '&Language:'
        FocusControl = AssemblyLanguageCombo
      end
      object AssemblyDescriptionLabel: TLabel
        Left = 11
        Top = 32
        Width = 446
        Height = 42
        AutoSize = False
        Caption = 'AssemblyDescriptionLabel'
        ShowAccelChar = False
        WordWrap = True
      end
      object AssemblyLanguageCombo: TComboBox
        Left = 112
        Top = 5
        Width = 121
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
  end
  object ResultGroup: TGroupBox
    Left = 8
    Top = 122
    Width = 468
    Height = 176
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'ResultX'
    TabOrder = 1
    DesignSize = (
      468
      176)
    object ResultMemo: TMemo
      Left = 7
      Top = 15
      Width = 454
      Height = 152
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object CancelBtn: TButton
    Left = 318
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 401
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object ClipboardButton: TButton
    Left = 8
    Top = 305
    Width = 165
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Copy to Clipboard'
    TabOrder = 2
    OnClick = ClipboardButtonClick
  end
end
