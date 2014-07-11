object GenerateUrlDialog: TGenerateUrlDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_generateurl'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Generate URL'
  ClientHeight = 251
  ClientWidth = 484
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    484
    251)
  PixelsPerInch = 96
  TextHeight = 13
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 468
    Height = 95
    Anchors = [akLeft, akTop, akRight]
    Caption = 'URL options'
    TabOrder = 0
    object UserNameCheck: TCheckBox
      Tag = 1
      Left = 11
      Top = 20
      Width = 97
      Height = 17
      Caption = '&User name'
      TabOrder = 0
      OnClick = ControlChange
    end
    object PasswordCheck: TCheckBox
      Tag = 2
      Left = 235
      Top = 20
      Width = 97
      Height = 17
      HelpType = htKeyword
      HelpKeyword = 'ui_generate_url'
      Caption = '&Password'
      TabOrder = 1
      OnClick = ControlChange
    end
    object HostKeyCheck: TCheckBox
      Tag = 4
      Left = 11
      Top = 43
      Width = 97
      Height = 17
      Caption = 'SSH &host Key'
      TabOrder = 2
      OnClick = ControlChange
    end
    object RemoteDirectoryCheck: TCheckBox
      Tag = 8
      Left = 235
      Top = 43
      Width = 97
      Height = 17
      Caption = 'Initial &directory'
      TabOrder = 3
      OnClick = ControlChange
    end
    object WinSCPSpecificCheck: TCheckBox
      Tag = 16
      Left = 11
      Top = 66
      Width = 97
      Height = 17
      Caption = '&WinSCP-specific'
      TabOrder = 4
      OnClick = ControlChange
    end
    object SaveExtensionCheck: TCheckBox
      Tag = 32
      Left = 235
      Top = 66
      Width = 97
      Height = 17
      Caption = '&Save extension'
      TabOrder = 5
      OnClick = ControlChange
    end
  end
  object UrlGroup: TGroupBox
    Left = 8
    Top = 107
    Width = 468
    Height = 105
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'URL'
    TabOrder = 1
    DesignSize = (
      468
      105)
    object UrlMemo: TMemo
      Left = 7
      Top = 15
      Width = 454
      Height = 81
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Lines.Strings = (
        'UrlMemo')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object CancelBtn: TButton
    Left = 318
    Top = 218
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
    Top = 218
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object ClipboardButton: TButton
    Left = 8
    Top = 218
    Width = 145
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Copy to Clipboard'
    TabOrder = 2
    OnClick = ClipboardButtonClick
  end
end
