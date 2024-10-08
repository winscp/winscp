object AboutDialog: TAboutDialog
  Left = 373
  Top = 123
  HelpType = htKeyword
  HelpKeyword = 'ui_about'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'About application'
  ClientHeight = 501
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  DesignSize = (
    410
    501)
  TextHeight = 13
  object OKButton: TButton
    Left = 243
    Top = 468
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnMouseDown = OKButtonMouseDown
  end
  object LicenseButton: TButton
    Left = 62
    Top = 468
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&License...'
    TabOrder = 2
    OnClick = LicenseButtonClick
  end
  object HelpButton: TButton
    Left = 325
    Top = 468
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 1
    OnClick = HelpButtonClick
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 410
    Height = 460
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 3
    DesignSize = (
      410
      460)
    object ApplicationLabel: TLabel
      Left = 62
      Top = 12
      Width = 52
      Height = 13
      Caption = 'Application'
      ShowAccelChar = False
    end
    object VersionLabel: TLabel
      Left = 62
      Top = 28
      Width = 127
      Height = 13
      Caption = 'Version 2.0.0 (Build 12) XX'
      ShowAccelChar = False
    end
    object WinSCPCopyrightLabel: TLabel
      Left = 62
      Top = 56
      Width = 180
      Height = 13
      Caption = 'Copyright '#169' 2000-2003 Martin Prikryl'
      ShowAccelChar = False
    end
    object ProductSpecificMessageLabel: TLabel
      Left = 62
      Top = 100
      Width = 277
      Height = 13
      Caption = 'To send comments and report bugs use support forum at:'
      ShowAccelChar = False
    end
    object Label3: TLabel
      Left = 62
      Top = 259
      Width = 91
      Height = 13
      Caption = 'Portions copyright:'
      ShowAccelChar = False
    end
    object RegistrationLabel: TLabel
      Left = 62
      Top = 144
      Width = 127
      Height = 13
      Caption = 'This product is licensed to:'
      ShowAccelChar = False
    end
    object IconPaintBox: TPaintBox
      Left = 8
      Top = 11
      Width = 48
      Height = 48
      OnPaint = IconPaintBoxPaint
    end
    object HomepageLabel: TStaticText
      Left = 62
      Top = 72
      Width = 128
      Height = 17
      Caption = 'http://XXXXXXwinscp.net/'
      TabOrder = 0
      TabStop = True
    end
    object ForumUrlLabel: TStaticText
      Left = 62
      Top = 116
      Width = 148
      Height = 17
      Caption = 'http://XXXXwinscp.net/forum/'
      TabOrder = 1
      TabStop = True
    end
    object ThirdPartyPanel: TPanel
      Left = 62
      Top = 277
      Width = 338
      Height = 168
      Anchors = [akLeft, akTop, akRight]
      BevelKind = bkTile
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
    end
    object RegistrationBox: TPanel
      Left = 62
      Top = 162
      Width = 338
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      BevelKind = bkTile
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 3
      DesignSize = (
        334
        85)
      object RegistrationSubjectLabel: TLabel
        Left = 8
        Top = 8
        Width = 241
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Someone'#13#10'Somewhere, some city'
        ShowAccelChar = False
        WordWrap = True
      end
      object RegistrationLicensesLabel: TLabel
        Left = 8
        Top = 43
        Width = 106
        Height = 13
        Caption = 'Number of Licenses: X'
        ShowAccelChar = False
      end
      object RegistrationProductIdLabel: TStaticText
        Left = 8
        Top = 65
        Width = 148
        Height = 17
        Caption = 'Product ID: xxxx-xxxx-xxxxx'
        ShowAccelChar = False
        TabOrder = 0
        OnClick = RegistrationProductIdLabelClick
      end
    end
  end
end
