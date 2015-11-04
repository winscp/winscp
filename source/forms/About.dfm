object AboutDialog: TAboutDialog
  Left = 373
  Top = 123
  HelpType = htKeyword
  HelpKeyword = 'ui_about'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'About application'
  ClientHeight = 487
  ClientWidth = 394
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    394
    487)
  PixelsPerInch = 96
  TextHeight = 13
  object ApplicationLabel: TLabel
    Left = 46
    Top = 12
    Width = 52
    Height = 13
    Caption = 'Application'
  end
  object VersionLabel: TLabel
    Left = 46
    Top = 28
    Width = 127
    Height = 13
    Caption = 'Version 2.0.0 (Build 12) XX'
  end
  object WinSCPCopyrightLabel: TLabel
    Left = 46
    Top = 56
    Width = 180
    Height = 13
    Caption = 'Copyright '#169' 2000-2003 Martin Prikryl'
  end
  object ProductSpecificMessageLabel: TLabel
    Left = 46
    Top = 100
    Width = 277
    Height = 13
    Caption = 'To send comments and report bugs use support forum at:'
  end
  object Label3: TLabel
    Left = 46
    Top = 259
    Width = 91
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Portions copyright:'
  end
  object RegistrationLabel: TLabel
    Left = 46
    Top = 144
    Width = 127
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'This product is licensed to:'
  end
  object IconPaintBox: TPaintBox
    Left = 8
    Top = 11
    Width = 32
    Height = 32
    OnPaint = IconPaintBoxPaint
  end
  object HomepageLabel: TStaticText
    Left = 46
    Top = 72
    Width = 128
    Height = 17
    Caption = 'http://XXXXXXwinscp.net/'
    TabOrder = 2
    TabStop = True
  end
  object ForumUrlLabel: TStaticText
    Left = 46
    Top = 116
    Width = 148
    Height = 17
    Caption = 'http://XXXXwinscp.net/forum/'
    TabOrder = 3
    TabStop = True
  end
  object ThirdPartyPanel: TPanel
    Left = 46
    Top = 277
    Width = 338
    Height = 168
    Anchors = [akLeft, akRight, akBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 6
  end
  object OKButton: TButton
    Left = 227
    Top = 454
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
    Left = 46
    Top = 454
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&License...'
    TabOrder = 5
    OnClick = LicenseButtonClick
  end
  object HelpButton: TButton
    Left = 309
    Top = 454
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 1
    OnClick = HelpButtonClick
  end
  object RegistrationBox: TPanel
    Left = 46
    Top = 162
    Width = 338
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      334
      85)
    object RegistrationSubjectLabel: TLabel
      Left = 8
      Top = 8
      Width = 249
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Someone'#13#10'Somewhere, some city'
      WordWrap = True
    end
    object RegistrationLicensesLabel: TLabel
      Left = 8
      Top = 43
      Width = 106
      Height = 13
      Caption = 'Number of Licenses: X'
    end
    object RegistrationProductIdLabel: TStaticText
      Left = 8
      Top = 65
      Width = 148
      Height = 17
      Caption = 'Product ID: xxxx-xxxx-xxxxx'
      TabOrder = 0
      OnClick = RegistrationProductIdLabelClick
    end
  end
end
