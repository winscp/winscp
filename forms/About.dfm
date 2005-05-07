object AboutDialog: TAboutDialog
  Left = 373
  Top = 184
  HelpType = htKeyword
  HelpKeyword = 'ui_about'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'About application'
  ClientHeight = 334
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  DesignSize = (
    388
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object ApplicationLabel: TLabel
    Left = 72
    Top = 8
    Width = 105
    Height = 24
    Caption = 'Application'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object VersionLabel: TLabel
    Left = 72
    Top = 40
    Width = 130
    Height = 13
    Caption = 'Version 2.0.0 (Build 12) &&&'
    ShowAccelChar = False
  end
  object WinSCPCopyrightLabel: TLabel
    Left = 72
    Top = 56
    Width = 173
    Height = 13
    Caption = 'Copyright '#169' 2000-2003 Martin Prikryl'
  end
  object HomepageLabel: TLabel
    Left = 72
    Top = 72
    Width = 129
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://XXXXXXwinscp.net/'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = HomepageLabelClick
  end
  object ProductSpecificMessageLabel: TLabel
    Left = 72
    Top = 97
    Width = 269
    Height = 13
    Caption = 'To send comments and report bugs use support forum at:'
  end
  object ForumUrlLabel: TLabel
    Left = 72
    Top = 112
    Width = 146
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://XXXXwinscp.net/forum/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = HomepageLabelClick
  end
  object TranslatorLabel: TLabel
    Left = 72
    Top = 137
    Width = 73
    Height = 13
    Caption = 'TranslatorLabel'
  end
  object ImagePanel: TPanel
    Left = 8
    Top = 8
    Width = 49
    Height = 318
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvNone
    Color = clTeal
    TabOrder = 3
    DesignSize = (
      49
      318)
    object ImageShape: TShape
      Left = 0
      Top = 0
      Width = 49
      Height = 318
      Align = alClient
      Brush.Style = bsClear
      Pen.Width = 0
    end
    object Image: TImage
      Left = 0
      Top = 0
      Width = 49
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      Center = True
      Picture.Data = {
        055449636F6E0000010002002020000100000000A80800002600000020201000
        00000000E8020000CE0800002800000020000000400000000100080000000000
        0004000000000000000000000001000000000000000000000000800000800000
        00808000800000008000800080800000C0C0C000C0DCC000F0CAA600F0FBFF00
        A4A0A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0060C0EE00582000000000000000000000000000000000000001000100
        0000000068E0EE00E0DFEE0001000000000000000000000078544C0060C0EE00
        2020000000000000A0D7EE0048E0EE00C848EC0000000000000000002000CC00
        00000000D01F4200A8DFEE00AC1F4200A8DFEE00000000000000000000000000
        0000000000000000000000000000000000000000A8DFEE002A07000056070000
        78544C0060C0EE00B81F000008E14100E0DFEE00F014EC0018E0EE0004000000
        78544C0060C0EE00981F00000000000000000000000000000000000000000000
        20000000200000001000000001000400D490E300280000002000000020000000
        0100040000000000000200000000000000000000100000000000000000000000
        00000000000000000000000000000000000000000000000078544C0060C0EE00
        201F000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000088E1EE00
        000000000000000000010000010000000000000038E1EE0038E1EE0098010000
        A86B430000E0EE00B80FEC000000000008000000600000000000000060E1EE00
        60E1EE007001000000000000000000000000000000000000D478430000E0EE00
        84E1EE0084E1EE004C01000000000000F0E1EE0018E2EE005CDFEE0000000000
        000000002000CC00000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000E0EE00
        0000000000000000ECE1EE00ECE1EE00E4000000E8E0410088E1EE00B80FEC00
        C0E1EE000800000060000000280000002300000064B841000000000008E14100
        88E1EE00F014EC00C0E1EE000400000034E2EE0034E2EE009C000000F017EC00
        6874000048E2EE0048E2EE008800000000000000000000000000000000000000
        D4784300649EEE006CE2EE006CE2EE00640000000000000000000000301BEC00
        0000000088E2EE0088E2EE0048000000E8E0410070A0EE00B80FEC00A8A0EE00
        0800000060000000280000002300000064B841000000000008E1410070A0EE00
        F014EC00A8A0EE00700C00001F00000090E746000000000000000000F0E2EE00
        0000000000000000ECE2EE00ECE2EE005C060000000000000000000018000000
        04E3EE0004E3EE00440600004C616200140000003F0100001873420000000000
        0000000000000000000000000000000000000000000000000800010000000000
        807B43001CE3EE000800000040000000200000004E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4EEE4E4E4E4E4E4E4E4E4E4E014E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E0101030303030303030303C40C4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E0103030303030303030303D001D04E4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E01C4C4C4C4C4C4C4C4C4C42503014E4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E01C490EBEB90FC262614D0ABC4010C4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E014E010101010101010101012503EE4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4ED001030101010101010101010103D0014E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E0C0103C4D0D0D0252525C403C40101014E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4EEE01010C0C0C0C0C0CC4ABC4C4034E634E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E0101EE0B0B0C0CEEEE23C4D0C4C4010C4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E01010B07070B0C0CEEEEC4D0C4C4010C4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E4E4E4EE5070B0CEEEEC4D0C4C4010C4E4E4E4E4E4E4E4E4E4EEEEEEE
        EEEEEEEEEEEE4E4E1D070CEEEEC4C4D0C4010C4E4E4E4E4E4E4E4E4E4EEE4E7C
        7C7C7C7C7CEE4E07070BEEEEEEC4C4D0C401EE4E4E4E0B63232323234EEE4E0F
        EEEEEE0F7CEE4E0101010101010303D0C401EE4E4E4EEE7B898989894EEE4E0F
        0FEE0F0F7CEE4EC4C4C4C4C4C4C4C425C401EE4E4E4EEE89898989894EEE4E0F
        EEEEEE0F7CEE4EBCFCEB90B782722D1DD001EE4E4E4EEE0B090909094EEE4E0F
        0FEE0F0F7CEE4ED0D0D0D0D0D0D0D0D01DC4EE4E4E4EEE63212121154EEE4E4E
        4E4E4E4E4EEE4ED0D0C4D0D0D0D0D0D0D025EE4E4E4E217B7B7B7B894EEEEEEE
        EEEEEEEEEEEE4E0101010101010101010101EE4E4E4E217B0B0B0B0B0B4EEE4E
        214E4E4EEE4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E0B15210B0C0CEE234EEE4E
        21EE4E4EEE4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E0C150B08070B0CEE4EEE4E
        7BEE4E4EEE4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E21150713E5070CEE0C4EEE
        4E4E4EEE4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E21210BE51D070CEE21894E
        EEEEEE4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E2189212121212121218909
        4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E21090909090909090909A5
        7B214E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E0C0C8F090909098F8F8FE2
        07154E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E0C0C0909090909090909
        E20C4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4EEEEEEEEEEEEEEEEE15
        21EE4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E
        4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4E4EFFFFFFFFFFFF000FFFFF0007
        FFFF0003FFFF0003FFFF0001FFFF0001FFFE0001FFFE0001FFFE0001FFFE0001
        FFFE0001FFFFE001FF803001FFA02001C0A02001C0A02001C0A02001C0A02001
        C0BFA001C0802001C0537FFF80537FFF80537FFF802EFFFF8011FFFF800FFFFF
        8003FFFF8003FFFFC003FFFFE003FFFFFFFFFFFF280000002000000040000000
        0100040000000000000200000000000000000000100000000000000000000000
        00008000008000000080800080000000800080008080000080808000C0C0C000
        0000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000
        0000000000000000000000000000000000000000711111111111000000000000
        0000000011111711171870000000000000000000177777777778180000000000
        000000001777777777787100000000000000000018BBBBBBBB88717000000000
        0000000011111111111187100000000000000007111111111111181000000000
        0000000717888888887811100000000000000001117777777888111000000000
        0000000117777774188871700000000000000001178887771888717000000000
        0000000000088777488871700000000004747474770888777878717000000000
        070BBBBBB70887771778717000711111070B477BB40111111178717000777777
        040BB7BBB70888888888717000777877070B777BB70BBBBBB888811000778888
        070BB7BBB7088888888888100071444407000000070888888888887000477777
        0777777774011111111111700047788886704100700000000000000007477777
        4640740040000000000000000748887746707400700000000000000007488887
        4767000700000000000000000778888777767470000000000000000004777774
        7778000000000000000000000488888888887400000000000000000007788888
        8888840000000000000000000077888888888700000000000000000000074774
        44444700000000000000000000000000000000000000000000000000FFFFFFFF
        FFFF000FFFFF0007FFFF0003FFFF0003FFFF0001FFFF0001FFFE0001FFFE0001
        FFFE0001FFFE0001FFFE0001FFFFE001FF802001FFA02001C0A02001C0A02001
        C0A02001C0A02001C0BFA001C0802001C0137FFF80137FFF80137FFF800EFFFF
        8001FFFF800FFFFF8003FFFF8003FFFFC003FFFFE003FFFFFFFFFFFF}
      Transparent = True
    end
  end
  object ThirdPartyBox: TScrollBox
    Left = 72
    Top = 155
    Width = 306
    Height = 141
    HorzScrollBar.Visible = False
    VertScrollBar.Smooth = True
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
    DesignSize = (
      285
      137)
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 87
      Height = 13
      Caption = 'Portions copyright:'
    end
    object Label7: TLabel
      Left = 8
      Top = 32
      Width = 272
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'License agreements of all following programs (libraries) are par' +
        't of application license agreement.'
      WordWrap = True
    end
    object PuttyVersionLabel: TLabel
      Left = 8
      Top = 72
      Width = 196
      Height = 13
      Caption = 'SSH and SCP code based on PuTTY xxx'
    end
    object PuttyCopyrightLabel: TLabel
      Left = 8
      Top = 88
      Width = 145
      Height = 13
      Caption = 'Copyright '#169' xxx Simon Tatham'
    end
    object PuttyHomepageLabel: TLabel
      Left = 8
      Top = 120
      Width = 277
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://XXXwww.chiark.greenend.org.uk/~sgtatham/putty/'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      OnClick = HomepageLabelClick
    end
    object Label8: TLabel
      Left = 8
      Top = 264
      Width = 181
      Height = 13
      Caption = 'Filemanager Toolset library Version 2.6'
    end
    object Label10: TLabel
      Left = 8
      Top = 280
      Width = 137
      Height = 13
      Caption = 'Copyright '#169' 1999 Ingo Eckel'
    end
    object ProlongBoxLabel: TLabel
      Left = 8
      Top = 296
      Width = 63
      Height = 13
      Caption = '                     '
      Transparent = True
    end
    object PuttyLicenceLabel: TLabel
      Tag = 1
      Left = 8
      Top = 104
      Width = 71
      Height = 13
      Cursor = crHandPoint
      Caption = 'Display licence'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      OnClick = DisplayLicence
    end
    object Label1: TLabel
      Left = 8
      Top = 152
      Width = 117
      Height = 13
      Caption = 'Toolbar2000 library 2.1.5'
    end
    object Label2: TLabel
      Left = 8
      Top = 168
      Width = 182
      Height = 13
      Caption = 'Copyright '#169' 1998-2005 Jordan Russell'
    end
    object Toolbar2000HomepageLabel: TLabel
      Left = 8
      Top = 184
      Width = 176
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://www.jrsoftware.org/tb2kdl.php'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      OnClick = HomepageLabelClick
    end
    object Label5: TLabel
      Left = 8
      Top = 208
      Width = 69
      Height = 13
      Caption = 'TBX library 2.1'
    end
    object Label6: TLabel
      Left = 8
      Top = 224
      Width = 188
      Height = 13
      Caption = 'Copyright '#169' 2001-2005 Alex A. Denisov'
    end
    object TBXHomepageLabel: TLabel
      Left = 8
      Top = 240
      Width = 118
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://www.g32.org/tbx/'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentColor = False
      ParentFont = False
      OnClick = HomepageLabelClick
    end
  end
  object OKButton: TButton
    Left = 221
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object LicenceButton: TButton
    Left = 72
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Licence...'
    TabOrder = 2
    OnClick = LicenceButtonClick
  end
  object HelpButton: TButton
    Left = 303
    Top = 303
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 1
    OnClick = HelpButtonClick
  end
end
