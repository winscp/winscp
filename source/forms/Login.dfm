object LoginDialog: TLoginDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login'
  BorderIcons = [biSystemMenu, biMinimize, biHelp]
  Caption = 'Login'
  ClientHeight = 411
  ClientWidth = 873
  Color = clBtnFace
  Constraints.MinHeight = 399
  Constraints.MinWidth = 600
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 512
    Top = 0
    Width = 361
    Height = 387
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object ContentsPanel: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 346
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      DesignSize = (
        361
        346)
      object ContentsGroupBox: TGroupBox
        Left = 2
        Top = 12
        Width = 347
        Height = 331
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'ContentsGroupBox'
        TabOrder = 0
        DesignSize = (
          347
          331)
        object ContentsLabel: TLabel
          Left = 12
          Top = 20
          Width = 31
          Height = 13
          Caption = 'Name:'
          ShowAccelChar = False
        end
        object ContentsNameEdit: TEdit
          Left = 66
          Top = 15
          Width = 270
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'ContentsNameEdit'
        end
        object ContentsMemo: TMemo
          Left = 12
          Top = 42
          Width = 324
          Height = 277
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'ContentsMemo')
          TabOrder = 1
        end
      end
    end
    object SitePanel: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 346
      Align = alClient
      Anchors = [akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        361
        346)
      object BasicGroup: TGroupBox
        Left = 2
        Top = 12
        Width = 347
        Height = 255
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Session'
        TabOrder = 0
        DesignSize = (
          347
          255)
        object Label1: TLabel
          Left = 12
          Top = 72
          Width = 55
          Height = 13
          Caption = '&Host name:'
          FocusControl = HostNameEdit
        end
        object Label2: TLabel
          Left = 255
          Top = 72
          Width = 63
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Po&rt number:'
          FocusControl = PortNumberEdit
        end
        object UserNameLabel: TLabel
          Left = 12
          Top = 122
          Width = 55
          Height = 13
          Caption = '&User name:'
          FocusControl = UserNameEdit
        end
        object PasswordLabel: TLabel
          Left = 178
          Top = 122
          Width = 50
          Height = 13
          Caption = '&Password:'
          FocusControl = PasswordEdit
        end
        object Label22: TLabel
          Left = 12
          Top = 22
          Width = 62
          Height = 13
          Caption = '&File protocol:'
          FocusControl = TransferProtocolCombo
        end
        object FtpsLabel: TLabel
          Left = 163
          Top = 22
          Width = 55
          Height = 13
          Caption = '&Encryption:'
          FocusControl = FtpsCombo
        end
        object WebDavsLabel: TLabel
          Left = 163
          Top = 22
          Width = 55
          Height = 13
          Caption = '&Encryption:'
          FocusControl = WebDavsCombo
        end
        object BasicS3Panel: TPanel
          Left = 12
          Top = 195
          Width = 324
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 15
          DesignSize = (
            324
            26)
          object S3CredentialsEnvCheck: TCheckBox
            Left = 0
            Top = 0
            Width = 324
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Read credentials from AWS CLI configuration'
            TabOrder = 0
            OnClick = S3CredentialsEnvCheckClick
          end
        end
        object EncryptionView: TEdit
          Left = 163
          Top = 39
          Width = 173
          Height = 21
          TabOrder = 4
          OnChange = TransferProtocolComboChange
        end
        object TransferProtocolView: TEdit
          Left = 12
          Top = 39
          Width = 145
          Height = 21
          TabOrder = 1
          OnChange = TransferProtocolComboChange
        end
        object HostNameEdit: TEdit
          Left = 12
          Top = 89
          Width = 236
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 255
          TabOrder = 5
          Text = 'HostNameEdit'
          OnChange = DataChange
          OnExit = HostNameEditExit
        end
        object UserNameEdit: TEdit
          Left = 12
          Top = 139
          Width = 159
          Height = 21
          MaxLength = 128
          TabOrder = 7
          Text = 'UserNameEdit'
          OnChange = DataChange
        end
        object PasswordEdit: TPasswordEdit
          Left = 177
          Top = 139
          Width = 159
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 100
          TabOrder = 8
          Text = 'PasswordEdit'
          OnChange = DataChange
        end
        object PortNumberEdit: TUpDownEdit
          Left = 254
          Top = 89
          Width = 82
          Height = 21
          Alignment = taRightJustify
          MaxValue = 65535.000000000000000000
          MinValue = 1.000000000000000000
          Anchors = [akTop, akRight]
          TabOrder = 6
          OnChange = PortNumberEditChange
        end
        object TransferProtocolCombo: TComboBox
          Left = 12
          Top = 39
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = TransferProtocolComboChange
          Items.Strings = (
            'SFTP'
            'SCP'
            'FTP'
            'WebDAV'
            'Amazon S3')
        end
        object FtpsCombo: TComboBox
          Left = 163
          Top = 39
          Width = 173
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = EncryptionComboChange
          Items.Strings = (
            'No encryption'
            'TLS/SSL Implicit encryptionX'
            'TLS/SSL Explicit encryptionX')
        end
        object WebDavsCombo: TComboBox
          Left = 163
          Top = 39
          Width = 173
          Height = 21
          Style = csDropDownList
          TabOrder = 3
          OnChange = EncryptionComboChange
          Items.Strings = (
            'No encryptionX'
            'TLS/SSL Implicit encryptionX')
        end
        object BasicFtpPanel: TPanel
          Left = 12
          Top = 169
          Width = 324
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 9
          object AnonymousLoginCheck: TCheckBox
            Left = 0
            Top = 0
            Width = 170
            Height = 17
            Caption = 'A&nonymous login'
            TabOrder = 0
            OnClick = AnonymousLoginCheckClick
          end
        end
        object BasicSshPanel: TPanel
          Left = 12
          Top = 198
          Width = 347
          Height = 0
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 10
        end
        object AdvancedButton: TButton
          Left = 238
          Top = 219
          Width = 98
          Height = 25
          Action = SessionAdvancedAction
          Anchors = [akRight, akBottom]
          Style = bsSplitButton
          TabOrder = 14
          OnDropDownClick = AdvancedButtonDropDownClick
        end
        object SaveButton: TButton
          Left = 12
          Top = 219
          Width = 98
          Height = 25
          Action = SaveSessionAction
          Anchors = [akLeft, akBottom]
          Style = bsSplitButton
          TabOrder = 11
          OnDropDownClick = SaveButtonDropDownClick
        end
        object EditCancelButton: TButton
          Left = 116
          Top = 219
          Width = 82
          Height = 25
          Action = EditCancelAction
          Anchors = [akLeft, akBottom]
          TabOrder = 13
          OnDropDownClick = SaveButtonDropDownClick
        end
        object EditButton: TButton
          Left = 12
          Top = 219
          Width = 98
          Height = 25
          Action = EditSessionAction
          Anchors = [akLeft, akBottom]
          TabOrder = 12
          OnDropDownClick = SaveButtonDropDownClick
        end
      end
      object NoteGroup: TGroupBox
        Left = 2
        Top = 273
        Width = 347
        Height = 70
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Note'
        TabOrder = 1
        DesignSize = (
          347
          70)
        object NoteMemo: TMemo
          Left = 7
          Top = 15
          Width = 333
          Height = 46
          TabStop = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Lines.Strings = (
            'NoteMemo')
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
    object ButtonPanel: TPanel
      Left = 0
      Top = 346
      Width = 361
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      OnMouseDown = PanelMouseDown
      DesignSize = (
        361
        41)
      object LoginButton: TButton
        Left = 75
        Top = 10
        Width = 98
        Height = 25
        Action = LoginAction
        Anchors = [akRight, akBottom]
        Default = True
        Images = ActionImageList
        ModalResult = 1
        Style = bsSplitButton
        TabOrder = 0
        OnDropDownClick = LoginButtonDropDownClick
      end
      object CloseButton: TButton
        Left = 179
        Top = 10
        Width = 82
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Close'
        ModalResult = 2
        TabOrder = 1
      end
      object HelpButton: TButton
        Left = 267
        Top = 10
        Width = 82
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Help'
        TabOrder = 2
        OnClick = HelpButtonClick
      end
    end
  end
  object SitesPanel: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 387
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      512
      387)
    object SessionTree: TTreeView
      Left = 11
      Top = 12
      Width = 490
      Height = 338
      Anchors = [akLeft, akTop, akRight, akBottom]
      DoubleBuffered = True
      DragMode = dmAutomatic
      HideSelection = False
      Images = SessionImageList
      Indent = 19
      ParentDoubleBuffered = False
      ParentShowHint = False
      RowSelect = True
      ShowHint = True
      ShowRoot = False
      SortType = stBoth
      TabOrder = 0
      OnChange = SessionTreeChange
      OnChanging = SessionTreeChanging
      OnCollapsed = SessionTreeExpandedCollapsed
      OnCompare = SessionTreeCompare
      OnContextPopup = SessionTreeContextPopup
      OnCustomDrawItem = SessionTreeCustomDrawItem
      OnDblClick = SessionTreeDblClick
      OnDragDrop = SessionTreeDragDrop
      OnEdited = SessionTreeEdited
      OnEditing = SessionTreeEditing
      OnEndDrag = SessionTreeEndDrag
      OnExit = SessionTreeExit
      OnExpanding = SessionTreeExpanding
      OnExpanded = SessionTreeExpandedCollapsed
      OnKeyDown = SessionTreeKeyDown
      OnKeyPress = SessionTreeKeyPress
      OnMouseDown = SessionTreeMouseDown
      OnMouseMove = SessionTreeMouseMove
      OnStartDrag = SessionTreeStartDrag
    end
    object SitesIncrementalSearchLabel: TStaticText
      Left = 14
      Top = 330
      Width = 142
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      BorderStyle = sbsSingle
      Caption = 'SitesIncrementalSearchLabel'
      ShowAccelChar = False
      TabOrder = 1
      Visible = False
    end
    object ManageButton: TButton
      Left = 403
      Top = 356
      Width = 98
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Manage'
      TabOrder = 3
      OnClick = ManageButtonClick
    end
    object ToolsMenuButton: TButton
      Left = 11
      Top = 356
      Width = 98
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Tools'
      TabOrder = 2
      OnClick = ToolsMenuButtonClick
    end
  end
  object ShowAgainPanel: TPanel
    Left = 0
    Top = 387
    Width = 873
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnMouseDown = PanelMouseDown
    DesignSize = (
      873
      24)
    object ShowAgainCheck: TCheckBox
      Left = 12
      Top = 0
      Width = 849
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        '&Show Login dialog on startup and when the last session is close' +
        'd'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Images = ActionImageList
    OnUpdate = ActionListUpdate
    Left = 44
    Top = 21
    object EditSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Edit'
      OnExecute = EditSessionActionExecute
    end
    object SaveAsSessionAction: TAction
      Category = 'Sessions'
      Caption = 'Save &As...'
      ShortCut = 32833
      OnExecute = SaveAsSessionActionExecute
    end
    object SaveSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Save'
      OnExecute = SaveSessionActionExecute
    end
    object DeleteSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Delete'
      ImageIndex = 3
      OnExecute = DeleteSessionActionExecute
    end
    object ImportSessionsAction: TAction
      Category = 'Sessions'
      Caption = '&Import Sites...'
      OnExecute = ImportSessionsActionExecute
    end
    object LoginAction: TAction
      Category = 'Session'
      Caption = 'LoginX'
      ImageIndex = 0
      OnExecute = LoginActionExecute
    end
    object AboutAction: TAction
      Category = 'Other'
      Caption = 'A&bout...'
      OnExecute = AboutActionExecute
    end
    object CleanUpAction: TAction
      Category = 'Other'
      Caption = '&Clean Up...'
      OnExecute = CleanUpActionExecute
    end
    object ResetNewSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Reset'
      OnExecute = ResetNewSessionActionExecute
    end
    object SetDefaultSessionAction: TAction
      Category = 'Sessions'
      Caption = 'Set De&faults'
      OnExecute = SetDefaultSessionActionExecute
    end
    object DesktopIconAction: TAction
      Category = 'Sessions'
      Caption = 'Desktop &Icon'
      OnExecute = DesktopIconActionExecute
    end
    object SendToHookAction: TAction
      Category = 'Sessions'
      Caption = 'Explorer'#39's '#39'Send To'#39' Shortcut'
      OnExecute = SendToHookActionExecute
    end
    object CheckForUpdatesAction: TAction
      Tag = 15
      Category = 'Other'
      Caption = 'Check for &Updates'
      ImageIndex = 63
      OnExecute = CheckForUpdatesActionExecute
    end
    object RenameSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Rename'
      ImageIndex = 2
      OnExecute = RenameSessionActionExecute
    end
    object NewSessionFolderAction: TAction
      Category = 'Sessions'
      Caption = 'Ne&w Folder...'
      ImageIndex = 4
      OnExecute = NewSessionFolderActionExecute
    end
    object RunPageantAction: TAction
      Category = 'Other'
      Caption = 'Run &Pageant'
      OnExecute = RunPageantActionExecute
    end
    object RunPuttygenAction: TAction
      Category = 'Other'
      Caption = 'Run PuTTY&gen'
      OnExecute = RunPuttygenActionExecute
    end
    object ImportAction: TAction
      Category = 'Other'
      Caption = 'Import/Restore &Configuration...'
      OnExecute = ImportActionExecute
    end
    object ExportAction: TAction
      Category = 'Other'
      Caption = '&Export/Backup Configuration...'
      OnExecute = ExportActionExecute
    end
    object PreferencesAction: TAction
      Category = 'Other'
      Caption = '&Preferences...'
      OnExecute = PreferencesActionExecute
    end
    object EditCancelAction: TAction
      Category = 'Session'
      Caption = 'Cancel'
      OnExecute = EditCancelActionExecute
    end
    object SessionAdvancedAction: TAction
      Category = 'Session'
      Caption = 'A&dvanced...'
      OnExecute = SessionAdvancedActionExecute
    end
    object PreferencesLoggingAction: TAction
      Category = 'Other'
      Caption = '&Logging...'
      OnExecute = PreferencesLoggingActionExecute
    end
    object CloneToNewSiteAction: TAction
      Category = 'Session'
      Caption = '&Clone to New Site'
      OnExecute = CloneToNewSiteActionExecute
    end
    object PuttyAction: TAction
      Category = 'Session'
      Caption = 'Open in &PuTTY'
      ImageIndex = 1
      SecondaryShortCuts.Strings = (
        'Shift+Ctrl+P')
      ShortCut = 16464
      OnExecute = PuttyActionExecute
    end
    object PasteUrlAction: TAction
      Category = 'Sessions'
      Caption = 'Paste Session &URL'
      ShortCut = 16470
      OnExecute = PasteUrlActionExecute
    end
    object GenerateUrlAction2: TAction
      Category = 'Sessions'
      Caption = '&Generate Session URL/Code...'
      OnExecute = GenerateUrlAction2Execute
    end
    object CopyParamRuleAction: TAction
      Category = 'Sessions'
      Caption = 'Transfer Settings &Rule...'
      OnExecute = CopyParamRuleActionExecute
    end
    object SearchSiteNameStartOnlyAction: TAction
      Category = 'Other'
      Caption = '&Beginning of Site Name Only'
      OnExecute = SearchSiteNameStartOnlyActionExecute
    end
    object SearchSiteNameAction: TAction
      Category = 'Other'
      Caption = '&Any Part of Site Name'
      OnExecute = SearchSiteNameActionExecute
    end
    object SearchSiteAction: TAction
      Category = 'Other'
      Caption = 'All &Major Site Fields'
      OnExecute = SearchSiteActionExecute
    end
    object SessionRawAction: TAction
      Category = 'Session'
      Caption = 'Edit &Raw Settings...'
      OnExecute = SessionAdvancedActionExecute
    end
  end
  object ToolsPopupMenu: TPopupMenu
    Left = 152
    Top = 77
    object Import1: TMenuItem
      Action = ImportSessionsAction
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ImportConfiguration1: TMenuItem
      Action = ImportAction
    end
    object ExportConfiguration1: TMenuItem
      Action = ExportAction
    end
    object Cleanup1: TMenuItem
      Action = CleanUpAction
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Pageant1: TMenuItem
      Action = RunPageantAction
    end
    object Puttygen1: TMenuItem
      Action = RunPuttygenAction
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CheckForUpdates1: TMenuItem
      Action = CheckForUpdatesAction
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Preferences1: TMenuItem
      Action = PreferencesAction
    end
    object About1: TMenuItem
      Action = AboutAction
    end
  end
  object SessionImageList: TPngImageList
    PngImages = <
      item
        Background = clWhite
        Name = 'Unused'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF000000FF89C02F90000000174944415478DA63FCCF
          F09F8114C038AA6154C3F0D50000BC451FF12F5559220000000049454E44AE42
          6082}
      end
      item
        Background = clWindow
        Name = 'Site'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30342B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30342B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39303535313231362D616238322D326334362D62
          3963382D6238646461636330346261342220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A633130636535
          66332D343432362D363934302D393333612D3964326664366263646132642220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A34343432643861302D333364622D326134372D393033312D353566666330
          663533663634223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A34343432
          643861302D333364622D326134372D393033312D353566666330663533663634
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39303535313231362D
          616238322D326334362D623963382D6238646461636330346261342220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30342B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E721A5462000000F64944415438CBA5934B0E823010865BC209
          C4103D8C7B12DD7030124D483071E1C203E8C20507F1106AC403D0E9D4B614CA
          A310139AFC1D8699F93A902915429039CB575BB24F5F9CF3101189B48D3ABE7C
          461B7BA68764DD000020BC4531E1256AA1B16DB5DF6D1F9755DD8167005512AB
          C48D2A5F6871639554FE0080D04DB2455DB0B26E402F69E81B28886940BFDDDA
          6F40F253196343802D7277826035DA01EF1556BE0197B20BA3118070FE710D82
          F10EF41C30069F4D9E2D796B70D00C8F7BB0F0DD10A646F9783A0B25574CD529
          51BD51DA095EEFB92EFA1685F61741A06DBC8B68FF60DF797296FD7D99E8DCDB
          E89199EB07484FA5060B7430FF0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30382B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30382B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A35663032383561322D316132302D323334622D39
          6134332D3864613739653739636230372220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A396134636665
          39332D353035322D393234362D626635332D6664393133393763383665312220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A65643030343635612D306431302D346234652D613034622D643535353137
          346263323835223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A65643030
          343635612D306431302D346234652D613034622D643535353137346263323835
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A35663032383561322D
          316132302D323334622D396134332D3864613739653739636230372220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30382B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EFA5AFC8E000001774944415438CBA553C94A0341107D9D7422
          669710841C72103CE5A0070FEAC983F805A2DFE0C96FF1E2AF088A200425E021
          113C89B81D348B5BD6D9BB9C697B42C68C104841772D5D53F55EF7142322CC22
          11CC28DC3A2FDE8258D90F30904D8C0EE3DB8DE3690A30F3AC38C181013A0967
          CBD4E929ECA367A3F555DE83395180D416D09E44E795EF6D727FA4CCFAC6B5F6
          F6C1F4D3A28C201273571CFE9D329E005FDC4134BFE93AB150F864343FB9106E
          838555C4970E7EC18F4E6DC06C039D1A843D00840E38BAD4A434E2F91A77DC02
          DD1E43F7F208CB6BBB6E92090A4B1EF77DBB7F7F12F110B49B2F289456E40139
          43576B7291D2BE3DF285EF0F2A1281D6FF46329553097E2763D4390C1113BA79
          D7B8A9738B25BCB700E751557D1CA611A0102824B4BAF794DC12492433590535
          AC9BA1A8FD2DA455E49FA8990CE96C41F1FCA7DB389D51CCAACA5918BAFC53E9
          DCC48505ED61304E0E6C665F4904A6DEEBB41E2EB2BDB9E8D403C4C0AAA5FDF7
          5769CF3ACE3F8E1C4FB1FA2B63420000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A31372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30323A30362B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30323A30362B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A37326638363636642D326565352D333434622D61
          6131642D6131633665613863643332622220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A636261383932
          33352D373931382D636234642D396235632D6361336530343335636632612220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A37626231396662642D666237622D356634372D383762382D373365613533
          303365366534223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A37626231
          396662642D666237622D356634372D383762382D373365613533303365366534
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A31
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A37326638363636642D
          326565352D333434622D616131642D6131633665613863643332622220737445
          76743A7768656E3D22323032322D30392D30315431313A30323A30362B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EE2CF74B0000001574944415438CBA593CB4AC3401486BF4942
          ACB5A02805EF6BC15770EDCBE843B871E35ED057A94FD08D886E546817A57843
          916ADBDCCF71D124366D944207C29CCCF9BF7FE6CCC5A82AF334C76BACED5854
          F6B2015BE2FE69F3AD797282FC0766139BE86AF34361B598E5DC3D7C3A9EC5C0
          9982010C476163E34ED4B44B693B69016D002788CA67B017D72FECEA1620A002
          AA69ACC4DECB00A8013803B671AB75344D5A964DB5BE8F5BDB8564386620A926
          C1B51696F2C5C6AD335589712A2BA93841C34FF03A48EC81F890F8203E9A8C62
          63C03EB83600D6F3CD254606A8D745FA6DE4FB011D764662092009400234ED91
          00897A79A996C43E96E15750028CE734D36406402EFC13C8C673588B06B9A80C
          98349DB85FA315649B34B961E9BF4A1A6B3275DC7909859AA5A4E61218C001F0
          FBAF20214888A67DFE69041217A05860393350D5FBF7EEED1798DEAC2FD0C063
          6660E67DCE3FCC57282A86352ACF0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31302B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31302B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A31663161303237332D353761622D666534312D61
          6532382D3837306565616630356230302220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A383164356232
          64652D353262302D303534342D393834312D6334393336663161613532312220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A38373863313639612D393434342D313034372D626666342D393037663831
          313835353631223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A38373863
          313639612D393434342D313034372D626666342D393037663831313835353631
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A31663161303237332D
          353761622D666534312D616532382D3837306565616630356230302220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31302B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E58A253E8000000D24944415438CB63FCFFFF3F032580C5E0E0
          6D8A4C60F9FEF617652E28787392C1DBDB8B2CCD5BB76E63600131E4A425B02A
          983E6B1E43665A125E439840C4FF5F3F197E3DB88E55C1EF7F3F199E7EB989DF
          803F2F1E303C6F8A86DB0AC230F0FAFB2386891793B0CA81C1B49973FFFFFAFB
          E3FF93CF37FEA30390DC7FA0DCFF2FB7FE630320792682B6FC78CCF0E74A167E
          17E0B20524F7F73750EAD53FFC2EC067CBF70FFF192EADFE4DC00538003E3998
          3C2388909496262B213D7FFA1492908C0CF4C94B8930034049925CC048697606
          006C5C0EF4977CD7B60000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF000000FF89C02F90000000174944415478DA63FCCF
          F09F8114C038AA6154C3F0D50000BC451FF12F5559220000000049454E44AE42
          6082}
      end
      item
        Background = clWindow
        Name = 'Open new session'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30372B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30372B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A63313532663730322D626162612D636334632D61
          3662382D3233656531613363363061302220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A333836373636
          65632D333261642D393934382D396533342D3736626465623966646462322220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A37313634353830652D326261382D333134362D386638342D663134306134
          636337363934223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A37313634
          353830652D326261382D333134362D386638342D663134306134636337363934
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A63313532663730322D
          626162612D636334632D613662382D3233656531613363363061302220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30372B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EF8C6EB10000001EC4944415438CBA5534D48545114FEEE7D6F
          CCC9DF1C1D1B349A18DA840B050B5ACCA268318B842CA8BD18D2A66D5B372ED3
          218A4926D0952B410D05218C20C24D9B5AA92DA24562CE3C9971327DEFDD9FEE
          7DCF37BEF027C103E77D9CCB3DDF39E77BE71229250821F89F8DE7279F128A67
          EBCCBE3A3C34F42738279AE079F6E52FCE795C080185550F627D27994CA2A1A1
          1EABAB6BB02C6BE3D58BD1842630F58731169FCDF4833BC273B18F357B0E5C57
          A2F7F73AD29D75487526B0C16A50287EB898CDE6DB4B51510908FC44D777AEFC
          4AB9808CB5062312015577D2D7D3A06AD44667193DDDDDBAAB9FE7A42CD18040
          30A912A522F0FD87D188EDF317D0D2DC8CFBF7FA10514484523C7AF800776EDF
          826150A6041839200855D7B8CB29DE18292C3B75987BBBE069C1B4368CE3DDD2
          7B351A7FFC647060EC1081AE1E74A271D3A1688BB779C98CF924A6694032F625
          2C62A8FD7F3BB9666E29F513985BFA848A55C0E54B1D88D64661DB76974AFD6A
          863BE0A1C42036A48DCF2BDFF1B1D284959D0EDC2C9770A3BE5CDD8F10C18180
          61314777523EE9FEAF9DDE8EA1F86D0AF3B3D3535502D765C5F462AE95871649
          683C66B15C2E36AB2DE82D3BCE5EE727A4F693569C1CF51666E617BDA42DCBF2
          E29658CCC3FEBB99438FC63CB2722E87D31A396984D318C519ED2F7B9D72EEE9
          5D1CFE0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          36000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA864000000174944415478DA63FCFFFF3F0329807154C3A886E1AB01008BE3
          2FE1C7ACA4590000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site color mask'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A30613764626466392D363338632D326134632D38
          3731632D6266363033323033323163322220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A383162323039
          65342D613265352D313734392D626138312D3134313632666461663564372220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A64626238303532312D333162342D343234642D613066642D353733343631
          353061333934223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A64626238
          303532312D333162342D343234642D613066642D353733343631353061333934
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A30613764626466392D
          363338632D326134632D383731632D6266363033323033323163322220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E9FCD8784000000A94944415438CB63FCFFFF3F0325800544F4
          4E98F2F2EFDFBF62FFFEFD6300D2708CC207B2FF21E45E4C99D82B093601E482
          8EEE7E20F99F689C9E9DFF1FC6630219F2E7CF1F929C8DAC7E881AF0FBF7EFA1
          EE0564F52C103FFD79939A912B829C70FE41130FB684F5EFCFBF577013F025E5
          19B3E7FF07616C72B084C4082618195124D76FD901D6F4EEED5B305F4858184C
          07FA7830A25BCC82D5E6E9D3890E0F464A73231303850000E1CACB3F7B9E4526
          0000000049454E44AE426082}
      end>
    Left = 44
    Top = 189
    Bitmap = {}
  end
  object SaveDropDownMenu: TPopupMenu
    Left = 276
    Top = 21
    object SaveSessionMenuItem: TMenuItem
      Action = SaveSessionAction
      Default = True
    end
    object SaveAsSessionMenuItem: TMenuItem
      Action = SaveAsSessionAction
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Setdefaults1: TMenuItem
      Action = SetDefaultSessionAction
    end
  end
  object ManageSitePopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 404
    Top = 21
    object Shellicon1: TMenuItem
      Caption = 'Site'
      Enabled = False
      Visible = False
    end
    object SiteLoginMenuItem: TMenuItem
      Action = LoginAction
      Default = True
    end
    object OpeninPuTTY2: TMenuItem
      Action = PuttyAction
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object Edit1: TMenuItem
      Action = EditSessionAction
    end
    object Delete1: TMenuItem
      Action = DeleteSessionAction
    end
    object Rename1: TMenuItem
      Action = RenameSessionAction
    end
    object SiteClonetoNewSiteMenuItem: TMenuItem
      Action = CloneToNewSiteAction
    end
    object GenerateSessionURL1: TMenuItem
      Action = GenerateUrlAction2
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Setdefaults2: TMenuItem
      Action = SetDefaultSessionAction
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Newfolder1: TMenuItem
      Action = NewSessionFolderAction
    end
    object Shellicon2: TMenuItem
      Caption = 'Site Shell Icon'
      Enabled = False
      Visible = False
    end
    object DesktopIcon2: TMenuItem
      Action = DesktopIconAction
    end
    object ExplorersSendToShortcut2: TMenuItem
      Action = SendToHookAction
    end
    object Options1: TMenuItem
      Caption = 'Options'
      Enabled = False
      Visible = False
    end
    object IncrementalSearch1: TMenuItem
      Caption = 'Incremental Search'
      object SearchSiteNameStartOnly1: TMenuItem
        Action = SearchSiteNameStartOnlyAction
        RadioItem = True
      end
      object SearchSiteName1: TMenuItem
        Action = SearchSiteNameAction
        RadioItem = True
      end
      object SearchSite1: TMenuItem
        Action = SearchSiteAction
        RadioItem = True
      end
    end
  end
  object ManageFolderPopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 406
    Top = 77
    object MenuItem1: TMenuItem
      Caption = 'Site Folder'
      Enabled = False
      Visible = False
    end
    object Login5: TMenuItem
      Action = LoginAction
      Default = True
    end
    object OpeninPuTTY4: TMenuItem
      Action = PuttyAction
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object MenuItem3: TMenuItem
      Action = DeleteSessionAction
    end
    object MenuItem4: TMenuItem
      Action = RenameSessionAction
    end
    object MenuItem5: TMenuItem
      Caption = '-'
    end
    object MenuItem6: TMenuItem
      Action = NewSessionFolderAction
    end
    object MenuItem7: TMenuItem
      Caption = 'Site Folder Shell Icon'
      Enabled = False
      Visible = False
    end
    object MenuItem8: TMenuItem
      Action = DesktopIconAction
    end
    object Options3: TMenuItem
      Caption = 'Options'
      Enabled = False
      Visible = False
    end
    object IncrementalSearch3: TMenuItem
      Caption = 'Incremental Search'
      object BeginningofSiteNameOnly2: TMenuItem
        Action = SearchSiteNameStartOnlyAction
        RadioItem = True
      end
      object AnyPartofSiteName2: TMenuItem
        Action = SearchSiteNameAction
        RadioItem = True
      end
      object AllMajorSiteFields2: TMenuItem
        Action = SearchSiteAction
        RadioItem = True
      end
    end
  end
  object ManageNewSitePopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 151
    Top = 133
    object MenuItem12: TMenuItem
      Caption = 'New Site'
      Enabled = False
      Visible = False
    end
    object Login2: TMenuItem
      Action = LoginAction
      Default = True
    end
    object OpeninPuTTY3: TMenuItem
      Action = PuttyAction
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object MenuItem13: TMenuItem
      Action = SaveAsSessionAction
    end
    object Reset1: TMenuItem
      Action = ResetNewSessionAction
    end
    object Paste1: TMenuItem
      Action = PasteUrlAction
    end
    object GenerateSessionURL2: TMenuItem
      Action = GenerateUrlAction2
    end
    object MenuItem21: TMenuItem
      Caption = '-'
    end
    object MenuItem22: TMenuItem
      Action = SetDefaultSessionAction
    end
    object MenuItem16: TMenuItem
      Caption = '-'
    end
    object MenuItem17: TMenuItem
      Action = NewSessionFolderAction
    end
    object Options2: TMenuItem
      Caption = 'Options'
      Enabled = False
      Visible = False
    end
    object IncrementalSearch2: TMenuItem
      Caption = 'Incremental Search'
      object BeginningofSiteNameOnly1: TMenuItem
        Action = SearchSiteNameStartOnlyAction
        RadioItem = True
      end
      object AnyPartofSiteName1: TMenuItem
        Action = SearchSiteNameAction
        RadioItem = True
      end
      object AllMajorSiteFields1: TMenuItem
        Action = SearchSiteAction
        RadioItem = True
      end
    end
  end
  object ManageWorkspacePopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 406
    Top = 141
    object MenuItem2: TMenuItem
      Caption = 'Workspace'
      Enabled = False
      Visible = False
    end
    object Login3: TMenuItem
      Action = LoginAction
      Default = True
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object MenuItem10: TMenuItem
      Action = DeleteSessionAction
    end
    object MenuItem11: TMenuItem
      Action = RenameSessionAction
    end
    object MenuItem18: TMenuItem
      Caption = 'Workspace Shell Icon'
      Enabled = False
      Visible = False
    end
    object MenuItem19: TMenuItem
      Action = DesktopIconAction
    end
    object Options4: TMenuItem
      Caption = 'Options'
      Enabled = False
      Visible = False
    end
    object IncrementalSearch4: TMenuItem
      Caption = 'Incremental Search'
      object BeginningofSiteNameOnly3: TMenuItem
        Action = SearchSiteNameStartOnlyAction
        RadioItem = True
      end
      object AnyPartofSiteName3: TMenuItem
        Action = SearchSiteNameAction
        RadioItem = True
      end
      object AllMajorSiteFields3: TMenuItem
        Action = SearchSiteAction
        RadioItem = True
      end
    end
  end
  object SessionAdvancedPopupMenu: TPopupMenu
    Left = 152
    Top = 21
    object Session1: TMenuItem
      Caption = 'Session'
      Enabled = False
      Visible = False
    end
    object MenuItem9: TMenuItem
      Action = SessionAdvancedAction
      Default = True
    end
    object EditRawSettings1: TMenuItem
      Action = SessionRawAction
    end
    object TransferSettingsRule1: TMenuItem
      Action = CopyParamRuleAction
    end
    object MenuItem14: TMenuItem
      Caption = 'Global Preferences'
      Enabled = False
      Visible = False
    end
    object PreferencesLoggingAction1: TMenuItem
      Action = PreferencesLoggingAction
    end
  end
  object ActionImageList: TPngImageList
    PngImages = <
      item
        Background = clWindow
        Name = 'Login'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30352B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30352B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A66323036323435322D366438652D306434382D62
          6237372D3735326334656332366235612220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A613931393965
          39322D383939372D303434322D623537382D3633383337326334633563312220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A62613536386132332D333035652D313034352D393534322D623535633966
          326335666465223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A62613536
          386132332D333035652D313034352D393534322D623535633966326335666465
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A66323036323435322D
          366438652D306434382D626237372D3735326334656332366235612220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30352B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E1D8D1185000001614944415438CB63FCFFFF3F032580054478
          4F13BECFF08F5181249D4CFF1F6CCD7AAB083600A459D4E20F032313900D73D0
          7F04135DECFF5F0686B7675914E02E00014690C43F060673A160860CA539403E
          23568BDFFC7CC8507C5E1FD50B60C3A1B608B24A30EC7C318D61F9C36A842E98
          6B80C422CB0F608BE03E4136002401360888BD25F3194C04FD1062FF105E410E
          78B8010C5085DB9F4E6558F6A09A818F458C21576D11439EFA22061E6621B0A6
          FFFFFEA38609BA1740CE430726427E0C1A46360C736FE7339C7EB309A216AB17
          FE11911EFE316018C0821C503187F9E1EC68E556064FE91CB0AD736F15307CFE
          F516255031BD0035D553268741904D92E1C3CFD70C13AFC6329C7ABD09251670
          BA0016FA30BCE5E104144DC8B6FEC7E602702C00A9773F9E334429B73078C9E6
          600D86D73F1EC2C302C5807FFF20A9F1F88BB50CC79EAFC56DEB7F6C2E60FCFF
          E8D3755639923213500F84A2303B03009683A6ED726D96990000000049454E44
          AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          383A33372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35373A31312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35373A31312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A30383332613165612D313938662D656334332D39
          3934332D3933306134313665623763382220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A353566623935
          65312D356330622D386134662D613234632D3531363232373739636534332220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A35373733623437622D643238642D326634352D386363652D626430376431
          323234653932223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A35373733
          623437622D643238642D326634352D386363652D626430376431323234653932
          222073744576743A7768656E3D22323032322D30362D32375431353A35383A33
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A30383332613165612D
          313938662D656334332D393934332D3933306134313665623763382220737445
          76743A7768656E3D22323032322D30392D30315431303A35373A31312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EE783E6AD000001FD4944415438CB9D924D48945114869FEF9B
          CF197F931A30C3AC1621445412411141AB40288B92762D23DAB47111D4AE95D1
          C2652A86642D1209AC28B056FD202E94C01651092DCC0AAD49EDCFF9EEBDE7DC
          16F3C3148E48EFEEC2E5BDCF7DCE092893AE6BDD9F45A45144702288738808C5
          B30862DD7C54AEC03AD7F8E04807122B2EABA4E21FEC895FF2343E80C4826495
          B6A99B0D61B902672D6A3D62944D7696A1C4054E844F50A38851C42ACE5AC255
          0810A3EC35930C5574D25C63B8FDBB1DB18A5A8F1AC53A47B41AC1A978848BA9
          4192A96A3E2CC14476D7DF04CE11AD24CBABA5BDE505FB52738489242A96C1A5
          A388F179821C85B596E85F59D56689EB894BEC4ECE4250015E30C6F02ABB9DF5
          9A61CED6E7084C9EA054965A6587BC85D033B35C4F73E52284154461C0ADA62E
          C6DFD5D23BB913710E278215F7352AC8D23CDA58DCCAF3C56E6EACBBCC96CA05
          F00EEF617AA185F7C90EEE0E9D0D4A5DE50934F73FA3A8519AFC270E564DE135
          44346078721BAEE90C89248C3C1CF500278FB505B9821202B13949E7EBEE1006
          9E650BE7A63B715313FC1C1B5E715A912D10D8DCEBB5EE3BC76B9FF1C5D471FA
          CD155E2F6EE6B01BE7F1A3FBC1CA05CECD1F1AED6D28ECFAFEAD337CACA9E2EA
          682B1B7FDD239D97C55AD3D73FE0FBFA07FC5AEF17B10A72BE6532006C48A729
          95552EC555EEEDE9E17FF20722BF757B3CE3D3050000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename 2'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31332B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31332B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A65636533633164332D653863652D666334302D38
          3138332D3130346536636262663434662220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A386362383738
          38652D633063632D316234322D383433332D3138333865326631366636352220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A38643066383331632D643930362D633434322D616338322D373363666333
          653537646237223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A38643066
          383331632D643930362D633434322D616338322D373363666333653537646237
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A65636533633164332D
          653863652D666334302D383138332D3130346536636262663434662220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31332B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E77C6659E000001664944415438CB63642003BC495399F6F3C3
          27DD8FAF3E643293AAF9C3CC5CB75FF7AE4CF8F1FEB3FCFF9FBF0C48D2BC7F3F
          03CB8FAF9F2FDD39B6EFC39530F5FF376C183C984831C08EC3A288E14A3CE707
          569687F7425BB6681C61D8C142ACE6EF476CE418FE7FAD67FE7E854BFF77C487
          3F6C5E6E2071A25DC0CEFC6BFAFF3F3FB9FEFFFDC9F0FFFFBF1A0BFF392F8936
          E0CF31CB807F7F7F7A813433FCFB75A6EDD88BE9303946429A5FEDD7E61161E7
          B8FEF7F7171986DF5FFEFDFFF7CB98CDEDF505983C411708737037FEFBF35386
          01E474867F939135835D306DE6DCFF383573BE6708525ACA00B499E1EB4F2686
          D58F0B187EFF63435504320017B8BFC1EFFFC7ADEAFF7FED96FDFFEFD5360C79
          905E142F4C9F350F35C9DE3BC270EDC24D860F2C8E0C8CA29E585D89330C9E3F
          B8C4B0E796388394551583A8C37C9C618433213DBC7B89C1CC239B41CE211B6F
          20A31890999604675B38C71095C058B0F99D1400003EDAC2E98933A1B6000000
          0049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35303A35392B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35303A35392B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A63353266366130332D666161392D306134642D62
          6162392D3663383962306635623838612220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A353430636237
          63642D393431342D633434622D383733392D3561633738663663633161652220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30316536616431612D356136652D323134352D383133352D306463613636
          633434643535223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30316536
          616431612D356136652D323134352D383133352D306463613636633434643535
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A63353266366130332D
          666161392D306134642D626162392D3663383962306635623838612220737445
          76743A7768656E3D22323032322D30392D30315431303A35303A35392B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3ED3D85E110000016D4944415438CBC593BF4A034110C6673731
          FFE40E94D8888585902AB5904AAE1052181B09888DF80C96BE449EC127508B60
          73DA18ECAF8A294511BC227A92C3ECED8CBB897BB9E4A208291C5838D8FD7DFB
          CDB7738C886091E2B06869077ABD1C6F545F4F362F7A47ABF64F67BB7B56F9E9
          60EDFA717FA56AB8D84186675A88D4B032A5F63C110D97F20517897681F356AA
          05118A2691F480A066B142BB579F8818585D592589DE201A36CD1E332132C6E0
          F970BDCC3873491F24EA0CFA9F75CC8A5C120EA5702A57811F7349015D5A0400
          5D42052075D4BE05A3EF096CB29B2B30125196B1501C395130CCC24981B9CF18
          980348E695021E05C33FCD411CD8B76DD08BA8B6C46D152CD8BF0ACCC2DA7688
          9103923CE5A79625ABDDDB9E1689337868D829D8F4DCDDB1CAB9228D5F07B183
          6F617DEB9EDEA71C28F8720CCB546095DBC01F861F0E69276A4ED872FE3CD582
          14F2540DD25D28A3297822027E24068ECAE4464474966AE1DFFEC62FCF241C5F
          463794990000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35313A30342B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35313A30342B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A64303163313134372D323662322D326434662D61
          6230612D3336313131613163306561322220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A353463356534
          33372D666465632D316634372D613935662D6236343466356533633330642220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30336534663638622D343132392D386634632D386139352D366164396530
          336462363262223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30336534
          663638622D343132392D386634632D386139352D366164396530336462363262
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A64303163313134372D
          323662322D326434662D616230612D3336313131613163306561322220737445
          76743A7768656E3D22323032322D30392D30315431303A35313A30342B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E6A8148D20000020D4944415438CBA5924B6813511486FF338E
          4D33D1544117C93445082D6AB14141F0592B18A1E0AA6A85162A156B4816BA54
          77E2C26D208B82B8110425812A2842A952BA74A1151F50D10A4D4A52B146322D
          C62433738E8BD63153AD54FC37F770B9DF77CEBD5CC21A73F3D6ED8BA4E072C1
          AAB65E8BC5CA3FF7692D703299DCE4F36FCEF97C3EAD5C2EF70F9D3B9B7604B5
          A781B7106AFF65144B482E351CFB349C4AA5B6AAAA6A91DA78A5AD351CD7F5E0
          C6E7932FDFE4B38B073D1ED64A5E5EA4DA93A0ACEC48406574AEEFFAECF71D37
          54751D2B8A42BDA77A4821C2BDCC0864292C220655C67E1700404D348C7F8DC3
          F684D1D575049AA681996133E3CB7C11E3131355D3B6AE522E1312456D5CC604
          A428F007F66243601FA0783039E3C7AB690BBDA77B6033836DC6C8FD07302D1E
          885F18BC43323B2C20B5AEB700A60129CF404C03EF8BDB305DEDC4E1430760DB
          0C66C6D8E82318463592480CBDA6C2C30ED9128E425DEF05B802B12B0057007B
          A97EFCF1289A02BB5159C863EEB301BDB9058651C27C71A13F113B7F97B2695D
          427B061DC08197D7CC54376C51B0B3E90542DA074C953AF0CE88C066D409227D
          0EE04C515FAF90DA4CF01ECF11005036AD4BF3AE937F055CB55800808668A14E
          D07E62D52BB82612D3796AB7607B74D52B3832AEB9FE894BA0B7753A87FFD89D
          AB2ED8B4005FB723087E0348C33F8480672D67F2FB01804404FF931F95D05DDA
          9679C8130000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open Workspace (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C069545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30362B30313A30302220786D703A4D65746164617461
          446174653D22323032322D30392D30315431313A30362B30313A303022206463
          3A666F726D61743D22696D6167652F706E67222070686F746F73686F703A436F
          6C6F724D6F64653D22332220786D704D4D3A496E7374616E636549443D22786D
          702E6969643A64633966343633362D653732662D623234312D386665622D6266
          663361353036373334652220786D704D4D3A446F63756D656E7449443D226164
          6F62653A646F6369643A70686F746F73686F703A37313730336339322D353865
          302D336234332D393931322D3766646431663365346130332220786D704D4D3A
          4F726967696E616C446F63756D656E7449443D22786D702E6469643A65323937
          666262352D326131662D383934662D623264352D613163663364333931616439
          223E203C786D704D4D3A486973746F72793E203C7264663A5365713E203C7264
          663A6C692073744576743A616374696F6E3D2263726561746564222073744576
          743A696E7374616E636549443D22786D702E6969643A65323937666262352D32
          6131662D383934662D623264352D613163663364333931616439222073744576
          743A7768656E3D22323032322D30362D32375431353A35393A33392B30313A30
          30222073744576743A736F6674776172654167656E743D2241646F6265205068
          6F746F73686F702032332E35202857696E646F777329222F3E203C7264663A6C
          692073744576743A616374696F6E3D227361766564222073744576743A696E73
          74616E636549443D22786D702E6969643A64633966343633362D653732662D62
          3234312D386665622D626666336135303637333465222073744576743A776865
          6E3D22323032322D30392D30315431313A30362B30313A303022207374457674
          3A736F6674776172654167656E743D2241646F62652050686F746F73686F7020
          32332E35202857696E646F777329222073744576743A6368616E6765643D222F
          222F3E203C2F7264663A5365713E203C2F786D704D4D3A486973746F72793E20
          3C2F7264663A4465736372697074696F6E3E203C2F7264663A5244463E203C2F
          783A786D706D6574613E203C3F787061636B657420656E643D2272223F3E5770
          54B0000001B64944415438CB8D53BB4A0341143D934C12638C8988E003030A16
          62A1851FA058888AD808A210626BEF07A4161B1B41D01F50412C622C1405512C
          45141B09A2A2F802F330BBD9ECCE388EBB613789980B9773EF9DFB3AC30C4199
          F4256F386A94EBB15E42CA83AB6B1B7C6262FCDFE244620FAFCF0F1EAA1DB45D
          81933EEB603D05443A5A4B898AA24AF4FBEB1CF68FC4E371DD652FB64B6AAA5D
          E2E6F68ED472DB126A77ECE4BB779F24C6A2B380F125353637F99B6564E1F768
          383A1AA29471B3D2E511EA45BD4FD84CC1E2D60896479780E2074068C586F393
          9DE085E80BC9EFB77377D300BCDD0B226CBB53AE03DA9BB88407305D6CC0047F
          4395C84D84B7F9901A0CC8640932A72BE8199C16491A78B564BB6FD9B9DBA48B
          89066F2FF76889F4CB036EE4052A52B989965DF299E57F9DC80D94DC27020D61
          33C19A54284DAEB61161AAF668A42F6991D40BEA1A28759BDDED6B161C141C8D
          9872D1357CA7D2220B20D0183257AD36AD60522B6FA49CC877A06804C1508BC9
          F38F69763AA558F1FCA7812B9F4BA32118AEB830A79D77C6B9019DE86772034D
          CDA45F53C7A1ACCF5DEB2714AF859C4766DEE553259CD7FC7BABCA37A26457AD
          0F4F02C00000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open saved session (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          373A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35323A35312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35323A35312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39633730303166322D373964312D613934382D62
          6432312D6264336564306266336166632220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A303837363134
          30342D336233302D653434322D393162622D3235373334623933383364362220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30386237343738312D636533622D313134622D613839342D353538326631
          666466353030223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30386237
          343738312D636533622D313134622D613839342D353538326631666466353030
          222073744576743A7768656E3D22323032322D30362D32375431353A35373A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39633730303166322D
          373964312D613934382D626432312D6264336564306266336166632220737445
          76743A7768656E3D22323032322D30392D30315431303A35323A35312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E13041446000001B54944415438CB9D92316B544110C77F73B7
          46F172DE054C30162904AB145AD80936168AA58876F6567E0C1151D04FE057B0
          B3B01044B94648303622162A9278823177DE7B6F7766D7E2BD3BDE8B77200ECC
          CE7F96F9FF67665961813D78F4644FCDD64C1535E37054554C75D72D120821AC
          3DBB7A1DCD229A1B9A1B961B9A4FF3C8B5EDA7A79C7FB1BE4392CD2951489A24
          DDBD37085811316F988F256EB81142C0D5C900097192E46108A149F435AFF252
          A0412E8F08C74208B3C27E9A60316249D164288661A8055C4C15B375045A4BA4
          548AAD7485FBEB1FB972322032FFA9463F2F2293E7A7537BE53C4B67EE00521B
          47C10F21FB42D4DF1073B01C624EAAE2BBAF7D9C4538180907AF1F73F6C20D30
          4F9A53DCC82BFCF67D316EC508C3BDCFAC6E9C03CB493601CBC0325215A77896
          C712DFBE34BCEC2C4236DEA7B3DCAF0AA69D8A59E7791349CCFD87DDED2D17E4
          3888C7B976A55E1FB368ACD0108AD9D6E64DBC0BB143E744AF1A755EB7A25AED
          B050F60AC0655EE8F656AB3D1774ABAF33BB0B0380D664BCCF72B7FFD78335F1
          A4799F0C157D03E07C3EFAF5FDD3CBDEE8689B7F3541061BB77E7C039034FD7A
          FF697F0023C2BB888ED08B810000000049454E44AE426082}
      end>
    Left = 41
    Top = 253
    Bitmap = {}
  end
  object LoginDropDownMenu: TPopupMenu
    Images = ActionImageList
    Left = 278
    Top = 77
    object Login1: TMenuItem
      Action = LoginAction
      Default = True
    end
    object OpeninPuTTY1: TMenuItem
      Action = PuttyAction
    end
  end
  object SessionImageList120: TPngImageList
    Height = 20
    Width = 20
    PngImages = <
      item
        Background = clWindow
        Name = 'Unused'
        PngImage.Data = {
          89504E470D0A1A0A0000000D494844520000001400000014080200000002EB8A
          5A000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA8640000001B4944415478DA63FCFFFF3F03B9807154F3A8E651CDA39A0756
          33004FE33BD9870B3EBF0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30352B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30352B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A64356261363438302D666365342D383334302D62
          6162302D6463363362626565613536392220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A633664646432
          30382D656561302D666434652D386635332D6336346135613566386165612220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30306561626138632D323132312D613834312D386264312D356163646631
          333966663565223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30306561
          626138632D323132312D613834312D386264312D356163646631333966663565
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A64356261363438302D
          666365342D383334302D626162302D6463363362626565613536392220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30352B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EBC75A5590000014E4944415438CBAD94314E43310C86FF543D
          01E5028F856BA00E88A112E2060C48DCA1130740082450416540628785A112D7
          60E9025BB77281C6BF19D2A4499AD7F7845E24CB7E89F5D9719E6D54155DAE3E
          00DCDC3D7C93AC4882248404454055A7D7FBB148FAFD737F7B7D10802252BD1D
          9F8256414BA757B1CDCDD94A339B187DBD563EC3DE1A0815EFA82938E8FA3311
          4102B422A055A8106A156ABD76D214600B28223B33C903F8A0DEA706986522CD
          57F570BB05B4B675BD4A672DAEECB3D48D0E76E4273B6A186A232CD629C94CD2
          CCCB194A9B076116C8C16B7F9B76FF9E460FE202C440DF2938FA9CBA36CA5A4E
          0A6DE765FF6A0600588C8719D0CA9CCAC300ACEFD94416E32154154ACE3DD0A8
          2A8C31B5D3E3E9F9258CA3CB8BF35A473FB58AC0F78F5980FC2E97617F6F3008
          F6D9E8C49480FDA6F916435ACFC37C3D4E26FF1EB0A6EB89DD43C7EB0F6F01A9
          0A7A51303D0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30392B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30392B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A63393238336631342D336435392D656334322D39
          3463382D6135336637616335363631372220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A333962623931
          39302D616161372D356634352D386164332D3438313664343438653463332220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A38663434356265382D396461372D316134362D393965662D333036383638
          313266643031223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A38663434
          356265382D396461372D316134362D393965662D333036383638313266643031
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A63393238336631342D
          336435392D656334322D393463382D6135336637616335363631372220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30392B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E94AD5135000001D74944415438CBCD94CB6ED3401486BF7163
          57842A222DD750950DBB486C0175532490FA0420C443F034F02C2C0A1B2A8A54
          8915EC90A059D034522FA86932E378CE6131B66B4353A8940523D93AA3F1FC73
          FE6F8E8F5155663922663CFE7FC106C0E46DE7336ABAD50583666AF465F2B8FF
          FA22824655996CDC3EF3660C5815BF965AFD3E4D60C70D0EBB4F49FF49B02800
          2D5E2682689EA2321405E59BDC7AF09013D81EEFEE978265F544319804AD889A
          C665E29BEB448BF7C1C453EDAADB1D3400BCE440AFAFD1587E1ECCD6BE9C80DB
          839F9F503F46C581A4E0DD692C0EE6AF6D195565F42658DEE71E1235B9D35D07
          9980D4375485C23C17CA1FEFED8B084014BC42BFF785CEDDD55CC0E6022EC4DE
          06216F2B6B36CCBD43BD63E2DCBB06800864A6C55CEC89E304157B4E2629EA4F
          E3227323E9D78547DBFDC05061240BB4965A958FF3CD95F80F663524F67D59D8
          2286E13863697925D898CAEC37319F56F96D94BF9E080C8F0F68B56FFC8599AB
          310B878783C49F6C96197AD304529224066FCF6556435064ED6DEFD2931FBD52
          D04993567BB1C2683AFC33796AB659EB36C7C321EDAB9D70BBA5255B5A0A960B
          042E775170CD30E25FD5BA8D1D1D1D1DEC6C5D19EFCD5DB85D19CCC79567830F
          B5E630CBF10B30A0CCB8CAC10D420000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A31372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30323A30372B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30323A30372B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39376431326332652D656436352D613434372D39
          3538622D6665323536616339383636342220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A343639653934
          38322D373636342D336634322D393263662D6261313766666131373030612220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A36323737376532392D616463312D643434662D616363392D396334656265
          646238623130223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A36323737
          376532392D616463312D643434662D616363392D396334656265646238623130
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A31
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39376431326332652D
          656436352D613434372D393538622D6665323536616339383636342220737445
          76743A7768656E3D22323032322D30392D30315431313A30323A30372B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E36E04C15000001704944415438CBAD94CB4AC3501086BF9344
          AD28542C88E2652DF80AAE7D197D886E5CB916F455C4171004413722583745B1
          28E2A5A489C9FC2E526B935E52A103C3CC70CEFF0FE7CCC549629AE23165993A
          6100109ED5363D2ADBFD07BE255F8717AD8B7A1D2B23E9FF362789E47CFD55B0
          3C7893E3D9BDC783FF1006196E081980633F3E5BBB31B9C648363FBD071A39C2
          B19F3CBF7AE257D601CB522B6F2D7C6E038B39C24F6D3033BF9281A4CCBA804A
          6D077F710BD2B07B663DABAE0DBCB985DCA324913E1C499610CC2D75B30B4851
          FC066113A51D6411580469F4E75B84C3E1EF5EBA5CDB3C5D9DE214A1A885C226
          D66E609F77286C228B90C5F0ABEAF32DC6928FC13EB4A483E73CB0380F2E897B
          FED0C6B6EF3F55665512F77438E188EC65F1A8D12B022623D398595601A002B8
          188F98C8DC93513CB6A2FA8D954EB06D26ADA892F26D03D0F97ACEAA97665554
          AE9A71D71F244B0CAA454249B72FCDEB0F70EFFFDD7F0EEEAAC5D19BA6FC0055
          36826DB9DD35A90000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39383132346331382D313931622D623234342D61
          3764352D3461303034623530393138302220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A656361663636
          39392D313139382D613334352D626537312D3639653638343935316330392220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A64353632623331322D316137322D373034302D623233362D383664363166
          373232386361223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A64353632
          623331322D316137322D373034302D623233362D383664363166373232386361
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39383132346331382D
          313931622D623234342D613764352D3461303034623530393138302220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E94089F9B000000B24944415438CB63FCFFFF3F033501A3E1E1
          DB543591517BFB75EA1AA8BAEA32750D9C3673EE7F6F6F2FBC8AB66EDDC64048
          0D4C1DD8C0CCB424BC0AA7CF9AC740480D4C1DDCC05B762C6041B5437F701A18
          BE9D1FCC5FE9F99134034112300092C36620BA1A0C03A9EE657CDE8119F87B8F
          3498CFEAF2943403717919D940FA7B199F776006EEAAFB01E6BB35719066202E
          2F231B485F2F4B4A4B53251F3F7FFA94B8BC4C2C80E765AA9636542FB1A96D20
          00B8E4B64DD171B7D90000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D494844520000001400000014080200000002EB8A
          5A000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA8640000001B4944415478DA63FCFFFF3F03B9807154F3A8E651CDA39A0756
          33004FE33BD9870B3EBF0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30382B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30382B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A65613963323132622D356664622D366334322D62
          3033362D3536346135393937346164352220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A656665626461
          34372D393764332D393934662D386332302D6330383735353637653633352220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A33386134663032332D653133632D653634322D616363372D646236353361
          363464353339223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A33386134
          663032332D653133632D653634322D616363372D646236353361363464353339
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A65613963323132622D
          356664622D366334322D623033362D3536346135393937346164352220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30382B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EA1C1D108000002704944415438CBAD945D4B545114869FE39C
          9CD042492FCC2432A1AC30C38B2E22248C3E2E2291A0AB88A020C27E4157FD81
          02A350C928F02E24290CA2088234E82648C31A283F30FCCA8FACC1A999BDD6EA
          623CC71935AD68C1662DF6E63CFBDDEB3D7B7B6686E779FC4DDC6869D99D1BC9
          7DED9C9DBD7CE97C67E69A67665C6FBA35A0AAE5AA8AAA22AAA8086A96CE0BF3
          C1282B2B636FD51EFADFC7F8108B212283B79AAE6D07F00144A4FCC1D17AD419
          EA349D5399B5866B35F1312AD627A83D7880B1B97926AD84C2BEA7E581C20088
          892D7C64D960A7EC8A7FA12635C3C654025F1D0D474E22A21C3FB89FD9078FF0
          AAABB973B7BD57B1C73E8013419D61A29833CC05393D7624BF525910E570ED61
          F2F3F330334484DC689433A74F118FC7E9ED7BB7677864A4302750B8A868B9CA
          AE755B199C9C65606C0235455490A0D7A64CCDCC30343C32E77EB8BA0CE092A3
          CAE206F392C37D7F3BDD2F7B1051441427828820A27477BF4292C9638D8D173E
          A67BE8DC92BE2D573921517C5B80A8A26AA1EB0009DF06B34CC986052A6D318B
          01309FF8C19BDE7E06868629292D655F6505914884A8B862606AD1E5C004C934
          44B33620E2D1F1B08BB73F0BE9F9BE85AAB96F8C8EBCC047C1FC0DD90A7FF3DB
          643ADE9EDCC6546A1DB3C90892323EC78B783E5540FD7407659B8BDF8440179A
          A2ABF6F2532A2FAC8313249CC7F8C4386DAD37354BE1A167B7D34D5E72E564C9
          B5CB1CC5579F00307AA58EEC9BE224A6A63B4360C647AB0147AFD46166986A2C
          EB7158EDB5696DBB67417DF1C2B9359FA515819D5D4F42C8CCF47438BFA9A828
          AC1B4E1C5F11EEAFB56326E44F6245604B7333FF1A9E99F13F2387FF1CBF0072
          187F8990E630C40000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D494844520000001400000014080200000002EB8A
          5A000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA8640000001B4944415478DA63FCFFFF3F03B9807154F3A8E651CDA39A0756
          33004FE33BD9870B3EBF0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site color mask'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39363034353766642D663233332D623034332D62
          3330632D3065613762333966323737652220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A333930323862
          32362D656133302D333334322D393162352D3038633931393035306631302220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A39333938333539352D626636622D303434392D613639352D346136366439
          373063623965223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A39333938
          333539352D626636622D303434392D613639352D346136366439373063623965
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39363034353766642D
          663233332D623034332D623330632D3065613762333966323737652220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3ECB9E9CDB000000E44944415438CB63FCFFFF3F0335010B88E8
          9B38F5DEBF7FFF1481980184FF82E8BF7F19FE012D03D3507164FC17957F7FCA
          841E25B089201776F54E0492FFC9C619D9F9FF613C2690A17F81AEA00420EB07
          1BF887DA06FE1D6C06FEC130F0CF9F61EE65DA1B48CD64C30213C8CC29846423
          B42CF7174BB6836191861D1003AB9CD00CFCF3F7E6BFFFFFD4E106E2CEB328F8
          19D0207096FBF7EF26CC404690002323234EEFCC9CB3005E1CA5A724E054082B
          B5B01AB87ECB0EB821EFDEBE858B0B090BC3D9813E1E8CD80C642114E0C88610
          5D1EA28319D3A7931DE38CD42EB19918A80C0005F322494DC809CF0000000049
          454E44AE426082}
      end>
    Left = 151
    Top = 189
    Bitmap = {}
  end
  object ActionImageList120: TPngImageList
    Height = 20
    Width = 20
    PngImages = <
      item
        Background = clWindow
        Name = 'Login'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30362B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30362B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A33616438633339342D633165662D326234662D61
          3564362D3632396361323438346636392220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A343335303765
          31662D323932612D363434302D386366622D3131616136643538623335622220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A65356634386136662D363061352D633934662D386366622D343839633738
          666162666330223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A65356634
          386136662D363061352D633934662D386366622D343839633738666162666330
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A33616438633339342D
          633165662D326234662D613564362D3632396361323438346636392220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30362B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E2B4BBAD6000002154944415438CBAD95416813411486BF4936
          D14434B641AD1A9088484B052B82B14A0F4544A1172F8A07BD8907033D4A2F8A
          27ABDE15312078D0E2C583280A8A9785DA203D09811451949AB634B5A5A24993
          EC3C0F31C96CB389423B30CCEEDBB7FFFCF3EFFFDE2A11613D870530742F3A81
          A8046BC1F649FAE59585A316005A25B6F757401909E2BE96167100EDC0C2A495
          A8335C9DB0C9DF417FF41C960AE2C5BA162AEB15ECF9C7ACC82FF791014C298F
          45CF73367683B9E2274FC2B54057681F8EAEF06EF6613360FD0D011F3E660A53
          5CFB38E049ADB6F9CDBEF7582AE0DACD673234670DE0CCEE11022A8468E39931
          AB31690644005D5DB3CBE3BCCEDD45044E745D62F4E0043D5B060C10698008AD
          19760663F4460609FBB7B2549AE34064104B05D8B6610F23BDCFB918BF43406D
          6C0231F56F68A8E170C71017E2B73D6DA6509CDC7999BECE53A4A6926496EC7F
          3194FFF3B518F2D46E3D190A4CE65F90FB9D75250CF73C22EC8F20086F73299E
          7CBE4EC929B83E4C0DBCC987F9E234F9C234F1CD878885BBB167C7A8E832F3E5
          AF3CC826C92CDA78D94CBC8E6CEED81D39CEE958121178F33DC5D50F09323FEC
          864DB4DB5A480B864833F8B32FB7AA61F12E99B61AD68B5D34BBC2FB193D32DE
          A2EEAA6347682F15A7D482A121AC9D1B43B4C6F20569D72E4B4E117BE6A93743
          7140FD55F467699157DFEEB76F63666D3BAB0195A497B381C49A5AB592747559
          E75FC01F9CD0106DF9609D670000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          383A33372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35373A31322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35373A31322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A31663532363031392D363537392D393334392D62
          3835302D6363613834383661353730332220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A363035636632
          35642D653239622D396434352D386666332D6533633234316534306661372220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A39643339303136372D353731622D383834392D623737612D646338346465
          323032633631223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A39643339
          303136372D353731622D383834392D623737612D646338346465323032633631
          222073744576743A7768656E3D22323032322D30362D32375431353A35383A33
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A31663532363031392D
          363537392D393334392D623835302D6363613834383661353730332220737445
          76743A7768656E3D22323032322D30392D30315431303A35373A31322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E96F2CFD3000002784944415438CBA5924D48545118869F3B73
          CD5B3AF9176A21B868D322A108A1C8566D042312A31FAC459B56416DC2858BD6
          428621A55862644810A1818285D0D216151682F4879088E14FFECC4C33DDF39D
          735A8CCE8C934E56075EEEB9E7DEEF3DEFF7F138645937DBDA67B4E872630C5A
          EB848CC1ACED33DF45CFBAD90C4549F9406D3DDA3768DF60569FE94A3FAB1BEF
          2D0D6435144914A984B4321865A95053B4D24C812CA195C5AC4A4470FF6468C4
          AE2B3AA8DED0EAB520DA125EF6D22E325B344C4BD7204FB99ED7879717A2EBEB
          01E27E30799116BB75C380F269D26D34E48F92E3E5138B86791CAD432B9B4C67
          FC2D260CA9255ACC0DAAF3A708B81E58C3DB95DD4CC6F7248C2425A5146E3634
          8A7217E8F72F52180AE2045CB01AE5C779B4721E233621959AB188E06E86C661
          794593DB839713042C580DD6128EF8BC081F49FCAFD61390304C1BFCDA875D32
          C79D9C662088118D1374C171711C43AEAB19DBDB483466687E7688C5684E7A67
          F3AE88AC0E3785C68C5FCCC958178E525CF5BA391E1A07349600DB77E4128F2B
          3EC66A78F0B0D7C99CBB9B89C6DAFE935F41812C72B4F01DD800600043386218
          5D3EC7375D45FF60A305A83F51EB6C686832929EF586D8E608D606C11AA62379
          DC1DD9C7FBC90960624332922D274C52495DF9C9A59D03380E58A3195BAEE4C2
          8766AA66FB783ED4E76C865A32617ABB462CA7BC110A82518C85A1F96AAE7CBE
          C68FB88B88644377CD30C5935616E31B2E973DC15A689D3ECDADA933886F9368
          64355422B3C7863B4A751AD8455E84B2CA396EBFDCCFEB2F73D498F67568F02F
          ABEB7EB7EDBCD763FFB6EEB7E1F60F0E5B80EF0B0B00149794908946D696330F
          3A3B3AF89FF50B23FD65752A6318880000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename 2'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31342B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31342B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A30663531383165352D313432332D393334302D39
          3232622D6433633561633933653232372220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A383735633239
          31612D363331302D643934302D386630392D6639366438383964613538622220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A66646538666432372D653463302D303734622D613337622D363730623339
          396431323364223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A66646538
          666432372D653463302D303734622D613337622D363730623339396431323364
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A30663531383165352D
          313432332D393334302D393232622D6433633561633933653232372220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31342B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E8C0CA337000001F24944415438CB63FCFFFF3F0323232303B9
          E04D9A82C6FFBF8C0B7FBEFF344566DDDBC58C941BA872E0E7A72FF63F3F7C66
          F8FDE53B65067EB974A0F0DB96E97DDF8EEF640019C8F0FFFF5CB20DFC70294A
          904D75CEC557AFDFCABEDFB7E617FBCAB64F3F9FBF5625DBC0BFC7CDE7FD6114
          72BDC755FDE2C9DBFF26BF1E5E2BF24E48EF27CBC0DFC72DAC19FFFD3CF2FFCF
          770606207EF0DB7ABEAAC7B224901CC906EEDFCFC062CF6E72F9DF9F1FC0D805
          1BF8EBEFAF5FBA1CDE6F6E9165E0DF13E615FFFFFC6807B90E8CFFFF6961777D
          5E0B9327C9C0EF476CE4D819BFDFFCF7F73B07D8B0BF3FEF3C7DF24C573191E1
          075906FE3B6EB6F5DF9FEF5E30D7FDFBF7DF8DC3FDD96E6435441BF8E7847908
          C39F9FAB41E1068E8C7F7F56B2B93E8B4057073670FAAC79FFF119C6C6FC9B21
          5C6D190327C35BB061BF7EFF6758F5B890E1FB5F6E147559E9C98C700333D392
          701AF8FF613FC3BFC70B1860AE63566F656092894351033403D3409020320D4E
          735F5F30DC5A62CAA0ACCCC9C0C2F49B8181478381C56C1BC873840D844920BB
          F6C3DD6D0C37567A83D932CA8A0C32DE5B81866A62F8826803CF6D2C65F875B5
          878185438041C56F2183808A1FD66021DACBF9A9BE0C527CDF180AEAE630B0F3
          2BE20C679C2E4407935B521932CA2632B0B271E14D561806325001C00DA42600
          005A1E559C4F8A0B1B0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35303A35392B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35303A35392B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A65386565346337302D323336632D366234352D39
          6131612D6464623639633662306136322220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A313333386265
          61322D303639612D363934642D626131632D3436623539306238633161322220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A31373237313535342D356138322D613234372D383132352D323162393138
          336531376332223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A31373237
          313535342D356138322D613234372D383132352D323162393138336531376332
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A65386565346337302D
          323336632D366234352D396131612D6464623639633662306136322220737445
          76743A7768656E3D22323032322D30392D30315431303A35303A35392B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EAA5A98A8000001A84944415438CBCDD4C14AC3401000D0D9A5
          E6202842B1D542297AF52B2A822048AC622D4230F64BFC100FD1DC72284A41F0
          E06778B187626D4514A1147A29CD8CB369935A9368941E5C6843363B2F3BB39B
          154404B36C1266DDD40CD5EF8CF1D76AE1FCE534AF278D6D1F668CD6C1B2A562
          7D47F829BF55D72CEE3A0182012196572E9FAE7FC280D0E270C9E32FF2B57773
          2A6522BC631DF9051ADF3A9DE39C9E04034454B1A11A66AD963D745D53107AA8
          94C2E91C85D1AF18129A85ABAE1DB92839BB63BB44A6F0C6912624319AD59362
          AA05351442049DCF95554308B25005120D5C17CA52D0A287A1EA0B638113057A
          B3A9640C49C0A8F0504652715822D04F51C078A6FC17877D065349F629E0F84A
          3444177A7FFA52BC05C049CD46AB0F1AD7D269EE2CE8BF02038C263543974C1F
          E5EAC4A23209A66A56A8776DF2B714A39CBED3DC9ED7BF05DBFBE948CC7F5EA8
          F7789FA239FAA240E3D23A8DAD69544E61026231BFADDFF46D7E68AA31E0A58F
          4EA3A8E92190D7B044DE9E8BC702F4B66FBB882314397D10BBA1E3EB7E03B456
          295D7BDC5B32921E5F8DCD39E3A1A8451F5FFFF6C4FE005BB18DD33A21476000
          00000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35313A30352B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35313A30352B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A61353939656631612D666230622D633234662D38
          3530652D6530343632653931336537372220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A346533363131
          36342D633562652D316234312D383362332D6433643663666637336430622220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A32646330643636322D613830372D393634642D393562332D643662383430
          366331623734223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A32646330
          643636322D613830372D393634642D393562332D643662383430366331623734
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A61353939656631612D
          666230622D633234662D383530652D6530343632653931336537372220737445
          76743A7768656E3D22323032322D30392D30315431303A35313A30352B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E504E8CC1000002854944415438CBAD545D4B5451145DE7DE73
          EF1DBD8EE9E4E768421A198560425460115148F85011614F05064530108404BD
          CD1FE8A5873009821E7AE883C47A09CAC020EC45C2B440441CC99A01A726BD4E
          DD8F73760F33A3F3E594D486C365DDC35E67ADCDDE9B11111863D84CDC1A1CDC
          ADABFA3BCFA3F3A12B179F66DF29D86484C36145E7C6835DEDEDA6A6F1A17038
          ECFB27C2DA86E66B5BFC95B587BB0FB260631DEA1A9AAF67DFB3BFB13C74F75E
          BFA6E9978510DB055145DF9953AA699ABA655978F464D8660A5B5598B228889E
          7300705E364E81D89E9C97401E31BAAA1F8BDE668A7ABA3A50BDF7C8A16ECD34
          CB4144104240370C9CEB3B6B5896654C4E4D5745E6170229CB79640040609C91
          72D37951BFBF537B3C108B45E3D158D4922421A4809012424A4892F89E487891
          4824E1DACE515ECA2A013EA6F2F1CEE007B438318C8CB96869B9009921931252
          12C6DEBC75C9F37A43A14B331C0096932A182F4B5110A52C2B0ACA6AF7C1577F
          00500CD4017017540821D264942694608C895555CE02000780AD5D3700C60BF5
          B93F402B9F40DE0A48D8007A90FCF90B1FA72711999F4353B0016D3B3BA06BDC
          336CBB06C0120780AFEFEFA0A6AD075C37016983A40D081B90761A3B8074C018
          F06C64183BAA6671A2690273CBAD187DF505AE50FD205EB1A6D04D2E811BFE34
          899B3A94FA5216EE6D1D855F8BA342FD06481B8140041D95AF717F6EE0643CFA
          7962AD0F171E36D3B6AE7E40A4D5659465A92C85F5E38BAC7052A4B366ADD8A1
          8D300845478FA403505E02E525E763C88256CB519849CA514379CA329844D1DE
          2D20A41256D7957A1B0E43C91A16AFA95B7291E4D6B084EDF5BAE5869B27363D
          1E945C9C192BFFF33664C5FE8C17ECC3FF19BF017FD7D12829D91A0A00000000
          49454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open Workspace (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A62356233383736302D376361302D303134342D39
          6235302D6266393136616661633637352220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A373065386234
          30332D363262302D333034662D623331622D6136306130653132326138662220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A65343032643730312D333839312D653634372D626135632D353936653134
          323364313332223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A65343032
          643730312D333839312D653634372D626135632D353936653134323364313332
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A62356233383736302D
          376361302D303134342D396235302D6266393136616661633637352220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E815EC57C000002364944415438CB9594CF4B155114C73F777C
          A3BD878ACF324B2D37E146A855AB16E52268A32D4325AC4D8BFA7BDAB52C8808
          22DB44484149094221D4262A48287F843FCAE79B7BDF7BF79E16F7CD38E37B4F
          F4C030E7CCE57EEEF77CE7CC28114129453A2EBCF92A1C3196C6461440AED9A2
          FAB34B4F181C1AB65D7549DE147867EB231313E38706CECEBEE0EE414080C153
          7DD8AD7500DA8A270F0DCF0154E64E7F46D468FCF0C10F7FFF7E7D008091B735
          A2480390CF1F4BF2B86E00A66199C594B2274F9F0130737332C9E3BA11D8227A
          1F2FEDDB28607799999AF0390276877C58A5B430D9BF18AD6C24408907250829
          74002EE2DEC38B80F0E8F612EEEF2702F70F54A3865BE343C0F4EA15B3B2A644
          04FD6A5000727D97C99D9902B2738954C1AC81FE85D80871065C05ACD9CB9D81
          8EBEE74A4428BFF4C00DCEE38202C3A3D7C055C16537A441BEAE83EA97B57A3A
          007002566075F90B03E72ED501BA0E303EB7C683AC4EAD695F5B835843D598D7
          3900E7A0A6BA690B2D61D88E387D80928A87D9AC72E52ADF3AC7165773E0D595
          5D175DBD9D5EC9FECDAD3CCB1CA6DF2563E39CA21455393E34ECDB68E9D93E98
          ADA4FD9B0308E2964B3B9B7417FB539EE9269E998C67FE707F90B3BBF38942AB
          0A4085F6F610AC6EF4CCB6B020566DF572FEEAEFE504685C81EE626FCAA346D0
          5ECB4DFC94DA7C3CB201C04EA944F1C4807FBB494B3A69C9B71C5B60EA5DC4BE
          D650CEDECF7CCBBABCBDBDF9F3434FB4D676D41F350AB570F6C6FAFBA41639F2
          DFFEC0F80F4B0AD8621E95B8E00000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open saved session (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          373A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35323A35312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35323A35312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39613838336330612D393866312D366234632D38
          3736392D3034626139303666353137622220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A663430376164
          36322D616635342D306634342D613934612D6330393132376165613865642220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A63656333643864372D383066302D646134312D623461362D306237303065
          613066616432223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A63656333
          643864372D383066302D646134312D623461362D306237303065613066616432
          222073744576743A7768656E3D22323032322D30362D32375431353A35373A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39613838336330612D
          393866312D366234632D383736392D3034626139303666353137622220737445
          76743A7768656E3D22323032322D30392D30315431303A35323A35312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E93EF7622000002344944415438CBAD93C16A14411086BF9EEC
          2E1AC29A184D348678F122821E3D08A2A02878F224E243F820221E441FC08B57
          8F2A21178306F4A6DE4430A024A2219035E99EE9AAF2D033B3B36B3646B0A0A1
          AA7BFAEFBFBEE9766686738EBDE2DE8387EB223223312222441144845DEAB516
          FB88581433CFAEDD448212BD2241521E04F14A0C69EEC6FB27C7B2FD08163122
          41D3C805C92B1145F27204A588916C9F0E07364A5074A8965C298A821640BE78
          FC03E6CE34451C16CDD9DDCE95B5C7830EB5DF6EEDB044503B1C1203305CCB59
          763F7F317BBEEF50FE68735787A3428D03B8B195583A74B930651E31219A224E
          5027442788534C635FD0AC4CB236B80ED6983F7A28E3E9E412E74ECFE3B2F648
          039B3F2EE2CC0CFFF28401B4662ED19ABF0D0CDD4B2B20AC83FF8AC90EA60134
          0709FD5C03EF3E4FE6CECCD87E9E047F7216CDC63979E63A68013AB8A12994EA
          345F0D117F272B5921066BAB1F993B75A114F0A54048B9F82424BEB1E6532D01
          934011C2520B4015A2EB32D616DAED0EA67E0F273926FDBC72EE34FF3471F96D
          7A7A62B0AD1374A7BB8D8FCBCD8D7C98D92012FF0AA074E8E8ED44A6E717521B
          23990D8949DEE4B708A48BAD0ABDAD0DBA53B37F61160698A5C3D3412ABF966B
          87E2C6819C4EA70DE2F7643680A0722D7EF5E0D56FABB560D071BA53871B8C46
          C3DF95A7C5E5EACA66005BBD1E5347E6D2DFAD5BF2754BA9E50A4128BBA8B846
          9CCAA34AB005E0B7373737BEBC99DC591FE35FC3E156166E7D7F5DD7563FE2FF
          13BF01227B86CDEBD5FE260000000049454E44AE426082}
      end>
    Left = 153
    Top = 253
    Bitmap = {}
  end
  object SessionImageList144: TPngImageList
    Height = 24
    Width = 24
    PngImages = <
      item
        Background = clWindow
        Name = 'Unused'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000180000001808020000006F15AA
          AF000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA864000000204944415478DA63FCFFFF3F033500E3A841A3068D1A346AD0A8
          41A3060DBC41007F4147D1A1B250B20000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30352B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30352B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39653835376365662D646338652D626534362D61
          6432302D3639333762346332656264372220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A383630313836
          36612D346332632D386434652D393866322D6233383730343431653338352220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30373730333232302D343838622D666334612D626264342D633136333438
          653036393465223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30373730
          333232302D343838622D666334612D626264342D633136333438653036393465
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39653835376365662D
          646338652D626534362D616432302D3639333762346332656264372220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30352B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E5AD28608000001A14944415448C7B556BD4EC4300C76DA7B01
          74BC062A30B223319C90780406069E80FD66844002DD490C2CB7221606467803
          D089C7282C6CC49F199AB4499A0474472355757EFAD9FEECD8552242438E8206
          1E232B5C5CDEBC82A41280601F11024012CC636BDE9CF1767D75BEED2980A0BA
          DF3F248110B4906821B0911904DDC860BB87EE5CBB0E020B4DDE17558F2266EE
          C0D980735FB673499C6BDE4C7105C1C14686AF34227BE05A88818802A06FB546
          67A90BA6D137C6A18A631EC0F540FB967AEB216D3656EC2A4D50E4024B2A0626
          9092A28D53142563106415C762008FAA28455E0CB40F9CA54A875425280273E0
          36F2E9AA139EF26F59642E9644C06C56A5A932699A0C7232439AE0C6A86A8D71
          9470EEA2E532241B03C730381475B508A0BD974557CCD82F6E4D4173E7A6C889
          B9ED006D9E3D1309513DDDE92B60E665F9FDB55538555101A4002A9CAA995BFB
          98EE364A49961657D986A394CAD6F5D9ED9DD7994E4F8EB31F58DCD19F1B4759
          AED470B21E3C3C3EB5567FD6B5B7B7311EB7F2D1E440ADED810BB852CB8C8DF9
          6CF67F3D3936344BB5AE0235F46FCB0F34028D9D809B037D0000000049454E44
          AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A31302B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A31302B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A66643665363663622D663733392D643834632D39
          3562372D6565663332316261363264662220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A376634663737
          63302D393637612D656334652D623438652D6265306164633966346561642220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30663264303138322D323631342D363134632D386437322D316430333066
          316564313536223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30663264
          303138322D323631342D363134632D386437322D316430333066316564313536
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A66643665363663622D
          663733392D643834632D393562372D6565663332316261363264662220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A31302B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EF92CC43E0000022C4944415448C7D595CF4F134114C73FD3AE
          A5B4945620442BA8473C196F5E4D3C78F15AE3DF604CBC7BE98D2B89E13FF06F
          500F90789090909090C80963BC80402052A52DBBB3DD791E76BBDDD9FE32180E
          6CB2BBF3366FBE9FF7E6BD9D5122C2555E19AEF8BAFE0007C05FAFAE28E1B5A0
          2CA002D7202BCB1B876FEB75CC65004A44E8AC5583B478CAE9E359A3539BAF9D
          342F05F0D7EEFC532B251B4EA287D80E1D81D5C2B3C33763015D3149280AE0CC
          3EC6597801D9E2B87090E6B70FCE28B1242C33799BDC628D4CFE16B4F6904E0B
          111F8C0F46876FD148642B0435FD70D10130919A9B7BC0F16F87FB8F5E922FCC
          D901190FBC9F48EB3B984848B40590842DC6875F1BEF7B008183A3060B4B4F99
          C86AF0F66D674908196DD9FDC0706C7C7F2B5C220328C5F9D901A599BB80B127
          2604D37608ECCF063C73D2F0B6E20C3415F2452193C9DA4289C992889868FDA5
          0F16CFD9AD3EDF6EC7003798A4582E87E2E968E2E28DCE2669AB40EFC45B8531
          D0BC3054E6EE4511246E4966A3A36C749F5F6C473083BFDE0388A2DD3EA7509A
          EDEF8C34302D2EDA5EBAAE8FEFEEC47B91218B77F18742A932A23306778A55AB
          3803AFBDFCE5C76E0CD094992A67FA231FD329C36BA5B7BA9B6308E8384C4DCF
          24966040A78CCCA60B8CC6986DEB3C68BB1EA59BF3433AC32EEEA002F7B2D120
          0126083E5B80891B0ECDC61162BCE1C5B4BEA5B3E989BB1A36378F3F59074E4E
          4EDFA9D6E9ABFDAF7BFF75C2A9F0975A7D52A7636DD7D7FA4CFE0BFCBC8E6456
          5CA8660000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A31372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30323A30382B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30323A30382B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A38646363316564662D656131302D666534302D39
          3939612D3461313332636534373035662220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A373061616434
          62342D616661622D303934642D383961392D3938646434653762326161342220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A63663066656532612D623136302D646634312D383866322D653531353964
          313337656230223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A63663066
          656532612D623136302D646634312D383866322D653531353964313337656230
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A31
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A38646363316564662D
          656131302D666534302D393939612D3461313332636534373035662220737445
          76743A7768656E3D22323032322D30392D30315431313A30323A30382B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E44EA93C6000001E64944415448C7D596416B13511485BF379D
          24A621A5429B80155CEACE75A582D5655782BF41FD1BFE0C7F8A8B40DDA87B37
          A282D09DD06ADAB4219937F38E8B4CA6EF8D33492A64E18381E170E79CFBCEBD
          EFDD319258E78A58F35ABB400C70FCC6C48FF6FB0F700A04E5DCE4D6D1E9D79B
          92FAB61B49A483BD81C4D3AA600383CBCBF18BDBCF87C37F11886740353980E0
          59A7BBF9C9BE6BBF742E5B2C1219F7E1E3CF2F401A082C4F89FBA6D57DBFD1EA
          41919DF2F7F90328E3E0A0710C1C0602365DCCDFDCDD67A37F082EBD26F4C975
          2D223B7AF2579187E306CDEE5DA2B899C7CD824DD4A0D37B48DCE9A3F3CF289B
          20674109380B2E01970458D4EA61CA02F1E61DBA7B8F696DDD2B6595A164881B
          7D9B112805D9D94E9C055954C2DCF847D0FB31C0E4E284DDF6364A4EF3AC2CCA
          B343B62090B30531B2682E54C62A0F9AE6C1299A932EC128619A639502CE7A16
          F836E4E44B30F9DF2EDA817C21F9B6D463926755ED5DE46C6E83678BABB0AA02
          2B6A80EA05A4D002BFB8812D7598DC92DBD4B3E0E656652B5CD741C16A2CA8B3
          6AA5795014CC232D0E53880556AD320F8C89209BE6DD10F6B74A3D2F7F676835
          81667B4B1767DF4DBBB35DD1EBE1D928ECAB29EAD58474A72C30BDFAF57A3AFE
          FD6A747612AF3EB74CD5A04991DE0651FFFD5FC51F0A0F2C142D0CC48E000000
          0049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A35353532316433612D613231322D363134302D38
          6231622D3931393133393536356637662220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A643162633438
          65622D333365622D373934322D383333642D3039383138643634626264642220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A37353432613130642D366635382D393634382D383939642D663539306332
          346335623531223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A37353432
          613130642D366635382D393634382D383939642D663539306332346335623531
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A35353532316433612D
          613231322D363134302D386231622D3931393133393536356637662220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E5A4F7E35000000BA4944415448C763FCFFFF3F032D0123CD2D
          303C7C9BA636306A6FBF4E5B0B54575DA6AD05D366CEFDEFEDED4594E2AD5BB7
          3110AB16A69E3E1664A62511A561FAAC790CC4AA85A9875B70CB8E052CA876E8
          0F410BC2B7F383F92B3D3F926F0148121980D4E0B2009B5A0C0B681E44C4781B
          66C1EF3DD2603EABCB53F22DC01744E8160C8E2022C6DB300B76D5FD00F3DD9A
          38C8B7005F10A15B30388288A616484A4BD3A4247DFEF42969851DA980E4D294
          6C0B8676A54F6B0B00231BD0AF29B8F2420000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000180000001808020000006F15AA
          AF000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA864000000204944415478DA63FCFFFF3F033500E3A841A3068D1A346AD0A8
          41A3060DBC41007F4147D1A1B250B20000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30392B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30392B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A30313237313765632D396237332D643234642D39
          3733632D6436656461373539313537352220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A373863326232
          32322D663730372D643234302D623732622D6233353031656563613162322220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A36663162316131352D646235652D646234382D613237352D636462303535
          353831386537223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A36663162
          316131352D646235652D646234382D613237352D636462303535353831386537
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A30313237313765632D
          396237332D643234642D393733632D6436656461373539313537352220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30392B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E67F167D6000003134944415448C7B5555D489351187ECEF76D
          2B9B619B53286C0ED2C4F53313222808BD30BAB0A0288AAEEA22B09B2E82A86B
          6FFB218232899022228812A48BCA94D545E5F0229394847E2C324AA773F3676D
          E7BC6F17DBB77DDF36D68F7460F09E73D8F3BCCF79DEEF7D053303008410F897
          75B5B3EB88A689CE848A6F3BD9D636927BAF6189CB61B79DF3F9D63A57389CE7
          0BDD0B43C1C54B575E1338C04420E3C70C2202E7EC8D3387C381DADA1A1C3EB8
          1F5DB7EE606C6C0C3F130990A2A12B972F340080CD6022A6C0FD96BD60629064
          B064904AC78A4032159332EE0847E56704FC3E084DC3C60D7E0CC38DEEC46AB4
          BEBD1D307033044AA92CB84A83AB6C6C4B4A1C9C1E435972111A184249E89A86
          9A75CD2045F0D7D761D3C828FCB630B4860674DEB839AB0B2D6825300166E354
          F64211CAE23134EDD8069FB71A36BB0E4D6820262822D8ED761C397400524ACC
          2F2E22141A744E4E4ED6674C564479599324705AD102E9B8B7B20EC117214C86
          A7208448FF27EB9922C26C348660F0392291D9FE647CBE31EB815981B43E9571
          FE55387177590DA837883D2DCDA8ACF0A4C119C40426C6E3DE3E4C87A770F6F4
          A95D9632554A598039578D41C22BF04078D1D71F84522AA5C2A440772CC7878F
          9F50D8E4821EE4549562ACB6333CAEF254E6E6F265867B55194A4A4A0A10983D
          9056E05C52BF1641B5B716D1580C036F46F16D6202DEAA35D85C5F874A8F1B15
          1E4F3E01296501234905CB9514C34511BCFB308E1F038378197763285E85AD73
          33F8F2F9293C6E174A4B4B8B28487F5896ACA5B5AA427A0516BE02AFE2EB114F
          0A9024F42C54A26FC6859DD1085C5FDE17F620CF54698EB3A44F1215A682A04C
          E545958E9EEF6E348D8FE7373BC36403D86270AEF1D2F46C32FF0989A8800744
          D8FEEC76B699296B734B3538F33EDD0899331F9BE74C106020DCBEA5E0130DEB
          C9B94D9AA9730A22082268A6522C7636DDDE9822050FE7B5EBDF0D9C8EEB5D6C
          DE9F387EEC8F2694ED4F078B4DD7FF69201555D0FDF05126EB9970D872E72A2F
          CFC4FB5A778B252B3003FECD2A4A70ADA363A923BB3881541C582A41C683FFB5
          7E0189611E4D127944F90000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000180000001808020000006F15AA
          AF000000017352474200AECE1CE9000000097048597300000EC300000EC301C7
          6FA864000000204944415478DA63FCFFFF3F033500E3A841A3068D1A346AD0A8
          41A3060DBC41007F4147D1A1B250B20000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site color mask'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A36653164346332612D656433652D616334372D38
          6261392D3362336539646632333636622220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A643663636265
          34352D383139362D343134632D613634662D6538646566353839613264312220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A33623162336134382D363331332D626134332D613831382D383935383265
          333438326266223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A33623162
          336134382D363331332D626134332D613831382D383935383265333438326266
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A36653164346332612D
          656433652D616334372D386261392D3362336539646632333636622220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E6733DD78000001154944415448C763FCFFFF3F032D0113038D
          010B8CD13761EA857F0CFFF5FFFFFBC7F00F8681BE03D1FFD1F8D8C450F87FFF
          5D9C32B1C7006C30288840B8BB6F12944539CEC829F80FE3C183E8EFDFBF540B
          967F4866D1C482BFC0A0C2B4004990620BB0F9E0DFD00F226A5A80D507548C83
          7FB48E83814945B48F036A2653A4D06041162C4FAA431466FF6105D97F784106
          2A5BFEFD87158690428E01A90014293F00716C8F0EA605401F5C7ECFFE411767
          C989C4C725F6A6571722C7F0FF32CC5C465885C3C8C888D7DBD367CF47A99932
          5313F16A8099CB4274C5C1CC4C567CE0F5C1FA2D3BE0AE7EFFF62D8A9CA0B030
          9C1DE8E3C148B10F900D24ABCAC406664C9F4EBD3A191BF8F3F7BF3EA51630D2
          BAD902005F1E546642A1E71B0000000049454E44AE426082}
      end>
    Left = 279
    Top = 189
    Bitmap = {}
  end
  object SessionImageList192: TPngImageList
    Height = 32
    Width = 32
    PngImages = <
      item
        Background = clWindow
        Name = 'Unused'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200802000000FC18ED
          A3000000097048597300000EC300000EC301C76FA864000000284944415478DA
          EDCD310D0000080330E65FF450C141D21A68DACEA5080402814020100804822F
          C1020FCA5FC1C10107C40000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30362B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30362B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A63356330343664652D623635352D636234392D39
          6336342D6337633135623930333537392220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A656430316130
          31362D363664612D656534392D393466392D3533353864363738613663652220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A36303736326532302D353838622D346334622D613230372D626536376265
          356566366639223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A36303736
          326532302D353838622D346334622D613230372D626536376265356566366639
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A63356330343664652D
          623635352D636234392D396336342D6337633135623930333537392220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30362B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E2E2CB383000002244944415458C3E5573B6E1441107D83F702
          C892EF63CB015A70E02310107004133AB7B01062914072E40C890456C8F239B8
          0164964FD0EF39D8E999EEEAEA9E315A6B03465AA9FBCD6C55BDAA7AFDE92461
          97CF33ECF859C4C1FB0F9F7E4A5A52824848423A1EE63D968EA7DE95EFB9FE78
          79F1320B80E4F2DBF109004014140486712CEAF178104480D95C38F97DBD2C4A
          4032732E0260E28082021C1C3EDEDB906B83650F6CD21DFF840D23EB2832B558
          8623F9A5591A317A0188CC0D4A1B460105130CAC4D19FAC00BD60677032059D4
          8F86613446CF81976E8BF7369848BF0C2049A1DF030DD6A12CA387D733107216
          63F7F606E5355F3BDD7E865A25F018D263A26A93B9DF66A56DAA206AD7AB2B46
          AD17B8CF7AC01395B57BA07700ABE71ACE1A2E834FC8904686AED669B41E1E83
          272AA8AF03230B784D36E026AD164F587B78430515AD87544233F08435BDBE68
          96C0D6DA480BF4B55ED4DAC563D02D15285D034A25B0B2C9B455636CB45550D1
          AFD57AF037AAF6FAA1A91254D6F5E6D69A6F6069BA19FCA57C5A86C1D77A891B
          86262078DBB3096091CAF0F0F6EB7084B247AB613CF54E1B07DE38CED31E5864
          193067377B2E9C1D84991F9CDF0C0EFF9C1D426E00D25AE4725B4CD3EFFFBE3B
          1A704010B08E7EBB184DD775B38FD2AB2F57EE65E2ED9BD7B38D44BF8B7F3ACB
          EFED6DED5E302B03DF7FFCCA18DFDFDDB9DF3DDFDFCFE6A7AF5E744F9201EB68
          2B37A3D6F379B57AB2AB59F7DF5F4E771EC0034C93A31480BE18800000000049
          454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A31302B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A31302B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A36313233623332322D366338642D383634372D38
          6638322D3836393434313835616165612220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A343937353032
          33302D313430352D363534612D623562332D6637623161363061613266632220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A35613362653335642D616638662D343434662D623538662D336338396635
          373162346166223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A35613362
          653335642D616638662D343434662D623538662D336338396635373162346166
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A36313233623332322D
          366338642D383634372D386638322D3836393434313835616165612220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A31302B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E789F8CD3000002E04944415458C3ED574D4F1351143DAF9D3A
          A5205F0114A884041313B7B272635C484C4C5CF803DCB962E33FD01FE04AE342
          FF841B890969AA0B232B97EE8889600A114C0CF483BEF7E61D179D4EA7F3DE0C
          45312C649269DFDCDEBE73EFB9E7DE9911247196470E677C9C07701E800700AA
          32FB5C40AC12C21990009A8678E6AFD49E9C2638490892D095B9200D3CF18707
          FECACE9BD30C200700838003402E275ED7D7A62F9F260B82245465FE44D388D1
          47F4155DD0CA1210A026F0B27477E7B193816C9A7AA709CFF83553AEE3BE86C2
          23B19A2AC238585F96299926A777966F8F15E1A506607842B0D0961B9A45F1EA
          2AE0CF0C56BABDD8D6ED1F3F014C7971902CB0F832972FA150BE0F6F621968D7
          C0D637801AA0068D8ED6A0068CB27E13082086AF17459701B207D60A2E625FCE
          63F1C643F8A5A9945C0240EE03ADAF60A0002A1B3CB6662C08180D5283079FD7
          F3AE127CDF2796966FC1CF4BA05D0BED41940D1D00DD4D419562B7D93054957C
          9C01000802E2A8758891F158ABC7C098D8C402313A6223B23B4A426A18CA754B
          8475E96174EA4A67F8822900FD99D2C106A852EC1D7F41B95BBCB9B149261868
          B47D4C94AF75C08FD9C8555B5B882AC51E54FBDAB02BC2C386C4C2F452443313
          943A959E28091D6C24ED2650EB8939201004843106C5E1B14EE626BB95922549
          EB804E09FB9308A47A6F31D094058C4D2F1EA37495D101196CC40215D45B4377
          BE6C590134640133E5A50E488A782C1097D2BBE54B49C2407EB49E880C817AA3
          89D1C9F984D25528C470DD050FD78CF9F4FC75C25FF5F953056BD6BD402A83BC
          E7A370C14FB498CAE8809309B4CB9492F5AA15C091F6311ED67FF056FA038142
          6F8EDCDBDBB502684981B985F231335D0D3CEB5347344CD5793B6E341B189BBC
          F477B3DE51923E8182A011EF9C8FE543C512EABF767AB33E21AC9EA8B425AA7E
          7FDD135B5CA0208E24F869A3F6D6C9C0C4B07A75B8FDE1D1C1B6F807EF090202
          34A47871FB29B4F3A1F4BF7E33FA0D96D715843AED1B470000000049454E44AE
          426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A31372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30323A30382B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30323A30382B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A65376262636163322D303963322D393734312D62
          6261392D6137323164643232343363622220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A643833396133
          64312D383761642D326634392D393862332D6633353363353335636231622220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A37333330343964372D646363392D363434662D383634622D656537306637
          633265653135223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A37333330
          343964372D646363392D363434662D383634622D656537306637633265653135
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A31
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A65376262636163322D
          303963322D393734312D626261392D6137323164643232343363622220737445
          76743A7768656E3D22323032322D30392D30315431313A30323A30382B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E9516A97F000002104944415458C3ED97CF4A1B5114C6BF3B9A
          44FC0362F10F29EDC6859B2E5DF802820F50E84388E80BF8062E0471E33B74D9
          55ED33140A5D09822084821045136D72E67C2E329A99CCFD974470A103819BB9
          DCFBFDEEF9CE9C396348E235AF04AF7CBD03BC034C0240F757FDD0103B84B102
          19E041C183DA66637F1C31DB136748424EEBA94BBCB001F8B5B6D9F8FE920049
          6F6393C4F9654EEE7E2CAEBCB805D1270016AAD54AA3F3B33E9488018506C700
          F6C60248663E63E2E337606266D8E04FEAF5EF5D27C0EDBDF10B27154C2D6FA0
          B2B401DE5F82720BA800EC0214F069AC0250B27BBD39A8C04060E6D6DC1670F6
          0B6A739F30BBB20E7B3A10903BB0750E6A07609A09A5A0F6C7FDFBC5FFA40037
          7FDC00EDABBFF8B0BAD53BD9D306B91390FD71419C36F13C547F8EECBA0B11C9
          6CB3ECA7F98596D3A9455C7D50D283F25642DAA8C50325A53541282F80DA4F0A
          47989D50036B0A50E108D8160E245408CAB2E6192ACA02F5F8382414F3368121
          8040665BC32FB9E41D144F83E2A508D0163ED7B30E8F4D4CB3B00B408DEC074A
          BEF93CCE0B14D794A0621B127728C50A45060023C4CB49A8BE50DAA01C454B53
          AFEF511678934B03450B3A424F3858365DC9E5B249D368DF232B614455540BD4
          285DB129D48198FAEF48BC51DBF2EAF43C5ACD8B5C28D3C04B49CAF53F9074AD
          074F4FF8BFDD3CEAB49BDBCD7F67637C27185F4FA8048EE76D736FFEEBF811CE
          1E741B096736330000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A34613731366165332D373031362D636134632D62
          6131352D3433626331646136613364352220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A666666353564
          36382D373633352D643434362D613832652D6337643265326361363432632220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A36393063653464622D343033392D326334372D613966612D343731316131
          383032346437223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A36393063
          653464622D343033392D326334372D613966612D343731316131383032346437
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A34613731366165332D
          373031362D636134632D626131352D3433626331646136613364352220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3ED1936C55000000D04944415458C363FCFFFF3FC34002268601
          0603EE0016106174ECEE80C4C3392B6546B003FE7CFF3BB021F0E7DB9F8175C0
          EF810E81A20FA719BCBDBDB0C7D3858B0C4606FA54950381AD5BB70149DD4192
          0B40404E5A02AB025CE294C8112C076ED9B180315D43001F983E6B1E8658665A
          12C97230711400AA0BA6CD9CFB9FDE006A27F628F09E2202C6A351306051C070
          C11B8247A360A0A24079E925301E8D82018B825D753FC078340AE81105F010D8
          B075E7C03648F0359F6801B63E7D3A883A26884622FD01E368E774A01D0000EA
          E2CCAACFC5B1CD0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200802000000FC18ED
          A3000000097048597300000EC300000EC301C76FA864000000284944415478DA
          EDCD310D0000080330E65FF450C141D21A68DACEA5080402814020100804822F
          C1020FCA5FC1C10107C40000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30392B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30392B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A34396639643961382D333233352D353234382D39
          3638332D6239643636316239633332612220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A653538313761
          31632D376636352D343034312D396236622D3837633030363933623032362220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A62666561656338392D656565322D343234612D626164362D336662393265
          396538346630223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A62666561
          656338392D656565322D343234612D626164362D336662393265396538346630
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A34396639643961382D
          333233352D353234382D393638332D6239643636316239633332612220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30392B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E433B2F2E000004204944415458C3C5575D681C5514FEE66777
          3671F3B3BB6121A509A98DD154DB12147D35D142BB5AA12808820F852AE8C3D2
          577DF4C1478B165DC18A82FA66A9D636B169F5C19226282A1849BAD9A6291A8B
          2DC4A49364369BD97B8E0FBBF33FB34D35211786B9F79BCB9CF37DE79E73EF95
          98190020491236B39D3CF9F18E78B3F29B222B9F1F3BFAF2F1A87932B6A83527
          E3C777EFEA6963F0AB270A859EA87992A5C089F73E38CFCC396206138199E1EE
          DBE33AE6EE877D7BA0B717CF1F790E33A56B18BB32813FE7E75DDF69F8FD77DF
          79060054CB1322CA7DF9F46100001383058384D367E20DE37DB482C7945B48A7
          DAB17FEF23F875721AA3FD4F60452860621C9EFA2217080111798C3301209701
          62B040088E007E5059C49EFE3E101162F1181EEE7F104FE296EB1F6487C05180
          5D3F24D418F90D594CEB58D2ACE0D0EA1F48D13AE21243AE9A90484006B07BD7
          20A81E8E7D7B1E42B158C4FEF8225892210F0CE0D4A79F9588E8B4ED00133946
          A8164FB858BB95409D75E79A8E2EB18ADC8121689A8684A6415664A8AA0AA69A
          A3C40C2DA1E1A5175F40A5B28EAA102897CB989BBBD15B9C29BDEE098127AE82
          416EC3C251C252A0A8653029B7E1D20F6388C762D0340DAAA2D69953ED6D3F0C
          5555A1AA0A4AD766317BFDFA822968480D38608520720D7817DF70BC1B77CCDB
          30CF9CC5A10343C8A4D376461031989D2CB9B3A4636C7C1CFAF232D68DE5FBF3
          F9BCEE554078A526E15D7CCC618B8F71191D189177E2DCC8452F6BAE31B79E8B
          DF7D8F49D181F12BE3C8E7F37A200B8272B337FE221A2B5513A8480A882D8314
          08453299C4E5BF859D715E077CEC2864F1393807F0565181D6D414CADC72A223
          93C6DEFBD640F5E217AE40DD003C0C118DD79D6CA735B4B5B4D8EC674A257CF5
          F537989ABE8A6AB5E650369B45B75AF628E05B840E3B0A0B818D3BB5C2C2DBB9
          82CE7416866160E2C79F707BB98CF3EBDD78BC7813A5D9393C3AB00FA9F6145A
          6905EC52C057071C7670B10EE24EADB0F04EA58A555DC7D99151FCCE199C5EEA
          8210C00475A24F5D813EF10B7674A4A1282A128944840221B9EE973B0AFF995A
          119B5FC2056327E6CD265B3922C674B9196F1B3D18D4FF4197A6C1308C0621F0
          C7DA4E39F6E1DE305CA5164C5592815A61CD3785840B8B69B0603C659A0DB280
          DD352098090ECE3EBC51D6F8FED1380B829B0FFB5947D58ABBD60F2BBCD4C081
          10D61CC13AB051B1B76493082FE50D1C4060430A652D2218FA1C826F6F715299
          C2D370F0D229FBF8E53F76D9FDBB7DE39A81B0BE35A6B03A4021673BFFB970C3
          4EF8C6D9B7466D837FBD31185E8888799889729BC5D43DFFE69B43360E301818
          0E9C8AEFE55E50F8E8130EC35F7BE5E83D5F2ED4FF72E6571565D3EE0F1B52E0
          CCB96F3D8C17171642E7A53219CFF8C8B307A52D51C06FE8FFB40D39F061A1B0
          5537382704DBD5646C73DB7607FE05BB883E57C63D2FD30000000049454E44AE
          426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed (unused)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200802000000FC18ED
          A3000000097048597300000EC300000EC301C76FA864000000284944415478DA
          EDCD310D0000080330E65FF450C141D21A68DACEA5080402814020100804822F
          C1020FCA5FC1C10107C40000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Site color mask'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A30332B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A30332B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A39343232313331642D386632312D623134362D38
          6631392D6337323236323261323632302220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A346162616335
          61662D363034332D393834342D613863342D6663336362656538343366652220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A62663436393134302D336330372D653834302D383235372D646362666531
          643662636634223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A62663436
          393134302D336330372D653834302D383235372D646362666531643662636634
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A39343232313331642D
          386632312D623134362D386631392D6337323236323261323632302220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A30332B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E788D8DF3000001284944415458C363FCFFFF3FC34002268601
          062C3046FFA4695B81A1E1F50F1822FFFFFD6300850C321BCE878A21B309C961
          CAFFDB366542AF37D86298829EFEC9302534C799B985FF613C7814FC03BA8C5E
          E03F925D0807D03131FEC3E680FF740C01AC0EA067142087F6C03860A043E0FF
          8047C180E782119F0646CB81419A0D4773C14875000B72C2C8CA2D42B45BD09A
          56FF90DB34F8E440CD2FE4A618121BCEFFF71FD301FFB0B4EDD0DB85443B028D
          2FD6B40B6EE1D34A4706E496380B52CADC06D4E8452D9F22AB7F56E504170786
          3510326C83D9CB08730D232323D171387DF67CAC7936333591684360F6B290D5
          966766A65A82242A04D66FD981E2E3F76FDF625527282C8CC20FF4F160A44908
          A05B44959E113E3063FA749A95098C23BE733AE00E00007E0F25AE6405CCB400
          00000049454E44AE426082}
      end>
    Left = 407
    Top = 189
    Bitmap = {}
  end
  object ActionImageList144: TPngImageList
    Height = 24
    Width = 24
    PngImages = <
      item
        Background = clWindow
        Name = 'Login (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30372B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30372B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A31353863343637662D386264352D623734372D38
          3662612D6334346361383535383165622220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A316633373630
          32372D333432342D346534652D383565652D6339383062313864346437662220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A39623233373661322D646536322D663434362D626437332D343163393335
          663731353335223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A39623233
          373661322D646536322D663434362D626437332D343163393335663731353335
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A31353863343637662D
          386264352D623734372D383662612D6334346361383535383165622220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30372B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E81F0D21D000002844944415448C7B5964B48945114C77FDF63
          46747ACC88DAC347DA221219525C54842D6A53062DB2562D22A5A0A876416814
          84106EDC6450540441AB0A49C955AB24C128A420AC28A51C4BC3B27C91E3CC9C
          16334EF77B8D43E807977BCF7DFCEF39FF73EE399F2622ACE46702D45F0FDDD1
          D01B116D7950354134B9D573FAE7491340436F2CA88D63E480C320B18EC56B4D
          9163F330F9C63801242F4034747F0ADCE5902C01981EA664C39FC44C5364D99C
          022CCB0BD354DE81A199AE2CB879AE7FE211DDA3ED4E1F80424DAADF1E3AC4A6
          BC6DBC9F7E4E4CA2DE7CA7F697AFAA6647C111BA221E17D8B936353F001D1F8F
          31159D70055569B950D5C51A5FA1C334DDC2B5DA94F98A400D25B9958E3DB834
          11410D7DDD4291D27E45BF3132F79658224E51CE66AE849F71B0F83C3A861514
          70BC25F1B040D5B067B48396D7BB985D98040143F3D150DAC2E5F0538A73B7A6
          35151107BDAAEC70F2FE8D6708FAD65914DA90BB253DAE08D4D05ADD4BE7481B
          DD23ED24246E55DEEB82C5C9BAC2A394E455667CA886E6E370D9456AF30F70F3
          C32922B3839630F7B0E03F73920DD00E63DA37F78EDF67ADBFC841514DFEBEB4
          1C97053A3FB7D1F5A59DB84A914B2630ED8B4F22D71CD1B0B3A8217DC1F0CC00
          37DED968C13B5F59C2540DD5FAE2B35CADED23608610496AFD60B8954BAFF612
          991974C67F564E565E68D0BF9ED240153A06E3734334BFAC4B02BB24377B2ECB
          2A17A90787A606DC33AA0B4592D1074A1F4F2C0070AEEA9E35D979045BF9EA30
          93F363999DAC6AD9FFFD31E1D01E026630AB68FDF1E72B7D630FBD2DB05334F4
          7B80E617BB3357B82CAA9CB958431351D07D2ECE7391972AAB89D8BF493375E0
          EEF427F3F8B2157D04B4C4EDA4EE2BFCDBF2172EFE4FD0192697B30000000049
          454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          383A33372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35373A31332B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35373A31332B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A62376464656563352D363133342D663834632D38
          3339632D6334396534313866653833372220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A613966643766
          30642D316639342D396434312D613738642D6261383364326366343332302220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A33376466363938652D643837662D653634652D616130382D373433633863
          336532376632223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A33376466
          363938652D643837662D653634652D616130382D373433633863336532376632
          222073744576743A7768656E3D22323032322D30362D32375431353A35383A33
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A62376464656563352D
          363133342D663834632D383339632D6334396534313866653833372220737445
          76743A7768656E3D22323032322D30392D30315431303A35373A31332B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3ECC276AFE000003254944415448C7AD955D685C4514C77F77EE
          DDDD449B74EBD20801C588818A0644A80F45A16015257DD117E98B46FCE883F5
          A12ABE685F144AA1A022428D15143FA00AF5835AAD9F2F0541210A9A526D89A1
          66AD1877EDC624EED79C737CD88FECDD6CE88D38309C61983BBF7366FEFFB901
          09DAFE83CFFD82DA88A8A2AAA84823767551C59AB139371B2501A8E8C807B7DE
          8D7A45BDA1F566F48AD6AD6BBCB2667CFACD11970420229828E60DF3ADD8E8CE
          D757366DC74617EF4906F03EF6A17A23E5CB3C63CFF246B03B066C25A1DE1011
          1257D099614EE6391CEC6567F60C3FC896AEEC1BD1D60F687CB8C59FE6EDF011
          6EC895F1F50A47ABE3AB8EA6055A7705B7EB17BC967992E16C1F2E08F87929CB
          746D1495E6A6D20112C38B10250378F6C82B4C648E91EA1F04A05E59E6C8F23D
          ABB3979563922480F977376F98AB7EC9F5FD25C2D4256082612C946B1CFF677B
          EC623B2FB80D3870F0F933AA3A1A3391192AC2C6F402B5F4278C6D2EE15C0426
          804355F86E6E9845E9EF29CF16484488BCC8682F138DC98FBC143DC560E42108
          315302008C5AA54AB9EC78ACB08FC25286993F37C61CED864AA4C7CEB394BF82
          682D13DD1C7C835A40A99E226355FA220761043852A9903BC7F288183FE51D07
          3EDB1A03F8629ADAD4107E29381BF532917AE5453FC10BF57B31AF1CCB3EC035
          A90B600A182E7028CAF9BF37316DF771DB8E4BD9FDE044D0EB0EA36E13C5A4E6
          8D6DE11423D11F60216080A356F39C3C9BE3F7813D68141201EF7F74C200EEDA
          7947D003A02B5AEE949A180F0F1EC139C34C01A8563DFBF3BBF8F5DB0285E2D1
          8B4ABCA382D56A180D66D99A3ED5586942A912B2F7DCE39CB870133B64924F8F
          7F185C14E0457ABC920DD043D977700ECCE0B74A96FB679F667AF1AAB6C693B4
          AE0A3A1E342B32BEE12466F0FDD2D54CCCEEA350198899681D80D5EFF9AE818F
          7166BC57BA8527CE3D4AB51EA21D6E5D1760FB5787630E56553257E6792B752D
          9F9FEE639B4EB6351E6C5A2473E31CCBF3C34901FE94AA5DD7FD9FFD7AE6F2E6
          383E6FA514F5A921FC829B490258530593AFBE6EADF15A26FA4F809661FE2A16
          DB7397E572F43251A223EA9E78F9D021FECFF62F082057A0FBCF61E300000000
          49454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename 2'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31352B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31352B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A64333662336564322D663465352D666334312D38
          3761642D6363633231633230646262352220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A633162323234
          35632D653164372D376334622D626638652D3235373130663536393561632220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A33313861353936382D386236622D653034302D613262612D306366633731
          636335636433223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A33313861
          353936382D386236622D653034302D613262612D306366633731636335636433
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A64333662336564322D
          663465352D666334312D383761642D6363633231633230646262352220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31352B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E4282ACBC000002674944415448C7AD564D485451183DF739A3
          E8385259866452CC90495454607FBA0A852CAD562ED28D8B5484C08D8B56462D
          A4459B7086263468978B82A259843141F9038185599981FD48938EA666FA7CCF
          FBF3B5C897A68F7AE3CCD93CDEE370CE3DF77CF7F24044481626EB7C57C66AB6
          3F18A9F416000011414B96F8789DFF280979491866051962E07D89D604203906
          9148C4959AEBBBC57553E3BA01B1B0980AB0D3B062240ACE79F3F4D4A4F8D8D6
          4C1F4A3D345CACC9B725384844891B2C3C3F94BF30D1D335FA35AA5E0E0CD2D3
          7B9D66FFF903416BF18C88C0185BB781EA2D7AA4A459AE6BBB065FE362DAC48C
          EE999B9285D5D5E5B3091B889EA2B320711FD20009034A9872489C39B7BF3CF0
          30E104B1C89ECCCD6919EF9434F268C9809108BB4BBF9DB238098D69765AE655
          A5781E290E280E46D210821A57F3D66560F61DDEC7483462499C940080CBE927
          C73E256CD0D202CD0D6A578ABBC8122735343C1DBD6EC78FBB03D97BA481A419
          803440D20049138C50EC2E8B76AFE6C6DDC1CFEE63398C442B888396B7E6B69D
          F85F0982A10E47A7ED44FE13F8BC6F40D200840943A4A073B409A64CB7E5375C
          A865AE152FFF569F7D0139D80648AB580ECFDE56D49655D9D283A18EB5255B1F
          ADE74A8C84EBB1A8CFFFD91A6D6311586ED57F53BB56454230D4B1260D9F8B22
          36D28F1FB11C783D1C3B767A915278CD516F8E4A9E1F7FF57BFEE762F81E9B81
          9E5D0F64F81D19B89C90BA1E87E1D6B3B06D93067FE51D64F92B1C4F9EA304CF
          FA3F23FCE53876D7F461431CE2B6255B3DAC842F7F2B6E84EE22734B41FCF70A
          112170B39D928D25CDE50EEC46331960C9FC6DB1C32FE933A92FED0E839E0000
          000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C069545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35312B30313A30302220786D703A4D65746164617461
          446174653D22323032322D30392D30315431303A35312B30313A303022206463
          3A666F726D61743D22696D6167652F706E67222070686F746F73686F703A436F
          6C6F724D6F64653D22332220786D704D4D3A496E7374616E636549443D22786D
          702E6969643A63616230316161342D323534382D356434392D386561622D3965
          383465353632336465352220786D704D4D3A446F63756D656E7449443D226164
          6F62653A646F6369643A70686F746F73686F703A63623031313465312D383033
          332D613034642D393934392D3332626439653464303332642220786D704D4D3A
          4F726967696E616C446F63756D656E7449443D22786D702E6469643A63383138
          663133642D363233392D316434612D613665382D323932616264656638663936
          223E203C786D704D4D3A486973746F72793E203C7264663A5365713E203C7264
          663A6C692073744576743A616374696F6E3D2263726561746564222073744576
          743A696E7374616E636549443D22786D702E6969643A63383138663133642D36
          3233392D316434612D613665382D323932616264656638663936222073744576
          743A7768656E3D22323032322D30362D32375431343A30393A35352B30313A30
          30222073744576743A736F6674776172654167656E743D2241646F6265205068
          6F746F73686F702032332E35202857696E646F777329222F3E203C7264663A6C
          692073744576743A616374696F6E3D227361766564222073744576743A696E73
          74616E636549443D22786D702E6969643A63616230316161342D323534382D35
          6434392D386561622D396538346535363233646535222073744576743A776865
          6E3D22323032322D30392D30315431303A35312B30313A303022207374457674
          3A736F6674776172654167656E743D2241646F62652050686F746F73686F7020
          32332E35202857696E646F777329222073744576743A6368616E6765643D222F
          222F3E203C2F7264663A5365713E203C2F786D704D4D3A486973746F72793E20
          3C2F7264663A4465736372697074696F6E3E203C2F7264663A5244463E203C2F
          783A786D706D6574613E203C3F787061636B657420656E643D2272223F3EFCAE
          D9DF0000021A4944415448C7D596416B13511485CF9B84496CE8424962DB4D82
          822EDE9F88B87021C5450C5270D5A5C54D662FF4074C16055D4A916E0411B4BA
          10178E3F62C05010C6C6201A105A24629A7B5CCC4C3243329D4CB10B2FB37DDF
          79F7DE73EF1B4512E71906CE3B4822CCC26DC1FCBE59DBE96FAC95CFC23A6C55
          B7BDE6251DE54E32705B302BCBB597807A982BE49DAC22BD56D506F92847C3F1
          D67D9158892AA5DA7D52DD0EB2D186B9B848AF55B549B64142C0B2CAABC73302
          D55DEF29C80EC292915AE58D5491283C38E70E4FFEDC9DDBE4CBBB9E25C20E88
          A98842A2C87CF8A8717DFF7890E8A2D5678716453A2001FFD38038FDF5E57256
          78A24D57F7BE5A10E90407414263A9E474039145E100A0428B2AA56684FA1B2B
          36897658322574097E24F0200D3EE19E260000FD7B2BB604B70509FAA7536F1E
          72532779EDF9376BEAAEC5E0D95785880F47080728FF6817F59A659B50D18606
          E95317556ED2F83309C4E07EA75D113E41C45D052AA7DB48163132DCDC1D9E8C
          1AF5573FB7C87167DA0FE8C205268A18C970B423E58835B4FEFAC802237302A5
          CD229D6E03E55481291C73E161D4DF1C59E474AD90D4797369462436073370A1
          3B94D3ADF8F956C906D09EB80BE28E47BF1BD73E7010CBE04BB3B299150E0057
          DEFD0A7617022340E78CE28B99121D7FFAB107E26D16781857DF0F2DC8D85F90
          C201295BF39F4C0DD3BB737127CDDB497170D3DC3EB861C69E4CF5DFFF55FC05
          808408935CED5A5F0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35313A30362B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35313A30362B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A30666666666465612D326536662D633434392D38
          3731662D3330363162383938343563382220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A373162356333
          36392D636365622D383334622D613339662D3030303732626633626137362220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A61323733326539612D343338652D386134332D613930642D623761396335
          323865356662223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A61323733
          326539612D343338652D386134332D613930642D623761396335323865356662
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A30666666666465612D
          326536662D633434392D383731662D3330363162383938343563382220737445
          76743A7768656E3D22323032322D30392D30315431303A35313A30362B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E952AEBA8000003314944415448C7B5954D6C545518869F73E7
          DE19A9D399A1ED50D296694D2046AC4513A225FE84465C4862A20488D1952BC2
          82AD1B376E8C0B8C181340E302572E342871A3068B31114DC41249D45431C152
          95D23699B16367CAFD39AF8B990EF35B44E46CEEBDEFB9F9DE739EEF7CDF3192
          30C6F05FC7B1B74F3C177378CBB7FE83870E1C986E9E77B8C511F7DCC3C323B9
          E43AAFEBF576F3B76470EC9D138F2612F1ECE33B1F334213478E1F1FF95F0D32
          DDC957B68D8D7AC67118DD7A4FBC3B917CB9F91F23896072E00D230E09D36068
          A064D19BAF9EBDF2523A7D2495CAF44D46D26663F0A2304AC4628EF3FCB3FB70
          5D8F52B9CC07274F298A2239B1D835647DC7C4BE3492083F1F889A8337ADE293
          8F2FEF39B8C8F6E9471EDE71C7486E18D78BE118072B8BB5C25A8B248220A4B4
          52E6DCB753D1FCC2C2251760ADE00032E6C9A7863FFA6D7E658ACFBE09497527
          E9EFEF27B216598B55C5C04A2C158B7C75F66BEBFBC19995E5BFF61A495C3B3D
          A8B50C9C3B737843FB2196E46ADEF0E9798F89895D64B37D95C056585964C5C9
          0F4F91CAA40EEFDFF3F48B002E40BED8BE0E1CC72339F010898DE3A834838202
          591BB0339762F28CCBBEBDCF60255435B05674AD7359B832FFDE6A0C17C05B3F
          4AA27B13C98DDB69A4250896D0DF175154061B221B50B6197AFB7A6BECEB11A5
          D369F285F2FDC0F73583D2E28FF4DE3581FC45B03ED800C9AFBD579E2152000A
          B8541860784B8EA562915F7E9A62F68F39060787D87CF77D647A36109F5BDC0D
          BC5B3390846C5409A61014540387354D75DAEFF92EC2995FF9EEDC3CF7AEBFC0
          EEC169A60B5B993C3D4B4F4F1664C61A1055685483DB00D51BA982A5DE7C6CC3
          CF783ACFAE2D17F04C09ACCF78DF2CDBD25FF043619CB970C76B0D8576F9FD21
          6D7AE005B07E154D701D95F5E1663459E24FFC695A5B85822A86B01195DAA0EA
          A4C9766E766A091AB468CDA81AB588B5BB6995FF6A225597935AC29B34D56B9D
          8AB4F656B7BA06541D34D54E5BB866C7759A4F911A56DB5E536D1761A5186F64
          605677A0D63A501BED3A167BC33BC30188776558CECF5498FE5B541D92BABCD2
          F8ED02F8A5FC51BF943F98BF7AF1266E38D34691151C6DB9D16EE770B8CDE31F
          51BB8474296C16210000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open Workspace (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A32613032343865312D306433382D663634392D38
          3938302D6539373731356132326564332220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A343838346662
          34302D666232312D333134312D396161652D6364303733663564313764332220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A38626364323830322D636137372D633934312D396230622D376137613464
          373532663838223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A38626364
          323830322D636137372D633934312D396230622D376137613464373532663838
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A32613032343865312D
          306433382D663634392D383938302D6539373731356132326564332220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E58D96DDC0000025E4944415448C7B595CF6B134114C73FB38D
          46DBA68935146DB58A48D153F11F1005F160F1E2CD93D04331A2E0CD6B6FDEC4
          834510F1BFA848F150442906047FE1A18282B4D2D2B4B6B649F64733CFC36E37
          B39BCD561AFA6099F77667BEDFF79DF7665689084A29926C74765EE8C03E5D1A
          5199B409F555974E2D95E07EE53D6363D7F6043C3DFD923BBB11000C0F1DEB5C
          81FBFAF86325DC1394657E7CFEC31FE72FFA798CBCD9DE1B4112783B7BFAEC05
          00A589F1D08FC7A589F128C16EE066E6E662D34F8A538B2C46838E3D29FA85BB
          5B81DA77A87E83F406A4746380DB571F4D674CB010530C3FB0338541589B0167
          09EABF90ED2A221E680FB4EB8FE22241AC1054DFE8492522D8334302601F3CCF
          F24686D3176E92ED2E4619B403CE6FC459011D00891B219058ACD00F32005AFC
          AC1797D63971EE0AD92E179C8560B2876817C4008AC5AD84BEAF3DAFEC6F9106
          9462F3CF22B9FE614047171A80F1D8274C56B3B26E9743052E050EF50896D515
          0532168B913192A24E7BACD5730C5E7F570B09ECC6617AF2791F3C964DB378E9
          6ACC78B59E07C002D01AB6EA9A42F1549081F188A9C60DD4B82DF3C238203BDB
          F7F956934014B5DA26DDB9A3AD7B19278C834B6CEB76E678F6C7F0A069BA70EA
          7FE9CE15523A23B95322B50A1538B5876F7F7E6D5E76E4E9CD5BAD99FF47A7B4
          3968E5C9497493603B436F5FBFB105099D92AA668730F0D11F76CEA70550B31D
          724706DA7446B4B849056EAA71411AE846633642903D90A1BAB18468A77D3123
          EFE26A9AE0B60B7373CBAF22B769562A536C554A0B5FE6AD4EFE5ECA3F525397
          2709EF772522ECA759ECB3FD0343C685B5ED8F8F570000000049454E44AE4260
          82}
      end
      item
        Background = clWindow
        Name = 'Open saved session (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          373A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35323A35322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35323A35322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A31633132643939642D613832312D363634372D62
          3764322D3262633365633839663939312220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A306161313632
          30372D643935642D613834332D613732372D6436383730333137663664302220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A33353934303365352D626236632D353534622D393536342D646466366661
          643162663132223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A33353934
          303365352D626236632D353534622D393536342D646466366661643162663132
          222073744576743A7768656E3D22323032322D30362D32375431353A35373A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A31633132643939642D
          613832312D363634372D623764322D3262633365633839663939312220737445
          76743A7768656E3D22323032322D30392D30315431303A35323A35322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EEAD50C42000002B74944415448C7B595CB6B534114C6BF49D3
          344D1393B48D342AD522A22EDD892B0B6E44DCB811FF0671ADD84D76C55D51FA
          17B873ED42C182825A282D14AD1B3745F1D197DADA26CD9DC7392EEEDCC99D9B
          A4358B0E0C77CE7D7CBFF9CE39C315CC0C2104FE673C7A3CBBB13390AB30118C
          3630C6D82BD935C11081ECB37CFDD75A1A3D8C9D4CB63277F92A8C24902418C9
          3092C258915B47CF6F7C7A3A96EA0560B40129062986510C5204D2E1342A84B8
          A9195A6BF404D05A7B424647EB38D4AE25416B8D3400C857D519C1B8C7101E50
          004D02CF4CBFFB3955AB81B475E009454E626EA2E91C7412070006B202E2C1D4
          95EAF38D6795BC51DA17899C4400E9C3955221A093B80712E27AA9DCBFABB576
          29890B916618CD0E1A015D8A3A8AB273E1165A1BDC2EAFE3FEF86FF4F7650EA9
          186371F15C08E824C609586AB08A87B7C63051F902D4BF82751DCC0A2005900C
          AF2CC13616608CE4CF8700B26ACDCC45ACEFA471E6D21D6473A3FE8628C044F0
          035CDF04C80AB1F4001C8B9914DEBCFF5017CC8CC6CB930C0656B7AB3875E11A
          8A95B300C87B191C1322E9C5ED40EB46A9C93045044008ECFEF98EC2F078BB78
          4C301987C0763740409BDBC1824B914409D921462AD5E70BC53EE6D88E61F3CF
          6D30F7CDCA899B4B0D07689A410C158BA1787237AE7807BB89C7C2C86500E139
          2002F6F609A5D1D37607B1C97137D2BA916DEFB9D8C2086AAE056081466317B9
          C2487B6724814971967EEAA277547319806D53F421D8FF8B5CA174406774E914
          F2BB29741034A6DFAEAE38804411F962AA7DE787744AF75AC9855A0DD402E834
          F2C7866329E8D02907BA8980760D5A8ACE670A001ACD0085F2F12E9DE117B753
          815B6E24C00664CC6B0F30D09FC6DEF61A9882EEC5F4EE25DDB4C49B12989F5F
          7F1101D20090E1AD27A2BE75F7DBC7CF3DFDE1924384476A76B206EDEE31338E
          72A470C4E31F7BD97F581DC883D40000000049454E44AE426082}
      end>
    Left = 278
    Top = 253
    Bitmap = {}
  end
  object ActionImageList192: TPngImageList
    Height = 32
    Width = 32
    PngImages = <
      item
        Background = clWindow
        Name = 'Login (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30372B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30372B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A65636131343664622D646465632D373534392D39
          3863362D3764326363383363343461662220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A386363313266
          32632D353062322D333134642D383663312D3437653763636561313838612220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A65343639313862652D326436392D613334372D623633302D643632386566
          303561373138223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A65343639
          313862652D326436392D613334372D623633302D643632386566303561373138
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A65636131343664622D
          646465632D373534392D393863362D3764326363383363343461662220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30372B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EDCF80636000002714944415458C3E5573B4F1B41109EB3CFA1
          49C031883A4D085D14050A1A224A0ADC23615BA92327F02F2015512A0A1E3620
          7A37E922819090B03B24249E150829042B168F82831B760F3F76EF76EFE62E20
          0AC65E7B7D3B9ED9F9667666D64044784A8AC11393D9980CFF4C8DC6E246016C
          23F1B826A365DF62EE57BEBA226D802B4FF6DA89B6D77597A0F39649E52D542C
          F9F05DD78C446D2F5660537903DCF21749044F486044851ADE443B8288B20944
          2BC3A0214D91180312B36BE79FDFFC80C1AE2C18EC15966CB885D53F4558389C
          083E051C7EAC5B8B8D1DB01155F9BD82387CEACEB664230501D7B386F2DC6692
          043F08061407FE399BD0F199D4E0F10B4851A19690928844172008BE1084B091
          7F5B845766AAC9871A3E698388CDE11B034AA5A25CF6D1974AC3E4FB0AF4B36F
          8F4294157A5008748102D6EC46D2C3F6922190EF2942A55A82F9837138B7AAE0
          5B57C82E08012B278EC6D4870AF4758E68D1904E981F02E2228F5E2A7134BEBE
          5B84721743639FA371E64D5E5404545652A9BF330DDF3F7234D2FA531114030F
          D61FB8E30F09A9F87F9597FF96606EB7EE0262C1D266C2CC7A87B6082D0DD6A4
          4717375598DDFD069BA72572B1F23D86A4522B583DCBADBE3EF357886A51EA53
          E0E26C583CB6DAD15C72ACDE7159AD2BC9D4185065409510C7EA9DD671531521
          6A2232A3743BD35B99E0C643F13F0C8A014A45C3104116DD051E41E8F404CB43
          B5882901C3654204B926AC9D2CDF0B89A8FCF7F182540F48B540DCD4CCF61767
          840E3242334BEA8A31445487E1955DC06E2CD6B921351E1EC802CAAC9447DCBC
          F5B975C97418687910E0D7A5ABA37801F091AF664CB96D63AEF5F3B9DF8EEF00
          650D926C6D74A3450000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          383A33372B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35373A31342B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35373A31342B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A35613533336431302D616233652D643934322D39
          3238372D3331343039316235356635642220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A363031346234
          34302D643037332D356634312D383161622D6330343830313838376132612220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30613334616537392D376439362D356234352D393033322D343531363064
          626565626461223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30613334
          616537392D376439362D356234352D393033322D343531363064626565626461
          222073744576743A7768656E3D22323032322D30362D32375431353A35383A33
          372B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A35613533336431302D
          616233652D643934322D393238372D3331343039316235356635642220737445
          76743A7768656E3D22323032322D30392D30315431303A35373A31342B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EB20532F2000004564944415458C3BD975D6C544514C77F337B
          B7DB7629D4168B48425224F80506DE7C50311223F12BF0A0092426B63E107830
          9117216AC293F040228941507980178801D298A0E24762824A40E5C9484542F9
          10C5425B2DED7677EF9D39C7875BB6BBDDDD2EDDAA934C6632F7EE3DBF3973CE
          F9CF1AEA68BBF7EEEB13914E11A152F72288F7886A3C567B47E442500F80F7BE
          F3E0634F95AC8953249278748244E3A3D38979C99AF0FCD943F7D83A01CA8D3B
          993416F7CACFBCF7CC18A09A51E322DADC40E9335F0A346300F59577DAEA0679
          2F7A9523B29E45FE42552F78E7EA07B8655CBDA04E51178F4BA2737C186E6245
          CB00C658FE722D958FC50BAE5E0F38EF2BBAFED1E804BBFD663ADB1224828053
          6E39D7A3F671C01872F211D49705CE951856E7E90AF7D3CD419ADBE69148A618
          1EB8C6E1FCC632D76B51AF1FC0FBC207932ECB9BE1DBAC4A9CA669F65D189BC0
          4721D7F2694EE6564C9911330450DA5D3F3BC2B778B0E1371AD3EDF14355F299
          9B7C9C7B86C8D99A695837C07D512F3BA3ADCC4D39928DB341158C827A72B931
          8E8EAD9E483BAFA5F399C6C0D23BCFF3863F4053731336680255143008613EC7
          77B9E5F4876D25D9A115EAC1B401B66DC36E79F8EEED62BE25994A636C125400
          03C6A06AC865C7F828F37445974F0672CE11DCAEB00436E4B9CEE398C415920D
          CD182C20A0B171D420A20C8E5882133FB0D27F4FB56FDA79C3343E748D91EB0B
          086E47583AA27E76C916169B2B98446ADCE5F1CE0D26F600967C6E8CD3971610
          39A96ADC7B0F030DF8331DF8B1446F504B580297E7806CA4DDFC1DBB1C89036E
          DCA81A8B51C078F2B99027EFED63D5923E5421276970373B3BD7FD79A9DAB106
          B584C53BC355E6838CAF29A4C9D0683DC9862640D0712FB4B6B680B180210C23
          DC70C84FB3765EEC39361780B5CFAE365501AA098B739697A25D85358D844F5A
          BB59941C2A8B81D81B42369B6768AC99730D9BC99BB95306763095B0DCEAC540
          4F04275998EC2FCD8082A478463221BD5753BCF3C5426EE63EAD995941356129
          ADDF13405DB38E923006558903D058504181D1D190C8C9A139775C7EF9F0D15F
          C3DB49ED60B2B014407C39D032DBCB8A542FC69A38130C1815446134EBF05EB7
          CE5BFBC78EE9D4165B2C2CB5AE53AFA48F606D5CEF5101159CF3DCC824F8E09B
          6574AC999EF12280E29D4F9EC7E37CD3CFAAE653D8421C2BCE0997465B79F1E7
          6DFC78B9BD9EAA3E11845A562A4B2F105DAD3D04560B3F0C1D9C195E4CD72FAF
          7323DB42C7A47A322D00715274612C0FC0B46478A1E5ABC2EEC3087A061EE1B5
          F39BC847B6202C330098FAFABC6ECE67A413395421F2B0FDEA7A765F5913D785
          2261A90BC079CFE39FEF9BA8D793FED1A87A36ACFD1A05B2A165EF8907387BF1
          7756CABB65C2529F079CEF1595FB4B008AC423B0114D09C7D0688A9D5F2EA5EF
          461A115F2E2C99E07C3D00A6D60BEFEFDBAF8D890C5E03BABB3718FEE556F183
          3DC78E17C27D6870B0B0DED63E916A9584A5AE23A8F542B1D1FFA25504D8BB67
          0FFF57FB0705F8407F2141035D0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename 2'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A35332B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30383A31352B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30383A31352B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A64376164643831632D363934622D376634652D61
          3531302D3166366537386463646263342220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A613362353662
          38632D353064642D623634652D613136642D3830353762313638633363642220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A30373566303232642D313163322D316434632D383663352D653661333361
          616139373832223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A30373566
          303232642D313163322D316434632D383663352D653661333361616139373832
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A35
          332B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A64376164643831632D
          363934622D376634652D613531302D3166366537386463646263342220737445
          76743A7768656E3D22323032322D30392D30315431313A30383A31352B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E57F6F33C000003404944415458C3C5966B48145114C7CF8CBB
          338B9AF9A10C3535C927E9D607D755092C24CBD2FA262118615B160841646550
          5A4952099550414FEA435182457DE88161D9EA6A254192A46B45A9CB660BB6CF
          59E7CEDC69C674F199EEAE930766EE9DB99CF3FF71EE39339710040108820039
          CC521A5781112A420EE7D1C807962793D747B4E502F8A98B5507906407EB6414
          C8E906CEC93C442EA62CA9054CB20354019065A52BDB39864D432E3720270312
          84A8F836F10DAF951DC0DE7CFFE4F08B1BC7868D1F47C519C01C2F295E4AD4E3
          325901EC767B184D5146ABD51AE2787C1970C305E0EC7649CE3CC4E2C48C76B0
          C90AC0FE7A59EBA2D20E586D7642BCC0D1F75550D5D71081DD86C2243D5F2F6B
          11A2D6CC1C12D846964EEEFA1E54A1B43AD878ABCD06ECE0C0FBFCED451A59BB
          E05BD30A550CBDA413632E0E300B028F7823ECD0F7B8346A9683F4C26D9B7A65
          05E0DBB42744D1E3C08BE222C0D8C8E2A0F2E08DC6DAE97CE60DC0DD9A194711
          EC27CC216ABCB878EB310E99525715022B2B0036685E611E658FA6FEAF384662
          70BC4E996B7E3D93DFBC00706DDA6282E7EE0823E292F0E82808B7A95CD3CE7F
          F9FA0D30D4B4267431ADECC6980D1B4BBB244E08FC6FC60DF12105268BAC00BC
          21FDBA80D1AE898587A44D29A136986FCDE6EF17006AC9CA24C961BDC0217252
          E1194EB798D6565501960DA0A90914D92ACD07B1F052C6F65C122730E2301256
          D39BCD5D7389E333006FC838240A9F995C7862A8B374AEE9F05CE3F804E06AD3
          2E57615E2C3C1408FCB0A7F0C4C90F8B1B92230A4C2EAF01AE5CBD29CCD5292F
          F62944057E81F1A9977AFEB9B918FA5C095E6572DF9E12C203203ECC5E78038F
          80E93C0241C1D484D493617910A0BEE695B8A8391560F4A5679C6C9FEFE580A3
          BF0592522341A9C040123C10240D8AAC66003ADC7F80710BD33A755C5C269E6E
          0647E664000929A94B41955A0D6494CEEB36F61A00394CD0511739E15D847A2B
          446F69002002E407E832DC85FE673A08A119B16B4808CF3808D1EB6BA45CF8F4
          15F51AA0AE5A078DEFCC909FD00B85A5E72034AEC0AF3FA8D700FB771780DD85
          A1B2B2126212D2FD3EC0789F81533AD85B7E1E28D5A279393FFAD486F3693366
          E07FD914005800F3002CA42D38C01F6015AFDFD166334E0000000049454E44AE
          426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35313A30312B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35313A30312B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A34316432306432662D336336652D616134332D61
          6566612D3865396438623861373262342220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A313434306434
          30612D373166622D386234632D623631392D3235653339306666306437612220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A64663039366435312D636230332D383834642D623866372D333665346265
          656463613638223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A64663039
          366435312D636230332D383834642D623866372D333665346265656463613638
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A34316432306432662D
          336336652D616134332D616566612D3865396438623861373262342220737445
          76743A7768656E3D22323032322D30392D30315431303A35313A30312B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E5C26A033000002634944415458C3ED97DF6A134114877F6711
          8B14299692B416FFB4173E4811A14890D222410A227D8599A738F30E52E88578
          2722052F7C1029A14D884B5A10A4E446E8FCBCD8DD6437D924DBEC96DC38B077
          3BF37D9C39E7CC8C90C4224780058F850B80E4E04BC6D5D196F63E3C335531BA
          0735D3D95FD33CE65804AE8EB694A401508944F7A0663CA88098CEFEAA4EDD82
          043E8806A9E1FB27A6349C00409062DA7B59898C80F7BE471220001224208486
          87B79718C219C3E3753D7A1305EA1F2F1C3C6C342192000008353C7C6CE68323
          05A77DFAE5B79B5A05F5E30BE7493B3619A2E1BB7553257C62196E1C771C3DEC
          101E672D020D9B93256E0B9FDA07364E3A0EF4164CED1F098A68B7593355C067
          36A28D935F0EBCB148E543B470A0DDB743890C9CC5E1517AA51A9088E4FE1436
          D70D4534BB2580785A00C8C0311B9E61161100806EB366C040472B24922A0E1F
          15287C166C7EBA74A0B765E1A50EA3CDCF974E3C5389590E3EE769E8E3341881
          93F0777D1C770FD68C87681E9C0004D4F3C68AB913815970245B02D1F3C64353
          A9C044B8A7F5BCB1099C71898281B65E1793B857069E24DC7963058068FC0348
          4220DADA5DC6F669DFCD1D8122700078FEF58F03A28E99BA4B4010686B77D9CC
          2550143E94B8769EDE62AC42445BAF264B0485E0057BFBF6B76B471F47021C24
          26413D7BF9C01412C885A37893D93EED3BC2DBD1F922C89508AA84A72590DC27
          807485E8D9CE92997C2724EA65E10389EF7DC741DB4ECA13F0607DEA71DCD95B
          55424C19787A44618F6EC69EDEBDF8F1D7669E83790F93F6DE236DBF59ADEC61
          72B6B3647EEEDCCF7D98C8FFC7E9A205FE01F9F6931E70061754000000004945
          4E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431343A30
          393A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35313A30362B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35313A30362B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A64383865646435642D333436642D643734612D62
          6338392D3266636164616164326565382220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A323936333166
          35302D333362392D306134612D626164332D3430633662653439353738332220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A65373331333537332D376232652D376634362D386238302D633438613238
          333764323330223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A65373331
          333537332D376232652D376634362D386238302D633438613238333764323330
          222073744576743A7768656E3D22323032322D30362D32375431343A30393A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A64383865646435642D
          333436642D643734612D626338392D3266636164616164326565382220737445
          76743A7768656E3D22323032322D30392D30315431303A35313A30362B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EA742B5CE000004574944415458C3C5575D681C5514FEEEFCEC
          6C7667D3CD46368DD9483489313F52A5058B14C4D6D6E20F7DD0D0A8080A225A
          30F459AC2C283EB6420979AA50B13E14B5F4A5C45AB06A4C29880F9262D3A4D9
          10539BAD8D4936C96E6677E61C1F66939DE9EECE6EACA617863D7BE69E39DFFD
          CE77CF9D11CC0C2104FEEB71FCF889FB7D01F9375992BF78EBCDD70F979B27E1
          7F1A01DD77B8F5C1962D0C7EFBD8E060CBA60288C7E31283DF79FCB16D4A4F57
          A74FD7021F6D2A80D68EAE1742A150305217C6B6477B6422EA1D1818D8BA6900
          34D5F7495767874444507D2ABABB3A25D51FFCB8D45C7137223C3638D8525B13
          FA4C08F190102260595688995500F26B7DBD505505C48CF44A065F9D39CBCC4C
          4288AC22CB0B16D30A117DAD0040F642E3A782F11E439464440069021FD5F6DE
          3CE25AA95076092176EDDFB747D5340D7E4D83244B5014054C0C22063143F36B
          78E5E0CBC230B2B2695935994CA6269198C2D8B5F143924D43F9E400C0404040
          7C607CD7F892D37F6BF6C69744D6A98B3F0E1B3E5585A669506405440462B27F
          D72F86A228501419E313D7717D72722E67D16EC1CCC85D68E26A2817C0DF4636
          D7AD3FFFD7ACD37FE2E4A92342E0C3FD7BF728F591088819CC3603CC64DBCC58
          5C48E1E7914B60815FE7927F3EDDDFDF9FDA1000AF91587D02C3B70FE0D5BEBE
          0200663015EC6FCE9C454343F4E48BCF3DFBC65A9CB2912452F001C84D070139
          5874AF3103F82E4B20C7CAD780705E0BBAAE6336993CED8C53006029E3BD0B24
          4985BF6127D4E84E7066066C2E0164029C03D8049389C5D41604FD4FE5EB5F48
          CA9C07438CFBEA23C8668D0300CEB900B0DE032DD40C7DEB0E94D62203E63278
          25012603600B6013600B4CB69D3234E87A6D5EF984F1F1098C5EF91DEDED6D68
          6F6B030044A351CCCCDC78B2A811A56F5F811EED069B4BE0EC1CD848DA2B4D4F
          819727404B5741E9293065DDC91D766A35083D1C453A9DC6C51F7E42626214BB
          9BBFC7E2CC08CE9F1F42F25612E17018AB86D1118FC77D6E0698F30FCB5F64AE
          DBCC05DB4E6602E44E0EB690CAD58116973134740E8FD44F627BEC5B08CEA1A9
          E132FE586EC1F02F4BD023CDABB22C8BBA86D8C30046DD2274262895BC0894E9
          8A69AD1DC3D882847DB11144FD37F340ED39B1C0187A6357F179E2FDA34296B6
          2B644CB95AF1F4E91837EF38E45861415CB62F977F986DAFFBF3F3ECB93947D2
          420C3B6CDF33D3A2FC61E4A2D949B1BB04B6E8AC227F51999CFFD7623C4F43C7
          642EF3D08D82626799C09500142BBB9CE20B9A301DE2BD33B955317911035C8A
          3EC75E77F9E15126B6F2B49B00937793BB93814229BC6AEC4CE08E2962A4527B
          5FCFCF55D4D1B33F94106135E78B4B84E445652950659A16599E75AFAA049EE2
          A20A4D0B54FD095BAA13327988AB5C99C8DA10F5C500A8424D4BEDF552311B1C
          52FE55CBB1DFABE9FF65FAC3BF181200F80261ACCC4F3BA8B42A1C4AA61B5415
          A25B592DEDB75FCBD3F303D9F4FCBBF3C96B77F1A1223CEE30313050F6C3E45E
          0E09F778FC0315CC5A7BB061F64E0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open Workspace (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          393A33392B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431313A30363A30322B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431313A30363A30322B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A63383731383462622D373264642D346134622D38
          3031372D6264343766383964383066332220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A656237336361
          36312D383736652D326234662D613964312D3831343363363662633735382220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A38386132333666662D643432392D316534342D623261622D633535313539
          373039313066223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A38386132
          333666662D643432392D316534342D623261622D633535313539373039313066
          222073744576743A7768656E3D22323032322D30362D32375431353A35393A33
          392B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A63383731383462622D
          373264642D346134622D383031372D6264343766383964383066332220737445
          76743A7768656E3D22323032322D30392D30315431313A30363A30322B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3EF33BA52A000003294944415458C3C5574D6B1351143DAF9936
          E987FD32B585D6520854716145BB72D1E2C28DD9B9547021284405FF81F8035C
          694114ECCE7F605CD4D2AA148B8B8A15DC941621482D3445FB913499F7E65D17
          C92433EFBD99269A92BB7A73E7CDBDF79D73EE9D194644608CA1169BF8B04668
          90AD4E8F33228255CF43851D1B8DB6BA0A78B8FD19C9E4359FEFCBD7555CBC30
          A1ED0DF203403AFD16F7CAEB1634D9AC7A1F181D1E0ABD3ECAAFDA3F21B03665
          616DCA6A1C02F6BBA1A70CEC3E81190B62405E129EBCFA610EF2FCE5ACE64BDD
          BD6DBCE7FA7D0584250700023A18C323F77AFCA3080D5AEBBD0A0561C94D969C
          892339136F8E088F85823023C3EC4B3FC8368C022B2819290B6F1D89D7DF0000
          1B37CF37860249C1C9286CFA8BDFC09FA5BABB39757D10A9E46C1640DCF22651
          93915A4DD9366E9C05F656809D79A09001F17D800440022445650D1280E4DA3D
          0607ACF35C8CB9081055931DCA13C816873176E916A21D7EA5A7260164D3001C
          C0CE02C55F208703C4F5E49E35798A80142012A0BD95B98889829FDB84C4E434
          A2111B286E96FD4EE5346448E00605F100BF8E86243E1FF12200008E43281CEE
          A3ABD733C73DC94809A22591A28246C56FA0844840923DA789F0C0B6D01D3F5D
          1ABE20437237100FF0578B30FB4BFB19D95BB1CBCBEB440A02B962147D23674A
          C98F0864E25617220FF03B0BBE367445B89FB3313A90A8C04C0AA446A52B9490
          010DD52F1D3EA7CC0106C721482911EBEC299DBC86405E4A823AA0A405FF211C
          9B2F6A08E4ED56F40C8C1DA1741ED2012168780A652432ED57BF67B40272762B
          4E8D244A4902C4A3253129DDA52FE01012F692F63A96041CE4F2E8EE1F5694CE
          CB422CAFA50701E9F113F7EC17CAFEEA9A8883B893D6BF88B844C48AA2B52DAA
          B4180FE980FA04EA22C5ED8305AD808268436F99FFDA5B2978D6070A1462BD2B
          B9BDA5517068B7A0F7E448054AF2C1EA0DA4C0EDA184A4EA2FC32EBD743AEF8D
          AFE35C3E879EFEC1FF9BF5064A7C0205811C96367E13B6C73A90DBDDAACE7AE5
          A45551094D54FEFD5571FA040A42C1067D5ADE7C6344A0AF93BFD8CB2CDED9CD
          B063F85362602049C49E5D790CA1DD252234D39AFE6FF817BCD4FA1399CD7790
          0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open saved session (reduced alpha)'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B000005C969545874584D
          4C3A636F6D2E61646F62652E786D7000000000003C3F787061636B6574206265
          67696E3D22EFBBBF222069643D2257354D304D7043656869487A7265537A4E54
          637A6B633964223F3E203C783A786D706D65746120786D6C6E733A783D226164
          6F62653A6E733A6D6574612F2220783A786D70746B3D2241646F626520584D50
          20436F726520372E322D633030302037392E316236356137392C20323032322F
          30362F31332D31373A34363A31342020202020202020223E203C7264663A5244
          4620786D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31
          3939392F30322F32322D7264662D73796E7461782D6E7323223E203C7264663A
          4465736372697074696F6E207264663A61626F75743D222220786D6C6E733A78
          6D703D22687474703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F
          2220786D6C6E733A64633D22687474703A2F2F7075726C2E6F72672F64632F65
          6C656D656E74732F312E312F2220786D6C6E733A70686F746F73686F703D2268
          7474703A2F2F6E732E61646F62652E636F6D2F70686F746F73686F702F312E30
          2F2220786D6C6E733A786D704D4D3D22687474703A2F2F6E732E61646F62652E
          636F6D2F7861702F312E302F6D6D2F2220786D6C6E733A73744576743D226874
          74703A2F2F6E732E61646F62652E636F6D2F7861702F312E302F73547970652F
          5265736F757263654576656E74232220786D703A43726561746F72546F6F6C3D
          2241646F62652050686F746F73686F702032332E35202857696E646F77732922
          20786D703A437265617465446174653D22323032322D30362D32375431353A35
          373A35352B30313A30302220786D703A4D6F64696679446174653D2232303232
          2D30392D30315431303A35323A35332B30313A30302220786D703A4D65746164
          617461446174653D22323032322D30392D30315431303A35323A35332B30313A
          3030222064633A666F726D61743D22696D6167652F706E67222070686F746F73
          686F703A436F6C6F724D6F64653D22332220786D704D4D3A496E7374616E6365
          49443D22786D702E6969643A35666139643532332D663164312D313934632D39
          6636642D3831306633656538336662632220786D704D4D3A446F63756D656E74
          49443D2261646F62653A646F6369643A70686F746F73686F703A376661626362
          32352D623465302D386534312D616532642D3766393630653631636338372220
          786D704D4D3A4F726967696E616C446F63756D656E7449443D22786D702E6469
          643A62303930386230612D633231362D343334342D393933612D613965363832
          623064663465223E203C786D704D4D3A486973746F72793E203C7264663A5365
          713E203C7264663A6C692073744576743A616374696F6E3D2263726561746564
          222073744576743A696E7374616E636549443D22786D702E6969643A62303930
          386230612D633231362D343334342D393933612D613965363832623064663465
          222073744576743A7768656E3D22323032322D30362D32375431353A35373A35
          352B30313A3030222073744576743A736F6674776172654167656E743D224164
          6F62652050686F746F73686F702032332E35202857696E646F777329222F3E20
          3C7264663A6C692073744576743A616374696F6E3D2273617665642220737445
          76743A696E7374616E636549443D22786D702E6969643A35666139643532332D
          663164312D313934632D396636642D3831306633656538336662632220737445
          76743A7768656E3D22323032322D30392D30315431303A35323A35332B30313A
          3030222073744576743A736F6674776172654167656E743D2241646F62652050
          686F746F73686F702032332E35202857696E646F777329222073744576743A63
          68616E6765643D222F222F3E203C2F7264663A5365713E203C2F786D704D4D3A
          486973746F72793E203C2F7264663A4465736372697074696F6E3E203C2F7264
          663A5244463E203C2F783A786D706D6574613E203C3F787061636B657420656E
          643D2272223F3E1C2DFE50000003804944415458C3C5573D6F5447143DB3BBF6
          DA6BE30FB089310EB26524240A5340459320014D22217A9454A952E41F90FC00
          22A1840628E9A9B0906C6428A250D14113392E205923E1C2C1BBB6F7CDDC7B52
          BC7DBBEFCDFB58DB8B92919E66766634E7CCB9E7DEF7D690843106876D3FDFBD
          B7A1AA0BAA0A5585A84245A064D8B7E7E38F247FFF79EFEE9DB3151CB189C8C2
          E3EB37A08E50A7616FE363EDAE597A63C5D7AF1F2D0240A90F02A044073349A4
          D3E7AF8908FA22E044A08EA028E808BAA80F9F5E84FA2620228537F5094524A3
          3D9F88807753E92D7D44C6B509540020589DF9C5C07C4F984C4206D855E24EF5
          7AFDC70E01E70E1CEFACB5840245E00040A0660C6EB7564EDDCC0F81072AC584
          12048AC0E3AD54320F1ACBD333B921C853416284A40F0F90981A1C1CD86CADCE
          324A433A767BA7EDB17AF37103862424EE811EA0F150A4D2F060B9CF98014372
          990432C1980627C3107CB9FA302CAB5EE9958C321C3DA593FFA0BA54C7CEE65C
          9780B218CC332440E0C4F036066BD3F8E1D6128E8D8D1F3A8D1B57FF960E8108
          240FCC57A554AEE1F6B713A84C5E005A7570FB0F800EA003D575C6A003D4A6D6
          0C042323E7F7130422B03D3986ADE034E62F7E836A6D2AAF0C01C116B0B7018A
          0568D3E0B1316324A00EA4033FBE5A296785E0AF2D62F1D217A89603A0556FCF
          4BE736CC00880E056DCE7C5A0DA57D56F6432042ECEFED60746226A67B178CDE
          212910751D353AF31921211D94C14ACA848DA082B1A9CFC3E20BE600246FCA0C
          35409B331FEE370CDE0F5D7EB94E7A0A345B554CCE9D0BC17B1C9415DBB4116D
          CEBCAC4502274CB8D30C70667AB123333D49339DEE8584196AF8F32A76254140
          692042A82A8646C6C39B6B712AF921C9CB803084C94B48609FA714D80D06303E
          3DDFC3E9B620030AD4881135746F87AFBD799B22D00C0670726E3104C9314F0A
          24CBE951F8722EA1087E4BBC61A32C6834773176FCB4E774DB36627B1C81B7C7
          8CEDE9EE77DE7E9BD84F2BCB7102E1179155942B550C0C56BD14B30519703883
          464AD9A0B19622B0EFAA9868C7FFE0A9740483C2AD8F7EF5E17D8AC05E60307B
          66AE474DB707AEF5B9251ABAE6BF552A00D0DC6D62FCF867FDD5FA8C90240C0A
          826A9EA63EF3006078A886C6F666B7D67BC6EA9ACAA54C95DCEFBA668B1B14C4
          7E00FEFEB2FE245381C9117B7FE7DD8BEF3EBE3347FE9F90DF0C0CA8A4F9F5CA
          4F70A955FA5F21FF712BE17F6EFF0212AE295F693694860000000049454E44AE
          426082}
      end>
    Left = 406
    Top = 253
    Bitmap = {}
  end
end
