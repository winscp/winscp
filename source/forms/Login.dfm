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
  Constraints.MinWidth = 660
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  TextHeight = 15
  object MainPanel: TPanel
    Left = 472
    Top = 0
    Width = 401
    Height = 387
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object ContentsPanel: TPanel
      Left = 0
      Top = 0
      Width = 401
      Height = 353
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      DesignSize = (
        401
        353)
      object ContentsGroupBox: TGroupBox
        Left = 2
        Top = 8
        Width = 391
        Height = 342
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'ContentsGroupBox'
        TabOrder = 0
        DesignSize = (
          391
          342)
        object ContentsLabel: TLabel
          Left = 8
          Top = 22
          Width = 35
          Height = 15
          Caption = 'Name:'
          ShowAccelChar = False
        end
        object ContentsNameEdit: TEdit
          Left = 66
          Top = 19
          Width = 316
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'ContentsNameEdit'
        end
        object ContentsMemo: TMemo
          Left = 8
          Top = 48
          Width = 374
          Height = 284
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
      Width = 401
      Height = 353
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        401
        353)
      object BasicGroup: TGroupBox
        Left = 2
        Top = 8
        Width = 391
        Height = 258
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Session'
        TabOrder = 0
        DesignSize = (
          391
          258)
        object Label1: TLabel
          Left = 8
          Top = 69
          Width = 61
          Height = 15
          Caption = '&Host name:'
          FocusControl = HostNameEdit
        end
        object Label2: TLabel
          Left = 280
          Top = 69
          Width = 70
          Height = 15
          Anchors = [akTop, akRight]
          Caption = 'Po&rt number:'
          FocusControl = PortNumberEdit
        end
        object UserNameLabel: TLabel
          Left = 8
          Top = 116
          Width = 59
          Height = 15
          Caption = '&User name:'
          FocusControl = UserNameEdit
        end
        object PasswordLabel: TLabel
          Left = 198
          Top = 116
          Width = 53
          Height = 15
          Caption = '&Password:'
          FocusControl = PasswordEdit
        end
        object Label22: TLabel
          Left = 8
          Top = 22
          Width = 69
          Height = 15
          Caption = '&File protocol:'
          FocusControl = TransferProtocolCombo
        end
        object FtpsLabel: TLabel
          Left = 178
          Top = 22
          Width = 60
          Height = 15
          Caption = '&Encryption:'
          FocusControl = FtpsCombo
        end
        object WebDavsLabel: TLabel
          Left = 178
          Top = 22
          Width = 60
          Height = 15
          Caption = '&Encryption:'
          FocusControl = WebDavsCombo
        end
        object BasicS3Panel: TPanel
          Left = 8
          Top = 192
          Width = 374
          Height = 31
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 11
          DesignSize = (
            374
            31)
          object S3CredentialsEnvCheck3: TCheckBox
            Left = 2
            Top = 3
            Width = 245
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Credentials from AWS environment:'
            TabOrder = 0
            OnClick = S3CredentialsEnvCheck3Click
          end
          object S3ProfileCombo: TComboBox
            Left = 253
            Top = 0
            Width = 121
            Height = 23
            DropDownCount = 16
            TabOrder = 1
            Text = 'S3ProfileCombo'
            OnChange = S3ProfileComboChange
          end
        end
        object EncryptionView: TEdit
          Left = 178
          Top = 40
          Width = 204
          Height = 23
          TabOrder = 4
          OnChange = TransferProtocolComboChange
        end
        object TransferProtocolView: TEdit
          Left = 8
          Top = 40
          Width = 164
          Height = 23
          TabOrder = 1
          OnChange = TransferProtocolComboChange
        end
        object HostNameEdit: TEdit
          Left = 8
          Top = 87
          Width = 266
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 255
          TabOrder = 5
          Text = 'HostNameEdit'
          OnChange = DataChange
          OnExit = HostNameEditExit
        end
        object UserNameEdit: TEdit
          Left = 8
          Top = 134
          Width = 184
          Height = 23
          MaxLength = 128
          TabOrder = 7
          Text = 'UserNameEdit'
          OnChange = DataChange
        end
        object PasswordEdit: TPasswordEdit
          Left = 198
          Top = 134
          Width = 184
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 100
          TabOrder = 8
          Text = 'PasswordEdit'
          OnChange = DataChange
        end
        object PortNumberEdit: TUpDownEdit
          Left = 280
          Top = 87
          Width = 102
          Height = 23
          Alignment = taRightJustify
          MaxValue = 65535.000000000000000000
          MinValue = 1.000000000000000000
          Anchors = [akTop, akRight]
          TabOrder = 6
          OnChange = PortNumberEditChange
        end
        object TransferProtocolCombo: TComboBox
          Left = 8
          Top = 40
          Width = 164
          Height = 23
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
          Left = 178
          Top = 40
          Width = 204
          Height = 23
          Style = csDropDownList
          TabOrder = 2
          OnChange = EncryptionComboChange
          Items.Strings = (
            'No encryption'
            'TLS/SSL Implicit encryptionX'
            'TLS/SSL Explicit encryptionX')
        end
        object WebDavsCombo: TComboBox
          Left = 178
          Top = 40
          Width = 204
          Height = 23
          Style = csDropDownList
          TabOrder = 3
          OnChange = EncryptionComboChange
          Items.Strings = (
            'No encryptionX'
            'TLS/SSL Implicit encryptionX')
        end
        object BasicFtpPanel: TPanel
          Left = 8
          Top = 166
          Width = 374
          Height = 26
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 9
          object AnonymousLoginCheck: TCheckBox
            Left = 2
            Top = 0
            Width = 182
            Height = 17
            Caption = 'A&nonymous login'
            TabOrder = 0
            OnClick = AnonymousLoginCheckClick
          end
        end
        object BasicSshPanel: TPanel
          Left = 12
          Top = 198
          Width = 391
          Height = 0
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 10
        end
        object AdvancedButton: TButton
          Left = 274
          Top = 221
          Width = 108
          Height = 25
          Action = SessionAdvancedAction
          Anchors = [akRight, akBottom]
          Style = bsSplitButton
          TabOrder = 15
          OnDropDownClick = AdvancedButtonDropDownClick
        end
        object SaveButton: TButton
          Left = 8
          Top = 221
          Width = 108
          Height = 25
          Action = SaveSessionAction
          Anchors = [akLeft, akBottom]
          Style = bsSplitButton
          TabOrder = 12
          OnDropDownClick = SaveButtonDropDownClick
        end
        object EditCancelButton: TButton
          Left = 122
          Top = 221
          Width = 80
          Height = 25
          Action = EditCancelAction
          Anchors = [akLeft, akBottom]
          TabOrder = 14
          OnDropDownClick = SaveButtonDropDownClick
        end
        object EditButton: TButton
          Left = 8
          Top = 221
          Width = 108
          Height = 25
          Action = EditSessionAction
          Anchors = [akLeft, akBottom]
          TabOrder = 13
          OnDropDownClick = SaveButtonDropDownClick
        end
      end
      object NoteGroup: TGroupBox
        Left = 2
        Top = 272
        Width = 391
        Height = 78
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Note'
        TabOrder = 1
        DesignSize = (
          391
          78)
        object NoteMemo: TMemo
          Left = 8
          Top = 17
          Width = 375
          Height = 52
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
      Top = 353
      Width = 401
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      OnMouseDown = PanelMouseDown
      DesignSize = (
        401
        34)
      object LoginButton: TButton
        Left = 113
        Top = 3
        Width = 108
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
        Left = 227
        Top = 3
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Close'
        ModalResult = 2
        TabOrder = 1
      end
      object HelpButton: TButton
        Left = 313
        Top = 3
        Width = 80
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
    Width = 472
    Height = 387
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      472
      387)
    object SessionTree: TTreeView
      Left = 8
      Top = 8
      Width = 456
      Height = 342
      Anchors = [akLeft, akTop, akRight, akBottom]
      DoubleBuffered = True
      DragMode = dmAutomatic
      HideSelection = False
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
    object ManageButton: TButton
      Left = 356
      Top = 356
      Width = 108
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Manage'
      TabOrder = 2
      OnClick = ManageButtonClick
    end
    object ToolsMenuButton: TButton
      Left = 8
      Top = 356
      Width = 108
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Tools'
      TabOrder = 1
      OnClick = ToolsMenuButtonClick
    end
    object SitesIncrementalSearchPanel: TPanel
      Left = 26
      Top = 309
      Width = 139
      Height = 23
      Anchors = [akLeft, akRight, akBottom]
      BevelOuter = bvNone
      PopupMenu = SitesIncrementalSearchPopupMenu
      TabOrder = 3
      OnContextPopup = SitesIncrementalSearchPanelContextPopup
      object SitesIncrementalSearchBorderLabel: TStaticText
        Left = 0
        Top = 0
        Width = 139
        Height = 23
        Align = alClient
        AutoSize = False
        BorderStyle = sbsSingle
        TabOrder = 0
      end
      object SitesIncrementalSearchLabel: TStaticText
        Left = 5
        Top = 4
        Width = 154
        Height = 19
        Caption = 'SitesIncrementalSearchLabel'
        ShowAccelChar = False
        TabOrder = 1
      end
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
    object ShowAgainCheck: TCheckBox
      Left = 12
      Top = 0
      Width = 405
      Height = 17
      Caption = 
        '&Show Login dialog on startup and when the last session is close' +
        'd'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = ShowAgainCheckClick
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
    object SearchSiteStartAction: TAction
      Category = 'Other'
      Caption = '&Find Site'
      SecondaryShortCuts.Strings = (
        'Alt+F7'
        'F3')
      ShortCut = 16454
      OnExecute = SearchSiteStartActionExecute
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
          643D2272223F3E721A5462000001484944415478DA63FCFFFF3F032580116440
          EF84292FFFFEFD2BF6EFDF3F06200DC7287C20FB1F42EEC59489BD9270033A7B
          26FCDFE011C8F0F7D73F30FE07A59131B298F795C50C33A64C60841BD0DAD1F3
          7FA35B30C3BFDFD834FDC730C4F3EA02863933A6200C686CE9F8BFD92D0CABED
          D8F8EED7E6312C98330361406D43CBFFADAE117835C15D0274A5DBB5D90C4B16
          CC41185059D3F07FBB4B1454C37F82E1E07C7D06C3CAA50B11069456D4FCDFE1
          1C8BDFF9BF117CC7EBD318D6AE5C8A30A0B0A4E2FF2EA7781C5EF88F61B0C3CD
          C90C1BD7AE4418905B50F27F8F631251B683B0DDCD890C5B37AE451890915DF0
          FADFBFBF227F9112CE3F68E2C196B0FEFDFDF76AC7D60DE27003181919B126D3
          19B3E783D379466A2286025816C06AC0FA2D3BC0B2EFDEBE05F3858485C174A0
          8F07235106B87BFB63CD613BB76EC46E00C5B9911200003F40C1F079F9D89500
          00000049454E44AE426082}
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
          643D2272223F3EFA5AFC8E0000018D4944415478DAAD53394B035110FEC6DD6C
          F056B65057232A5A8882110349AC1442046B8F7F205AD9E46FD86AE90FD0C603
          118F5EB451B0F1006FE3198DD71E263BC6B8E6201B1474E0CDBC99F7CD37330F
          8698197F11FA3381B156B30BA6B654001C63E271297035F93B825525A7050234
          36E33D86C6C7764927FACD43DB108C1C02B65496FD14A1D0F23F55521F7199CF
          BFA586EF495B51921114381247C2F797905804B12A0841EE4E380EDBF659BF8E
          D0DBB2C242A51B52D3D857F3A9D71860DC02EA19CCD82B606A405C4B5AB62C24
          799D9E97147E953AF1F4A2A3C533900019603B70A69FBE8728BAA8F06954465D
          6B1015B2CB4AD093A09F8838FEE6A5C8BCC2876101EEDE1188A29801D0530976
          44646AC65E78BB94AE179AF9246CC01318B56953CFAA9C490453DD74F4EC7BE9
          7CAE83A3AA88D6AEFE3CD5AC717288D409297016A28319373B4BAA51D7D89EB7
          5AD638A9D8FBA0B3EF6A9676A61BD8D5E24379A56CFB6169223D3D1A4CBCB351
          5B1CBCBBA4AD29F9B1B4D8515EE8147EBF40C046FDF085FF5FB6F10320264D3F
          0CF45A500000000049454E44AE426082}
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
          643D2272223F3EE2CF74B00000016D4944415478DA63FCFFFF3F032580F1DB4E
          215926060E0D9800F3BF3F5F5A4FBE3AD9D0C0F00F9F4698C58CBF774BBD0532
          85506519A6B2B93DCB21CA805FBBA5FEE35091F1EF3FE33DAC72CC7FEFB23BBF
          00CB317EDE86DD00664E7106662E69200BE893FF20FC1FCAFECFF0E7FB8BAF5C
          4E9779C006BCD866F69F8D4B14E86A88241313330397A836031B8F1C03C3DF6F
          4806FC83AAF9CBC0C8C4CEC0A058C90836E0CFDDAEFFFFFFFD6160E110802AFE
          CBF0FFD77B0686EF8F18FEFDF90E14FA0134E80798FEFF17C266046A65B63907
          31E0F15AE5FF923A110C4C8CFF210AB06880B3A172FF8134ABE35D88010F574A
          FF97354CC0AE19A6014D33886675798264807E24410D282E027A95D5E529C200
          19DD208804360DE88602C30804500DD0F222E867B0385433A6011ACED835A068
          FE83924E500C1091D1016AFE05C6FFA1341CFFFF0DA45135FF01C636BF37D480
          072BA4AE3332FEFF04647E243A073230DC920D7B0ACE2B8C94666700197D36F0
          653E1B4D0000000049454E44AE426082}
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
          643D2272223F3E58A253E8000000D64944415478DA63FCFFFF3F03258051FFC0
          2D8A4C60545B7B853203A6CD9CFBDFDBDB8B2CCD5BB76E83189099968455C1F4
          59F31870C9C1E4C10664244433FC7E768F814D411343414A4A34C3AB6F0F18A4
          79D4711B90E261CDF0AC2192417EDE39B02008806C06B1FDA36D19265C4860E8
          B23E8A22876A000E5BC05E00CA317C7FC4C0C0AD8ADB05B86C011B1063C7F0E7
          7226038BF96EDC2EC0650B48417A5212C3F7F7FF19B8451971BB00972D20765C
          5022C3A555BF192CB3D8F0B880D268949496262B213D7FFA944A29912CDD50C0
          486976060042E799FDE6A5D6060000000049454E44AE426082}
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
          643D2272223F3EF8C6EB100000020A4944415478DA63FCFFFF3F032323230321
          3073F6823C462686F2677F7EAA36A4A77F838933820CE89D30E5E5DFBF7FC5FE
          FDFBC700A4E118C607A951505060E0E5E561B879F316C3DBB76F5F4C9DD42709
          37A0B367C2FF0D1E810C7F7FFD03E37F509AEDC72F86DFBFFF33987C79C6102C
          C3CDA02C23C9B0F5D435863B270F30A82B6B497CE0FCF7196C406B47CFFF8D6E
          C10CFF7EFF831BA2F8E13583C7DB5B0CCCACAC0C4C409BC243831898805E5DBE
          6A2DD84540FCF7DFFFFF1FC00634B674FCDFEC1686623B0BD0F6C82F771994B9
          59189C1C6C19B8B8B8205E02E237AFDF32EC3B70E0E7EFBF7F2AC106D436B4FC
          DFEA1A81E10510EDC7F48641E7C7738630A00B409AFFFDFDC7B06ACD3AA061FF
          E332D31217830DA8AC69F8BFDD250A6AC07F1443AC19DF33848BFC62B0B5B102
          06E83FB02B366DDEC2F0F3DB0FFDACACD44B60034A2B6AFEEF708E856B427649
          3ACB7D062B654986876F3E317C7EFB9A415E569AE1F3FB770C1F3E7E88CE4A4F
          590636A0B0A4E2FF2EA7780C2F80702E0B302019FE331CFECCCF70E32B178325
          D70706339EF7C060FD8F3020B7A0E4FF1EC7240CDBE1FCDFA87C87DB9319B66C
          58C3084F0719D905AFFFFDFB2BF2172921FD03D138121630205FEDDCB6511C6E
          00AEA43C63F6FCFF203A233511675AC76AC0FA2D3BC01ADFBD7D0BE60B090B83
          E9401F0F46A20C70F7F6FF8FCDB69D5B376237801240B10100FAA09ED5B67FF0
          220000000049454E44AE426082}
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
          643D2272223F3E9FCD8784000001214944415478DA63FCFFFF3F032580116440
          EF84292FFFFEFD2BF6EFDF3F06200DC7287C20FB1F42EEC59489BD9270033A7B
          26FC2F2F2D20DAD68CEC028619532630C20D68EDE8F95F5D5942B40129E9390C
          73664C4118D0D8D2F1BFBEB68268031292331816CC998130A0B6A1E57F73630D
          D106C4C4A7302C5930076140654DC3FFF6D606A20D088F8A6758B97421C280D2
          8A9AFFDD9D2D441B101C16CDB076E5528401852515FFFB7B3B8836C03F289C61
          E3DA950803720B4AFE4F9ED843B401DE7EC10C5B37AE4518008CD7D77FFFFD15
          414E38FFA089075BC2FAF7F7DFAB1D5B3788C30D606464C46AD38CD9F3C1E93C
          23351143012C0B603560FD961D60D9776FDF82F942C2C2603AD0C783912803DC
          BDFDB1E6B09D5B376237801240B10100D276D1E1745DE7E00000000049454E44
          AE426082}
      end>
    Left = 44
    Top = 189
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
    object Search1: TMenuItem
      Caption = 'Search'
      Enabled = False
      Visible = False
    end
    object FindSite1: TMenuItem
      Action = SearchSiteStartAction
    end
    object SearchOptions1: TMenuItem
      Caption = '&Options'
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
    object Search3: TMenuItem
      Caption = 'Search'
      Enabled = False
      Visible = False
    end
    object FindSite3: TMenuItem
      Action = SearchSiteStartAction
    end
    object SearchOptions3: TMenuItem
      Caption = '&Options'
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
    object Search2: TMenuItem
      Caption = 'Search'
      Enabled = False
      Visible = False
    end
    object FindSite2: TMenuItem
      Action = SearchSiteStartAction
    end
    object SearchOptions2: TMenuItem
      Caption = '&Options'
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
    object Search4: TMenuItem
      Caption = 'Search'
      Enabled = False
      Visible = False
    end
    object FindSite4: TMenuItem
      Action = SearchSiteStartAction
    end
    object SearchOptions4: TMenuItem
      Caption = '&Options'
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
          643D2272223F3E1D8D1185000001B24944415478DAA5D3CB2F03411C07F0EF08
          8EB655847A7312F10A51CF102EC5D1C9F32CDA207174F61FE0A40E28F726BD11
          2238785584101AD44DDBAD6E493C7619D3695A3A4122FD6DB23B99DF6F3E33B3
          B34B28A548244818E89933DCE08314FD6B6412BD758ECAC51160268366366820
          492C115D10FD6A8A7DF41D900F93E1B4FA490CC8624038694AEFC548C93C08BB
          7E0AFFAB0793AE2AF80F0420D3A4F10273B605BA1423563D535FA3A2AB61B7C5
          C620867775908F0420A35EE305E61C0BF40C08A95EDCBFDC605F76C46D63B129
          88A11D0901578A00D469B159C2D15F380DB3D1828380030BEE093CAA32CF2DB5
          2818DA66C0B100186A35BEBC9FE2490BC076358E7DBF03CBAD0A06B7243C9C08
          407A8D8AA566E56FC0CB803605039B1282A72250AD827E3BAE81D26974E55AF9
          ACB64BB6853799A7ECED0CD860C09900E82B555ED09567853E3507CA9B0FDEE7
          6BECF91C71A7B0D2A1A07F5D82722E00BA0A951774E74700BB7B2AEEA5469F2B
          9D0AFAD624842E44A05CE5358D59BDB096DB7EFD907C2F1E8C6D552274290069
          656A6C08156615FBC2ED27F77760D6E0012505FFFA9908BD735AE44292E8EFFC
          09B47415F0B5728F810000000049454E44AE426082}
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
          643D2272223F3EE783E6AD000002334944415478DA85925F4853511CC7BF67D3
          ADD4592485A4624FF610FD83A25E22C712058B882111985034F7D09F1156B858
          C1022DF6B0974832EDDF6341886232A920909052025196E48BA4228B16BBB5D6
          EE76EE3D9D73DC56CABD74E1C7B9F7C7F97ECEF77E7F87C0E4B91D0A2F6B9A56
          C90B94974629C47BE15B54967E2566805BDD2136D4E086A6EAA0691D76F52776
          AB1FF1563DC87B5CCC7B4D534F600AB819EC62C30D27B95843A5BA881E124034
          5D059F120015000E6E9C7A680EF0DF08B211D729EC4D4F205C1442A9BD08AD0B
          97F03EB5538A350E3E32DD670EB8DA1960654E07AED99EC2662FC18202D42F3F
          90BF430580BB70CDF4821885C5F42C8ED58D615F6D0C16AB0D8C9FD3BDD48CFE
          84BB605F54FDCC3D90B561956414F458FDD8659B072CC520C40A359341DBA21F
          73A96AC4521B642E62FFE1E85D907FC312CDFD7412178B1F63A39E40CDBA0408
          87E83AE3EE18C63F97E1FED80EE9726594F41BC987A5E5ACD15C40FDE5D771C8
          F1099C004D0766E375F8A0B8D1EE39B72A3722C28AB85AA5FD3C64ABB6845795
          1E582C424CF07C721B6895570A365554C8F5C4D126092297AF74B25167DBDFD3
          39A4CB11464BF91BFCA656B4CF7580BE9B4032995C35A5D197832B800BBE0EF6
          DA7926675D476956C178CD69FCA0EBD1321B4434512DC31A7CF1CC70E4C47BDE
          17E323DC92BFEB076ABFC0B57D1E77227BA0FC2285B0468606361B02D6367AFB
          1E31B17A3D674D2F99216060382285DFE371C3B0FE0B686C3ECE8C36E4C3327B
          FE0078D65AB8224AA86A0000000049454E44AE426082}
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
          643D2272223F3E77C6659E0000016D4944415478DA63642003BC495399F6F3C3
          27DD8FAF3E643292AAF9C3CC5CB71F47D6EFF8F6FA03E3EFAFDF8F9164C0FEFD
          0C2C96669FCF3DB9785AEEC7844C7E9667373D4932E0EF718BB2DF4C52A95798
          0ABE3D7BF0F2915F68A82FD1067C3F6223C7C6F4F5FABF5F5FB9187E7FF970F6
          9B978685FF9C97441BF0EFB8D9D67FBFBF78FDFBFD15E8949F396C6E2FA682C4
          8932E0CF31CB00867FDFD6FFFFFD85E1FF9F6F67DA8E3E376F6860F8479401AF
          F66BF388B0735CFFFBFB8B0CD0E9FFFEFFFB65CCE6F6FA024C9EA0017F8F9BF5
          FEFFFDADE8FF1FA0EDFF7E4E6473795E802CCF386DE6DCFFB8340B73BE670852
          5ACA00B499E1EB4F2686D58F0B187EFF6363C03020332D09AB010F36FA3308B1
          DE64E064FBC6C0A23F938151D413457EFAAC79A8068004900D3BD32FCCF0E7FB
          3B0665AB38065187851816E035E0F9834B0C0B3B2318A2C20319E41C9A81224C
          A4197062EF12866F5F3E3238F967E30C600C0348057003C8D20D050041DBBD20
          634F792F0000000049454E44AE426082}
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
          643D2272223F3ED3D85E11000001794944415478DAAD93BF4BC34014C75FD2DA
          A6C5BA181CB482A05597FE013AD56E19D42E0E0A0EE21FE0E29FE0E4FFD1C541
          A40E4E465C0E9D3BD54D1045886D4DA0BF92BCE75D82B597560AD63705EE7D3E
          F7BE973B8588609252FE4DF07EB4988FA9F1B366D73ECC95EBF6A8E6DA76469F
          4E6A6574BDD3EC55BD2A09ACE3251311B6087DE6F82D232A11703AA999409407
          A4BBECA55594046F070BBA9A504D40C80321733E3A46EE26940CC2E463B5EDBB
          C5D58A6D49024551E0757F5E5754C524D148C45ACDAE8171371185D7AE1DABCF
          0D0A440909009A841C40627C3D03C1F70F2CFA7E1504123E326AA960120E4314
          1E2BE867C6208A10B05EAFC1CF04ECB10209E63B831084F9998B765F32521085
          C5D8624D43D544E0717C625EC331728F600F099E766686E0EFCCB542464FA428
          FC3B3C0E7EB68D9507B225C1CBDE1CE3F006BF481CF6A4030B25A0C7A7D22605
          936065F9B6B32B099E4BB39BAA4AE76DCF2B45E141492C96BC705D3A59BFEFCA
          5779E2C7F4D7FA02E72B4BF0947C98360000000049454E44AE426082}
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
          643D2272223F3E6A8148D2000002134944415478DA95934F68134114C6BF19D7
          A6D968AAA087649B228416B5D8A020F8B7563042C153D50A2D2895D6901CF4A8
          DEC483D7400E05F122084A0355508452A5F4E8412BFE818A566853924A6B24DB
          E27637BB3BE3741BD76C6B253E18DE63D8DFF7BE37BC25A831EEDEBB7F95505C
          2F5846F3AD4442FB7D4F6A81D3E9F4B640707B2E1008C89AA6F50E5CBE34E40A
          945F863E8293D63F8ADCE2845FAB3BF56D3093C9EC9424C92252FD8D96E66852
          51C25B5F4FBCFD909F593AEAF331B9E4674BA4FC22CCD77614B6F491B99EDBB3
          CB7BEE48D226462925DDE7BA0825048FB2C3E0ABC1C451893EBA5E6025CA5CC6
          D88F246C5F141D1D2720CB321863B0C5F9BE50C4D8F8B861DAD64D92CB463895
          EA2B1807A114C1D0416C091D02A80F13D341BC9BB2D07DBECB8199CD30FCF809
          4C8B5D4C5EE97B40F8ECA0A0A4AADEC290A9826BD3E0227F2EEEC294D18EE3C7
          8EC016F08A8BD19167505523964A0DBC2785A76D7C47340E69B31F603AB8AD3B
          19F66AFDFCEB493484F6435FCC636E5E85D2D824E012168A8BBDA944FF433233
          A4F0C8813E1770E14ACE4E76C2E6147B1BDE20227FC164A90D9FD49818075502
          B11E17705D54D76B446D46E03F9D7376C81168DC77F69F80A7E696F35275F142
          9540EB990D47F038E2A6FBD45E81DDF10D4770C558D9B3271E01A5A5DDFDF8AF
          DD99E1814D3145A0D31508FF1449AEF5AFACACFAABA60BF9C34E2DF6F97FD875
          F10B95D05DDA7E0FAF390000000049454E44AE426082}
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
          54B0000001C84944415478DA63644003DADBAFFF6720125CF5D4646444179C36
          73EE7F6F6F2F829AB76EDDC6F0EAF96356C65F7B24AF30FC67D48649CCB9D7CC
          9099960457F8FDFB0F30CDC9C981C29E3E6B1E43567A3223E3AFDD52284E8619
          70CF5F8A4169E33386858B9783C5E3632351D8580D0031E6A2B9000CFE7E4552
          0151BE60D1520605C595AC8C3F76010D008931B102311BC38AC715601B4A563B
          33F4B8773130FC7ECBC0C0C882351CFEFF7CF98EF1DB0EA9FFCC82060C6C4A99
          4021A430FDFF8781E1D76B60203C66F8F707E8827F40FFFFFD01A6FF43690636
          E1BD8C9FB749FDFFCA66C8F0E9CB4F06559310A0A25F0CFFB12946E623D8258C
          1FB748FD7FF451984146D38D41405816AAE12758112183FEFFFD66CEF86E93D4
          FF3BCF99190C1CD3185858589014FC846BC06610E3BF1FBF1EFFF9C0CFF872B3
          CAFF87CF7F3198B8646071E64F149B910D62F8F7FD14ABC32D73C6271BF5FF7F
          FCCEC2A069EC85C336A877300CFADECBE6F2B884F1F66A83FFEC3C120C328A3A
          386D43F10E5CEC7728BBFB8B358C17E72BFE97553567E01714C61A6008837E22
          BCC6F08FE1F7FF5FD2DC6E6F9E319E9E2EFC81979B959F939D99D84C084A2D27
          E4C29F5A82D9FFFF139D7BB1020091A85B0A6CDDBE960000000049454E44AE42
          6082}
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
          643D2272223F3E13041446000001EA4944415478DA8D924D48545114C7FFC779
          3393988C3221F5D2A8C88528389164ED8C6C14374124456D2372658BF6E24611
          5128081741D9BA367EF441B968D14272D3A09B3E36A5364A614DEABC8FB9F71D
          AFAF379FBE212FDCF73FF771FEBF73CEE512CAACD1F107EB42CA3A290494A254
          855215AF5139C0D0C8184F755F81301C0853BA5BBA9A3D3BE8494C82ECB9234B
          606ACE1A092C98B87F78FEEEC399786F2E5918A50089EEC5C70AF056E7D2EAAA
          2D73F07DDF811797AEBBC6AC4916C08425115F7A540C60EFB3AB03EF6EE1D5C5
          9BAE3122D3906A146989FC380A104B3C05996F1460D75111543B04F670F7176E
          A0F5F259741DCA8048F3BDA7CDDFABA0F46B9D03B531844EF6FD6B3ED78E00EC
          9F80B10C476C038E093583ABECE9E24A0D68F3A5CEDBA1D3F8BB65A1B1EDAA4A
          B2C13EC945672F9E9CABD8A2D4ACCEDF5351D437C551136DF00C969BF43F10CB
          743B6D4CEBFC351940ECC26D689A569060E50C7E20724CFB53F26335ADCF9CE2
          6F491B6D9D777CDAB48A2A1782E0181F821D9FDB6965AA95538686A6333D65AA
          79E3EC011963A1CEE57BF4E5598CC3070FA3FE444BD96A45E3E4FE657AC35D6B
          CF29F1E43837349E43A436EA7B617990951F0D0E326C1FAD8AFFFA410B13D13F
          D555C148653880FD2EF55AE68F5D5B3DEFC6CCBC6FA3DFDA010DD2B11D3248E2
          2F0000000049454E44AE426082}
      end>
    Left = 41
    Top = 253
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
          643D2272223F3EBC75A559000001B24944415478DA63FCFFFF3F03350123C8C0
          BE8953EFFDFBF74F11881940F82F88FEFB97E11F500E4C43C591F15F54FEFD29
          137A94E00676F74DFABFCECD9FE1DF1FA0017FFE41E8DFC8EC7F0839B03832FB
          1F83CF95C50CD3A74C60841BD8D1DDFF7F835B20C35F120C4156E37D7521C3AC
          E9931106B674F4FCDFE41A8CAA01C910EC1621D85ED7E633CC9D350D6160636B
          E7FFCD2E21A85E256008B2459ED7E7312C983B1361605D53DBFF2DCE61B8BD8A
          21861ACE6E37E6302C5930076160755DD3FFADCE91B8C3EB37FEC872BD398B61
          F9E2F908032BAAEBFF6F738AC2D480CB756896BADC9AC1B06AD9228481A51535
          FF7738C6608DD5BF045C07A29D6F4F6758BB7229C2C0A2D2CAFF3B1DE3C88810
          08DBE9F654860D6B56200CCC2B2AFBBFDB2101C556DC5EC6B4D4E1CE6486CDEB
          57230CCCCE2FFE0FCF466859EE2F966C07C3220D3BC0F9F7599513C3B64DEB10
          06666417DCF8F7FF9F3ADC40DC7916550EA816A4FFFFBFFF37776CDDA0013790
          91911167E93173CE027871949E92805321ACD4C26AE0FA2D3BE086BC7BFB162E
          2E242C0C6707FA78309265202E409281EEDEFE040DDCB975236E03A909A86E20
          0074D1DCF7705291810000000049454E44AE426082}
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
          643D2272223F3E94AD5135000001FC4944415478DAE5944D4F134118C7FFB3DD
          DD528DC4F0A6A51B7A10B8149B460E8A1AD29812891F004208173F80FA694C3C
          7920E1CA4B3412A25743D49B1C4830918258686D9A18B0BBDBCE3C6CA7DBEDAE
          6D05126E4EB2BBCFBC3CBFF93FFFEC0C23225C6663FF29B0F261700BC4128109
          5095183DD733872F2F0E7C1F6B2B930126099EB64DDAED04C85AF9526206F6B9
          800D37A8F1620AA084D1B0896A8384EF227A6F0227C0E772AEE8013D2B15CD49
          D4413E2853AF42BB390DA5E7AED3D13A964B562E2F81E6465DA13A90866ACCD5
          8B0DACAC00D69163C001889741C202845325B79A71ED1BEE5F95C03FEB756011
          4908E50AE289696741452EF227F841F5BE0B721FCECD79093C7E1793156FEF87
          90CABC80A6855B1288FB94F8E7DCF1DA6696654625F0F7DB18555937768F38EE
          649E9DA1C476E141E54CD8DFD4879F4624B0F426462734E8FC23DDB8957C124C
          F6C52D9EF96C60C27CAD4E7E7D2A81C535830AE57EF41A29F44587FFE1D95F30
          DE8C85E35FF8D1CE920416560CCAFE0AE1F6FD39E8BA7E6ECFFC55F0EA713C32
          F5734F020F5747E947C1C6787AC12DABB367010B1AAAB9B9A767F6E3DE49C92E
          A7C8125D18199B0CEEDCC6FCB67E5265499FCACD7BC0ADC561BA319444EF80D1
          B6A4A692560B9CB30E85AA0FB4C7F98F1EF0CBABBED2B588763DD215BAF87505
          6C0ECD1E4C042E87CB6CA77050F671D910727B0000000049454E44AE426082}
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
          643D2272223F3E36E04C15000001924944415478DA63FCFFFF3F033501E3D030
          F0FB2E615926060E0D6409E67F7FBEB49E7C75B2A181E11F2143901D0536F0CF
          1EE9B74021214C950C53D9DC9EE5906CE0EF3DD2FFF1A8CEF8F79FF11E4E79E6
          BF77D99D5FDC23DA40260E7106460E6920EB1FD8C90CFF51E97FDF5F7E65B1BF
          CC8362E0BBDDE6FF5939C5209AC0CE07D28C2C0C1CC2DA0CAC3C720C0C7FBF43
          E5FEC1E9FF509A91899D8141B19211C5C0BFF7BBFFFFFFF78781855D006A3BC8
          D0BF0CFF7FBD6760F8FE84E1FFDF1F40BD3F816601F1DF9F083610330221B3CD
          1954039FAC53F92FA917C3C0C4C80856F4FF2F4203213EC87056875BA8063E5A
          25F35FD6380DAE80014D032E3ECCA5AC4E0FB018689888A208DD6BF8F8AC2E4F
          B118A81F8DA281686F03C31CAB8132BAA178C309ABB7A19908BB813AFE186183
          D7DBFFFFC2D32A7603B53C898A51B0A5FFFFA0247EEC066A381315A30CFF7E63
          E426AC068AC8E8006D062AFEFB1B4CFFFF07A1C106FCFB0565FFC130EC0F3018
          F9BDD10C7CB852FA3A23E3FF4F40EE4742250B3A009A744B36EC690E8A81D404
          00FDE9C8E89B03300D0000000049454E44AE426082}
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
          643D2272223F3E94089F9B000000B54944415478DA63FCFFFF3F033501A3E1E1
          DB543591517BFB75EA1AA8BAEA32750D9C3673EE7F6F6F2FBC8AB66EDDC64048
          0D4C1DD8C0CCB424BC0AA7CF9AC740480D4C1DDCC05B762C6041B5437F701A18
          BE9D1FCC5FE9F99134034112300092C36620BA1A0C03A9EE657CDE8119F87B8F
          3498CFEAF2943403717919D940FA7B199F776006EEAAFB01E6BB35719066202E
          2F231B485F2F4B4A4B13544C0C78FEF4297179995800CFCB54310D0A18A95E62
          53DB4000B8E4B64D92AA64360000000049454E44AE426082}
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
          643D2272223F3EA1C1D108000002AE4944415478DAAD946D48535118C7FF97B9
          11A96569503290F2439A1821E517237C290B7A99A685959AA035635961599A61
          4B2A0CCBD0100CFAD0B73096F36DA665F429418A5E34CC8158B6AC0FF9523914
          77EF39A7BBBBED6E6D53313CF0F03CF71C9EDF7D5ECE7338C618388EC362565D
          43C3269542D52B082CF7F4A97CA3E7196707D6D4D60F534AD78B02BB10BB2604
          543C93B473DF256AB51A9B636330F0C98C41B3198490CFF5B57736C8C0EA9A3A
          D694AA01154480401D9AF7B4A97C1667FD81A465333871683F1A4DDD6819E511
          D2FF0C0DF5B59C0CACAABECB9A53D341E680444FFD441C3F81607E0601544066
          FA01AC0A0E82D56AC5A3A6567044804AA9ECA760260978BDEA366BDD95E180B9
          A09276D869BF87B075A512293BB623307039EC3EAED2104225705FFF473A62B1
          8C4AC06B376EB1B69D99FFA6EA11ADCAC6E3B87500C909F188898C70D692396A
          2DCA57CB37F4F4F44E129B102F012B2A6FB2F694C33EA9CA518A7BA1FC34B26D
          6614E464B91BE704373E368009245EA73BF95A029657543253CA119F54BDA3BD
          24BC43CEB12C278CC95D373C31621AC29A0B5AED98042C2DBFCA3A928FFA76D7
          2BE232F61E1907D3F0B66F00C35F46B0363C1C5BA222D1FDE2256C6436BA48AB
          1D948025A557586752B66FAAA2EDD9F972451F9842810FB3217835B502B1AA3F
          4808FA85001008946D2B2ACC7F23018B4BCA585752AEDF86B8236488A05318E3
          9598B4299C3F121B265E23CDB801EA75610ABD5E4F25E099E28BEC79629EDB99
          F79FB2DFFA8ADF8943F7D0DE6C705F6CDDD9F34C1E2DAF91235E63E72961FA4E
          697EBF5F4EC6D336A31B58A83B374819DD28033D9CE6038A332C5D724699B9AB
          A3254A06CEF7DADC7FF090B96C6D41DE82CF925FA0B1BD53864C8C8FCBFBAB43
          43653B7DDF1EEEBF8073AD450177EFD52C08EC32B5CC0D5CCAB5E4C0BFAB86B3
          3200AC8AD30000000049454E44AE426082}
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
          643D2272223F3ECB9E9CDB000001724944415478DAADD4D94A82411400E09957
          C85EA06EEA39BA29A8C8902449CA562DC94CD3DCD034354D51334B142569DFB5
          C5B037A99BBAEBCEDEE09F93BFE44C863FD1E081C3EC1F3317673000A0760616
          C1D8CEDE1B21A4AB96484C416C050191DA5ABDFD9EFF9942F3F83D9588765330
          124B82C56CE0BED5A2DE88D2A904A6602812079B75951B5CD02DA36C7A9781FE
          50145CF6356E70767E09E5B3FB0CF406C2E071AD73839A192D2AE4330C74FB82
          E0F338B841F5D41C3A2EE418E874FB20B0E9E60655EA69747674C0409BD303A1
          A0971B54AA26D1E5E921032D361744C27E6E50A19C403717270C3459EC108B6E
          718372C5382A5D9F33D060B24232BECD0D0ECBC7D043F18A81FA1533D032FA55
          72428BB26B64E746A50E7E38FAD0D3FD2D03757AE30B01D24341E99A6D5EABED
          15CF0381D74AB9D44B418CB1E47332B902FD8EB4731AC98D8D5FAB25587CAC50
          E4B35AA5F31D3219ED8F0E0D602E502AFE05F60F8EFC093E97EFA4C17646DBC1
          2F9D1138E8CBE5B9EF0000000049454E44AE426082}
      end>
    Left = 151
    Top = 189
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
          643D2272223F3E2B4BBAD6000002754944415478DAADD4DF4F52611807F0EF41
          A224CBEC688AC32D9ACE6AFD100B51C35B6675D3DA2A2FEABA52D6B5B5D5BCCA
          F0AECD1C6573D446F907C456CDB4465B34D72F9D1A8E9CA5220EC91FE500CFC1
          D30B1C0E8717B40B7DAECE79F7ECB3EF7BDEF3BC8C2008D8CA6262E0D92ED60D
          81316233B642F8E86C0ED626C0CE42615F1D4F78598390FE2CACB31EABB52810
          FCA484D332CFA48362EDCC29401D7B114A46856CA9934BDC5A04AE800391E80A
          E60729B0A836059A8BAFE382B60D73616FD6C0C98592DC7238266FA2DFDF9399
          B0C8C84B8D8D9A16D4B34DB83DDC90355AF21CEF567DC0DB393B5EFB1E22F899
          020B6BD2C15385041C6AC0396D2B9CBEFB588D8632E0183810076DF8FD651B05
          1A78A95197A747E98E4ABC0FF4A2D3E08D7FA3C75E0BC6965C62C24463BBDE8D
          01BF087EA540F6240F56A58526B7222D85A5D20E754E3E7915D037DB8DDEC93B
          52DAF6EA04F86AC686856F3458CDC3ACB98ACB3AEB86BF5B20F213DDE32D185D
          74E1DE0902CE8AE01005EED57330975EC395FF8161115C20A0C18DFE18386DC3
          E2300D567160B7932DAB2B209FC61B879EA4B6ECEBC6B30971CBA4C79A04A708
          38428105C739E9DBE976E9A1551F84CBFF1C5DF53FE287F2C89348253F696B0D
          017D76BC24E0D2280D1E4B81A7CB2C301537E1D6A009E7F7B7E2C514F96DF850
          EAACC41D7418093823826314B8E72827359E918134221F998E5A37DEC4C05F36
          2C7FA7C12329B0B1AC194D07DA301BF2AE337789D2A8CBF1D4D38ABEE91E2C7B
          2830FF302735E6290B602AB904A542858DAECBD56818EF7C0E84F915FC19A7C0
          DD951C1845FAAC66A413A8B0C9D926D7D7DF0939F840BC6037530CB9605BC40B
          762BEB1F7DABA0E845B3AB020000000049454E44AE426082}
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
          643D2272223F3E96F2CFD3000002CC4944415478DA95936D48535118C7FF67BB
          634B1B6A668521F53188A20821FA602CCD178C44162262464925242A98182C58
          A4A1C3344332B29448F48390A5A969482021FAA120C44CA1175324F32559B9D7
          FB723A77D3B53977A30BCFBDF79C73F9DDE779CEEF10285CB7EB1BBE8B82B84B
          922488A2E80DF62EADBF6F1C0BE202510256D7DCA1CF533321BA254F486B4FFF
          F09F4B1F6F8522B0B2AA8676251B21F1FE008ADDEE1914D14698EDA55872E97D
          D0B40F8F9581372AAAE88BE4AC804C0EBBDFA1566B81205218165BE070A97DEB
          29132DCAC0EBE60ADA7B32DB979D917F8AB22DEDD085EBD134730816EB25364F
          BDEBAC8AE48987CAC06B26337D999403E276A34CA887316C041ADD56386CAB48
          9BA9C617476C40F649930F948157CB4D7434F1342CA219F161B350715A10A2C2
          F07C0472E62DBECCD681868F8D204A6A4469979177B00F917A35888A03BB41E0
          79147DCB47CFAF84A0DD3E3ED900124A8DA3C228EAB89BD069D40CA4F66406A8
          B06275227EFA897733F8406D12A6EE82C86A74A71803FEB69D5FC480269B01D4
          A0AC2B44CD31A01C0476BB0B12056C0E09A6AE2358B1697C95B12A97C8666AC8
          B1579C0661E515EB9A91A81FF76429972C87D3C9E3EDE23118725B83F6806C54
          C31F1A21AC6068671E749C6A0D06FCB64918B166635E38806DD1D11E48E6A954
          1F98ACABE185D0006881AE0D25516D8CA5963FC5DC6A381A07F761ECAB1490D5
          406FD75FA0AC467FE2D9A073CAF12E0CC7E62292B381B29EBDB7EE41EE940907
          C6DAD1D9D11E523752525A4E5F9D3817582E4F91A5ED4365CC3DB62940EF523C
          0A3F95C0EEE43C6A7477768406161697D241C38520A75EC7E5238E5B40EDDC19
          D4CD6641586B87AC465F776768E0E52BC53F98C43B443FB1A374ABB895F106F7
          87F663F47334FCA597D5603D8B09090CB5D0F4A8994A5485828BE7158FE73F81
          CF7AFAE5B6E1E7F2B267BC991AFF054C49CFA09B7DE8AF86D2F5079E262FD9BE
          60814B0000000049454E44AE426082}
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
          643D2272223F3E8C0CA337000001F24944415478DA63FCFFFF3F0323232303B9
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
          701AF8FF613FC3BFC70B1860AE63566F656092894351033403D3409020320D02
          BFBFBE60B8B5C494415999938185E93703038F06038BD93690E7081B08934076
          ED87BBDB186EACF406B36594151964BCB7020DD5C4F005D1069EDB58CAF0EB6A
          0F030B8700838ADF420601153FACC142B497F3537D19A4F8BE3114D4CD6160E7
          57C419CE385D880E26B7A43264944D646065E3C29BAC300C64A002801B484D00
          005A1E559CEFBF1AD70000000049454E44AE426082}
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
          643D2272223F3EAA5A98A8000001B54944415478DAB5D4CD4AC3401000E09DF8
          8350EC45294AD012A89E7C0A8B0882072D062F8B5A3CF912E26B086A55287AB0
          75290882D4B7F022086215B4888250343633CEA6A61A6DDA0A750E21C9CE7ECC
          EC6E0244243A19F06FE00680B1968E6F22616168E746B533B9B410938894DC3E
          2AA7D789300096D35686042D09120E21DA437BCD518D09C20C4F37387F7724F7
          B81C001F56E21284C820270822C7ADBAB699BD53AD30811C84CBA3F9A7FD0008
          00E24E9AB20BA08EE23BD9E661106D84C58F9FF7EBCE775087460DA43AAADB37
          0FEF55334C8F85821EBA382C01BE50D715B60114F530D4EF82584BD0AB663126
          0DD26B0A1ECA487718D616E8B708E2B352BE84617F02FD36BD3C426E9F6CABF0
          ACFE0C7A187E6E0057C633446D4DD1C12ADAD6C98B6A1BFC89E93685AB73FCF6
          D1E1B1001A0A36C2FC35BB9E8DF2D8D7EE739E6D9D565428589A1F907CD310F3
          E36A26C2BB5F3BFCA4DBE72335765651BFC0DBD420638231309AEDA6874E4724
          D754FB8E19255ED344F14D05C0526A30C78F73AD303F2EA7FAA4C1DFBE770290
          B612C5D7D500783101BDFDE30307446EBE155647277B247794CC9E3BBF7F5F9D
          8A8E831F91B3D4E8B5F484080000000049454E44AE426082}
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
          643D2272223F3E504E8CC1000002904944415478DAADD4FF4B93411800F0E7DE
          B9B7F45DA1A1254EAD650CB3129122052D09438A8299948156308B2D4922D81F
          30FA138A52540AFC5149C314090A242A8C6290B8A810F3CBE64254A66DB5D7F7
          7DEFE9DE577BDDDC1A1A1D1CDC73C77D789EBB7B5F8288400881ADB47B6D6D45
          BC817F27CB78F5D6CDA6BEE835B255D0ED7673D9B97B3D85566BF197AFE30B01
          DFB73C3617F967F0415BA76B5746C69DFA8BB5398343CFE76766FCF79B9DD7EF
          6E096CEF7C6C371A7987A2281605D1545F67330882C0874221E879F254241C09
          7384F3B3B5010D945EE68C0192433167012823C1DB7CF5F7871D8FBA9E656565
          D65455561805210DD43D945250D4AE5050E1D1312F9D9A9C0EAC822FCC982833
          967704A952F571EE78D023DA864F5656982C967D2615A31435501DFBFDB3F2EB
          376F97A588549E14D450F538080F8B2BBBA1DF678786C66B7A767FE0EE9EDE5F
          28CBA79A9B6F8C68E0427F3E929454B69DB9B86A138E83D4AC63B07D4F1900B7
          4D9B6B1D34C09586CB6B18AE81147AFBFA43212A5A5C0EC7BC0682AF95DD4CCA
          86DCD8BCB404F873929DE60F404584F6F7355077C1069FBCA330353901E69C6C
          28B01E81E1E157C1B0182E6F71383E6B6060A004330B6A20851700A808C83A30
          401D47C71D9EF36034001C481F87833B3D30B1BC1FBCC15290148322532C6B71
          367DD0C0E9EE5CCC3BEAD4115C83D4581FB33EBB940E3B8C0B60322CEAEBA20C
          D035E13A3717F00DB1074ED7C1527B1C805159268BF9D37EFD21AF83258D4981
          BF1D030B18381B0FE6165FD2CB4856764C8C8A8624060FDB3655AA9E19CAFA7B
          480C169D4D5E5A74D6548A796089C1C2EA98323796BD1EAFC47D4D0941B3F5C4
          A66E74639358E5C29938D01C66C3B44DFF14637F2023F9F5FEF218F07FB6DFC3
          A739305B476C310000000049454E44AE426082}
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
          643D2272223F3E815EC57C0000026A4944415478DAAD945B6B135110C76793DD
          242B2A9136D536B10149035A6DA33E79215E485110EB93961A8A5841D107F113
          F8E277502AD65B31055F34515144117D1051A116FB20558ABDC63486D452BBBB
          49CE193767B39B4D73318207969DE1CCFCE67FFECB590E1181E33830AFCE57E3
          08FFB8460FF81984AB040CDC1F41A760A91BB690A5F0E9C4F6EAC0AB0383D8DD
          7DB46E602CF6082E9C3B531B78FE6C3F90F43CCBADEB9A6A02AF5DBF590ACCBE
          6C1903E4DAF5821B1357200F1C0FF22CF7BFC98124C92C16458711EB7939F085
          BBE423E8C089632D2CDF149D833B43C32C3ED5D76BC47A5E3770414AB2DC29BA
          0A3B6A19592EC6A0B5DDBE1B81E35B5E6FF820C553061075A44580E199CB6C72
          7870336BBA777A14E8AF11B0D04555025FDD4C259E6040F9B9A69077ED037EE3
          C9BCF0D242CCAAC5090079169048805401A01955AD528CF36FBB2BCA80CBCF34
          600A3A805A5681B7FDB05A906545E6063348CB0BA0C243881C66C0A5A76E76E2
          2FD35608842E8120D8CB1A9094361B7B441B921FA6287233032E3E71638E5B0B
          DF130476842EFE4589D6AC8374E51CCD7CE3F7BE6F63C0F46337FE4637487435
          F83A8F943757F3CC348CA3F22D3EF8B99F0153310F262517347802D0D8ECABE1
          D90A98C906AAFA673FF835C280C9871E9CFC69856DBB7BC166B3158A2B2B317B
          663E05C92D79C5AEB92906FC11F5E34C32033BF7F7158EB5C23352C5025D3591
          A76CA169AF7153261F0450A10E68DB1A2C9D4C2A3457F213B3115B573C6C00C7
          867CB8BEB5031A9A3C158F5454526E01520216CCED110ECDBF35801F071AD36B
          44C1293AAC75FF03F5A5DEA977AD3DB3BB8CBC7889FFCFFA03741F258CF5DD3B
          FE0000000049454E44AE426082}
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
          643D2272223F3E93EF7622000002764944415478DAAD934F68134114C6DFE4CF
          B6112D85D6489AD01C6C05A9C6A827B5D6A0A9B17A12515B423D085ED5A3220A
          1554A8600F82282215855EADD55A298A372DD25B8B162B6253AB8969A948B5BB
          9B9D7DCECE24D94DB28D111C18DE9BD999DF7CEF631F4144208440B971BDEF66
          8A52EAA59A062C82C6A6116DD6495209F05AEF0D1C8C1D06AAE8A0C93A8B54E4
          46948D28F60E4D3C808A8097AFF6E250F488092901B35CA6D0F1EE7E65C04B3D
          57F0C9BE63599080D8298CBDBF278099970D9380A4C50A21801A123C234593B7
          CE5FECC1E1486711A414BCFFC3DD2CF0851FED9431DD32EA3472E1D5A9B1677B
          E25C8528D584586D884EDF2E0FC4ECEEB9D11330D2DA0D44D560359505D850A7
          52D0792E805BA7FA4D60EE3238DC4C9A046881F6BD390A1DBB7DB065630088F1
          7D85F1633E2180F2A850E8F246C015E812C51648CD00282966C01C205D06D415
          005D0526CBCC591CFF54AB72E0EFE702B80021D01DAB20D872801DC8F043D60B
          5690588BFDDCA4548E73E0D2889F573C35EB8470F42CB8DD552517909AE0826F
          D97DE33145917D1CF873D88F1AA981CF290ADBA2A7FFA244CDC20B95135DFDE8
          6A7DDBCC818B4FFDF80B1BD83F5203EB43070B2F5BF262CFAC36105DEE77B54D
          9CE4C085A100A697D7425D200CF5BEA6329E15C1A899EBCCBFAABDD3031C981E
          0CE0CCBC1336EFEC0249922AF6CC5A05D596829EF6AF090E4C3EDE805FD22A6C
          8F7467CB5AD9B3020B72AAA99C90A2B341DE5D0670E6511815BD1A9A37B515BE
          6C63BEAD9F981990DABFC5F3C0C9874DB8AE310475DE806D49A692520B58AF83
          03B55DEED8F7D779E0F89DFAC5351E77ADA7DA09FF3A584F8D351E9FDB915F9B
          4DFC7FC61F18B8AE8682F3857C0000000049454E44AE426082}
      end>
    Left = 153
    Top = 253
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
          643D2272223F3E5AD286080000023F4944415478DA63FCFFFF3F032D0123DD2C
          E89B30F5C23F86FFFAFFFFFD63F807C3403910FD1F8D8F4D0C85FFF7DFC52913
          7B0C502CE8E99FFC7FADAB1F502150C19FFF0CFFFF801442D97FFF81E97F5031
          88DC3F843AB8F83F30ED737509C3F4C9FD8C281674F64CF8BFDE2D006A201EC3
          6096420D83590CD1F30F4C7B5F5DC43073DA24540BDA3A7BFF6F740B423318A2
          09C55224C3301D0311F3BABE9061CE8C29A81634B777FFDFE41A8C66D83FB4A0
          421247F725541CC4F7BC3E9F61FEECE9A81634B674FCDFEC1A8A086BA4B04776
          3D7258A3061BC2311ED7E7322C9C370BD582BAC6D6FF5B5CC2D1BC8DE6032457
          228BA32404A098FB8D390C4B16CC41B5A0BAAEE9FF369748AC918BE1526CF181
          1454AE3767312C5F3C1FD5828A9A86FFDB9DA3707A1B3D7251C21ECDA7AE3767
          30AC5CB608D582B2CADAFF3B9C6290BC8D2522912DC5E553A098F3ADE90C6B56
          2C41B5A0B8BCFAFF4EC7588CF044368CA85405C44EB7A732AC5FBD1CD5828292
          8AFFBB1CE389CA58FFB1452E92B8C3EDC90C9BD6AD42B520B7B0F4FF1EC74482
          B914A3C8C092AA1CEE4C62D8B2610DAA05D9F9C5FF7FB0B0230AB3BFA8851BA4
          4043E6430BB9FF501F03D922E507181880C6BD6D3264D8BE793DAA0519390597
          808A7471969C189661118395AC0C0C97776ED9A0876201232323DE727DFAECF9
          281547666A225E0D707389B560F6BC452816A426C5516EC1FA2D3BE086BE7FFB
          16454E505818CE0EF4F160A4D8027C806C0BDCBDFD89B260E7D68DE459E0ECE1
          A7478C057B776CBA44D0025A01009151DBFEDC997E780000000049454E44AE42
          6082}
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
          643D2272223F3EF92CC43E000002804944415478DAED95CB6B135114C6BF9B57
          6B2496A651D24C4A44F159316D17C6855022010B22EA26C555FF0031B8105C88
          908DB8128AE03FD0850B0517515BD40A22B8523729E2C24720DA94366A5A4CD3
          CC64728F9399693293578742770ECCDC3933F77EBFFB9D399C6144849D3CD87F
          802540E575609A11AE1198CDF4122873D0F4DD774BB79249F06D03E4F940B559
          BC69D25C61558EEF8BE78BDB73302F58CA93319BA45FC83C4156E207EE89A5EB
          5B0236C5C8A058BB770C9C86233809D8776FB51D50F1CBAC0A905E09D44ECC08
          B3ED1A8473280E5BAF1FD8C882E475E57905E0B553D24692407ACC1405B627BC
          A002CA2F3540D9750CCB6B0EEC1FBD821EB7CFBC212E02620E24E65501D2058D
          006A8A19F84D15507AA1001444667510C1A331F4ED3D5853AC4FD6C40C42B5D1
          10B7027537954A5405ACCF2900C6B090A9E2D4F9DBB0D9EDE68506C1E65803B6
          BA51ECF2955FA24705FC9D154862FDF891278C9C4D987209C36232EC187AFEA9
          05B6B9464C3BCF7C08AB80B5E70215790025DE8743A3175B72D9007677638C59
          559C718CA7A75440E1A9402B1B7E788511F884E366BB54B1E4A6B1731D46D294
          6BFCF38C0AF89D0A52F68F1B87C72EC1EDE9EF5A19EDD2D7CE1D97CBE19E5826
          AD02F2A9107D5B1411994834AAA04D29B6AD146EAE260D2096EEBCFDEEA9F52F
          15B0983A4985A20DC391CB1D4BD14ADD3780E21B57EC67B4DE2A324FC688B9BC
          081D89B4F978C6FC4A1DDD6940FD1ED57BAE58EE461DF0E9E130F94361787D82
          C5BA6F061ADC50159C572FF49E5B7E56077C7D1C26A77B0043074E2893E4D6CA
          68017670431C6565F8F83EE78C2621D701D947C27DE5E772B5DB3FC152EF5708
          B5761D9ACC254CED7A278F1D07FC03960CCB0111CA9D300000000049454E44AE
          426082}
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
          643D2272223F3E44EA93C60000020C4944415478DADD944F4B1B4118C6DF5976
          136DDAA8A0064C1A7B6929B61A143DE84593F622524AA5D483971E24C5839F41
          8A9FA27E03153C89E4602027FD08FD7B288604233D041A63D86CE6EDEC66B33B
          B31996AE211707965DDE9D797ECFFBCCEE1044847E0E723F0085CF445D5A8C3D
          078A0AFF12296D0CACFDF9115494376D018C7C3CCF6E19A903807CAD567F3FF2
          AE5ABD33A07916F7CF89C077D2C22CA52D7F8842E8F945E5DBCA2E1AC100E65A
          ED214078DCB4D7F1693F772EF3D6026C5C15D457C5B400A8E7FC01A1B1455063
          6C0D355C415E1C5D0836FF0279BA470440F9F809861E25405143F6BCF664A268
          10194F41281203ACFD62061B6CE39BECB5CE60EC4E75EBE26B0AEB5259381501
          D7B9251C4A2E43383AE971C55AD6AB80B725470029278EB6B8A7A62E7F150197
          87097CBCB063F6E74C46DB9D5F0DB92EF89A96FE2901CC6FBB42A88BAEFEB3D6
          894ACBFC9600E6B638177CCB9C739F1AA2DB8DF6BA2401CC7E74C57B898AED9F
          1C90DAB45BF60805A921B544A580C4CC072182C051B19FAC33E480E975CF8605
          89CA39197C002FDE701B264620AB095179861C30B5DA1D8B1003F7E7725D38E7
          901FA07894C4F8B315B6D0682FE2BE6F471C75CF17638A53E9D9D505A89CBCA4
          E107513218196E1F6896A0613B35ECDCDBCF56CD348272F19B0618A36F4B9A18
          D1C1441608F9440851A1878168B9FA92DC28EF0B807E8EBE03FE018DDC8FEFD3
          CB3B0B0000000049454E44AE426082}
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
          643D2272223F3E5A4F7E35000000BF4944415478DA63FCFFFF3F032D0123CD2D
          303C7C9BA636306A6FBF4E5B0B54575DA6AD05D366CEFDEFEDED4594E2AD5BB7
          3110AB16A69E3E1664A62511A561FAAC790CC4AA85A9875B70CB8E052CA876E8
          0F410BC2B7F383F92B3D3F926F0148121980D4E0B2009B5A0C0B681E44C4781B
          66C1EF3DD2603EABCB53F22DC01744E8160C8E2022C6DB300B76D5FD00F3DD9A
          38C8B7005F10A15B30388288A616484A4B13AD8914F0FCE953D20A3B5201C9A5
          29D916D0C47428A07DA54F6B0B00231BD0AF00D4D2850000000049454E44AE42
          6082}
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
          643D2272223F3E67F167D60000036B4944415478DAB5956948545114C7FFEFCD
          6850448C5AA494F621A984ACA0088A427329CA164DD34ADB05A32C5B2C45936C
          97B44CB3CD50A828284CC7A5265BF483995A541A154CCB87AC6999DC4BC759DE
          EDCE3E7726CB942E3CEEBDE7BEF7FFDD73CEBDE7718410E81BC77118483B73BE
          7015CF73E7D53AD5CC6D7171AFECD7B9C1022E165CFE3466AC87FBC766856CD3
          8698857D024E64E73D1740A6104180607EE89ABE277673B3CDD9D919DEDEE311
          151186C24B572197CBD1AB5643D0098D793959531940E6C95C5214B4847E4845
          B404843E82CE34D609865E30D98C6B02D6693F20C4671C7C7C26A2E1F1535C7B
          F905C56A7784BCBC8273A7B339069091994D8A839799041DC5C46A2D225AE518
          A1E9010F024EA78588E7B13A2A1C62B113BA7B7A70BDA8043AFA2ECF1962DE29
          E2F86A0BE0484616910687D909EB41C6DD0FD168B0F1EB33F8CD9E89719E5E10
          3B89A8104FC3A60F992994544BABD5E2278535343CD12995CAF716C0C1A3C749
          69D072D6032A6E1BAAD1AA2E44FC906351903F3CDC471B84896D6E68DFDEDE81
          9A87B550AB3577BBBBDAC22C80F443C74859508435D636B1B7022944DD8515BD
          EFB09842468D7433259D183CD1E7EFC6CD12B4B67C4752E24E360769E9874979
          60A45D0E580F88C92B2F4D07A2F966AC080F35EC9A98007A90F45625EA6B6B70
          E14C0E0B48493B406E05AE7448AEC51B9B9CCC12752052D203BFB973D8E34BFB
          DABAC7A8AE7A805359192C2029753FB91DB0CA418C3DAE465BAC133D9E53BDE1
          EAEA82FAA6D7F8AC50C0738C077C274D80828EABAAAA919A9CC802F624EF23B2
          79D1163183B8C37135DA93B817701B390ADF944A3C52B9A051351C339CDB307D
          6827DC5C24F8A2F88484F8CD2C60D7DE1472C73F86BD5866613B8F02445FD1AD
          E550A79240A5E12C9B194634983BBC1D92E61A9CCECE640109BB9348A5FF5A26
          F66C8205BB5039DE72B3DDEF4D2ECA8A6FB080F81D89E49EFF7A0731DBD8DB0A
          5BBD13EC4248016F7350212D62015BB6EF222AF1106B31D3B1C5CD58E06CE7D6
          DB6BF0988EDDF656835611B41C98065979090B88DB9AD0445F9ADC57E57484FD
          C666AEBEC08BCA0AA9EF3FFD0FCEE61712DBF9E6D8F5FDFA81F41B905F708901
          C46E5833784071B9CC22DAD6D2C2AC495C5D2DE3D09005DCA0017F6A0306CC5F
          B4B45F803B15D2810102162CF1ED0FE0BEACB4E9AF80FFD57E0117017D4D6163
          02560000000049454E44AE426082}
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
          643D2272223F3E6733DD78000001B74944415478DABDD5DD2F42711807F0E75C
          7363FC19DEFE07176C4C6B6BACADD5B292A62895524A242A4584501331AF4D49
          2617367F82970B7F85B971EB3C8A158F292D3F7EDBD939CFEFE5F99C73F33D1C
          22C25F0EEEDF80F9E0CA0D0FD8823C0F7CE1CAADE5EFF8A5FE6E8ED42FFCEDF2
          A2BF9500FE85101A47B44CDE5AADD14338B4C011C0EB0FA2D9A46302A8D443B0
          BEBA4481196F00AD16031340A1D240746D9902AED939B45B4D4C00B9420DB148
          980293D31E9CB05B980032793F6C6F6E50C031E9C629A78D0920912960772B4A
          019B630ADD2E0713402C91C3FE4E8C029671277ADC4E2640AF580A877B710A8C
          8ED9D1E771310144BD12481CEC52C060B661C0E76602084562481EEF534067B4
          6030E06102740B7B207D724401ADDE84A1E01C13A04B20824C2A4181C16103D6
          3ED77C841916820C8B4196DFCB63210CDF430E3E056083F9FAADD783BF112ECE
          921418D0E8EE729B9A4A26E7A7BAE45C215901EE2F33A96602701C57F6B3C391
          18F971A895F2B2078A7D2B05229B710228FBA4BF0792996CB1E9D3E32359ABAB
          AF2F3E0BBB3AB85F03E546D5407BA7A022E0F2FCB43AA0ADA3BBB912E02A9BBE
          FB11F8ABF10A215175E097E2347C0000000049454E44AE426082}
      end>
    Left = 279
    Top = 189
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
          643D2272223F3E2E2CB383000002E24944415478DAED966D4F134110C7E73E82
          21F1EBE8D1968742D3A011A3060C26246A20489AA6169B865404092960ADADF5
          A02DB52202B5ADB4E5413F8760ED5B5F123F01371E855E77F6EEE8B560F08597
          34B7FBDFBDD9DFCEEC74474044B8CC47F86700164291A2D2B6CB4A1F65198E75
          B6ADF64F35B6DD684C3B2E97C28BC11E02105C0CE3A6D5516DA3AC4C3C523E3A
          AAB7ABEF66F5EA1B141D89EEF8FE0122A1798100CCCD87F07387937C58354A0C
          3270DC3C8D5E1B93910228361CFBEF211A5EA400B3C15798B539EB066BC6F885
          88CEEC58D58101D0D77BF753108B8428C0CBB90505A04FE342D5ADDCCE78B79E
          2C049CFBF5F59EFD1590DE8629C0F46C1073B61B260C021716A370E9E8A763F6
          8315588EBDA10053337398B7DDA46ED5891FD16A61E0E7726190B933D47D9080
          8414A5008117B3F8C57A8B3BD55CAC89AEF58EB14E37D57D1087E4728C024C3E
          9FC12D6B7FF306995D536F713A33D6F5631952098902F803D358B0F4D7E3CCE5
          B8A1CE1FCA33C375A2779697209D5CA200BEC9292C5A6E3797EB4DE975BB1D65
          095653710A30E10F60C972C7F0E0E88685D95D33BAADFC0ED6D2490AE0F54D2A
          00771BB8D5A4CEC1F3BAB51C83F5D51405F04CF871BBFDDEF973DDC45FB9A51C
          85CDB53405703FF5E14EFB808EFB68AE3774B7CE45C5EB969F11C8ACAF520097
          E719EE8A03DA5CE7D2CA583777131EBFDB2B11C86E7CA400E36E2FEE89832D19
          D4E43A1F7F4E172B61C8673E5180319707BF8AF7F50FCE1957AB365CC687AFA6
          5FAFBC86ADEC0605181D77E33771885EADE7C8F5B32EB46B951014F3190A3032
          E642B56CD229AD4C8F1D975F6C29C6B4D9FEF65696023C1E1D47BE76E3EBC296
          0095DFD5C09E5A84FEF28A556DA790A3000F479E14950FED17B553763E3B56AD
          39014ABB851C2D4A0541305D4A47A5846E2DFF6878C8B41175DD5600A4784A17
          60F8C1E0DF01C81676C882BF0F0F75E75D696B23FDBEDE2E43A3E70230FB5C18
          40678FB32580DD62DE3CC0653DFF01FE0067213F2A6F84319C0000000049454E
          44AE426082}
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
          643D2272223F3E789F8CD3000003424944415478DAED964B4C13511486CFED6B
          A02D0494807D68A18228C6C4200441B410038A468DAC4C1A0DC6D7828D3B97B2
          72E54AE3236E0C1B4C7401BE4559B852A331D11812131B4208B6508BD102D276
          1EC799693B4C3B33656848EAC29B4CE7F6CC9DFB7FF79C7FEE0C4144286423FF
          01FE03FC1300F498E31A01D28F400CAA8300FE700857A9EEE0E5B51417B44500
          66CCC96A8967DDD04B758786D71C801E73E9AA03211089C7E91DF6C33F660A02
          20DD28FD4827E94FF64482C50820C39F6E580F862EAE1A40EE513D628AB1CB1D
          C6D61332E704C835016A08AD34560E5A7228485401E2AF5219D02B968A198A1D
          5054DB0F4055C2AA5B3C3C07AED3152240ECA53203D962F2AEC16805B3FB2898
          CA9BF8898280F4BC9061F1408E91FAE2C1D18A6B045820B68645527FC52E022C
          8DBA302DB6C4964024E182EA5DA780B25668E0B30089082F1E026405015A292E
          EBA30C428A033B6C6C1EED1501165F2C972010B6C3E6263FD8CB1CB2A5B3D26A
          504540E827E3B4465C990D0EE97E73CBEB9B22C0C2F32400CB6F475FA7015A8E
          0C80B8FF25CD2B4D8A599328443846CA86145729891067315147B5BE098800D1
          674980F99811A2DC46D8D6DA97B2AF9A40E64A51251BC275F578723CC1C48CA9
          EDAD437A0A7E3F4D02CC46AD50EADE0B1BBCBB579C48B3B6195EA055E3049921
          53FB7BBF04F0EB49D284133326D8DE7E1E8A6C65B21BE81C02CA92A04A36B2E3
          BC71FB2CBE4F8312C0CFC76E14EA3F19364073CFA59C22EA4EA7359F80640933
          17C1C4694F71D7F8940430F7C88D0B3113C4CDD5B0A5F1580EA7D31AF115B221
          03E5D33F65F28D7B3276C2C8433786E78BA1D2BB0FD63BEB35CDA31051737AEA
          BAE622203164E9F8E6CF00088FB871721661A7EF0C982D453A9C4E6BC4D3E369
          CD4520C3F8A9AE89A10C80E9074E9C8D52D0D87956D744F918340D9B882D38D2
          DF131240E05E0D125B35781B7CBA1FA5BC0C0A4CC0B27FBA4EF136FC32E84567
          ED1E5857B9292FA76BED078A2D1AB83B96AED00505C0BB5B55D8D471128C0643
          FE7BBD4A49323CC2DB0F39384E1D088E28003EDFAD41CFD636282DAFD2DEEBF5
          6CC35A2F2CBEC512801F3F042D9D03C02800A6EEBB6FF3DF6DE7F47C19E7D3F8
          B9394472DD73E2BBFA3761215BC101FE0246488BFDC994B0AF0000000049454E
          44AE426082}
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
          643D2272223F3E9516A97F000002494944415478DA63FCFFFF3FC34002C65107
          8C3A605038E0F75EA9098CFF1972FF3330326155C4C0F0E31FC3FF1E76D7E7B5
          945886CDB36007FCD923F51797E5280630FC0F013A622DD51DF07B8F3451F100
          0C89773F7FFDD6E6F17EFD62401C402E6064F8FFE73F23C35456E76705143980
          895B8E81593A9C8181999B54BF33FCFB709E81497B3A235607BCDB2C83D7014C
          4CAC0C1CE2160C1C62160CFFBF3F01FAE73303C3BF3F40737F03F11F86FF3036
          98FE031583C881C418198098579D81496F097607BCDDEFF19F9D57968147C284
          017B5A04BAEFCF1786FFDF1E020DFE053518CD72A865A88E42A86364F8CBC06C
          7B09BB031EAF96FD2F6B5E8CD084A41164E87F140B10BEFD8FAC0EAFA3207C16
          FBEBD81DF068950CD0018508C54886FEC76201DC72E46087598EC751AC8EB7F0
          38C02C0FC5028861BFA9EA2856A77B781C6092852328D113D86F8CF8454F7418
          D10465B33A3FC4E300E37402F18B6639118E82A71FA81CABCB133C0E304AC1EA
          6AAA390A9893585D9EE27180610211D90A57FC62EA4115FF07B60CBF030C6251
          131DD1791D530FAAE57FE196E177807E24AA466CA51ADEF20147F42101BC0E90
          D10BC3B0002328497514C37F121CA01B8C5180108E5F3C5991E11F46818EDF01
          3AFE44642B5C8E422BB490E29D780768FB102CCB89731476CB093B40D383ACA2
          F63FBA3A3C00A7031E031D20ADE102763D71E53F8E54CFF09F3C07BCDCA2F39F
          8B5794818B5F02A3F423AA52C291E890C1D71F0C0C023E38A3407A1250269B98
          96313900D826FC070C9BA972614FF3B03A6020C1803B0000DCDA00FD6888E473
          0000000049454E44AE426082}
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
          643D2272223F3ED1936C55000000E04944415478DA63FCFFFF3FC34002C65107
          801C6074ECEE80B8E29C953223D8017A7B6F0D88032E39AB411CA0B5F9DA8038
          E09AAF16C401AAAB2E0F88036E87E9421C307DD6BCFFDEDE5E58159DBB7091C1
          C8409FAA7220B075EB3686CCB4A441E4002087F6618E048076E276C02D3B1630
          AD76E8CFE0700048313A80A927450ED90EBC0EA007C0EB00EF2922607A6BCE9B
          C1E1809117050C17BC21B4C1D6C1E180911705CA4B2F81E9BBD17A83C301232F
          0A76D5FD00D36E4D1C83C30123230A24A5A5E9EA80E74F9F12D722A21520BA49
          463707D0D57628803B6020C1A803007D7C61D0C1E655A10000000049454E44AE
          426082}
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
          643D2272223F3E433B2F2E000004BF4944415478DAED967B4C544714C6BF7D20
          A5DA5AA0B12991A8095035B18931694C1A5B97B728259482A285062349691424
          84502C21845A2D25487988A68AA0281081E5B90894A6362D886DDAA6544B2ADB
          E00212DD02051658F675A797651F77EEEEEA4A30FDA7936C66E69BD999DF9C73
          E6DC111042B05804020156B29494947BAD7A5ED42F128AAE1E49883BEE689EE0
          59019457547DB1618377EA90625837BB30B7353529E9FE63010A8BCB646C3B8C
          61FB8461B0A873DB96BE49E3B6ED8DF9FAF8202AF21DDC1B94A3A7B70F23A3A3
          9C71A6FD6CD199BD1440C197A5A43E30DCD8260C3BD1C02E6AB0B68DB593BA1F
          338B78D123C4444540AD5E4065751D4AB019B30691713CFC8F6B38575228A000
          F2CF141369700467612C2DCADD88B331C39BC7D55344A3D8BD6523B66E7ECD68
          8DDEDB3FA376408956DDABC635C207AA70FE6C110D905750441A8322AC0B9A37
          E16FC4D1D76835D833370C77468B550202A15E07016B0621BBDEA103D1707111
          1B01E6E7D4A86B6C66DBEC1A0221848C1E22B148CE304C8305E0F3FC42162092
          362DD7DC66DD04C3B0B5DFDCDF08D73E4058903F5C5D5DF11CFB138A84108BC5
          4BF339B1C16E068D460BBDC1C0BA458DA1A1FBF8F3DEA0CA02702AAF803405BD
          CB33276D5EB31BCCE65F3C52C89C023B5C1610191602373737D3A6A6C065B800
          4BB5DEA0C72FBFFE06854231A1D6E8422D00274FE793E6A028CACFC605B800A6
          31860A3EE04D9D12BB0C8FB087B584A787076F53EB2D999E9A41CFAD5B9851A9
          A09D57AD4D4E4E9EB100E47E96475A02A37951CD0B3E4AA7ADB39D4C2358AB40
          5C6CB415C06405735BCAC6C15FAEDE18EBAEC6E54B5FD14198F3E969D21A1863
          E7F4B66EA0AEA1497BC1A0C51122C7E1FD9196935B73C31240F737DFA274E445
          6CEAAF4155E5451A203BF714690B88B1FA9977C71DEA26C8F57A15E2568F2376
          5FB0E5E416804518B6DFDFFF3B1A066731D32BC5B5CBE5344056CE49220BD8EF
          F45DE7EBDBF4E378DF4B80D05D3B8D4138C866C03B7707E0EBEB63CC8A8B45A9
          544276FB2EFA6FCA50535541039CC8CE25ED01B176820F8EDDC2B1C65B863124
          6E5B07DF4D1BD1F7E34F50AAD490CDBF8237C4E3F0126BB063FBEB707FC91DAD
          373AF1FD7737517BB59206C8CCCA61010EDADC75473980AF1F108D40B27E0D86
          C71EE20EF144C394070CE6D42C9EC57B6B1FC2EB650F4C4E4EA2AFF707545E3C
          4F03649CC82637FC0FD9DCF5C7A766ABEE8769EC749942E7BC0746756E36F345
          AC20593D096F570D867B5A71BDFA0A0D90FE7116E9F08FA3CC4AB9C146B77503
          63B075993D3D60B00CD2BA1A1A202DE313D22989B3BDEBBCEBE658E76DCC7717
          A7F69797A1A9BE9606484DCF245D92F8652D68D17919D27A7A5A97C84BD122BD
          4E03A4A46590AF251FD8A65B4EF0394ACD4F4CD9BC1BB55B5E82B6A67A1AE058
          6A3AE99624D09F5C27738055B7B59ABD0FDADBF262B4B7486980A32969C49AB9
          6C9F5D4E8F993EBDF6DADC7E475B130DF0D1B154C27FDBF1DF85CB02647FEB72
          BB2C8FD0079912A3D6296BA6013E3C7A5CC6FE316CA54ECA9DCF1D33BE3981F6
          2E5933FD287D9A67F9B90B15C49E9E9498F0D46FFB65015CB874C52E40E2E1F8
          6703D0D8D6416DF8CFC484DD79EE9E9E543F725FE813819605E06C59318090BD
          11CB023047BA5300FF55F91FE05FF10BD8932D69F28D0000000049454E44AE42
          6082}
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
          643D2272223F3E788D8DF3000002224944415478DAEDD6516FD2501400E0D39F
          6096F87F7CD20C75CE6DCECD890C87C80663B562A136B57640576B812182A46E
          1042706E12460673F3FFF8B8F80F388231A467622438EF7CD84DDA9C73CFEDBD
          DF4D5F0E8788709E83FB6F00E96CBEDD8DC73BDD1C3B1DE8CD3BE37EFE73CE19
          FFA9F66BBD7398CB582E02B0323914F820935B2F879E403E9BE208C04C653122
          AC32010456D6A090CB5080616DA2F834CC04E00FAC42319FA5800D338DD1673C
          1380CF1F04FB5D8E029286855254600278F86805B68A6F2920AE9B284B112680
          C5A50094EC020568090315596402F078FD50DE2A5280BAAEA3AAC49800DC1E1F
          544A3605285A12B5171213C0C28325A896DF5380ACC631FE52660298BFEF855A
          659B022445C3E4BAC20470F79E0776AA650A88C92AEA099509E0CE9C1B766B15
          0A1025050D5D6302989E5D804F3B550A884465348D3813C0D4CC3CD4776B1420
          88CFD17A956002989C9E83C6DE070AE023314CBFD69900266ECF42B3FE9102C2
          82889B298309E0C6AD193868EC5140888FE09B8CC904E0BA3905ED669D028261
          01FB6DD380D66AE85AAFFD72B6628ED8997F3E6850C07288C7D3BDDBE9BE7024
          60F7B9AC7DE9DFFE6BECCA8FB9A3D63E053C0EAEB5BB1F8E9FD54D9DEB9DB5DE
          E8BE0F8F5BFBB429E5386EE87F58B04B037BF9806F71E84DFAE78E02B0B72B03
          013EAFFBDF001AAD2372E0B7939381EB2E8D8D917CF2FAB5DF6EFA578061C799
          01AEBA2646021CB79BC303CE6B5C00BE0316414BDFCFBBA6CF0000000049454E
          44AE426082}
      end>
    Left = 407
    Top = 189
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
          643D2272223F3E81F0D21D000003054944415478DAB5D6CB4F13411C07F0EF02
          854894525A401490D0065128B4E0A368BC70018B9E88890D27DF81C2810308E5
          A007C1E081C4041250E12FF04168F03FE0258F022A054C0405130E85F234D22D
          5DF745DD59B62A09CC69A6DD99CFFC7EB3FD4D29866170988DE2006B9BA69342
          D86D30D441ADCA2EC5BC7C5FB1729F074ADAB48C2E7F07E151C09E8018B2CF84
          FA4E32F66F03DEC970F4DA97290168D5310997FC212731FF5830D8958C3DC311
          E8ADF44880023FB1606AB41177D25A114E45286641E9E4863C6FE0FCD1220023
          3220DE42467023F9114A92AA31B3D1073FE30B9D6FF1F9B4A326787D4B708C17
          F0E3E5513970D14F4CB2A536A2E8B81D552E03D67D1EC545A569A9CBEA418C2A
          1E0E97088CC9810B7E22D7B6538D28E6803103B49129A003BFB0F8D3BD27D772
          A0DE65E1C72B2E1509E8CE93115C3D5189CB3A1B9AA6AEC1A82EC403433BBA17
          9FF1390E0476C880D835EAB39D7F0076FECAB81C38E7577C1BB866D196A222A3
          93EFCF6DB9F062B69C8F46BAA17AA3088C89C0840CD0E60B00B7F35855220124
          1DC9804953141CEF3034DE2D34C3B9C046C308D1EC0275A302E09D94037902D0
          641A4072F419FC4F9BDB74A1838B66CB0D070744B2C088087C940171669A9FF4
          D43CB83F6046047204E0E1B070C8AB8A002B5B4F56411D99B02745E6B8623245
          DF9AD1F3BD85ED0B296AC8155214043EC901131DB2D6142494C29ED915DC75FB
          B4B06BE9C30D2611F860E13F5EFB2C0334B93431C19A5C852B89363C99288151
          5388F2CC0EBC9D1776BD7BB0D2E71D6627D42C503B2444B03625077268E2175A
          A66F64914A94F7EBA18B4A818FFBA16DBAC9E0A41170007B06B58322E09601B1
          469AD855994104FAF47CA950ACA852204F006A0604607D5A0E6493C04DFD635C
          4FAD867B5556EC425C80E9316CB1DB5E424DBF08CCC8812C9AD8657A8C19774F
          3F47588872ADD4FA975EA3FBAB50AE37666580FA2C1941D0FADB0D278E95DEBC
          8D2F5280BD328FE9FD085329D7A27DA16C3FC01685AD79AE5C8B57A6B555DB45
          51B87560973EA7508157BD76EF3DEAB0FFB6FC06CC04EBE0B9C1405200000000
          49454E44AE426082}
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
          643D2272223F3ECC276AFE0000037B4944415478DA9D95696C4C5114C7FF77DE
          2C2DA69D511AB157DA44A2FD2296448252B127824495482B88B5282A9556A92D
          456D09116B2C95DA4B17A5F882F820121F8C8E250C5A5B4DB7CCB4E6F52DD77B
          6F6ACC9BC5BCB8C9CDB9EFDD7BCFEFDE73CEFF3D020D6DCFFE831F20D2044114
          21CA5D10BC36A0CBF3B4CB76BD73102D805DC525F456DA1C88BCB489A710B92E
          2B3F733460FC77CD0CDB45680214EDDE4B2BD2E6FE75E0E794701C3A795D00D8
          3B9E6E3BA70D5058B49B56A6CD53010CFC2FE4D37D48101C48FF7532E86682B4
          66DAABB3DA00F9853B68D5C4F93E07717C230EA010C9312E9435A66067474E88
          10514CA93BAD0D9097BF8D564FC850360DE5ED38CC14A28FA51B3C1D6ECCFFBE
          1D3636D1E7D43F0793EDA7B40172F30AE89DD485982C3C4091E108BAF7B00084
          C18B1F0CE6361F539FDE2F3F93EC27B40136E4E6D1A4541659A64A18A2634088
          0E1CEB41C1D7745C699F1EF2F4B24D7B7D3C32A0F16AEF1EF5EC605772BF5630
          8668E9E4D21609D0D4D68ED42F67E0E2A24326581E4F7C730CA478FFA1B79220
          925422A254B1B1C6362C48AE417C2C079D4EAF3896BB3CFFD0DE0BCB84927FEA
          21F5ED519070224A115EE2A83E1F317A5E8937A0534223DFC0E361515B37081F
          9D2638DD26BCFF19AB52B42EBE15C694AF703F1900124E44ABC819CC632A9530
          99288B28BD0460BCB7F03A020481E275830EC5B52355006A64017307F89F51EF
          482811F9DF864AE34ACB122446B774858828B7E10511F52D563C6DCB044BBB63
          F9D2AC90F924812252C5537A3786798E53D66D60F48C2F079D9D221EBF8BC337
          F35A886014473DE3E2143B7BE6541588F84414A6D42E5873313AAACEE79C959C
          EF69C8C0E7C74E389B9A824E7CAFFAB61AF04744A13E5649C4818AF8D5520579
          17B7B246E47C5A8FBB2DA314115DB9743E6299939CDC2DF4DEF84541A526D772
          89652F66991F41AA4A7CF158B0D851009B6BB04F44D72F974606ACDDB099D68E
          CB0C4A704FB1094F06644A11A678E11E822CC756383D669588CAAF954506AC5E
          B791DE1F9B1514A26C7329B2AD9751DE3A169B3E6583E598201155DCBC1A19B0
          62CD7A1AA860F979C4C006743374E2BEBDBFAAC689D505D3F07AB43FEB8B8AB3
          359101CB56AEB189221D16F89F1542FC731511E93920D60DBED9F8BEE6467562
          4440B88913A7CFD13FE37022D2D282369657DD551C37FBD5783811FD1760CA8C
          5934DCE240116969BF0170CB1F0A8D42A81A0000000049454E44AE426082}
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
          643D2272223F3E4282ACBC000002764944415478DAB593596813511486CF2499
          C6368B22B5502AA5DAD01202468CA6557CB244345297177DE95351DB80587CA8
          D8172914B5082152B1051F2228E2821B8A7DABC1D045915AD1AA295511C4B8C5
          9AADC92CF7DE7132A643B4316D9CF187CB65E6FEFCDF9973EE508220004551A0
          86226DB53D28C5D953B168E7EABB8929295B2DC09736CB460D42C36C3CA56163
          090EA5D9637541EC5305100804746B1FF63E4B4F3DB7B1F159E0C425EA415D10
          35A902E079FE6832113B19BD725A8BEE9C03219D2688221BAC41E1A962407AD8
          510DF567FD11AE664BE4FB0C157BF38A33DF3CE55F7779C2A3CA0CC898F33EC1
          AC3BA5A99B7C0187F5DFA2294372065B5B5ADC71C50034EADC0D02BA0D981137
          0608627108EDDA6377F7DFCB9C2B027C0DD88CE5FAB2D704332B852C8012D020
          EDFAB463CEA30880471BCE0884EDC88483B4380621622DDDFEF9BD6200FBA861
          0D8DD1B858BD4EAA1EB36210E9A25DE1DE5CDF3F01BABB41737C9BF331E199F5
          73E140F8D0F48FB0DDB61738C5003CD6E81143FBA5C1CAD5C3667A6B78E44F6F
          D180C4C8A60A03C54D8BAD3167862A552FE00B25AE706B3EBF0C1838EF171603
          68AA1E825AD34BA972402C30480BD73F1C011697E6F57B0EB65232407C289C1E
          7F0278B21DE4D688009DCD0B54E5BEBC7631733E20FB52DE73F5EEAA032ACB67
          81A689D41ACD5207681DB7FE5A4F5E40CEC16F663E1986F1BE2AD01B2BC064E0
          A1669509748D430065167500D1B78310BAF6EB07CD5C08ABFB0498ED5D053B5A
          14E0C6C021A03F5E82AAE51AB0ECBC08CB2CCD0B5E88A2001D079A81613178BD
          3E30AEA85F30BCE821F7F5EC87F64E1F942C312D2ABCE017A8A5790055D3B392
          01FF533F0112BAD7E0C6C2ED4A0000000049454E44AE426082}
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
          D9DF000002274944415478DABD954F4FD44014C0DFF3B0C4289B90E00631B024
          207BD00F3109312660BCE84A34C428478FFD0670A61F81B309897BC0B3E07E88
          E1423424188CB17A90857677BBF378336D079BAEA535AE93DDA6FFE6F7EBBCF7
          6606890846D9F0BF09101164132AB7C6EB5BA1DFDF9C7E7BE295851D376B1B4A
          853BF5773FA5E5262707CF50C35B7C6B859492836E28CA48BE346B2EB31C54E4
          0DFA4ACCEEFE9029C1F7D773EB84B80DC0D77C8F7F52F58A491278D44FF787F6
          4CCB1399107D7B5577F9A913BFA45F9614AA5C49164ED20FFB6271F7979711E8
          F6F5E58CCB67B184FF8A250443257F8237DE9F7A991C24022359BBC32301271E
          051F9484CE9998E68E45E0FA79AEC0485EDC7609D0029073727A7E660057C10B
          09743B793EC5A0CB91A00E17509B2FDFE4C10B0B8C6475CA55F1D71A68D43B17
          5E4A90C49B7B38D1AB57C3CB0B9E4C46F9B0709D779281FA07020B37E1B19310
          E290C980067F1FA22170A914B5D126194C09773B241A1F4B2639058F4836E647
          8FAB9C936B9725CC6B57D7C794245710C17F9B68904DE8D1A36A5CC2514EF4E8
          7A418725903FD18AC093F67979DCE509689715BD7685BD7323192AC8C0B952FC
          9C4A31928737CCB262AB0B78A9EF0762719FD28BDDF1D3DA3A026D978127EDD3
          83EB2C4127A93250D09EFFE08B94E0E03E56AA8DC9165FAF94815BC9D298CBFB
          89C37D7578C4C25E37BDE1982DF31E546E2E4C6C052ADC2C034FDAE1526583BF
          7EE7EE7E2FBB658EAA8D5C7001F3F48CEFAA43B4680000000049454E44AE4260
          82}
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
          643D2272223F3E952AEBA8000003494944415478DAB595DB6F0C511CC7BF33DB
          4156BBAA5AADBD94A4446D5BD1A85BA82008125245897B5BD56C8BC48B177F80
          070F441042903441344444A84B09714914D184CDAA07AD65D516BB4A77DB9D9D
          F9399DDD8E99DD4D5BB7934CCECC77F67C3FBFCB99B31C1181E338FCE9387AFC
          F47A038F632139347D6775B52BF63DF7B78093A7EA3E586DE631EFDD9E866D15
          9B96FD53C0D113A78B538CC6C67565A5C299BA73DD3F7ABA26ED76385AFF19E0
          ECF9FAFBF979F662BB3D174D4DCFE5574E571DCB626B1C406C341FE4083B198A
          D7BD040232E8D0BE871FF78E1871C0644A4D6F9488C6B37804292C0D3518787E
          C3BA35484A12100806517FF132499244BCC1D00392433C67B8A700C2B7CD52AC
          794C14D7AFBC2B757C46916BCEEC59C3C6658F45926000CF96C8244396895D2C
          94DE60C53002DD41343D7926793B3ADE4632B86DA1C194C4DB9D8D1B1D9558BC
          7829323333156362C6324500BDB3DFFF0D0F1E3E924321B1F1BBFFF36A05D073
          AB7F003F3C1B82B50C3024E3938F43C37301F3E72F4446467AC4B837039609B1
          F9E2A5CB30A59AF6979596EC517BD07EC99A10C0F30292CD3360CC9A090AB481
          443F208B70FB4CB8D3361B6B56AF54A2A628A01774F3C635F8BEFE28ACADDDFE
          42057CB9BB8486A6D8909C55047D2B1857EC64E66F4152503167BDC31B5F0E5A
          8273316F6EB15A9ABEF9E993C7686D6B2FAF75549E5101EE7A1BD9A6ED608BC3
          CC241431A2907A1FABDD6C5D00F3840518352A0D2DCE67707F6887C562C5F889
          05F0783C703A9DF555155BCA54C0BB0B56B215D5448C2862485A7312A3CF11ED
          547309B232D3E1F57A9137B21939292EB8FC76B47416202D2D035FBE76BEAEAA
          DC9CAB074CAD520D4903EA2B8B16FEB47D32047461526A33042E105D2322280E
          C14BFF4C34FB6655D554579CD4030ACB233FA498B228C6BFA1B1660F59E4518F
          865F80291BE3CA3250A9E2349214D38400EBE4B59AB268A3FAA5510C5CAF85D5
          BD971850B04A6FAE2B41624D31EFD3342331206F454CCA1AF3041A693681F2BD
          0C08B02FD5EC98BEC842093592B5E672DC091007703380257761345271F0A58A
          367540C0A7ABF9644CC980D134BADFB2E8344D53B5A3AB1B18B93C2E03CB6176
          EFE8EF3F613083538E3D1C19BBD6B34B07F89FE3BF037E0293EAED363DF1B373
          0000000049454E44AE426082}
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
          643D2272223F3E58D96DDC000002B84944415478DAB5954D68134114C7FFD3D6
          545A626D8D58B38568C5A86D316D0FF6A656024A4351F1209E0A0A8AA239099E
          8482785310AC8A62153C082A7AA8A67E5528BD8A07AD1FD06A23AD0DD6D8C662
          3E37C93E677793EC26D9A4B1A50F9699373BF37EF3DEFC7796111118633032C7
          F0186109F67EB79DB16200FBE38F4B028C1D6A290EB87EB39F5CAEAE4505F778
          0671EAC4B18501278F1F5D14E0C6AD3B1A20FEC67A8511CE7054997ED2ED890B
          9001633B2BD4928D241607480C5993B9C10B01E485B2C9E3E97EAE9FEE6B190C
          09868799062CBD4406003E8C7EAF0A70F559D4833BFD1B088F03A1CF00AB5800
          41A0E0F8A002105FAB00D2DE29FDBBDF3540E36A2BAE765D02623F81C8242811
          E29B880392FC886A4B2228E5331E81AD728C2A80E82B1510356DC3CC7C0536B4
          1D416595257B43528C07F781627E2500A502EA0194E33348E71440F825077084
          F7CF7A346C75A266ED2639626A325F282F205DA01C3F1F98CA261EEF5400A1E7
          82FC3160D49BC40ED779949597672FD405CCF555A07136FED968B502F83B2890
          C86A31E527B4EE7167D512BAC5A4DB31A84876BC9D8B98B1CEF94255D1BC47A0
          A0644558AAC1E6B6FD79B5D480C5B3D1FBE3814634753F500181A702FD8AD4A3
          4E68854568CA4E97E2256593F1D330127B4CBBBEDC5300B3030D343957057BFB
          0154996B8B2A23BF7C46A5132125A28E4AA7F78302F00FD8E8DB740C1DFBDC9A
          0A0CA468A814295B4D2A2016BE383261EEED85A400A607B653205886E68E8305
          A5588AEE35606CD8E4FCD1297F3E0AC0FBA49D98A90EB62D1D0687A7AFAF5830
          3B1598EA2379D9E4F49DCD003EDD6FA67A9B037516A144DDE70275D950129294
          EC5EB977E65906F0F591834CD56BD0B0B1854F4AE42B230F58201B9210E5CDBB
          B7BE159DBD486400530F853EF91A37FA27FC8FF10B4EE237CE35DB619F3B3326
          0396D3961DF00FA182F10D3C2D73F00000000049454E44AE426082}
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
          643D2272223F3EEAD50C42000003144944415478DAB595DF4B145114C7CFECBA
          6B2D89690AEA58DB0FCACA68D507D72090B5C52C8BEAA5E8C93FA0DC8802C108
          2488080AC452242A42A187821EEC87545A161541DB8B21DB43B664B9A5E62F74
          D79D999D7BBA3B33EEDED9D9B53568E072EF993BF7FB39E7DC33F77288081CC7
          413ACFE5B6F6F1D94C5B3E12027244065996B59E6863DAD339A2CDAD0A4EFEE2
          960368EEBC85FDBB6A4016A9086DB288CA58B125121B2FCED70F75C3B2004DD7
          6FE08B2AB74E4496F4A27120C2BEA13BCB039C69EDC097CEDAB8909424120652
          E7BBAD02A4FEA2560EA191A24CAC20C5860960EBA5B73FCFB5B4003975E51ABE
          AAAC33786F04AAADD677530544FA8AE444711D08B1777A2672F4C270F3DC6BE7
          7E83A7BA3D61207B7C9D5A047D3CA69322CFFBD3F0A6F2402C25ACE7863DA0CD
          F5B9233500B537B1093A38F9CE03F6FA2A685A370516B3F52FEE2078BD5E1520
          3E5701AC1826C04C2B0B21B0BA1136E4D39D5918018C04E97B0968B9D026AA3D
          8A809ACD51856FA11215107EA602C2D66D30369B01EBCB8F43A62D4FEF101100
          8400A030A108A026C80230C1EE1E30071540E8290550847FA6108AB7BA213B7F
          535431F6B12AC608457BC63602B56824C9A50082BD7CF467804F7E192AEBCF83
          C96CD62F6404136D15688C86864BC67F0B590A60EE098F229703DF2710CA6A3C
          BA5C02B318198F41CB3F1A608B6B8441CB6EAF4301CC3EE6719E14418864C3E6
          F243865CC6814B47C3DA9C2C7465540F362880E9873C8E2F14402E5F0679FC76
          7DB828A5154DDC730D866283B5DAD7A500267B8A7164CA065B2A0E832D2B67C9
          CA4896BE64D19148D891E9F60F2A80891E3B0E8F0AE0ACF3C4AB20492926AD14
          A2AF261520842EBEFE9A153DBF14C068CF4E9C9E3741A9F348CA524CA7EEE340
          61C0EAFEE1D2CE3104FF830AE4ACB9602F7126D93C36BF62CAE854A03606F9AA
          D51D381B030CDD2DC502BB0372F3F834EB3E11C844833210221F5CB177EC510C
          F0E5BE032DB635B076E30EFA51C4581906608A68904098761F3F042CAE1688C4
          0023F7F8367A849D58EA4E48E7A1071CA1274EBBFD58C0C3DC25695D05FF0EFD
          DF803FE9CCB7C6B3A7FF810000000049454E44AE426082}
      end>
    Left = 278
    Top = 253
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
          643D2272223F3EDCF806360000030E4944415478DACD97D96F12411CC767A968
          624CA02D9A185F7C321EAD772F54B4FA64FC0BDA6AAB4F266A53FC538CC7932D
          574BDF49880F6A15AF6AA9B7560BB63E19136597CB18C302E32ECCCEEC31B3EC
          621A1C08D91966E6F7F97DBFC30CC34108412B0BF7DF009CBEDE31E468E342A0
          CA39D734A2038AD50A1CBB332ECC6A00CEDCEA2CB977569D1BDA9122B0F6D616
          9A5890F29549BF529E03F9B4438C5FE2D76B016E78E0166F9939D0764093BE99
          E43A10BF92E1E8000C7A3B6A681E29FDD80003652AF985EDD780CF330A38C001
          BBA50A2A20F1230C82AB5709C022036073BFCE024410E8C936155C0D717EBE13
          D7F9972C803EFA1A08F6E66A8F630B6E4BF2E3AAF4111EA88F1D9D2763F9578D
          007416841480176E66504881900B067846C6B2017ACBD4C513EA4300CFEB938C
          EF903CFDEA07055168A846D85B1F7BEEA90BB709AF9D74004F0FDD82503FCA42
          0640B2FE2A0B20B0EA07493E468550E68D1CC91300D44F786306604356B92C0A
          311058F183A2A4066D5BC7004F540A30010E97994169B22A4556636A65022433
          46352247EB00671F1380EC5B0640E72162813A4BAB45B623F0455683C79BD7F4
          3104F04805F08E05705005E0B50FA0A83199266A4CFB7400525BF6BD09807EF1
          340DF03356AB2B002309A2408E05D0714024DE350120673D954216D4930533C7
          11C04315C00716C07E513B23E31052B2D2649D9A000B286B1C5D0638A102406D
          B98F6600B0F151AB0690B39E94B32EF1C68ED2D8994104F0C085A7CAB300DAF7
          89D4A07821255C58D65AD6CBBAAC7563E5C72802189E2316E49758007B45C344
          341FFD5D112938F9B919E2ABDAA22711C07D15C0271640B7681400EA7CA46449
          B50AD5A3A70880B251163E3300DCDD22D5730CA0F2D1EA913C8B0086EE1105D8
          005D8C353098FBA73F24507A0DDF25C771619905B04767017AB8B8EB26F06D1D
          690A420E3EF72D086E2FF9B105C51403C0B55BA4CD60AC5A905F69A3ED23C5B4
          1900F56CB718D0625F3A807431D9B8ADE2746E82B6B2B40C87DAC4DF1CF8F3BD
          4D8C5FD65D4C6A5733877435836B7C35E3A4AB599572356B556939C05F0AB375
          DF0E31BAE30000000049454E44AE426082}
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
          643D2272223F3EB20532F2000004EA4944415478DAB5576B4C1C5514FE667697
          DD855616286DDADA34584A62ABFEE2873F7C020594D252D3685B3545D4181B34
          100C0F0382348552511A5A2A7D59B4B6B4B10DB640053451DBB44613638CA640
          B168A9E1D165792FBB3BAFEBDDA7CCEE2CB09374B23767F6ECCCFDBE7BCEB9E7
          BBCB40C5D5D078A25F92A4383AA03444A715454884B86DB06724E936A386407D
          C35172F6A934994F1228182FB9AD402DEFB12EBF924FC2969BCD5045A0AEFE08
          39FFECF372F060A07389F908BAEF33BBCFA823505B77885C48C99483FB81323C
          8F48611CF784288FDF8F24B59B7B4EAB2350537B90B4A465818804A2C24A4D82
          05D55C29D6933E64F387718B8B53880641464F933A0255351F934B692F28AE6A
          3DDF8B7DFC0758B35482757A125B664F61848B9685DEFBDE733DA7D411A8AC3A
          405A53B70784FE49EE2ACAC41A44992241E80EE832AF419EB552063A3762E9BD
          27D51128AFAC226DA92FF92622BC886CAE0939388BF0C815D0E8F4981C1DC2BB
          13C5B8664F0C5A9CA9B78EAB23505ABE97B46FDAE19A4427D850EAA842B2E667
          181F8805C36A200A22FAEED99139FE19788155DE11F4FBA6BE63EA0894945690
          2B29BB10238C603F57868DBABB304498E82F2C2803CC4E4DE09025039F4EBF1C
          A43FB8EF53FE6A5447A0B0A48CDC494A442D5F82657A013A43049D8871813BAF
          31F308D2CD2731EC2CBE79FA41F2ED23EA087CF1C90EB275C30D180D46B05A03
          F530AEC1300C38871D9DE675C8B59407AC5EF4EB0749FD874323505101B6F8F1
          55D512C316EAF474D5AC8EE2BAC1BD767A72027B468AF0E34C6240C8FDA3F174
          7F3D98C50A8B96E59019D781849801B061E1148A0D0097248281A1597CD8F10C
          04DAA482CDC9AE9884E1B1214C5F5B0D6631C2B29C1FC141A918F1CC00188DDE
          936B3730E323C0C2669BC5E5DF56A3F5CF7541C145BA18E87968226DE0470DDD
          CC42C2A2151C6815772286999087DC5B7474B8490013E3D3AE28D00F6D44805D
          8A0084A9B8B89DC3FF044B2BB390B068041EC7908F07C9A05BDFE9C411B0C2C0
          8AD085196569F87F27D062E478582639FC11B6170E66996BFE6D9BD3036A8E59
          4858FC0B89507FBB29070F19C6683A34B21AF046C36673606C361CBDBA776065
          D6FAC01409CC272C4AFD3B49731DF551FBA0D3E9E4A9F0A461C6CAA1FB5F3DEA
          BAE23165D7C9C03ADB2F051208262CCAED93E04B5301128DBD74F5AC2CE43433
          2E705E909A47F9C1EC8D2F825BCCD666FC854531F41EDFA36C37CEC51640AB75
          57BD37EFCEBA98B109B4C249C9F2ACC1FDA1F41666AEB0046D1C1EEB0C7DDA92
          9FC0BA02E9CEBD735759EC61387D3D0125073A42EEAC8C57587C115038BB39ED
          4A0CE3BB556F204C437C2F53D1C31D6B345EEF2DC4CADF3B71F1FC99D0093885
          A523E595A055EF2DC052D351EC36B579560F7002F0EB643C5EEB2982D9B6D425
          2C5F5F38173A8182C2F74967F2ABF31660B864C58DB5D958A2B5BBC179A065F4
          09E4F7ED8183677DC2D2DAF255E804F20A8A4857D2EE798FCF6F465E4471ECE7
          AE17E8E107D57777A161202B4058BE696D099D406EDE7B44D6AFFDFED11022E2
          A36DDFC36474C0C6B168BCBA01BFFC1DA3282C579A5514E1DBB9F93725223D2C
          2330879096E551BFFD0758392D6ABF7D04FDE6086561311BFB3A2FB725844C60
          A1078E9E6822068D1522D12227E72D5507989009B4B475F8F6DA98C5E2F347C7
          C4F8EE95FAFA7D2110ECBAAF04D232B62E48404958D45CFF01864FF578F1250E
          4C0000000049454E44AE426082}
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
          643D2272223F3E57F6F33C000003414944415478DAC5965B4814511880FF995D
          671635F3A10C3535C92BB9D68377820AC9B2B4DE2404236CCB022188AC0C4A2B
          492AA1122AE84A3D142558D443170CCBD4D54A822449D78A5297CD166CAFB3CE
          9939D38CADB2AE9AEE65F48799333367FEF37DFCF31F18421004200802E40863
          595C2546A818596DC7221F1A9FBACF8FB1E512F8A5894D559064176B6394C8E6
          00CEC63C4276A63CA90DF4B20B540390E5652B3B39864D437607201B03928448
          7C97F896CF945DC0D2F2E0D4E8CB9BC747759F9C700630C74BC4CB89ADB85C56
          018BC512465394CE643285589F5C01DC7811388B45C21946589C98D509665905
          D8DFAFEAEC54DA4193D94288075807BE09AA865A22B0575B94D4CA37B8BEEB77
          01D49E9D4B02DBC4D2C93D3F822A034C5636DE6436033B3CF4A1604771BAFBFB
          7E15F8DEBC4215432FE9C6988B03CC82C0235E073B5BFBECE9A92C071945DB37
          F7CB2AC077649E14A1278017E1A2C0F8C8E2A08AE04DBABAE972FC26E068CF8E
          A308F633E610E50A174F7DBA11BD7A5511B0B20A606DFA6BCCA375CED2FF8363
          242E8ED707E419DECC94E71701AE23B384E0B9BBC2185C023B4741B843E5E977
          FD2FD7678191E635A18BE9805E8CD9B0F1B24B7042E0FF300E880F29D41B6515
          E0B51937048C764F6E3C247D94526AA3E1F66CF93E09A0B69C6C921C6D153844
          BA359EF64C9B7E6D753560D9049A9B41B94E95FE516CBC94F16F2EC1098C388C
          84D5F41643CF5CD6F15A80D7661D16C167DD1B4F5CEA1C9DA73F32D775BC12B0
          77642E57615E6C3C1408FCE844E389173F8D0E488E28D4DB3D16B87AED9630D7
          A4FCD8671015F8155C4B2FEDF917861218B0277854C9FD7B4B890901F166D604
          34F41898EEA310144C4D2A3D19960F8AD4EB1EC145E65401E7C389D13DBEDCCF
          05EB601B24A92321408981247820481A94392D0074B8EF022E13D326755D5A26
          FEDD0C8F5D930A1252D44B41A5AE01324AE311DC2B0164D543577DE4A46711A9
          DB207A6B2300A1905FA0477B0F069F6B208466C45D434278D62188DE502BD5C2
          63B85702F5351A687A6F8082847E282A3B0FA171855E81BD1638B0A7102C760C
          555555109390E113DCBB0A9CD6C0BE8A0B40A916F90C9F5160B66DE8CF98B102
          F3155304E695EE8C0981858C0517F80B6015AFDF169F798A0000000049454E44
          AE426082}
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
          643D2272223F3E5C26A0330000026C4944415478DAED96DD6A14311480136F8A
          94222D76D7B58CAEEBCF838808457A51A8A58850D03E42F21499772805912254
          141141DA3E8994FE68EDD6B620A5580B9BE3493299CD7667A699C94A6F1A869D
          D930C9F74D4E72120A00E4320BBD12700528A5FAFEEBF53D21A56CD717B7E241
          407ECCD49804598F560EB8FADFC33C2FA0E058C7927A1E2AA1E10404D1DD75E2
          68E588E70A1C2CB45CB8522512803796762A49F4C2C17C39401CBD3FE49902FB
          AF9A0C6F4E03230192F0C69B72125D389074D8D5A3041E7D388C33055408DAF3
          7719D60A0BEF369628B1EB255104BFF3F1282E9C03AAFC9C8F18051C09B7B1FA
          911DDE78BB1787C0D3BA22012DF132D223D11B3F7C47E2482C674BF8C0BD058C
          C46D46800A20494769C71D3EB1BC1F57819712D0122F6E3120D7FA3BC63931F1
          CE48F4C0F5950F2F2DA025E6508252E1CE07D0E100BD9CCAC02B09E8AF9CAB61
          3870249CE5D91D117F7865012D315BC3705011020F12B0F146983091280F1F80
          C04D9C70B43FE63A6D13DEFC9F0245703B228079A2F9E9F7E043E003D7CF66D7
          4389E3C14DC25C38C65C920EA1499E009BB255D6C63CD1FA7C1CBE0C8BE076C2
          6D4EDDC05D940A0BB77D02EE1DAD2F27D513910FDC96CDA9119D27DCB384795D
          F64978099481DBB2F16C84D19C36ADAF27FE9B511FDCBEE3B1CE372687999A13
          06DE6D0FD8F6C1EA9F8BB7E310B82B41C09CACDC90E0A5257205769F8F07C353
          89A7C33A633A70922C0F7E7FED34FB48F67D665C3560A1705BBE3DB9CE7050C5
          B9E5193F5CFB9B7D285521D8991EC3F851160A772548723256F047EB67FCC249
          B83D3D2A88A4ED50782AF178481D58EA0A6E2292237019E54AE01F8CAC13EEFA
          AED1590000000049454E44AE426082}
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
          643D2272223F3EA742B5CE000004724944415478DAC5966D4C5B5518C79F7BFB
          DE51272C6915A8200185B139264416636274EB6C860CD92BDB04993217245689
          DF4C66F8E4073FA8C9D4190D331BCE19DC64443335812C3173BA6158C46E93B1
          580A8332D6924A4B6FDBFB723CBDF4E5DEBE5C6E0DB2939CDC73CFCBF3FF9DF3
          3CCFB9974008014110B0DCE5E8D1EE7CB55E31A220155FB61D6C7E33D33CE2FF
          02E8FEA2E7BDA22273A7C33941FB830B6B3BDBDBC7570CA0ABAB8B2C2C2AF136
          36D41B6E8EDD62EDD7AE9F3EF4F24BCD2B06D073BA77BB5AA5EADBB3F30592A2
          82D0F3D5D721261428EEE8E898591180DE33E7EC15158F54AE2D7F14386CFFF2
          95DFE9EB37FE3AF9EA2BAD6DCB0AF0C1B163C5F7E90CC7F1FA125CF52CCB1AB0
          3D151E521C68DA0D2A959207082C5070A6AF1F0F210ECF0B2B150A2F8BB8058E
          E3CEF200F460FE870482D7310A99D64F00010ED0FB1A8BEB88B0FF934F3F7F51
          ABD31FB76EDDACD26834A0C5955490A0542A01710822B62300FC93E320140A03
          C3B2405114381CE3307A73CCC7033003F96C267161C1A6766188B3B1F768B075
          EBF5FA7DF5DBAC1A9D4E1715E5F867042201B0F864580686AFFE014EA7D34385
          68EBE2090C142039478E4F622E14A62B73EAEE8A82A9FBC4A923D88BEF582D9B
          956BF2F29244B9F849FCE39D875F2EFD0A888061CF9DE9676C36DB7C560052C5
          11AC858BEE06D8DFD49400889E42ACFD6D5F3F984CC613F5DB9E6B1505A15C00
          72D543A028D88B436C55CA988F02E8BFAC851D3B76C7771E0389B96260F002F8
          FCBEBA43075BCE8B00E6BE2B9404204915684D9B406BDC0488BA0D88F101700C
          0E0A1A57060B30E09A5F0D43EEA7C1626D8CEF3C0E1081C1EF23237FC2B4CBF5
          59CB81A6C32200CF052BD218CC90F3400DA48F45CCC7F8010526B0E1102F0A5C
          423C0232EACE8729B4156A9F7C960FC2B1B15B60BF7603CACA4AA1ACB494B732
          3B3B0BC3C357EDAD2DFBD78B0026BF312373ED5B51638C4820B2538412EDC571
          3ADA9F983734550EC8D4080F1797C06F5786201CF040ADF167B0BB2B612E6C84
          0D551B21F7FE5C38FFC38FF4CC9433076750380E30D15B88013A130231717E87
          7406A80448A46FD0F91470861A9871DD86F2357F4375EE4FD838CDAF99F417C3
          C5BB759093670E7ADC1E22100AD7BCD1DE6617033C611309F0E20281A5A0C6BD
          0FC2A8B702AAF22E8151EB12BB095716DF34271D6FBF4B28C86A9AF2EFC2DF05
          BF18A0E6B514A328E9C8636D240595CE4DD1B67ACB44CA9D9F00A83E9CD1BF09
          C3745650F1F8898EA92D5312008FB7A5A55E36289C496ACBB404C0C656515A09
          0544FD493B4549F3E250A27E8E179306A86A16075D9A5C5F122ABA462CCEC6C5
          A40136EC132F1418CD9C8A495071F709E6098A2440E1637B5204528E325B2840
          5900ACDF991460B40CFF4AA422702917BA34C0BA06196995092AE9D212F85D3E
          40E5F31241970D547AF1A5012AAC59DFFFE2544C0D3AD9009318A0A07C0B4F2F
          EFFE4F7FD526079D6C803BDFAF437A8311F4AB4D29B79FAC8F5286A013968520
          406E7DC61328F808B7DBE5FC19FF9742F0FF47F071D1DE695B5A807B59EE39C0
          BFC6650987C5C1BA800000000049454E44AE426082}
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
          643D2272223F3EF33BA52A0000038D4944415478DAD5965D4CD35014C74F616C
          7C1AA204850DF912086884200411641025468931F2A2C9A241118CE2836FBE19
          12129F7C52036A40E4414C3451FC0045795063D46830A092284EC405F9986070
          806C6BD763D76E5DB7B65060063D49B3DBFF6DEFF9DD73CE3D1D818840100428
          B1CCA7FDA8E84105D6AB4F2558DF0B0148BBF5C16F009FCA372C1CA0FE521396
          95EDF2D2DEF6F4427656A6E85939DD69EDED1D70FC68E57F0A70ACFAF012830F
          D070F9CAD200FA8B54EC7DEA33CA3F006457CC3902881AE63640EA0506EF378D
          70B6E96BDD692900E782BEE68E94EF9C531701505DB10E39E7426B1CA883BF92
          02B24BABE878B901CA2E44B1F7ED27C6FF0D8025A7602E0014CC3035F0775320
          74863E03E74FF32007907CED1DAB7D316CF40F80EDB12B0202675211B8FACD07
          605F1CC0E47366346FFD8ACD669E00EDA12816C0FA481C011E44A0B5985C2940
          3B80A51BC03AC45C2640728AD128F6429AE2C7EC4593A239021C408465CC1069
          67C25980D94E2DBA9DCDD211306ED342C2A683A0098D92C17700D89902B48D00
          3A9C0E48B173C1180510BC0E8EDB81B99DE52CC0CC434F0A8CE67048CE314078
          648C20140E7E3728E1C039E674524617478346B22628EF493D0B30FD80037030
          EDE82313D5BCDDB5C0F63FD6B9C719FA2C227242537C34785D22254EDD81F614
          4DFE0B230B60E9E000A6AC8160A1E3203DBF820B87C8B97B215246F74048EBDC
          F304DA47555B5EC6F0A7E0573B073066098515BAADB02669F3BC0BC9E6D6AB16
          48499D40AA5555F8DAC0034CDEE78A70605405EB0BAB21382C52F00239870371
          4A50221ABE3A53B8156A7D4F0B0FF0F39E0E9DF91F340740EECE538A1712A644
          EE0470B5E0BD09CA46C68794F699788089BB3A9CB6AAC0169400A9D97BE6A874
          52469F271A025026FC2695BE2F9EEB3B2E80F13B3A344F8540745211AC8A4D93
          2D1E9113A94A77CDCB6E02ECADEAE2CF062F00739B0E07C710B2F49510A40EF6
          43A593B29B408A32684A075ABD00866EC6E2984503D92547142DB4980275C3DA
          ADD331E1653F46BD008CD71390084B84A40CBDE2A33457AF972D50A08CEA6D43
          299E0F9D0BE07D4B12C6AE2B8095D16BFD56E9922D9AA01BD5DB47AA4400AF1A
          56634EF101080C08587CAF974889578D30E58734ECD5EC186E1301F436276242
          7A01444446CBF77A256D58EE83C598D50ED8FD66585D520B9408C07443779100
          AC52F2CF7831C6AC4D2312E7E3F77F3FE9F57FC30DB09CB6EC007F00FCABEEFD
          642F58970000000049454E44AE426082}
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
          643D2272223F3E1C2DFE500000045F4944415478DAC5976B6C145514C7FFB3DD
          EE42BB54102DB65DEDC3228891182C41106D4BD9154425A031E062D344C50F7C
          D00F981A0C5AE2038260AAAD141AB054A028A8ADF465850F7C306234261AADA2
          5D1BD294EDC36AA02DB2BBF338CE631F333B33DBED7CE12677E7EEB977EFFF77
          CF397366962122300C83E9B6FDB5F5FD8220148A1D52E7A52BCF4310F793AF11
          BBBAF3DAEF7FD5D7EE2B66AC02ECDDFF3E7DE15D0F8113053941B9B2EAB1109F
          93EDEAB180C77E3D8686FA5AC632C0EEBDEF519B7703F86988AAD7ACEB6D4663
          439D75803777BF4B673C4F6A0554A2C660F1F1A3BF35E148E301EB00356FEDA1
          F6D54F695D3F85A81A6CEDEF1FE1E89143D60176EE7A9B3A2A9E3677BDCEA6CD
          13EFC5C3387EF4B002C09ECBF98001B34D44B119898978FF09847D4E6FE08DA8
          6DC7CE5DD459B1D93CDE6CF2E4F4FCD18893C79A1400EE5C2E6F26AE6EE2DA8D
          4EEF50AB34AEDEF13A75AD7A2649F6ABC6061E5AFDE7419C6AF938EA813C4AC5
          ED62A4C64221F65ED7BABF87B757BF46DDE55B92C65B03960054D1D780CF3F3D
          313D809827C4FE6A4F257ACA2B2D24A0325ED5F721DA3EFB646A0022AD70B4BD
          D2EDC3D9B22A0337A7560FCAFC75686F3DAD05301423BDB8B46E7BD7A678594D
          28BDBC41198E765BF65538170730F18D1B1DC73B1580D0D988074CC412DD2F7D
          EC39BF068E8C5BF1D296C5989575D3742228B7C92B977957F156BB0C10FC5AEF
          01B558A2576C691948773F01FB9C12201400B113E224277712B8D858EE02AB9B
          63C083C95C748D59F08E4B06B8DE934751B1EBFC2C8C85F350707F259C19B798
          F0F340784C141F02F19200AB17578D490511B3836F4D5BDAB35106B8F6553C04
          FE5117EE2CF1C1353B4775743E761A321090C68A9D35B1EBBD2110BB2D7DD9F9
          0332C064B702C08BE5E8E220B0ECF11AC8F54F168F8B51C2263A11818B792366
          37088964E7293CDFB9FC5BBF0C30DEA5004C04D3302EDC8EBB975729EE3014D0
          9E940CBC21CD1BDB95F50C8587ED2B2EE4C8CF21E9E36AA70230329E812CF743
          B8ADE8812937328DAD261758433B435C8B7DE5F7BE18C0950E2509FB87EDB867
          E556CCC89CADFA019B44401F1232F046A25D4CDC2A47E94FCD31807FDBDD24C5
          FFD2A80D4BD756271531CE74D6F40E5042A83D041762F3677A7A076200FF9C71
          D364D08E507A01EE5AB23E49A6B326F629BCA10215DD3F602FEDCD57EA4E0460
          EC4B378D4ECC4476D1C3989BBBC03479742246991E99373D04C22D8EB23E9F06
          60B4CD4D974608F7953E8774C78C14329D35B147D7B3A687208EF3393DFD2D1A
          80C1D3B93432EEC492F2E753DAC84A824661C3C1C91CE97D4203E03F59484C66
          018A1695A67C2B594A50707E47C5E0FCF8832E02F04B7311E5163F889BB3EFB0
          94E966F54057A221343A3C432FEA00BE6B98472565CF22CD66B35EEB0D42A2C9
          1131FD48C006E72381361DC0CF4D8594BF7005B2E6CC33AFF5A99461B30796D8
          8261D08F3F041CE535E0740003A7DC0719D00BA9BC195B69E2DEE27F56A62E7F
          D3E59735EF1B51801BD96E38C0FFCCF4AE1B64C656FD0000000049454E44AE42
          6082}
      end>
    Left = 406
    Top = 253
  end
  object SitesIncrementalSearchPopupMenu: TPopupMenu
    Left = 279
    Top = 309
    object MenuItem36: TMenuItem
      Action = SearchSiteNameStartOnlyAction
      RadioItem = True
    end
    object MenuItem37: TMenuItem
      Action = SearchSiteNameAction
      RadioItem = True
    end
    object MenuItem38: TMenuItem
      Action = SearchSiteAction
      RadioItem = True
    end
  end
end
