object LoginDialog: TLoginDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login'
  BorderIcons = [biSystemMenu, biMinimize, biHelp]
  Caption = 'Login'
  ClientHeight = 575
  ClientWidth = 630
  Color = clBtnFace
  Constraints.MinHeight = 375
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
    Left = 269
    Top = 0
    Width = 361
    Height = 334
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object ContentsPanel: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 293
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      DesignSize = (
        361
        293)
      object ContentsGroupBox: TGroupBox
        Left = 2
        Top = 12
        Width = 347
        Height = 278
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'ContentsGroupBox'
        TabOrder = 0
        DesignSize = (
          347
          278)
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
          Height = 224
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
      Height = 293
      Align = alClient
      Anchors = [akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        361
        293)
      object BasicGroup: TGroupBox
        Left = 2
        Top = 12
        Width = 347
        Height = 229
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Session'
        TabOrder = 0
        DesignSize = (
          347
          229)
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
          Width = 137
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
          MaxLength = 100
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
          OnChange = TransferProtocolComboChange
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
          OnChange = TransferProtocolComboChange
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
          Top = 193
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
          Top = 193
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
          Top = 193
          Width = 82
          Height = 25
          Action = EditCancelAction
          Anchors = [akLeft, akBottom]
          TabOrder = 13
          OnDropDownClick = SaveButtonDropDownClick
        end
        object EditButton: TButton
          Left = 12
          Top = 193
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
        Top = 247
        Width = 347
        Height = 43
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Note'
        TabOrder = 1
        DesignSize = (
          347
          43)
        object NoteMemo: TMemo
          Left = 7
          Top = 15
          Width = 333
          Height = 19
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
      Top = 293
      Width = 361
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      OnMouseDown = ButtonPanelMouseDown
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
        DropDownMenu = CloseDropDownMenu
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
    Width = 269
    Height = 334
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      269
      334)
    object SessionTree: TTreeView
      Left = 11
      Top = 12
      Width = 247
      Height = 285
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
      Top = 277
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
      Left = 160
      Top = 303
      Width = 98
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Manage'
      TabOrder = 3
      OnClick = ManageButtonClick
    end
    object ToolsMenuButton: TButton
      Left = 11
      Top = 303
      Width = 98
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Tools'
      TabOrder = 2
      OnClick = ToolsMenuButtonClick
    end
  end
  object ComponentsPanel: TPanel
    Left = 0
    Top = 334
    Width = 630
    Height = 241
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 2
  end
  object ActionList: TActionList
    Images = ActionImageList
    OnUpdate = ActionListUpdate
    Left = 36
    Top = 341
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
      Caption = 'Login'
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
    object CloseAction: TAction
      Category = 'Other'
      Caption = 'Close'
    end
    object NeverShowAgainAction: TAction
      Category = 'Other'
      AutoCheck = True
      Caption = 'Close and Do Not Show Automatically Again'
      OnExecute = NeverShowAgainActionExecute
    end
    object SessionRawAction: TAction
      Category = 'Session'
      Caption = 'Edit &Raw Settings...'
      OnExecute = SessionAdvancedActionExecute
    end
  end
  object ToolsPopupMenu: TPopupMenu
    Left = 144
    Top = 397
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
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF00FF00FF37581B7D000000FF4944415478DA63FCFF
          FF3F03298091640DFE1149CF9EBF24A84E4A527CC3F2B9200DA6F6DE8B97AEF8
          FFEFFFDFFF0CFF80E4BFFF50F23F03980472418CF2BC845307B640354C99BD1C
          E8B0BFFF18C0D2709D102E54B0A50C4943FB94152051883A10095204D1F01F64
          0F8831B13A16A1A1BA7F35481103480350DBF75FFFDF7FFBF71188BEFFFFF4E3
          DFD79FFFBFFCF8FF6A03920DC5DD6B81C6BCFAFCEFE9FB7FAFBF8054607AFAE1
          8A288486A8DAF5F7DEFCFBF60B5F285D9A1B8CD0609BB79960B01E9EE48BD060
          97BF85A08643137D101A641535086A787CFF0654436064CA9367CF498869B4B4
          64E6E00367438C444F4B700DC84A91019A3692532B00F4ADDCE1B43EDF370000
          000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001694944415478DA63FCFFFF3F
          032580B1A18181C988456E3F906D872677E8DC9F478E40F97F780DD8DC2C1722
          28C9BFDA3AC00045E2E8860B0CEF5E7C08F0AB79BC11AF019B5AE4AE5AFBEA68
          0949F0A148BC7BF189E1E8E62BB8F41DF1AB79640B33E0BF6B883649FEDEBDE6
          2A03D00046B801BE09862419B079C1793403E274182E9F7CC6F0F0D63B061223
          E515C40B41CA0CA70E7F63B04BD94CB4CEFFFFBE336CEDB67F063640DF4C94E1
          F3571E066D730506863F6F80EE02B98E114223B391E847B7DF305C3A74771ED8
          0051090E06157D35061151A0FBFFFFC2A909993EB9E71EC3EBC79FBDC106B0B2
          3133B8046A3130337E81C42D4C210ECDFFFE3230EC587DF3C7BF1FBF84C10648
          4A7232A8A8F2026DFF0355C8806400030A0DA2BE7EFFCB70F6E4DB1D7E350F3D
          C1069839DB3088C92921D904338001C14712BF75E618C3ED0B57337D6B1ECF60
          DCD42CF71A28264252E431307C67FBF34FC6A3E1C93B468A7323A5060000367C
          916AA60EF9940000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000010E4944415478DA63FCFFFF3F
          032580B1A18181C988456E3F906D872677E4DC9F47F640F97FD834C22C66DCDC
          2C172228C1B3DADA471345C1D12DD719DEBDFC1CE057F378235E0336B5C85DB6
          F650D21112E34251F0EED53786A33BEE61773603C3319FEA87D63003FEBBFAC8
          92E4EFDD5B1E33F8563F64841B404EE0A118E05B798624CD9BDB4DD00CC84E65
          60F87D1D3988C0084A2068A8D8E63927D00CC88C6060F8751FA706748337CF3F
          8B6640AA1703C39FE7981AFEA3190815DBBCE8329A0189C018F9FB114903B22B
          B0B860E90D5403EC9C151818FEFDC4125CFF517D00153B74E025AA013EA9E998
          4EC56A0884B165CE1C14033E01695E52A2F13FC3FF2F7ED58FC07A1829CE8D94
          1A000054D89AE168C695320000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF00FF00FF37581B7D000000E44944415478DA63FCFF
          FF3F03298091640DBA412DF79FBC2146A9A28CC8A5B5D58C3C660516C1E5C468
          38B1B6F3F3C97E9086B5F31A88D1109CD480D0306FD1AAA4B830080991C61441
          68787EA807CDB0FA85871BE36DD10425ED4AA01A1EEDEFDE74EC1650C8CF4A0D
          C800922D4B8ED4C4D8A009CA39964235DCDBD3856658C7F2A31591D668824A2E
          65500D377776EE38750728E461A6026400C9DE55C78AC3ACD004D5DDCBA11AAE
          6CEB40336CD2DAE379C19668823A5E15500DE737B7A3C94DDF702233C0024DD0
          D0B712AA212DD99F9878983577234E4F6305504F939C96484DAD0057D58CE15C
          125B0C0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed'
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
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001F24944415478DA6364200284
          95CD0C6060648CFBCF2910B1BA21EC574CD54CC99F3FFF0BAEEECDB8C6489401
          A5B39264C505E63E7FFBF1D2DF7F0C37599919FD7EFDF95BBDAA2BBD97A00101
          05F3053839FE6CF5B3D7B712E6E76678F3E10BC3CD072F19AE3D7865BEBA2BE5
          145E03C2CB67ADE1E7E6F033549763F5B1D76390111764F8FCED07C3ECB58719
          4E5FB9DFB8A22BBD81D1D4DEFB1E50AD223603D8B8F819A4D44D197C9CCC19C2
          DC4D1838D85819FEFF6760D87DE21AC3EEE3D7181EBD7ABF1E64C0FF29B39703
          25FE3300FDC7F00F48FFFB0764031582E8771F3E339C3E779A21CEC782414A54
          80E1C59B8F0CAB779F65B871FFC516A01DABC006B44F5901D108D504D10C31EC
          C3A7CF0CE74E1F611015E461606662629010E167D875E2FAE6959DA97E205782
          0DA8EE5F0DD10C3500C1666078F6FC31C3A3FB7718E49535187EFDFCC570EDC2
          11863FFF18FA5676A515C30D28EE5E8BD004F60203C3E71FFF193E013188FEF2
          F33FC3B75F400CA49F6CCC6290D1B5B75ED199760C6E405EE706B0FFDF7CF9CF
          F0F6EB7F86F7DFFE33FCFE8B3D662ECD0D66387D702B3CF6C006C4366C6678F6
          9181E1170E4DC8E0F0245F4C03ECF2B7109320C1E0D0441F4C036415358836E0
          F1FD1B1806DC01D2CA449BC0C0701F688012DC006C2A40AE82B1916DC3061871
          694407B80C02003A1403FA6C56EE420000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed'
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
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF00C9000EDD0440D5000000624944415478DA63FC7F
          928F8114C048B206FF88A467CF5F1254272529BEA1700D4883A9BD77C3F24B04
          353444EA9DEA3A3CAA81961A641535086A787CFF0654436064CA9367CF096A40
          C4345A5A322BB385B32146A201840664A5C8004D1BC9A91500C1524F7165D321
          E40000000049454E44AE426082}
      end>
    Left = 36
    Top = 461
    Bitmap = {}
  end
  object SaveDropDownMenu: TPopupMenu
    Left = 268
    Top = 341
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
    Left = 396
    Top = 341
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
    Left = 398
    Top = 397
    object MenuItem1: TMenuItem
      Caption = 'Site Folder'
      Enabled = False
      Visible = False
    end
    object Login5: TMenuItem
      Action = LoginAction
      Default = True
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
    Left = 543
    Top = 341
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
    Left = 542
    Top = 397
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
    Left = 144
    Top = 341
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
          61000000097048597300000EC300000EC301C76FA864000001D14944415478DA
          635468179AF09F81219B91818185810400D4F307A8672AA37CBBD06F491D6116
          16766652F433FCF9F997E1F995B77F18812EF82FA52F02165CE3B09741925306
          AFC6E7DF9F30841C7006B39F5D7CC3003600E802B0C0319F5B0C565BD4F01A80
          AC06E8028801E29A82608113FE77182C36AA80D9F5463D0C5D97EA18BEFFF986
          6200B29A97D7DF430C105513000B9C0ABACB60B64E19CE7EF2F51143EDA90286
          ABEF2FC20D4056F3FAD6078801C2CAFC184E3D137A0F4CFFFEF79B61CAE52E86
          E5B7E733FCFBFF0F2C6EB25A092CF7F6EE47880182F2BC0CE7221FE0F5FBF1E7
          87186A4F1432EC093CCB60B45C012CF6FEE167880102323C0CE7631EE20FBC67
          07196A8F1532EC0D39C760B8441E2CF6E1C91788017C92DC181A2EC63F827B61
          E2D90E86A5D7E782BD0012D75F280796FBF4FC2BC4001E314EB0C0E5A4270CBA
          F364E0ECC79F1F32941FC866B8FCFA02DC6064355F5E7D8718C025C40116B89A
          F694417B963498DDEE3891A1F94815C3B7DF5F515C86ACE6DBBB1F100338F8D9
          C002D7339F33684E97C41B16C86A7E7CFC0531809D87152CB027EE14830C9F1C
          5E039E7C7AC4E0B2C80CCCFEF9E537033833B1B233B3303232329002FEFFFFCF
          F0FBE75F7066A2283B03003162D2A9436FB7CE0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000002104944415478DA85925D4853
          6118C77F679E6D65065674A3D5364AA8AC95B5AC2EA63082A222220AFAA08220
          926E76D75D621041DD881084171131903EF022B1841538EDC3626AA62969BA95
          C329F6A152BAE63CE7F4B6DA611B6B7BE0F0BCEF9FE7F93DCFF99F23F19FD851
          796048A412B24750CA02D0EAEA1B58504151B5A447DC154DE81AD72E9D252BE0
          72EDFD78833A1B22AFC78D262FE5E7D63BA8FFA0B7AA4F6607B8AF3762F8F686
          25036E2C25050C0D2AF88ABD44621A73F31A81861C80AA0B27281ABF8A63DF66
          C63E8EF32C7C8C91C28B7A4D67FDE1CC809A1A0C0BE132C5B5E517E587CA31E7
          9BF0DEF6F1DCDA414C5EA1D7B5D71D44129302E26C4B886659E5A8FD333BCB16
          E1D8BF1DD92433FA7E94A6A6295A2776A70C0A053FC401296ECFFA8E63B546D8
          E8DC8024FD5DB0FDDE0B662667F8F4BD004F972D99119492DDFE0390BBCE0BE3
          5E211B65F69C7361341BD184DED7D64FC7CB49AA6E0CA6BCB694705B15459118
          7CF9A1B22C544BC52A2F9B2A4B894563743EE9E6F5DB288DBD6B98570CF1467F
          DB6349079CAA7EC4D8B4583F0A06750E57C8C9DED3DB50C55AFE663F4F7B1663
          2EEECE13E6AAE986C70115EE665D289AF2E0CCBF89CD6EC1DFD24BFFF22B3CB8
          EBD1276604ACB6ADD7852396164AED850C0F7CA535BC8B89C8CAB8DBD900C322
          AF4D08671C01F129151EBEB3301D3125E4110158971190E90F4C36295748E98D
          E9910BF41B9718F5BF3595289E0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001D04944415478DA63642011CC
          E7E79F2B2D2BE9F7E8C98BF2271F3E2C60244573BEB9B39EDBC727E78D755498
          1E9FBEC870F6E38760920C48CCAE5C9992181DFA60F79E7FAC933A3FBFFEF347
          96680336B6CAE819D8689EBFF6DCF4E7A35F86AC27366E6B98BF767E2BD1069C
          5EA877C2D85ADEFCCFDB470C57AF7F7D5BF82046E24043C31FA20CD8DC2267E3
          E06F70989B9D97E1D3E30B0C878E7F8AF2AB79BC1C24479401E7979BDFD4B732
          50FBF5EE35C38D4B379F6DB87F55B6A181E11F51066C6E960D700AB25ACF2960
          C5F0E6DA1A86E3A71EF9F8573FDC0A93670C2F9BF51F97664686FF0C85269B19
          CC6CED19BEBD79C670E0F02586450FC251D5800C58D8928CD5806757D6324872
          6E6760974E65787ABC838155AD864140DA182E1F5F331762C09CA6248694BA79
          0C469AF20CE7AE3F6400F141E0E8BC0C06352D310676B67F0CAF5EBE6150F19C
          816201480FD880E9F5896081CCC6F90C3036085C5AD7C9A0ADA5C1F0F8DE5906
          51D328066E510D140340EAC1064CAA49000BE4B52C6080B1416052753A8387B5
          2683BC652803BBA034861741EAC106F455C63314B52F64D0559363B87CEB1103
          880F0235D9310C652DFD0C7C82A258C308A4076C4067591C31C9010394772DC2
          1F8DC400000497B894A0FF699A0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000002D14944415478DAA5935D4853
          6118C79FF3B1B34DCF399BDB94CD74C3CFB4594A686520481021120445371178
          514E2A82BAE8AA4FEAA6ABEAA2C86910095D541411444184601F584D4C59593A
          9D3343DDDC3CE7CCB98FB3F3F6EED42451E9A2E7F27D9FE7F73CEFFFF9BF0442
          08FE27880C80200870B3AC5321896E12504FBB18BDB556F2CDFC7C964AC6EF11
          08EA0805ED3D2A495E15D0C5714EA0C85E67CD46CBB8CFAF2CC5126DEDA2D8F3
          777127C7598080A776C7869D168B09063C4381764172A800B7819FAEDB5255E8
          282E84E8620CFADE79944422B5AF43929EA99D7373AD3445BE2A2FB53B9DD5E5
          F079F81BF803D32FDA05B1E53780E71F590B4CFBEB375703491220485178FB69
          38994AC9BB098DC60FE954DFA68A127BB9A308BEFA26617462EAA3ACD5ED3A36
          371755010F098289F05CB789E70EEFA8A9049AA2605E90E0FDF04814DFC76B2B
          4A2C766B3EF8A667C03B1EF002826697248554FDB2225E04206D3C7B8DCFC939
          D95855060C4DC3EC82088AA280CD64841FA1300C8E4F0610A56972452281CCD3
          5600B2E1E6D9F3B95AEDA5EDA50ED0331AF56C064FE3F14F8548201A8F88E258
          36771D007F88A1C8BBF5C536DAA8D7A96713E105F8321B1AC1A33765465F17D0
          C9F327743479639BB5806471772191848CCD8C5A06BE4704188B081E2C5EF3F1
          6030BA0A709B67CFB2347DB901EF588F455C9465E80F86E53442C9ADE6BC1C33
          8678B12653D158AF5192F61C04482E03DC46FE2A4751671A781E18BCC63816AE
          7F4154E20A6AC36D7C98FFB29663D9020C19C22BFE994C3EC1BA1CB880909235
          D27CBD5E6F32D354060B1F624BB0A8A0532E51BCAEEA826D0E24D1EBD4692D36
          8D060697E2104CC99DD8AD1D7F0086560AD0E33A9A6646D36990105CC197E756
          58D9602803947E5D49D176168F3420CB61EC44F3B20658C0562CCB7D9C7BC725
          4AA7D7FA4C5D267D9192A69FE3CF5482CB5A709337ABD6F8AF7800C044F2F2F4
          D84C42A6F617F1FB6BF0524666500000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001FB4944415478DA6364200284
          97CD0E626460F0D3E07A9AD4D0D0F02FA66AA6E4CF3F2C7CABBB926F32126340
          58D9AC0C5971C1E9CFDE7C3CF5FFEFFFEB2C2C4CE17F7EFFA95AD19DDECFD8D0
          C0C064C422B71FA8CE0E4DDF91737F1ED95FF8309F8F8BF3EF6E7F077D133E6E
          0E86771FBF31DC78F082E1F6C3E7A62BBA32CE306E6E960B1194E0596DEDA389
          A2FBE896EB0CF3EE269E64E1123732D49067F5B1D36590111764F8F4E53BC3AC
          B587194E5F7B58B5AA2BAD9D71538BDC556B0F252D21312E1403DEBDFAC6B069
          EB0F86D37F42182CCCAC19C2DC4D18D8595918FEFF6760D871F40AC3EE13D719
          9EBDFEB01864C07F571F599CFE7FFC419061DEC52486586F730649510186E7AF
          3F30ACDE7D96E1C6FD179BFE31FC5F0736005F007EF82BC67086A3844144809B
          819999994142988FE1FAE9F50C3D6DBDE008001BE05B7906A701472F3C613874
          FE118397B50AC3E76F3F19166CBEC2A0F477174375F3522403B25319187E5F47
          D2F61F8CA004820652775EF0321CDE789521A9FE38920199110C0CBFEE63D580
          104318BC79FE5906BF9A474806A47A3130FC798EA9E13F9A8150B1CD8B2EA319
          9068CDC0F0F723920664576071C1D21BA806D8392B3030FCFB892508FFA3FA00
          2A76E8C04B54037C52D3319D8AD5100863CB9C3928067C02D2BC0C2480FF0CFF
          BFF8D73C06EB61FCFFFF3F297A310000D54FEADB815A8A100000000049454E44
          AE426082}
      end>
    Left = 33
    Top = 525
    Bitmap = {}
  end
  object LoginDropDownMenu: TPopupMenu
    Images = ActionImageList
    Left = 270
    Top = 397
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
          0D000000097048597300000EC300000EC301C76FA864000001724944415478DA
          6364A03260A48981A6F6DE6780943185669D3D7D70AB09CCC0FFF95DEBC1A2FF
          FF033198C1C0F00F4AFF47A2D1D960F5403CA7369001682023DCC0EC76A88150
          450C681AB0C9FD03DAF8EDD77F864F3FFE336CEF0D463530A36D3DC275682EFD
          8FC4FFF283016C00087FF9091287E8B8322F04D5C094E60D988641D99F819ADF
          7DFDCFF0E13B03C3DF7F30195470692E9A0B139A3620C20BE41D20E3DD570686
          D75FFE33FCFA83DD106470617610AA81710D1B19609683BCF3F21303C3EFBF84
          0D828173B3D02225BA7E13C31FA0012F3F33307CFD49BC4130707A4600AA817E
          E59B80DE03C51CE98681C0C9E9FEA8065A666F22CB2018383ED50FD540EBDCCD
          14197874B22FAA81CE8E8E241B72CC762E98B63A9CCCB077FF7E84812676DE27
          812C334A5C084CAFA7CE1CDA6A4EB0B49151544789A527F76FE2D58355121804
          F780942288FDF6E553143961716918F33ED08B4AC41AF8DFCAC90F7FF8EDDB04
          0E33925D880710EF424A00003300E5157B57B75F0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000022F4944415478DA63FCFFFF3F
          03350123D50D04119B9AE4B41998183603998A48726B7FFC791415D6C0F08B24
          034186313233EE57B7F7101557D5820AFF63B8796007C3CBDBD7B77EFFF32888
          14431937B5CABFD0B473151757D50472FF4331C4D01B07F730BCB87D938011FF
          E7FAD53C4E811BB877AAC17FD3E0082483FE03C301C1664061832884D8D7F7EF
          184EAD5B7F0568A02E9281FAFFCD8243109AFF6333089B652003DF339C5CB719
          DD40BDFF6641FE0C7FFFFC66B8B06D17C3A7D76F2888E3FFDB19F74CD6FE6F11
          E2CBF0EADE43865FBF6519D46D4BC832EAFBA73B0C7BA6462C66DCD9A7F6DF3A
          C297E1EA81130CCA16290C9CFCFF19FEFE7E8511768CD8C21389BEB66F3FC3AB
          BB7703C106DA44B8331C59BE83C13E7922C39F1FE781E1FE1BAF668405103E28
          731C5EB2F2EFBB6F1F848006AAFED77336667870F92D83914F3CC3EFEF57F118
          841C6908FAE38B570C67B71D38008C1C47B081922A920C3C227A0C92AAD20CFF
          7EBFC69E5CF0D0774E5F637870E94E897FEDE35EA0812AFFD938D818746CDC18
          187E3F65F8FFF727AAB7E0A9043DCF23F8574EDF63F8F2E693A66FC3B31B8C3B
          7B958106F232D8C456000DFB844503CC70EC06FFFAFE9961EFBC290FFC6A1E81
          CB01C6ED5D8AFFA555A418A4E58419FEFDFD8EC735A862B04879FBFA1BC38DF3
          CFA602BD9B0316DFD822FBCD26349C934F4C16A7EB50C4FFA3F22FECDA04CCEF
          F7DC7D6B1FEF021BB8A959F6140323A32903F9E0C8B93F8F1C1B1A18FE800D1C
          F4253600BA4C23FEC4936E5C0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000015B4944415478DA63FCFFFF3F
          03350123D50D04119B1BA444FEB3306F07724D1052FFCF30FEF9EBE9DBF0EC0D
          2143901DC508318CE580ACBE99B69299035CE2DEA9030C8F2F9EBACAF8E78F03
          2143510CDCD42A77464ED7C058C9CC1243E1BD53C7191E5DBA40C881477CAB1F
          DA220C6C91FBEF9A684F7698ED9E7F90016820238A81E44701046018E85B7986
          6CC336B79B601AE855BC81E1DBC7DDB020C6A019B188C1E8FD73E6601AE859B8
          94E1C7E7FD08C5FFB16B46170359B46FEE624C033DF26730FCFC720CAF4BB0D2
          C0E4B26FDE4A4C03DD73BB197E7F3B83E132622CD83B6F031603B36A18FEFCB8
          4284AB105E8559BE67FE764C03ED83C3187E7DBA8166000311FCFF0CC7775FC3
          1229799D0CFFFF7E420B707C8621C4B74FEDC434D0DAD398E1DF9FCF04353362
          B1E0D8EE7B585C985D8CD72038FF3F1A1F88B74F9B846AE0E616B9A340292B06
          3201D0A4633ED50FADE1FC415F620300B524DDD96C89D4C10000000049454E44
          AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC300000EC301C76FA864000001054944415478DA
          6364A03260A4BA81F2B11BAE3032FCD7A68A69FF192E332AC4AEFF1FE6EB4415
          F3566DDEC7003650484C862A06BE7BF50462E0CC7247B0C0BC45AB1892E2C2E0
          0A48E5A777EEA79181D7E6FA51C5CB5AC99B20065E9AE50B16D870F416980EB0
          56C3CAC605406A40402F6D33C4C0F3337CA8E242C38C2D10034F4F8318B8E5C4
          6D30ED63A18A958D0B80D480806916D4C01353BCA9E2428B9CAD10038F4CF202
          0BEC3C75074CBB9BA96065E3022035206093B70D62E0C17E4FAAB8D0BE703BC4
          C0BDBD1003F79EBD0BA69D8D95B1B27101901A305D0C357057B707555CE856BA
          036260908D00550C5C77E403C4C06D9DEE5431D0AB7C270DCA43AA1884040084
          9D9394BDB809CE0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed'
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
          0D000000097048597300000EC300000EC301C76FA8640000026B4944415478DA
          A5945F4853511CC7BF77F76ECE25B33FC3CA8CB68294EA41E98F6464AD5EC4D1
          2AC8C284088A12247AA8287A518A0C849E245C618F3D14D16CB2479B4D337453
          B08768461AAE657373A36D17DDDCEEEDDCEBFED6987FF6BD5CCEF79CC3F9DCEF
          F99DCBA1B0063534B4C9284DA99D58E7EB8E6BBAF4396A2DC0F3770CF5E5EAAD
          E6688CC377A7FB62948B0ED134B3DB33FB7B40041E3CA613BEB63F1744C248B1
          71FB1E50121A05EBD6A3B9B10EEA52155ABB4CD8A05480A12598F5851E2580FC
          CD0EA3B890E7C92B1A808BB742DFEB76E1EBB8154DF5D5903234AA2A764048C8
          D01464521AB79EBCC18CD7AF4D025B1EC7817168BA4FF43FDB2DD855C2E0FAB9
          5A842331506435C348D06F73E0D9CB5E980CF7A924B0B9DD984AF74FD2047861
          9EC5A8D588CE7B8D2852C8089082F04CBA3C68EF36836517EB92C0AB0F7BFE87
          C57D7081878FE5F16B6210DA8A02E88F5792710ECAA2428C4FB8303D3307AF3F
          847EBB633A09BCFCA027994610478C8F053C211E91E8D268F0CB2B94A914A4F8
          4102938B5BEF7E3B086F4489A0730CF262D5ED24F052DB3B1122284012B903C0
          622C815F522CFC075C2404A65085D08F3E54A9151875B851BCF702C69E9F85ED
          833955C3A656133935020A026C3813944DD1793F02DFCC9097EC83624B256C86
          339940FD5D13D91ED92AB73C2C9B86BB4E67020FB798D6044AE8D3537D26F0C8
          8DDEBC801F3B4F65024F6AB5AB860C1D7D21B6350357D067B1A480076A75C3C4
          1DCA2721F95F47EC5673F5B2B74D99A63CE3947E4E3972AEC93A494A30491A8D
          E0E7C8A590AE4D9BB725EC14D9E2CE9502F99A13FADCF57B6F126BB6EA8439B4
          F284F9E82F3E361DA182274D960000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed'
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
          0D000000097048597300000EC300000EC301C76FA8640000009A4944415478DA
          6364A03260A48981A6F6DE6780943185669D3D7D70AB09CCC0FFA70E6EA1C834
          337B1F06A0818CA3068E1A38BC0D74767424D99063B673C1B4D5E16486BDFBF7
          230C34B1F33E09649951E2C2FFFF194E9D39B4D59C606923A3A8FE1F99FFE4FE
          4DBC7AB04A0283E01E905204B1DFBE7C8A22272C2E0D63DE077A51895803FF5B
          39F9E10FBF7D9BC06146B20BF100E25D48090000D9537415156A030000000000
          49454E44AE426082}
      end>
    Left = 143
    Top = 461
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
          89504E470D0A1A0A0000000D494844520000001400000014080200000002EB8A
          5A0000000674524E5300ED001C002497DDBA3A000001E44944415478DA637C2B
          A3C2402E60046A5EB12A6BD2D1EE9F7F7E12A9879D853DCFBA34226C1A48B355
          FE577E552E362E562235FFFAF6FBE3ED6FC7267283341B67BF93361081CBADB6
          DF2BC9298355DBF3EF4F420F3A03194F2FBC393B5508AA59525718AEE298F72D
          ABAD6A5835C3A59E5F7E8BD02CA125045771DCEFB6E52655AC9AE1522FAEBD43
          6816D31084AB381970C77C834A87D9D48E0B351F7EBD47D60C9102325EDD788F
          D02CA22A0057713AF8AEE95A6520F9F1D7FBB67335FB9EEE40930232DEDCFE80
          D02CACC487E6C23361F7218C7D4F76B49DADFAF0F33D44D064952290F1F6DE27
          84664179DE73910F70C50D5067EBE9AABD8FB703D5182D57008ABC7FF819A159
          4086E77CCC433C9A5B4E56EE7DB41DA8C670893C48E4C91784663E496E340D17
          E31F41187B1E6E6F3E51F1E1C77B88A0FE423920E3D3F3AF08CD3C625C706D97
          931EEBCE930592400B9B8E56EC7EB00D4D0AC8F8F2EA1B4233B708075CC59594
          A73A73A4FB5D66371E29FFF0E31DB273205240C6D7373F109A3905D9E12AAEA5
          3FD39A2985D5F370A9EFEF7F223473F0B1C1555CCF7AAE394D12AB66B8D48F4F
          BF109AD9B811596A6FFC29193E39AC9A9F7C7AE4BCD00CC8F8F5F5375433304B
          FE65FAC3C8C4C8401CF8FFEF3FF33F166896A4A83020520F260000954319A8E8
          4E89CF0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000002B04944415478DA95946D4853
          5118C7FFD739D9C664951938DFD07C5B1A112A198B8CEA43D10B82A330840AFA
          9042F5C13EF4868C28220AA4B04030413F64249AA016A5812FA899A46E694DCD
          962F394DCD96D3DADCBDA77B8F615D37BBEB81C3F3F0BFE7F99DE71CFE5C06FF
          88D4F4031FF91405DFC3CA4800C9BDE272701CC0110296237C06DC732320CA30
          5A0B1AF99DAF9E3F0E49E08DC247B4510072EC2258D31590CF75206106B875F9
          60F96F841E06145CCA96065E2EA8A013703FA721EBCA859A194380528569773C
          1C498514240085438BF28F4A03F36E5582B1F74261CAC5BA2005E2D3F7A3B3A2
          184311F7311EA087C349B0E00216F83CF430CB0760DE69A8FAF210AA4B44CCF6
          DDB059CC30B599501FDA04B2A2DDFC207375A0D1083FC7480ABB2B660631FABD
          0849D842F5AE27A53033D9185E7FC6A3A7E5EE21EFC01AA35645FCFDCBE40A65
          E6A63D87B1461B4975C7CC24DE5495E26D441966D53BF80965A2BEE63B07C178
          F35A46D228B6E964D8BCCF0045A066591F6879CE5FD944EB7E7B34DAA79245C0
          51AB85023DBC36DF7D0D2EEB63BA49A5598BD423A7F86AE9323FEC5FD1FBA20A
          6DEF086AFAC2565ECECA78788D07B38403671F04D37102B16969D0EAB6D2DDDF
          C687D1575F8DEE612585BD6A7CEAF164CCDF5E63E9944BCB6FE21902072E22ED
          580EFCFCE5B0BDEFC1605B035EF66F40EBA76011A4B3A98E110105AF11F20726
          80D5AF33101BA74164B21E1F5A1B306219407BE07534569688005E273C7BB39A
          82A61C0433FCC26C17F4135948319CC4200F1B9F70A223B808F6001DF59A2430
          DB580BDB77C0E526544C1CCBC146650FDC2E27269DE1308797605116B4EC3549
          E0CE73B52231CE760121B3E5F8A2C980457B1B84918BBC26090C8F4A108BBC65
          D5F279CC2DAA3D1A04AF4901FFFB27CA03A357057A1385A9BD59C297605603AD
          0C5FC1BF008A6D634ED5DABBE10000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC300000EC301C76FA864000002984944415478DA
          9DD25D4893511807F0FF860B732D4D2DD4962E25C5FC58491A8A9664A424CB8C
          CA8B283F0669589844260DE5451B5A54E4A0B0D8B409117413D98514052A19DA
          62B9B5983A12F3CD694B63EEC3CCA1B6F3CA206A36B7FFCD7BCE739EE77771CE
          CB828FA10076388F2B777E8626ADD63BCEFD02A9B37C055B793C718A802F0F8A
          DA0AAD4A434FDBE6441576BBC62730F7447570BA7974E46C1827045326CCE80D
          E89BB5345558ED577D028BCA2ECA8E884417620511A09BA5707475CE1A2D1641
          3560F61A7C766DDB9E98D444D58425DDB1BC258FA3D119607CA4AC6A79FB52E6
          F51D5214D8D9613BB569A2AC04F6DC00745A16DA7AB29EDE93C98E3B8F97BC06
          3B1BF9E2CC9339F2C08D362CFDA461FC3C0D75DFAFDD85D4C4A0AB67CDE0138A
          1F1C278CA6E3331202FC96ADB019DFA3A7CBDE2A92D0E7FEEC5B33F8BC39BA2D
          E7D48152FF80CD9837BD835E6DB48F1B26F98594D9EC35481E22E5E05E55C48E
          24C0E1C077C30BF4774F951DADA3DBFFEEF508AE3CC42E6D46417A0267C33E98
          8714F8D04FEB7A678685CEB3A57FC0A29A07CBFF03633883A82DB181177E0CF3
          339FF055F70AB7DF64E3C762B8DB7E06544AC56E0F176CDF30DA2D46D2FE4CB0
          D61F8269A00E93B664C4E736B9ED2F96285640458318E27A0552E20550EBC740
          F624C3BD1D589C56819F9C86750BC3F8A2572326FF31FCFC03DD82C460C0FB54
          195328A7DAE05A9368BAE4E06FE280CBE5821EE9042F2E1F618945AB5E0F9967
          C0BB75A54CA1B2B11DAE35C96BA51479D9A9308E19C00A8A42A8F0B0B3CA5E15
          24F30CD82229610A55D28770AD494E17E440181789F335F5F00FDDEEE98760E6
          19F0566D312E352B91141B898F23E3207B929B924A945F6E002F28C42346420C
          06BC5E73664D039E72E54687E7FFD0DBFC0631E503BA9989C9D2000000004945
          4E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000003A94944415478DAADD45B6C14
          551807F0EFCC9999EECE656D3B6E5D5A6949400A6D6C6C2D5DB751BC24864462
          4242E29B810715DAE59218E39B893E19DF8C292D7D101E8C31468D4F8A5562A2
          AD4BB05C4B4B050C852EB0DD965DE89EB9EF5C3C336B575BA8F8C0799A3967E6
          B77BFEDF3707F9BE0F0F73A00044088537EF03300D92F4DCBCAA9EA4D7E6835E
          3E248A0916A3D79C927A240DA086D61218606B64F9085FC3EDB22DFBD71C515F
          A673F66AD87034DA0C1C1E8DC795E685F962C621645BBFEFAB21F8014221A628
          B5BB52A92E387F7E1AB2D9DC7739427650D459890D48523B87D02F9D9DED4A4B
          4B136432A7603E5FD8FF16210321783826EDAE8DC58E6E4D3D0D98C5E07B1E9C
          3C7D01F2F385CF28BA9BA25E759B72B49767B81FBA3BDBE5358906C8DE9C8333
          E7A66EDAAED7B34FD36E85E0A0243DC5B3786C4BC766311157C2173D8AFE767A
          028A774A9FEC51D583E13663C27616735FF674B48971A50EF2B78BF0FBB98B05
          CBF39EDFA7AA53CB321C16842EC0F8C7CE4DEB95B5897888BAAE0BA367276191
          E8EFF98C3F1361B9A3C9273771753119EE2C12C84C5CD42CDB7931ADEBE3C1F3
          CBC0600CCA35AD18713FB5B53CBE767D63229CB3CB0E8C4DFE019EEBFAC9B68D
          4816A24074034627A6CBB6E3BED2AF69C797E2B807AC568FC5DF3FD1F8587B6B
          530535EC4AB1A33C0FBA69C1D8F415DF74BC9D7D847CFBEF62DD170C51597E14
          101C6BAEAFEDEE08D0BFD7ADE0DFFE3903BAEDBCB197904F57567F559056950D
          DAA84E88BC9E6C6E02CC30E1FCA261C289EB59CBF29C97D2C4C8FC2F90623CC5
          BE6E10A3AF76D1B660E89AEB55BA26800B34BFF15C9E589E9F0A2AFB9FE02100
          8995A5638DA2F06C076D8B603E583FB550A0A80FDD0D0AB014CD53F4CC42B1E0
          95CBDD7DA679EDBE6025377F641D6D9FCD31A9921B5D3B7BB70473A6F995EFA3
          EB3287DF49D6D7018F19C8527472B134EBD0E2A7356D6E1938208A8D3CC63F6F
          1022AD1B04A1BA85295583AC651ECF95D4EDC1774D7FF4DD28663E4AD23E8C60
          0C33860197347D4A23EAD6B7018A5570F811E9603DE63EEE11FFC1AED0F6B86A
          D9E1471F9C244BF343B2FC66846E688B282089A2974D1366ACF2F2C36110219A
          9D3CB28EC5BD1B390EAE390E5C2E9769E0E8853D84DC5E59CD2149DAC923F8A2
          ABA686ABA5998ED33E2D3A6E1F7DF67035C34A41E4118541BD05DF9BB51D2FB5
          5FD76FAD767C0D89E2369AE5374D0C23CEBAEE8253769EE9338CABF75419CB72
          DA769CCF0F18C60D78C0188A467B1896ED77C1FEB09F5897AA193ECCF1173677
          F3E8118EA7C80000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000140000001408060000008D891D
          0D000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000002894944415478DA6364200384
          8636B0312A4A9D01321FAFEA4AF346966324C7C0B0D2195EEA0A925BFFFCFDC7
          70F7F1CBA83FFFFE1C636666517BFDEAC561A20D0C2DEC136262E169033A810B
          C8354A0AB0D656901261A89FBE8941908F8B81859989E1D5BB2FAD6003373748
          89FC6761DE0E74B009C288FF6718FFFCF5F46D78F606C40B299DE9C6CFCDB133
          DACB9C81958599C150439EE1CFDFBF6083D85899194AFBD6303C7DFDDE911162
          18CB01597D336D253307B8718F2F9E64B877EAE055C63F7F1C2086363085954A
          ADB232500E4E0FB167F8F9EB0F0323D03920030F9EB9C9B070D3F1032BBBD31C
          1937B5CA9D91D335305632B3C4F0E6E34BE719EE9E3A01E77FFBC7CBB0E57B01
          C3E4CA18061E2E36A0818C0C2078EFE96B86F639DB7E7FF9FACB9771538BDC7F
          D7443BA2C271D62E75061E0147063F070386FFFFFF31F0F170325CBCF594E1C9
          CB770CAFDE7E663870E6E623B081C446CC9ACF450CE212320C2F819AF9783818
          D243EC18E6AC3BC2F0E2CDC7ED40435880E1BE136CA06FE519A20C7CF6FA0BC3
          ABF75F1914A504187A979C041ACAC770F9DA358647273FB01E38D0F0079C0E41
          067A156F60F8F671372C7691E8FFD0C4FA1F43EEC91B1686F6E5420C6A4C4718
          6A9A17C3931FD840CFC2A50C3F3EEF4768FA8F6E30AA05C816ED9BBB98C1AFE6
          11AA811EF933187E7D398AE4B9FF44D0108BF7CD5B8969A07B6E37C3EF6F6730
          5CC64884C17BE76DC06260560DC39F1F578873152C4CA196EF99BF1DD340FBE0
          30865F9F6EA019C040800FA18FEFBE8669A0675E27C3FFBF9FD0029CB06120B0
          7D6A27A681D69EC60CFFFE7C26A899118B05C776DFC3E2C2EC62BC06C1F9FF31
          D56C9F3609C3C0C340DA86816CF0FF905FCD637BB881FFFF139D95890200B8D8
          399E6C933D3B0000000049454E44AE426082}
      end>
    Left = 145
    Top = 525
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
          F8000000097048597300000EC300000EC301C76FA864000002524944415478DA
          BD954F68D3501CC7BF599AD26DD43FB3582ADD24D56A75308A30FFAC4E45D865
          053DECA0A0202288072F1E3C88B0C3403C78F0E241041141410F1E14B6CB40AC
          DA29F6328449DDB4C5591C9D75EBDA35CD92A6CFA469435FD769DB55BF84FC78
          FFBE9FF7BE0F1206FF58CC7F01F41E1D7CC79AB883CD345672F2FB5060EC5011
          E0273B76F7005C7B73DCE50CBE7EFE885060943100BCBB1BECFE6158B8B55323
          3574AEE408940F2388CE4CD1802E9707ADBE110CED3323AFADD21F9062D55EF9
          A259A91AE36573C73FC9C80687311B09D300E7F69DB01EBB097F0F4799E90BC9
          2A4835734D81E91CD2AFAE21F6ED0B0D703879740CDCC2C01E937A02543F41A9
          BFD0A7434509486609522290160944996061FC2AE662511A607774C13E781BFD
          6E769579654C29D5685120480A7AE6958A8F5D417C6E9606D8B66E83F3E41D1C
          E0D98A5DEB0D410612CB9A319053AA5EB7A1D8F3CB48CCFFA0011D363BF8A1BB
          F076B618796A7549D48D332B7F362D57F4D9252C24E23460E3E62DD875FA3EF6
          3A74802011C4D328645AAFA69F5CC0D2E22F1A60DDB009DD671FC26563F07359
          BFB04635F5E81CD2A9240D686BB7C27BFE314C2D0C72F9C6CD354D3E38032193
          A601164B1B7A2F3E5D977149A17BA7208A020D307366F41FF63505F0E66D1092
          2CD1009665D76D6CBE3E53A8D20D37144529031CF107D50F775F53B65F12C144
          E8F5A8AFE61F8ECBE3A56E3E129EAC69ED9A93D4D8226AE14B6D49CCD271585A
          CB9B51350E57BD00D2C97B8CF6F768981AAF1CD3F2AE1BD077FC444DF14DBC7C
          D110808AE82FAA3FA266E9372EA14D285B1635270000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000002934944415478DA63FCFFFF3F
          032D01E3D0B700446C6A92D3666062D80C642AA2C9AFFDF1E751545803C32FB2
          2D0019CEC8CCB85FDDDE43545C550B2AFC1F8C6F1ED8C1F0F2F6F5ADDFFF3C0A
          22D712C64DADF22F34ED5CC5C55535918461C1F68FE1C6C13D0C2F6EDF20C1C8
          FF73FD6A1EA7C02DD83551FBBF45783C92A1C816A0D3A030C52DF7F5FD3B8653
          6BD75C015AA00B57BFB34FE3BF65541C168D782CFA8F5DEEEBFBF70CA7D66DC0
          B4C02A2A8AE1EF9F5F0C67D66D60F8F6E91339418D3DB0FEFF9F0AB440FDBF55
          6438C3BB274F183EBDE560D0716DA08AE1DF3FDD66D83B35720FD802EBC86086
          3BA7CE324868843208488831FCFA0E8B547CC1853F6EEE9E3CC1F0F0F2C55AA8
          05FE0CE7B6EC6130F26F6660627ECAF0F7F72BC206FFC72F7F7ECB3686772F5E
          B9022D50FB6F1DE6C57068E92606979CC50CBF3EEF0586DD5F82AE6764402F01
          10FCFFFFFE321C5CB4F2FFB79F5FF9C016E8BB5830DCBBF08CC124208FE1D7D7
          1338838158DF7C7EF38EE1F4A6DD9780A9491F6881EA7F391D65062636050679
          5D03863F3FEF10702DFEB800B11F5DB9038CD3CB537C6B1EE7822DE017E56750
          30F262E0176566F8F7E72D69AE4763831C7369EF5986D7F79F47FBD63D5906B6
          0018E80CD691190C4C8C0F81CC3F0817FF27CF92C32B0E02E3F29BBC6FC3B347
          8C3B7A95FFB3B0B03198B8B831FCFE721B87466C7CEC627F7EFF65387DE0D60B
          BF9A4792601F6DEF56FA2FADA1CFA06517008CFD6F040C216CC9DBC777184E6D
          5CBBC1AFF67120D882AD1D0AFFD5F415180445388116FCC6110C08C048C09227
          0F3E303CBCF5B6C4BFF6712F58FDE656B9FFF6D1F10C9CFC22A4B9F83F76F193
          EB9733BC7FFCCC1818FEE7C0166C6A967DCAC0C828C5402DF0FFFFD31F7F1F2B
          C12AA8A15FE90300252963BC5E1E63A70000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001904944415478DA63FCFFFF3F
          032D01E3F0B08091919161738394C87F16E6ED4021135425FFCF30FEF9EBE9DB
          F0EC0D2906C31C0EB6604BA334D0709603B2FA66DA4A660E280AEF9D3AC0F0F8
          E2A9AB8C7FFE389062098A059BDBE4CFC8EA9919A31B8E6609291E38E25BFDD0
          166141ABFC7FA7E408AA85FBBEB92B18801630A258E010A44B350B0EACBB8C69
          01D54C87020C0B7C2BCF50CDF0CDED269816F8941F62F8F2763554C97F1C34A6
          182316B9FD7366615AE05DBA83E1DBFBF5380DC2B0E43F6EB9FD7317605AE055
          BC96E1FB87AD64B81A537EDFDCA5981678162E62F8F96917F13EC0EA13087BDF
          BCD5981678E44F65F8F5F91056831889F01532BD77DE462C16E4B433FCFE760A
          8726E27CC408F5D19EF9DB302D70CFAA60F8F3E3325C0B238188C467D99EF97B
          302DB00F0E65F8F5E93A164388E523C48EEFBE812515E57530FCFFFB898041C4
          59B27D6A37A605D69E460CFFFE7CC16B20231E5723AB3DB6FB3E96649A5D8C57
          138ADC7FFC966C9F3609D5822DADF2478152560C540240938FF9543FB4865B40
          4B40730B00099F45E0023335AB0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC300000EC301C76FA864000001214944415478DA
          6364A03160A4B9050A31EB6F01AD51A589E9FF196E332AC4AEFF1FE6EB4413F3
          576DDEC700B64048549A2616BC7BFD1462C1CC7247B0C0BC45AB1892E2C25014
          112B864D2EBD733F9D2CB836C78F2641A495B20962C1A559BE60810D476F81E9
          006B35BC7C4200A65E2F6D33C48273337C68E203A38C2D100B4E4DF5060B6C3D
          711B4C7B5BA8E2E5130230F566D95B21161C9BEC4D131F58E5422D383CD10B2C
          B0F3F41D30ED6EAA82974F08C0D4DBE66F8358B0BFCF93263E702CDA0EB16077
          8F075860DFB9BB60DAC948192F9F1080A9772DD901B1607B973B4D7CE059B613
          62C1B64E8805072FDC03D3F6064A78F984004CBD57399A05D406700B826C0468
          62C1BA231FE8E4035A5B7003C856A7890D0C0C37695FE9D3DA0200F05CA9285F
          C197110000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed'
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
          F8000000097048597300000EC300000EC301C76FA864000003634944415478DA
          AD956D4813711CC7BFB7DBD6DC9A3AE703DA54B6694D0515D10CED89B4088D0A
          042B0AC2827A11BE88E855611154842F2AF28515113D62044D92155156E643C8
          2A03B1B40795A4683ECCE9B6DBEDF670DD363D3D33D4F4C7DDFDEFC7EF7FDFCF
          EFBEFF7B20B0C8283B76D52026894760D9530FAA0F3F9C5927160B283F7EB5A6
          283FED48D3BB9E518A76E8EA2F1DB5FD05C8DB50F296144BD6FC0F4093598CDA
          AA0A185F75C0D8F00C6E8715524524C452B95516A1CA980094B2FA5599804431
          A7202B9671F3E47C9E929C80AAC3DBE0623CB878FB39D4910AE8343130357762
          C4662FE501DAD40C90ABAB2093FCDB3596DB3D831DD02BAD38B0B30004412046
          A504CDF8B87340BE4C02918880B9AB1FD5D78D30D638481E90A43320ACF00CCA
          72A4F007A4421BB776A13170F00776BF0FCD4DCFB0216B05CA8A7360A7986031
          002345220E2246E5853A74B63D458BE92EC10334C929506E3C8FD24C092F36D9
          35CB51A64318C68DA617469C3CB805C909EA6013224E2AA0E6A23D385DDB80CE
          F7AD785177790A10AFD1226A733536A789B93BC0EC773031DA4687D1D3D18873
          953BA10A57809870D5CD78E1727BF073C8862BF71B611D77EEE60171F149882B
          B98875A9E45FE2336D7AFBBA1EE59BD290B53211969131C4AAC3A15286E143F7
          003A3EFFE0E6B0B08E39D1DDF7FB130F888E4D8066470DF2B5E48CAE4309E501
          861D2C46C65DB07F7A084D9C0ABF066DF0B2248A72F55893A5C7FD27ED18A054
          B07F6B04E574204293BA9E074445C7415B568BEC4451108089CEC7E890B0D3CD
          F24F93D7690916C93035FC5E1AF4F706ECD99A8B3B2633C2D3CBD15F5F09EBB0
          05E626D3D41A44A8D458B9FB06D2E343008A6161B103B487C55CE11C688784EA
          855B1C03A5BE185FEA0E626C7444085086472263DF2DE8A2090C39003B3DB730
          FF7EF8BDA02C5D90A9F520A5CBD175773FECE3362140AE5022BBE21EC4DC8BE2
          F5CF5F7CB6F878732FB706762140269323EFD08345094F86F9DA2ED034250448
          2552AC5B5BB82480E69656301E4608204972D1C2D2135F83237336153E9F6F1A
          607D692BF7E12E5892F62783459BF98DA970DE3F1C9D215BB0F2BDDD1FE775ED
          3F2771B6F572837632676897D00E59D8F4B48FB343B750009BA835F0F9405FB7
          A03EB316F07BC180824DDBE7655FDBCBC7FF05105834472CDCA2A58A3F6D1E8E
          5D9AC6B17F0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed'
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
          F8000000097048597300000EC300000EC301C76FA864000001A24944415478DA
          6364A03160A48B05A6F65EC79959582DA869F0DF3FBF4F9C3EB8CD126A81F77F
          65753D06FEEF3C5431FC23E71786BB372F319C3EB895116E81A2AA36C3CA399D
          54B1203CA59CE1FEEDABA816C8296930AC99DF43150B42124B181EDDBB816A81
          8CBC0AC3BA4513A86241505C01C3938777502D90945164D8B87432552CF08FCE
          6578FEE43EAA05E292720C9B574CA38A05BE11590C2F9F3F42B540444C8A61DB
          EA5954B1C02B348DE1CDAB67A81608898833EC583B972A1678042733BC7BF312
          D5027E416186DD1B1652C502D78078868FEFDFA25AC0CB27C0B077F312AA58E0
          EC1BC3F0F9D307540BB8B879190E6C5B4E150B1CBC2219BE7DFD8C6A01070717
          C3A19DABA862819D7B18C38F1FDF502D60636563B0B5B1A68A05878F1C65F8F5
          FB17AA05CCCCCC141BCC567D1B4CFF6A5565F8FBF72F920576DE478105B71555
          9C0F03FF198E9D3EB4D59AE80A4749C3E03F32FFDE8D0B44E9C5A908186CF780
          94228CFFEBC777D4E0E0E044E6DE07068712A916FC9755D480F31FDFBF81228F
          2E070A6F922DB072F2232AF88EEDDB44960528414400901E44D402001D69BC19
          62FAB43A0000000049454E44AE426082}
      end>
    Left = 271
    Top = 461
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
          F4000000097048597300000EC300000EC301C76FA864000003204944415478DA
          ED966B48536118C7FF67B7D4969AD7BCE6645ED24C11A58BA6E407A116F9210C
          C46E18451FA20F411015455111047D883E4451641942D187228B0205D32EA044
          62D6BCA0A666CDE66573BAB9B3ED748EB9E3E6DEA9733BF6A5FF97F39CE77D77
          FEBFE7791FC64BE11F8BFA0F600F720B5455EC5BB9D0860C031B28E65173FDCB
          FD4E0039052A6BD2BA4CD17254DDF9ADC5D6FCB646ECDC81421593989C012AF7
          ACB01D68BA84EE8E5634D5D7502E0009CA3448379D47698E8C6B95BD6560304F
          EC9263F89CE33AB7F6BA8D06FDF1027ABBBE9201E21353E19F7F11255952FE83
          F341108D1CA1E6E46AD5348C8DE7D0D7AD2603C42524435E7819AA0C294F3DF3
          5B3666F8D8B1AAD975384313E2864E0B0CF567D0DFDB410688895722A8E82A8A
          D3246EAB726730BB3E0B6A9802F446067A1383711360B630D0D59DC28FBE2E32
          4054AC02A1C5D7B02D45B2C07933C4758B15189D6430C69AEA8CDC3B7F58BC86
          DF9CC4CF811E324064F45A446EBF8E3CA5D869B8E61B3A336B3A32C14C1B7315
          DB3BE04E9A5727A019FC4E068858138BE89D37B05121763FE1AC6CEC83AB506B
          F8DBDA854C1D35F8E238867E0D9001C222A211577213D9F122E2C0596D6C0BD9
          6AB50680B62EDED451FDCF8E413B34480608098B8462F72D6C8811390D19E735
          CC56AB9D60ABB72DCDD8AE9EA74731A2D5900156878443B9E70ED2A244FC99EB
          D8811A1AE7AAF7CED8AEAEC787313AF29B0C10141C8A94B27B488AA0A6874BA3
          074CB46F8CED6AAFAE806E6C980CB02A3018E9FB2A112EA7A6DBEDC9702D566D
          0F0F605C3F460658290F44E6C12A9F9B3AAAE5FE5E4C18F46480800039B20F55
          0B0AF0E96E1926270D64003F3F7FE41E79222840D3ED52984C4632804CB6025B
          F3F3040568687C07B3798A0C2095480531959C6EE763CB9514D016DA1580BB92
          49C4A265B99259AC36D72B594EE18E0760A8728A82A0103397D24AF6525AE104
          E089129233887F10BD1DAD1E7F6F4900A9599B8900EACF1F7C0FC00EE717F691
          EE98B3D066E25E89543637D5C60EDB7A6F01983845AA53AEBF474DDC4BDA679F
          76AF00B614ED5A681B51EFEB9EFB04C0E5083C90F74720B4FE007DF2E030183F
          CFA10000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Opened bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC300000EC301C76FA8640000036F4944415478DA
          ED95594C13511486FF5159EA9A50312A5A344A4471894B6509C6082E4442010D
          3EB82BF1414D3431FA64ABC4F842D4371F4C346A34D11713AD3146491153C3A6
          084444402042A780554B432D50CAB4E32DAD38B7735B97D0F8C24D2673E6CCBD
          F77CFF39E7CE70F8CF831B071807903EDC2A42B472D2FC32E24E614F17ABAC02
          BFE950119C6101787C71FEA5C48D074E27A49D604E6EABBA8AA6B25B97355AFE
          CC98033CBA302F396AF2E44A75C1112E225AC19C3CEC1C44CD831BA2B3BF3F35
          EF9CB97A4C0188FA86A4CCACE5B10B17855CF0ADA31DEF0DCFFF319C5845B297
          1A044025661ECE1F0B514147E9CD87D0684D54D92980AD0793C30A5072BB3A34
          40D67E5558019EDD318506D8BE3B62C47E6B1460318B618521A38EC0ACA133B0
          D305611878655062EBC9B2F08516053C294E75E59CED8CA20036E708B0593958
          FA32B022EB1406ECE5ACD5FECB43D2E721A6CFF65D12BFD71603FDBEBBFDEB17
          D43C2EA92127424D016CDAE686B9934374FC31CC494CC6D040D3E862EF9D0BD8
          70D4FFD316597E894FF4F9BA9ADBD05251773347CB1752001B323C6869E490B0
          A5188A1913200C75075127574807FBF58EA3607DEF1A8DB5F8FCD17C48738EBF
          4D01A4A503F5B540CAFE6B64DE2778DC7DF40612859C34D8C8BB806C04664902
          5BADAF80BDF7BB3A57CBD750006A5291BAFA49D87CFC3A5C8E72124F90D54FAA
          9063040ADD1B225926A0F4EE2BF7379769AAF7A746012C5DC2E1CBE062ACCB3D
          8AE1815A66FD7EA7EE77CDE8B03950A97FF7811CC1246F5C0A4015C761E2EC74
          24ACDF40EADFCAACDFC8C6224B39BB1903B3D4D36E4583B1E39E46C7EF9101C4
          90C653A5E421365E098F6009A93074E707EF8DE6373DE868B49ECED5F1576400
          8A480E6B77EC83624A3F59E7609E6104A867F506B349FDB0AF9FF1B059FA3335
          BAAE17328009E429A3B0109CA773E46BC5FE98C83795FB8397CA709F8763D0AD
          DC5564EE95014C8F998655E9EB200C76495440A24E62535F457F0F88A2CCE74B
          BFEF2E086E54BDEC3391FAC7FF8C4B012C58B90ACB366613D14E7F407A833F0D
          180CC2D6CDA35CFFF2293901D94C80C4D57150CE8C949C7FC9254A41E84C7054
          56026D8C96E973B780B666D7798DD67C8109905E908FE9B173FF529DC7BF110D
          CB9A5B6F30C2D4DA939DA7333F9501E82FAABAC9C31C84770CD905C7ACBD45BD
          7619C0FF1AE3003F002B4C7E3FD11A0AFA0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Closed bookmark folder-stored session folder'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000001FF4944415478DA63FCFFFF3F
          C34002C651078C3A00E40046464686F90D0C1C222C727B81CEB1C2A1F6C88F3F
          8F9CC31A187E916B1936CFC21DB0A945B65BC33EBE44D52A0FABE6DB472730DC
          38B4B8CDAFE67135D51DB0B159D69C9D8BEBB869682A232B072756CDBF7F7C63
          38B57ACEDFBF5FBE98F9363C3B4755076C6E95BBACEDECA123AAA88CD780D7F7
          EE305CDDB78BDC0038E25BFDD0168703E4FF3B2705926B305160EFBCF50C4007
          30E274807D901E4D1D7070DD25FC0EA0A9ED5080D701BE9567686AF9E67613FC
          0EF02EDBCBF0F5DD2624A9FF54651F98338380034AB7337C7DBF856883194974
          C8FE3973F03BC0AB783DC3F78F3B48F3E17FE24361FFDC85F81DE059B482E1C7
          C7DD380C21354430E5F7CD5D42C00105F3197E7CDE8FC740CAD2C1BEB92BF13B
          C0237F3AC3AF2F87A912FCD8D2C7DE796B093820B78FE1D7D713040C243F14F6
          CEDB48C00139CD0CBFBF9DA74A7063F3C09E79DBF03BC03DAB92E1CF8FAB4459
          020E91FFF8D5A0B3F7CCDF45C00119790C7F7FDEA682CF7139601F7E07B8A527
          33FCFBF508876104428388A8D83DFF107E07D8FA3A03A3E019037680CFA7C489
          1FDF7D9B4049985BCFF0FFDF0F920D2656CDF6A9BDF81D60E5AE0974C06F5449
          2A590E02C7763F20501266171369F87F1C52B8D208046C9B36099F03E43E0399
          3C0C3404407BBEF8D53CE2C5EA808104A30E000049D3AFD07620F80900000000
          49454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC300000EC301C76FA864000001994944415478DA
          63641860C038EA0018432166FD2D204F954EF6DE7CB0385003D501B1EBFF6745
          BAD0C5F669CBF730001DC088E100151D73BA38E0CE9593D81DC0C3274417077C
          F9F40EBB0366963B82D9F316AD62488A0BC3D048AA382EB9F4CEFD83D401B7E6
          FBD3250AD412376277C08D797E74718046D226EC0EB83607E280B5476EA26808
          B65127491C1780A9D74AC1E1802BB3E913023AA9381C7069962F98BDE1E82D14
          0D01D66A2489E30230F57A699BB13BE0C24C5FBA8480413A0E079C9BE103666F
          3A7E1B45839FA52A49E2B8004CBD51C616EC0E3833CD872E21609285C301A7A6
          7A83D95B4FA0FAC8DB429524715C00A6DE2C7B2B76079C98E24D9710B0C8C1E1
          806393BDC0ECEDA7EEA068F0345321491C1780A9B7CADD86DD0147267AD12504
          6CF27138E0D0048803769D41F5919B890A49E2B8004CBD5D010E07ECEFF7A44B
          0838166EC7EE807D7D1007EC397B1745838BB13249E2B8004CBD53110E07ECEE
          F1A04B08B896ECC0EE80CA28FA348ADB97DDC6EE80ED5DEE74718067D94EEC0E
          D8D6491F0778956373C040774C060A8C3A0000D2EA0930F42CE2AB0000000049
          454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed'
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
          F4000000097048597300000EC300000EC301C76FA864000004A04944415478DA
          BD967B4C5B7514C7BFB7B485B20EC6FB551E45DE0FC165086E6C4CCC9630C666
          B274CB32896646DC1634D3B8688CC6696031E2665CD439D48D39928591195DA4
          C63FD431988BB60AC826A39A8CD7B060CBA39452FAB8D7DFBDD0DA3BEE8614EA
          B9B9B9BFFE7EA7E7FBF99D73F2BB97820F6DF791FA5D623FD169FBAC29ADF9BD
          17C7847C285F02EC79B95E7D48B5B9ECA3A61F4E35D53D7BE87F05287BEE64D0
          C39949C6A3072BC4B59FAAE9EE9E3B994D27AA74EC9A4A75544A27C4245F3AFE
          97CE0D50B0A9BC91E0EC5B2900799802AFBCF43C1E2BCC84DE3089EA374FC36A
          9E803C2402A90F281993D94AE9274C056E80759BCA9DA99979226F0599E04464
          A7284051732103FCC538A82AC1B4D58ED0A0400C8D8C83661824468762DC64C1
          0BEF3669CFD70E16FE9B81927226392D1754C16BDE018CF7A03CCB81CAED4520
          3A60C865B33B619A9E056944F84BC49CDF2A9904EF347C0BEDEF030F35D75575
          F2009252B220297A03AA75522E0817980B769FB1C75C57C74FD89C138A2D4599
          305966890F43AA4A2EA2C2664646B2A2EB1FC1919A0FF14DC3314E9B0790909C
          0159F15BD8992F718BDC0FC225EE09F2CBCF5750FD783E1263C3B80CB8C4A979
          BF8E9E01D4BC7F06EAB3B50B01E293D2202FA94579AE642E30E3D6E17683BBC4
          F8EB73E34EED551CD89103656C38FC48EA59010E800CD81E189FB4E0EBB6DFA0
          6EBB5173B1AEEA751E405C420A824BDFC6D62C315F681151D7FA98510FA95987
          CA8A22926E29689A4680540291888291085B6D76381D4E38C8FC17DF7540D3DD
          57C50388512811B6B50E8FA68B17A937B3609DDD5DD775350EEF590F7F52EB21
          FD04C9000545540882E532FCD13F8ACBAD9D5C59D83F38C9DD376C1CE20144C5
          2622AAEC0436A4F8F19AEB5E4DC7CED948BCB169067AFD1D30FA6B90C9643058
          4813FB87809A1925E5C84370900C57343A688CF130B61FC7C8703F34AD2D0B7B
          20325A81D8ED2751A8F4131463E67FD1E431390318CC0CA6ACF3FD416EDA3605
          917435307F1638678C887368B1AD38078D2D5AD8E22A30DC7218A3FA216180F0
          C858C4EFFC006B1344820DE7A40123D9ADC10CD89D9EF9B9B75906DA509C2645
          BBCE86C0848D18FCAA1A86D1616180D0F02828777D8C07E344BC2663B58C64B7
          8669B27BFABF09BB8CB65B30D5FB2502934A219147E3F6A50318338C08038484
          462065F727C88A11B96B3E39C360748ADDFDD2843D8DA11DA0447327E19F179F
          C1F8D8DFC200C16BC290BEF70C522329AEB9464C80D5EEBDB090F55ED88FC909
          A330C0EAA035C8AE3C870839C5A5DB75F8ACA4DD3CFF24A64C13C200ABE441C8
          7BAA71C5453DADABE1094C9B4DC200818172AC7DFA824F017EFD6C2F2C16B330
          4040800C0555CD3E05D0D4AB60B5CE080348A5FED858BCC1A7006DEDD760B3CD
          0A0348C4129F888A5FED758F1DC7D26177D81702B09F64E4CBC5EB4FB2A598C3
          49D3DAAB2D7E7C80926D9F83A1F69163DCA710EC2B0314734EDBAADECF03588A
          25A5E50A1E107DBAEE25C7F30A2023FF1141805B9DD7571E8034E70DF2C8F69C
          73D86D82BE6289F4EEA99BA4D972960BC0C42B33787383B76F09FA0AF9B9BA7D
          5900EB4B772CE626683F7E7F79450016946009B6FC12F8DAFE016AA249EA13B2
          08A00000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open new session closed'
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
          F4000000097048597300000EC300000EC301C76FA864000002374944415478DA
          63641860C038EA0018C3D4CE7B0990174D6B0BFFFF67F8C7C0F87FE99983DBE2
          501C6062E7FD5755539F891EBEBE7DFDE2BF3387B632A38680BDF77F25355D86
          15B3DB696A79446A25C3BD5B97194E1FDCCA88E10005152D865573BB68EA80B0
          E432860777AE6177809C9206C39AF93D347540486209C3A37B37B03B4056418D
          61EDC23E9A3A2038BE88E1F1835BD81D202DA7C2B07EF1049A3A2030B680E1E9
          A33BD81D2029A3C8B071E9649A3AC03F3A97E1F993FBD81D202E25CFB079F954
          9A3AC037329BE1E5B387D81D202621C3B065E50C9A3AC0273C83E1D58B27D81D
          202226C5B06DF52C9A3AC02B348DE1CDAB67D81D202422CEB063ED5C9A3AC023
          3899E1DD9B97D81D202824CAB073FD7C9A3AC03D3091E1FDBBD7D81DC02F20CC
          B07BE3429A3AC0D53F9EE1E387B7D81DC0CB27C0B077F3129A3AC0D93786E1F3
          A70FD81DC0CDC3C7B07FEB329A3AC0D13B8AE1EB974FD81DC0C5C5C37060FB0A
          9A3AC0C13382E1DBB72FD81DC0C1C1C97068E76A9A3AC0CE3D94E1C78FEFD81D
          C0C6C6CE606B634D53071C3E7294E1D7AF9FD81DC0CAC24A134B59AA6EC2D97F
          DAD4197EFFF98DE90050938C8599892E4DB23F7FFF6136C94CECBD1631FC678C
          666464A0A923A08DD285C04669128A0348010A6ABAFFB1893FB8759964F3C872
          808681255607DCB8709CFA0E0026CE2B404A1B59ECCFEF5F58D5B2B0B2A10B5D
          0526361D4A1DF05F56510345ECF1FD1B58D56253074BED1439C0CAC98F9032AC
          E0D8BE4D54710046149000288F025A0300B035FD21AB80C6790000000049454E
          44AE426082}
      end>
    Left = 399
    Top = 461
    Bitmap = {}
  end
  object ActionImageList144: TPngImageList
    Height = 24
    Width = 24
    PngImages = <
      item
        Background = clWindow
        Name = 'Login'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000029C4944415478DA63FCFFFF3F
          032D0123C802F956215F4666C6998C0C0C92D43014E8E4E7FFFEFECB7C54FD7E
          23D802850EE1A722CAFC529C02EC5471F5F70F3F19DEDCFDF8EC41C55B69B005
          8A1DC2FFA50D45E10A4AB51B197CA443189819998936F4EFFFBF0C5B9EAE61E8
          BE5A0FE63F3DFF9AE17EC55B46B80552FA2270C5473C6F30300221E941F39FC1
          66BB0698FDECE21B540B247584E10A8FF9DC02D3565BD488361C5DCFF32B6F51
          2D10D712822B3EE1771B4C5B6C5225DA02743D2FAFBD43B5404C4310AEF864C0
          1D306DBE41054C379AF431745DAC63F8FAFB0B4E0BD0F5BCBAF11ED502515501
          B8E253C177C1B4D95A6538FFC5B7670C35A7F2192EBD3D87D502743DAF6F7F40
          B500984C71BAEE74E83D7824CEBD3699610E1083520D3635A6AB95C0343099A2
          5A20A4C8C77036FC3E51E17DF9ED7986AAE3F90CCFBE3E868BC1F41AAF5404D3
          EFEE7F42B540509E97E15CE403A223F5EB9F2F0C1D67EA18B6DE5F07E6C3F41A
          2D5700D3EF1F7E46B540409687E17CF443E22D004678FBE95AB80530BD864BE5
          C1F487C75F502DE097E266B810F788B8207A739EA1E2500EC3D32F882082E935
          582407A63F3EFB8A6A01AF04174E032F253C8647F2CC8B13817802C3DF7F7FB1
          AAD15B200BA63FBFF8866A018F18276E17273D6178FEF52943D9FE6C860BAFCE
          E0540302BAF364C0F49757DF512DE016E6802BBE92FA144CEBCC9606D39D8E93
          199A8E56327CFD853BA3A1EBF9FAF607AA055C8288A2FA6AFA3330AD3D538AE8
          4847D7F3EDFD4F540B38F8D9E08AAF673E07D39AD389AF7FD0F5FCF8F80BD502
          761E5684E29CE76417D79A532016FCFCF21BD502362E16B8C266E75E8650ED68
          922C0119BEFAEA5286DABDC560FEAF6F7F101680AA4C663626292626D25D8D0D
          FCFBF79FE1EFAF7F882A93E6953E2D01008A32A1E056C0D8740000000049454E
          44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000003204944415478DA9D946B4853
          6118C7FFC7396F914357799D3A734D73792945D340CA0BA6416A36A12044A4C4
          0F7D0A22BA5074F9D4A78A9090B04022F33643C49CA3A5A5A289CBEBA6B8F232
          D162E696E936F5B41D75B8DCF29C9EF3E139EFFB5C7EEF7BCEFB7F09D0B4B894
          EC210288A09B6F3112E827E826C7A7649361E75F33E98FB1CA023002082F5421
          37960D92A456B7EE37DED748DB71F3A009CA9762668088C23738295A0738CD77
          C2BDFF0A8C3E5958125CB3022CB6BA067C185DC170C55966005151358E0B9DE1
          A2A982E7F87D78070462521F8AB9038FA05B26F1CB00E8373C69A60D3CCF6706
          8829AE41FADA0378EB1B20CAC8C3B0EC2D642E0F31C34EB45BA328CFA30F3896
          9A4916657941E03D8DC8F41C2CE9E6D1DE20858CF7DE1CB5DFA6F7592E3D80E4
          AEBF50FB7BD74854820861474F500D556DCD90CFA561DCABD4615D7759CECE80
          BA3B01A96C17768D203983E32B8C5AFF892B467CAA2C832CB00D4616D7616DD7
          D3D3FF0648EEF14AD86EEE4F4469392C8E5F90757E664401699302F29938BB75
          AE851ACA2B6F9B403852686C8016E2A4651CCACC87DB6E8EEDB7AD7B01FD8F59
          EA58764DEC418BCACF2EC81C1E261C2934C8D08444DD75B0C8656A2CCAC80537
          58608DAF180D1892D6A3AE650A551D2ED4DC945AB9ED8B103B299498EF055779
          09F1E2626C9E96A5052D06DED5A255E1845695AF39CFA6AFBA5BDE186A03D8AA
          50D2F26C0178284A112924E11F114B15FCD47CC3976609DA35228CEAF8DB763E
          A91ED90ED854A8C5B6DE2926FD5704F59F42E2B9123839B33133DC87C1CE1EBC
          EAE4A0BA5E4EEB885380E8E25A24ED67514D170D2466F524E61781F0EFB79012
          A646F091648C7D944235A647E7BE7274545C867995F401872FD62186E784092D
          09EDE2FA8DC55E5B40FA740A12F20A306A69AE0DC1679FC758253C28853202C4
          97D4C3954DC06022AD01FECF72C4B32AA8D33288335072AF5A7FB245A18C0009
          A5926D013FBD04117337A1DC7B03D39E629B9845A18C002121C174726D14CA04
          D067F6D1B4081B6651688FBCF1202D80BDC940BE90DC3AB6A750BA665368DECD
          B8D9F11DE4DA08E8BF013C7EB85DC0DF0AA56B7F00C86E77FA2A9D79AE000000
          0049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Rename'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC300000EC301C76FA8640000033D4944415478DA
          AD936B48537118C69F6D5E869B590B579AAB651AADAC54A4AB9AA576D3960466
          1165292A2915818AA8D9302DCB42D2B0082CED538211FA25A3922423B2B2999A
          D6BCCEA939DD6CF34CE765B39D2327BA796B3E9FDEFF7BDEF7F9FDF93F1C06E6
          411280B984C30964EA74AF6280A15FBF31E60370C79613EF6867973DCC44AF5A
          AD8D3B4D108FE60D906763E3C8635BCB76FA6DB1C1C4049A6B1BD1D4A72A891E
          2442E705101974F4C90E2BE3DE3DB6068CCBBBA06B53A0523DD014354888CC06
          1C0E8FF35DEAE45C792C34189A9E1ED8665D84B2568AAE71835F2C41549A0590
          4860B1C9D6A581B00B147AEC4AB292C99590C9DAD09B9F537AF9E39B10B33328
          CB10C46F16FB64B3ADD490D5F7A241133A52F171C430D0AF722D7B78ABDB2C40
          A944E0C877E6B7B8076E635B1B3FC1A0EF434F3B81F20AFBB8E8CC0FF9BFCEFE
          17C074FB873B8FEF0BB3B1D68161546358D984AAE786E637845C647ABA71B300
          65E94E3BD66C5BFF52E8268405731423AA3AC8655A7C91127EE23445E59FF373
          0290C16E5DB0BAD137CCC7C59A6D0FA35E09ADE22DAA9E698BC5A99D47FEB533
          270019ECD6833ED98B1C5682C5E242D35C820629A1EFEF1A5A7550D2D96D1680
          0CD65EE8D8E2B5671DDB92E30D7DDF3BA8E452D4BC5624986E7F7DAABD5903C8
          60038EFB86B179BB01230BAADA9BA8A91E3005DBFC57B0BF01C212EF4ECC64CE
          6775E05C4003566E5C0B26773F344D05E86E6BC5ED6A6F280D2BA6DDA5004599
          91530E188DA3A82EDC8BED418B61C1BF88316D07FAA539506844D820CE9BD63C
          3CA5601270EF5224222E14C073AD10359FDB419E6929EA5FA0ABE63104AE0608
          3CCF62E04332DADABF43242E8615973F2D80F4A40077251154235A720F744DAB
          F5FD538C754B2172F7C4B70E2974EA0A582E3B0427AFA8197323FD28407EDA29
          AA119B7E1F744DABAEBC00029E25381C0ED4CA1E6807E5703970050C066B4600
          E94701F2524F528D331985A06B5A5713227124D8171C860E4C9E0B78EBFC4DC9
          CD6C4EFB51809CE449D3F3970B41D73F830AF1C7907E14B979B97070F5989531
          2DD28F025C4F0A477C5611DC562F47FD5739C833AD1B29718849480777E1E239
          9993223D294056E289392FCF4649D71ECCEE4733473F0010855824F040DECC00
          00000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000044F4944415478DAB5956D4C5B
          6514C7CFD3FBD2F6BE415B0A8C212E01C310060ECA068C2D31D12CC6A8593431
          6ACC927DA0C5D7B8C50FC6C4D7C49868B6C56C0A2C6431BE2C6659962C4E374D
          50D9205870636C04C312824602227DA1CFBD6D6FEFBDBD3E17A15B6991F161E7
          5B4FCFF9FF9EE79CF39C8B4CD384BB6968058010CA383F02E05F0750362A76DC
          E9BCC7A4D3CE97B03A69FDB6B473003D12FFBC69A25EE2F97A16E303EF00A4EF
          44BC5B14DB6D14BA407458C3D0F6F963F1F339004B9C61D8CF77B5EF40636313
          10FA27FC895F965F5D4FBC4BE41EA329E6744B5B939DB2D96070604453539AAF
          5396C732806E4178C6EE60BFDABDCB874451004337A07F6018A251FC764096DF
          5B4B7CF950BDAD3BB7332E5701602CC32F978204A067037A246962FBB6EAAD15
          E59B32C9A99406FD43574091E32F939B1CCB298B24BCC2D9ED475B9A1B9028F0
          104F24A17F70C45455ED69127F3AAB445D82B09765E933BE6D357C71912B2392
          48AA7039386AC613EA7301453995298B24BC2B72DC5BAD8D75E074D8415553D0
          3F7C0D9484DAD189F189BC4D3E2E3ADB181BF36D534D95ABCCEBC940B0128781
          D1712DA5A7F6CDC6E2DF6F12842305E4F4ADF55B816518D0341D2E8F8E93B8C4
          1B7E8C3F5CC9CB3B45C704A19645F0537DD5166F45A937038990DA0EDD984CE8
          46BACF2D098FB6D4DE07144581611830343E09A198723880F1A1DB4B9817B054
          5BA7B30218EA524D795945555949C6BFB088617A7E019A2AB700B22130D32604
          6F4EC17C149F24273FB0BA476B0296E7BA8884FC5C59E2ADADD95C6205ACCE86
          2BD37FC14C387A760ECB4FE57B2FFF0BB0EC084021278AE7CB5D056D0DE5A559
          901B3373301D8AF411F14788782ADF08AF0BF8AF54745FB5D75D5955E4CEFA6F
          2A148189BF17C6152CEF390810DE30C06A3683E0626DB167F3BD055276D272EC
          EF0B61988AC406758CF7BE0820DF31C01A5716E873F55E8FA74CE032FE195981
          3F62323497149119A0967CD709E44F59BE3817931F5F5DAABC802E9E7F98A1A9
          B30FB80B79AFC391099E27AFF4B750246422F3B4483381668F1B1C34B5D4ECAB
          E128CC2513DFCCC6E4676F6F760EE033417892B5A1533E572153C83219F1702A
          0523914545D78DDD9DF1F8D52E913FC8D9A88F7DEE42C4D3F492D048741116D4
          D48900963BF2027A24613F8B6C277714484858BEBE65315D875F17639A6EC243
          64E9F5DF1A63DE8AEFF549222511884174863186A8A67FE0C7F29B7900E27803
          E7BCBF94B97572C5484350518C14184F58FB7D751397D634D8CE34F21CE32610
          8D6805C95AC1867E288095C3AB00523B8DE04223CBF285E406492B3899345548
          EFEF88295FC01A4696E41ECA86BE6B20795E92A792BCA16412129ABE33108F07
          B37A607D952C481D4DF33749691448BFE68F2947611DEBE6B846B2987EA8A329
          0F4374AE697A48D7345F209198CE99220B0208BE84B4D94B76FAFBEB89AFD8A7
          A2BD9A42CC8F60220ED2E907FD8A727DDD97BC512323EA280670BE40966F4E93
          EF96FD0B13694CEF65581A8D0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB0000032D4944415478DAB593794854
          5114C6BF374EB85793A6E192A464599428ED5A5A0A6AA6685222FD1304999586
          88DA464D419B468665A4A4869495562A5A46338E4BAB4B4A890B44199A56923A
          8ED338FBEDCD8463CE9B74B4BA3CB8EBF97EDF3DE73E8A10028AA230D3169594
          BD946D423D042127EFA5C716EBEF537F0BD8999C7D3560ADC781EAC6CE21A9EC
          876BE9E544E13F047059D1298EBD594762169454B780FFBA3D470DEA3905E24D
          1196A31AF2846903A2537276D15D8A664C87B2172FB45B7622761B46E50A6414
          F06033D7128B1C6DF1E8592B0687C5A13A4039D7C196B04D2AE94BAD9A28499A
          28A52A248CDBF7FD17203BCDDD6541F29E481F6DDC7C8E35A47215341E2D4C67
          C184C54253FB2764DCE2BFBB97D6E7A505549C72A4C5D935AE6BFC963B7BAE9D
          20DFF3B61E1F1B6ADB28A5D25F03F1DFCD35B3B773A88ADCE2B5212AD01B2312
          B9C68416A611B7306523FEC25D0C0845BE45E9712FB480F2332ECDB4B897BEB8
          1E44379711735488F7E1685C0C5C1C6C34A9028B06686E312A55807BBD1CBDFD
          C2E4A2F4BD17C70064CB9E68A3EBD0F5D50C572B96E06C7C0438B32D31564299
          5C49A74B81CFFD4264160AC890481CA303F86F5F6194386D16A7CB3623706330
          56BA3BA37F5084F9F3ACC1B1364773670F5A3ABAA1A63587472468FBF0A55D07
          30D6BD9458A2549204277B0EFA68A794EA0736ADF1C27A4F37143EAE07BEF130
          C7E43BDE4802B86A4A25D001C28E34199DA28EAE01A8D46AB839712012CB907A
          A506D141AB71A7B201D9C782C1BFB40EE1C7BBB589D301B6A5D6423C70FFB744
          18EA996BF44F854281151ADF73E06A2F427C8410D537729880D0E427900C954C
          29A8EBC9F858A60078CD5658EF2181CD6C25AA736F32015B931E6054F8C828F7
          14F44B36F1AC20F736131092580099E8E9D4CEF5D708734F9057CC04041FCA82
          7CA4CEA020358D9A68FAAABC32038083E7A090344CBBC80C23F4C7CF7FCC0404
          ED3F0CA5B495717826307E3E9F09F08BDA01B9A8C340B0B1F3F1B557BC4E03AF
          28E13C886AF88F419309EAAF5566A533013E21DE502BC5930A529342C6F75EF2
          BA0C3CD30349C6BB2693432AAF653200CFE8B12FFE592375E1C77BFC7480FFD9
          7E020EF8EA5A9F9886D50000000049454E44AE426082}
      end>
    Left = 270
    Top = 525
    Bitmap = {}
  end
  object ActionImageList192: TPngImageList
    Height = 32
    Width = 32
    PngImages = <
      item
        Background = clWindow
        Name = 'Login'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000003284944415478DA63FCFFFF3F
          C34002C641E300E94621395676C6F98C0CFF6D80C26CB4B1EEFFAFFF0C8C477E
          FFFC9FF8B4FEDD23140728B40BED1390E575E415E364606464A48DF540BB3EBF
          FACEF0E1F1E7FD0F2ADF39A13840B143E8A7B4A1281BAD2C4776C4D3F3AF7FDD
          AF78C78EE600E1FFD2062234B51C069E5E78C370BFE22D238603A4F4B03BA04C
          A791C14F2E9C819181F4D0F90F849B1EAD64E8BA520F177B76098703247584B1
          1A72D4E7265996233BC27A8B3A9CFFFCCA5BEC0E90D016C26AC071DFDB60DA72
          B32AC99663D3FBE2EA3BEC0E10D714C46AC809FF3B60DA62A30AC90EC0A6F7E5
          F5F7D81D20A68EDD012703218698AF8718D2613E95A1E3420DC3879FEF093A00
          5D2F08BCBA89C301A2AA02580D39157C174C9BAD5586F33FFE7ACFD07EAE8661
          DFD31D781D80AE17045EDFFE80DD01222AFC4405EBE9907B7036C801EDE7AA71
          86064CADE91A25B8D89B3B1FB13B40588938079C09BB87C2FF000C8DB633D50C
          FB9EECC0A9D66415C2016FEFE1708090021F5CD1D988FB44390619801CD07ABA
          0A253460E618AF50848BBD7BF009BB0304E578E18ACE453D20D901E0D0005A0E
          72C4DE47DB51CC315AA60057F3FED167EC0E1090E5812B3A1FFD906C07B49CAA
          843B00668EE15279849AC75FB03B805F9A1BAEE842EC23922D0759DA7CA20225
          0A60E6182C96838B7D7CFA15BB03F824B989B2E8623CAAE34016361FAF60D8F3
          703B4EB5FA0B110EF8F41C87037825B88872C0A584C770F69E87DB189A80967F
          F8F11EAF5ABD05B270B1CF2FBE6177000FB031820D5C4E7A02A675E7C9C0F920
          5F371D2D67D8FD601B5EC7A2EB05812FC046095607708B706035E44ACA5330AD
          33471A4CF7BBCC66683C520EF4F53B82A185AE1704BEBEF981DD015C42D81D70
          350D6288F62C6906520136BDDFDEE17000A7203B5643AEA53F03D35A33A54876
          0036BDDFDFFFC4EE000E7EEC8DE16B99CF286E90684D4738E0C7C75FD81DC0CE
          8BDD018D0E5D0C61DA316437C9565D5DC2507FA00C2EF6F3330E07B071B392ED
          4B52C0AFAFBFB13940E8272B170B300868DB2C0785C9EF6F7F309BE5A08E0933
          2B9323130B134DADFFF7E71FC3DFDFFF303B2603DE351B2830E00E0000F44308
          DF7A6C2FDC0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Open current session in PuTTY'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000004594944415478DAB5977B4C53
          571CC7BF9752E8905249E5616979D40818EAC42C9BF20A634C51D9F081DBB299
          251B59B68492FDB12C5BFCC38465664B16B3B864642E9931332A988888718B3A
          E204015948101F40A55206D442A9056A9F96B667B7175A292DA5ED75BFA43DE7
          9EDFFD9DF339BFFBEBF9DE5288D05E2DDD731BA0B6471AEF3602745291035492
          0C692E9BF531A652801580387323AA6B7F0021CC6E982F77BBF4DAB574DCE3A3
          3F574F7E05F5BF4A7600228914FB6B7F64AE5D6431AD8BAD6BB1439640CD3B01
          D33302A30D1868FE129A09153B80545106F6CA7F5A71F7EEF6A98DC060010C74
          6BB5136FBCEEEA1798D28CB103484E15E36D7903B3DAD29DDAE99DEA8C0433F4
          C20E2709183F79E5734C4FA9D9010893D6E3ADBA1358DC30605663DDD067B051
          09B89D7A3668FC784B2DF4BA49760089C2641AE037A6D01CDA1E48C7E5906C90
          40F5E8096EA677048D1FB9F02966F5D3EC00046B85A8A83B85A8F1266C9AFD0E
          79E57B303DA240876E075489B541E31F36D6C030A76707C0E727E0E0EE6C6493
          566CAE38086E5C1CBACF9EC0DF924ED839C2A0F10F4E7F0893D110394049F92E
          F2EED6C7C8CF4B44DE8E7DE0706330A9B88BAEFE28DC17FDB26AFC9D53EFC362
          364506D0FAAD2867C6B246915FB005D26DA5F4C8C2347D2DBFE31F5D310C44B2
          EA1C9D5DDDB0D9ACE103B47C9356CE8DE1366F2CDA2948CD79D93B6ED26B71E3
          5C237EED91D1BF8895A7E51C5630ADF3FB5CD8E7EDE101B41E15D7717971C765
          6FEEE308D6A7FBF8866F5DC3B92B53E81C4D0E6346D24385A26A1C8A60D7260D
          4A367320DB79003CBEC0C7EF74D8D17DE667A7DD6A161DA8D74E8BB3727C4E1F
          F5E8C315374A85A26ADB92FAF05AB60DB9A59574B17199B1E8581EA8280ED377
          179F3B031E534E45A3A12DC17B2D4C49F374477BDBFF90FA01ACA66A71AAE388
          D5342D04B8EC88A6AC283824672ADF27A1C485475D6D500C6AD1EBAAF1DB48F7
          8DCBA001283F8070548D3F7A0CB9825E6C2828F799DC617F86C1B64BE8BD3B83
          E67BE9B41E44054A66E00C84AA6A469305AF288B51F4CE07888D7F9E62AB6106
          0FAE5F84654E7FACCF31F1757DFDC26B40284685A36A59C6D3284B68A20F9EFD
          DE09E66849BD77AD1564DEF249D511F5C95017F60158AE6AF30E404B2F3C6725
          DE47E1F6944FBC8EA2CA32F093450BC537D48FFB1D3771BE2F0D172EB54774A8
          51CB556DD64CF0C4F4FCD97B2CC57C1D65388AFCAA43DE621B1E31E2625F0AC6
          268D7EC515168047D5A68D80C51EF80562BBBA1A852519584BD78BBBD894B399
          E84F6DC0E0F93A46D55801C4D307CB968FCEC0E90ABCB8C07607A57335905554
          63E0AF160C51D5184E3AEC0EF7AA1A2B80B835F1D8FA71E38A37C93472E4AD53
          6246A3C140E2116804EF797D1E556305C0E3BD84E2A2C2156FCAA4DA21A486A1
          24BBF194887D7C1E55630510B3EC440BC596AB1A0B00F67FB1DCAAD6DBFE6741
          4400C19CE1A85AA4E63721FD48547493E5EEEBB58F7D7CC154ED450290C237AA
          82060552B5FF250341EC8565E03F34C2422D497EFF330000000049454E44AE42
          6082}
      end
      item
        Background = clWindow
        Name = 'Rename'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC300000EC301C76FA864000004524944415478DA
          DD947B4C935718877F5F292D50CA00857229A576C2364683C286248866615361
          0E656EE2064199447689D310D92465EC0BA222646457C7162E82098931E28697
          6DA9669B92650351EB00B55C570AA540A137A05468D7E23E43976D52A098ECF7
          D7FBE53DE77D9E9CEFE41078C421FEB7029FB399A1CE60941284E9D41EEDD8C9
          251520019ABF87FBB58815BCD52353D390F7F689278D53597B0D86EE25112863
          B3B205DE5E1F45AC8D8649A787E26E179A15CA319319FC2C9D6ED8A102E98929
          A16152494BDA1381CE74B63BA6E40318EFEDC39511B5725AA7176401E30E1578
          7957F6F771B1311B57F138F03F7E0C6E1D9D68962BA09C366D7B53AFAFFBFBFA
          451510BDBB3959C77EEE744A7282D3322F367E6FEBC448D9A7C01571BDE5E8B7
          FCD39E4513384B7A7AB2BC96773139E15E2CC17EF3E8E463846A548B73E7BE33
          72240DC2D2C606A94305EA0B832A635F8DCF60D006D12F95A2A53FFADEAF7DCF
          98DBBBE5FBBFA92EFDF2DFF62D8A407D01777DB090FF5368CC2A30A76FC07C4F
          0BA54C8D1F2FB3C4A90577365996981C265045C2C5CF7D456BDC8E1705AE4E0A
          10660D0CC31D903412534AF944C45672A0EDBFF62F58A0BE907B744DD2DA835E
          BE1EA0430BA3A613A3035A345D1D3F9294D72B7AD8FE0509D49141E1818FFB4B
          22E223690C671A4C4615C6FBAFA3E1F26487D22013669030384C8024418B72E6
          35C5A73F1FC9745D6E1964825E2646FB6D237A3BF5EBB6E6F75E9DCB9C790B7C
          7B88B72F6A43F4C71C7E20E80C3F185437A151B4A3F1E7A10ACBD167CE75CEBC
          04CE90815C9F00DFF6359BC35D18AC18CBADD760F44E2D9A7E9B189ED0A94292
          49B5DAA102F585BC0BF1A9EB125D3C2341A3FB40233D09798F069DB715DBB7E4
          C94EDB33CB6E01CB83F35A586C546DF0D341A0B3366152751DEAAE1FD0D8203B
          9F94277BC9DE79330229EF7D6D9ECB620661C08E806ABCB08D0767EFB760364D
          63A8F143DC6A35A2569E8671B3875DF053C57B88070235871F7E6F5A2EE62024
          4C0977CE7A10CC68A8DBBEC090BC0734DFD71114996E173C5D546E2B50756837
          323EA84054181FCD6D3DB07ECFCE1FD76AA090542026910B576E318C23120CDD
          2C45B79C8DA8D43AD8FB37AD2C1B81F28237661A99F995A0EAD9B975B10CAEE6
          31C0DC0A0FCE5360EA2F41D667C0CA840AB82D5B69179CE2D8087C45DE876691
          95A0EAD9B971A60841017EF0E1F843373A04D9DD2AB80527801F7BC06E38C5B1
          11389E9F31D378BBA00A544D65CA6880F47C0984AB9FC5845E07AD5A059D5E01
          FE06110827C6BC04AC1C1B81CFF276CD34F6169E00555391FC22868BBA154F86
          083032A88089CD8577F8461034FABCE014C746E013D17DE8BEC32740D554CA8A
          7271F6C2256426C721717B06585CE1BCC154AC1C1B81D2DC9D338DECA3D5A06A
          2A0776BF8241951AE4916208C222170CA738360225EFEF44CEB16A8487F2D022
          95C1FA4DA5BC241769EF88E0E2E6BE28706BAC2C1B81A21CFB1E9285E660498D
          ADC092D2FFCA038147993F01C665D330F24A9E510000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Delete file'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000005D44944415478DAC5975B4C53
          7718C0BFD3734E2FE706A540A91A65334E5D41AE52449D4B96B925CBE2084F1B
          4B8C33B17889718BD9D3B261F6B498EC964D41E77C5816E38371C6A9C42C9BD1
          AD040515970E048BE5A2544AC1F65C5A7A4E7BF66FB15DA1AD80BAF8BDF4F47F
          CEF7FF7EE7FB7FB783A9AA0ACF53B004008661FFABA16600BD85A28C76491A8D
          FD4FDACD04D0CAB21BD023470083167B40F8F6698DB71A0C4B81247E4397C5AA
          1A6968E2A5B3590162C63538D656566EA57B9C7D100C853E68E2C5E34F6AFC3B
          86B19218B49594AE5A62CA37C29F57AECA8A2237D803E2D93480B8710DD666B3
          55D0E6A202E003025CB9DCA18664A56127CF9F5EA8F1439CA11657895F2B2AAD
          A665CB96C4D7C6C6C6C1E1E89476F8797A06C0118E9B36BEB68C369BF3939B4C
          4C3C84BFDAAFCBB2127973A720FC3E5FE32D34FD3A4EE0A7ABAB4AE94596C2F8
          9A1A8D427BC74D7830EEFBD9EEE7DF9F0990C34DD65496E45ACC05699B8D797D
          D0D1F5B7882836ED92A4AEB98C1F66980692C04FD8AA4AC982FCBCE94564A7F3
          A613EE79BC174603FC3B9FA96A7806C0518EFB25DF94BBC55661051CC7D33645
          8AD0D9DDE3935575D31E4170667D7396DDAE2389A3B6CA122C2F974BAE77A378
          728F781C0ACFBFB11B40480BC20318A62DE2981FF338B6D1566E052D49A66D3E
          786F14BA7BEF0C811CD9680F068766DF4731F4B141AFFDA2B6BC0458864AAEF7
          DC7143BF7BD829F1C2860F011E4E3B2443163403682C0CF31552DE5BBB663518
          F4BA3488BEC111E81D18EE4397EBED3C3F9EF2E60719836E7F1D824FD5730DDF
          07A7CB3DA44454DB6E51F424D61F5B075A18E653CAA03BB0AE64253094210DC2
          393004AE7B9E2E25C0BFEA05902C2CFB03471BB6D521682D49249F1B46C776A3
          6FC0034AC436DB638F0588BB9363F66A71E2EB5AEB4B580E4DCD24403A37905B
          47BCBECB48DB67E298FADAD52B66C48E676212AEDD1EF0CB51757DA698991320
          1ECD1CDD4862F8F1DA55CBC93C964983B8D67F37F603D52B8A41A3D1246F8D07
          78E8B83D2086A3F2E6DD7CD0912958E70530ED09EA2D0CF053D5CB8B75E694A8
          9EB1498AAE5F94C071DB25875565CBAE807401B2C8BC01E210A842A2DB6D654B
          17D34BF272B33E278442E0E8BBAB4E2991779B04E1243C4616041087A0E952E4
          E73F5E5E54687A21515C52241496C1313008C129B9C92E08AD30872C1C80A22A
          51945DB45A0A4CC519BC109211807B2406B00B011C7EA600F126858E608DA590
          5EC4B1599F13C361681F1C51A7E4486393289E7826002D2CF5368EE1A7AA8A0A
          C98259E9984937303505ED23A3720422F5F68074EEA9005A597A2B81698EAD2D
          2AC48DB3AA624CAF6B6CBA10561498004F49C309148C573D5E1165C293A7612B
          47EFD362F89735E67C8C9DDD1790CE2DD4A6EF8BD2A5582132EAB40DD5088248
          81782005E1866F920F47D5750B2E442D1CF33985E39FD4988C4011C46C5DE8F1
          F3E016A564292E62E92308727B2DCA0E3205E23E82B835E9F74664A5666728E4
          9E13A039D68C38E61B0627F6541B73409FA12DBB0411FA05A9175D6E9CDD8C28
          5CB3DF863224550F8142AF20CCDD8C12EDD848908D55A8ECA6BE494286D1D93A
          45316B3B6E61E9FD7A0D7E702DC70143FC07D12F49E092824E91175EF9088548
          4680D8409247125B2A50A4E319027214A5D82D31E893215CB7879FEA832C8220
          B66951E0563234969B727CFFA0E3189E0A671F48622359B95E9F6B26D3CFDCAB
          2870331812D15C38BF918C65EB49504F965306323F05A23B18028FA2641EC90E
          71549D16232E96EB747441CA193E8C44A03384EA9C0A9BD1507A692EE3494FD0
          F46B38AE39B346ABA5CD8F2062B6AEA33AE18D464EA0A1F4BDB420FC9E35C421
          CA08220E21A229B6231C5665C0EA9B78FECC7C8D273D6130D46808E2BC95244C
          8B1FBD940FEDD92587D3C7F2441A262056E238ED521455C6D4AD3B02E24F0B35
          9E90F8878906CEAFC089A54664A34B96E50816ADDFE117CF6504484010801F46
          9F66C79ED9A719415C4487F02220E3B112BDE06EF8B4D28C3E4E73693A679F28
          3E88FD4F03785EF22F94D231EEF16B1BE90000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Create directory'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
          F4000000097048597300000EC400000EC401952B0E1B00000A4F694343505068
          6F746F73686F70204943432070726F66696C65000078DA9D53675453E9163DF7
          DEF4424B8880944B6F5215082052428B801491262A2109104A8821A1D91551C1
          114545041BC8A088038E8E808C15512C0C8A0AD807E421A28E83A3888ACAFBE1
          7BA36BD6BCF7E6CDFEB5D73EE7ACF39DB3CF07C0080C9648335135800CA9421E
          11E083C7C4C6E1E42E40810A2470001008B3642173FD230100F87E3C3C2B22C0
          07BE000178D30B0800C04D9BC0301C87FF0FEA42995C01808401C07491384B08
          801400407A8E42A600404601809D98265300A0040060CB6362E300502D006027
          7FE6D300809DF8997B01005B94211501A09100201365884400683B00ACCF568A
          450058300014664BC43900D82D00304957664800B0B700C0CE100BB200080C00
          305188852900047B0060C8232378008499001446F2573CF12BAE10E72A000078
          99B23CB9243945815B082D710757572E1E28CE49172B14366102619A402EC279
          99193281340FE0F3CC0000A0911511E083F3FD78CE0EAECECE368EB60E5F2DEA
          BF06FF226262E3FEE5CFAB70400000E1747ED1FE2C2FB31A803B06806DFEA225
          EE04685E0BA075F78B66B20F40B500A0E9DA57F370F87E3C3C45A190B9D9D9E5
          E4E4D84AC4425B61CA577DFE67C25FC057FD6CF97E3CFCF7F5E0BEE22481325D
          814704F8E0C2CCF44CA51CCF92098462DCE68F47FCB70BFFFC1DD322C44962B9
          582A14E35112718E449A8CF332A52289429229C525D2FF64E2DF2CFB033EDF35
          00B06A3E017B912DA85D6303F64B27105874C0E2F70000F2BB6FC1D428080380
          6883E1CF77FFEF3FFD47A02500806649927100005E44242E54CAB33FC7080000
          44A0812AB0411BF4C1182CC0061CC105DCC10BFC6036844224C4C24210420A64
          801C726029AC82422886CDB01D2A602FD4401D34C051688693700E2EC255B80E
          3D700FFA61089EC128BC81090441C808136121DA8801628A58238E08179985F8
          21C14804128B2420C9881451224B91354831528A542055481DF23D720239875C
          46BA913BC8003282FC86BC47319481B2513DD40CB543B9A8371A8446A20BD064
          74319A8F16A09BD072B41A3D8C36A1E7D0AB680FDA8F3E43C730C0E8180733C4
          6C302EC6C342B1382C099363CBB122AC0CABC61AB056AC03BB89F563CFB17704
          128145C0093604774220611E4148584C584ED848A8201C243411DA0937090384
          51C2272293A84BB426BA11F9C4186232318758482C23D6128F132F107B8843C4
          37241289433227B9900249B1A454D212D246D26E5223E92CA99B34481A2393C9
          DA646BB20739942C202BC885E49DE4C3E433E41BE421F25B0A9D624071A4F853
          E22852CA6A4A19E510E534E5066598324155A39A52DDA8A15411358F5A42ADA1
          B652AF5187A81334759A39CD8316494BA5ADA295D31A681768F769AFE874BA11
          DD951E4E97D057D2CBE947E897E803F4770C0D861583C7886728199B18071867
          197718AF984CA619D38B19C754303731EB98E7990F996F55582AB62A7C1591CA
          0A954A9526951B2A2F54A9AAA6AADEAA0B55F355CB548FA95E537DAE46553353
          E3A909D496AB55AA9D50EB531B5367A93BA887AA67A86F543FA47E59FD890659
          C34CC34F43A451A0B15FE3BCC6200B6319B3782C216B0DAB86758135C426B1CD
          D97C762ABB98FD1DBB8B3DAAA9A13943334A3357B352F394663F07E39871F89C
          744E09E728A797F37E8ADE14EF29E2291BA6344CB931655C6BAA96979658AB48
          AB51AB47EBBD36AEEDA79DA6BD45BB59FB810E41C74A275C2747678FCE059DE7
          53D953DDA70AA7164D3D3AF5AE2EAA6BA51BA1BB4477BF6EA7EE989EBE5E809E
          4C6FA7DE79BDE7FA1C7D2FFD54FD6DFAA7F5470C5806B30C2406DB0CCE183CC5
          35716F3C1D2FC7DBF151435DC34043A561956197E18491B9D13CA3D5468D460F
          8C69C65CE324E36DC66DC6A326062621264B4DEA4DEE9A524DB9A629A63B4C3B
          4CC7CDCCCDA2CDD699359B3D31D732E79BE79BD79BDFB7605A785A2CB6A8B6B8
          6549B2E45AA659EEB6BC6E855A3959A558555A5DB346AD9DAD25D6BBADBBA711
          A7B94E934EAB9ED667C3B0F1B6C9B6A9B719B0E5D806DBAEB66DB67D61676217
          67B7C5AEC3EE93BD937DBA7D8DFD3D070D87D90EAB1D5A1D7E73B472143A563A
          DE9ACE9CEE3F7DC5F496E92F6758CF10CFD833E3B613CB29C4699D539BD34767
          1767B97383F3888B894B82CB2E973E2E9B1BC6DDC8BDE44A74F5715DE17AD2F5
          9D9BB39BC2EDA8DBAFEE36EE69EE87DC9FCC349F299E593373D0C3C843E051E5
          D13F0B9F95306BDFAC7E4F434F8167B5E7232F632F9157ADD7B0B7A577AAF761
          EF173EF63E729FE33EE33C37DE32DE595FCC37C0B7C8B7CB4FC36F9E5F85DF43
          7F23FF64FF7AFFD100A78025016703898141815B02FBF87A7C21BF8E3F3ADB65
          F6B2D9ED418CA0B94115418F82AD82E5C1AD2168C8EC90AD21F7E798CE91CE69
          0E85507EE8D6D00761E6618BC37E0C2785878557863F8E7088581AD131973577
          D1DC4373DF44FA449644DE9B67314F39AF2D4A352A3EAA2E6A3CDA37BA34BA3F
          C62E6659CCD5589D58496C4B1C392E2AAE366E6CBEDFFCEDF387E29DE20BE37B
          17982FC85D7079A1CEC2F485A716A92E122C3A96404C884E3894F041102AA816
          8C25F21377258E0A79C21DC267222FD136D188D8435C2A1E4EF2482A4D7A92EC
          91BC357924C533A52CE5B98427A990BC4C0D4CDD9B3A9E169A76206D323D3ABD
          31839291907142AA214D93B667EA67E66676CBAC6585B2FEC56E8BB72F1E9507
          C96BB390AC05592D0AB642A6E8545A28D72A07B267655766BFCD89CA3996AB9E
          2BCDEDCCB3CADB90379CEF9FFFED12C212E192B6A5864B572D1D58E6BDAC6A39
          B23C7179DB0AE315052B865606AC3CB88AB62A6DD54FABED5797AE7EBD267A4D
          6B815EC1CA82C1B5016BEB0B550AE5857DEBDCD7ED5D4F582F59DFB561FA869D
          1B3E15898AAE14DB1797157FD828DC78E51B876FCABF99DC94B4A9ABC4B964CF
          66D266E9E6DE2D9E5B0E96AA97E6970E6E0DD9DAB40DDF56B4EDF5F645DB2F97
          CD28DBBB83B643B9A3BF3CB8BC65A7C9CECD3B3F54A454F454FA5436EED2DDB5
          61D7F86ED1EE1B7BBCF634ECD5DB5BBCF7FD3EC9BEDB5501554DD566D565FB49
          FBB3F73FAE89AAE9F896FB6D5DAD4E6D71EDC703D203FD07230EB6D7B9D4D51D
          D23D54528FD62BEB470EC71FBEFE9DEF772D0D360D558D9CC6E223704479E4E9
          F709DFF71E0D3ADA768C7BACE107D31F761D671D2F6A429AF29A469B539AFB5B
          625BBA4FCC3ED1D6EADE7AFC47DB1F0F9C343C59794AF354C969DAE982D39367
          F2CF8C9D959D7D7E2EF9DC60DBA2B67BE763CEDF6A0F6FEFBA1074E1D245FF8B
          E73BBC3BCE5CF2B874F2B2DBE51357B8579AAF3A5F6DEA74EA3CFE93D34FC7BB
          9CBB9AAEB95C6BB9EE7ABDB57B66F7E91B9E37CEDDF4BD79F116FFD6D59E393D
          DDBDF37A6FF7C5F7F5DF16DD7E7227FDCECBBBD97727EEADBC4FBC5FF440ED41
          D943DD87D53F5BFEDCD8EFDC7F6AC077A0F3D1DC47F7068583CFFE91F58F0F43
          058F998FCB860D86EB9E383E3939E23F72FDE9FCA743CF64CF269E17FEA2FECB
          AE17162F7EF8D5EBD7CED198D1A197F29793BF6D7CA5FDEAC0EB19AFDBC6C2C6
          1EBEC97833315EF456FBEDC177DC771DEFA3DF0F4FE47C207F28FF68F9B1F553
          D0A7FB93199393FF040398F3FC63332DDB000004224944415478DAC5947D4C5B
          5518C69F5BFAC9360820D94429CEA9B171CEC464824CC362D58D194A9D29982C
          C630B57C085387B09030D7C9F8C375CCC8740BC887C445AD8866E8EA062B7533
          6333B03043D8161A050A43C04129D0AEB4F45EEFADA2EB76B9BD2BC5BD374DCE
          39F79CE7FDBDCF797B098AA2C00441100875641455BF240C13547966A71E6AFC
          70E704DB1E6229013277551BF3341B530F1BCC470CFAECBCFF1520B5A032E209
          C57DE3BADC3461798D91ECBE7C556138A8ED65DE69343A3129BFFBFEA68A3F7A
          970C20B3B87ADB76F586A3CA440546AED95158F1CD590AE4058948982C5F15B3
          6E72DA291E999C5A1F3200DAEEC9B56BEE899CD7914A84C8D5A4C0E1F2203A22
          1C43A3369074AE8455D1B04D39F1CE0143E7E7E583892103C828AA2A566D7CEC
          836D5B12C14852F4E3F67831E59805DD88908885CC2296C944D8FF590BD57969
          E0F146BDF6A21F40BD0ED21861BC999E25B125A111DBAFCD5995593AB816B0FD
          5056FA86FCE792149872CE82D1269887F85B5F46BBD23B308AB2AAE35506BD36
          C7A7792340F3BE78FDC329AFBEFB60F20ED62A2DED95B872BAE180AA74B088DD
          079D20B338AEA93C5FAD4E888BF139309F9CF197C9D475D98A8FBE30D51AF66B
          5FF7033856169F28090F3FB75EF3062192CA58E53DAEEBE868FC949A753A9F54
          BF37F40BDB9E9777551B74B9AA8CD57177218CB69EF8A7380684E9019BDD89E6
          D3BFE2C4D99E7D5FEBB5BBFF05F8BE5CDEFD8872F3DAD8D56B38EFFACFBEDFD0
          633AC9FA6ECC2B87E381BD78252D89B65B0C922421158B20101018A713BBDC1E
          78E7BC98A3D7BF3575A1A3BB5F7B034002A5DCFE62D04D485204CA0D0A646D55
          4342DFF5D5D1495FE27B57462172B90C968131BAF28BBE6B61BAD44BFFFA87C7
          87FC0052B6AE0B1AE0D2702C1ACE2B11111E062961853C7A0296B195503D9B8E
          C808197EEAE885F0CAFBC8D1B5F9FDDDFC0082CE0EA6C1049826A3B04260A3EF
          9DF4AD8D7BE3D0175B82D4A71EC5973F9CC133E45EA84BFB1606482BE95C0C03
          6B7C6CB8009178193C6E07127ECF86AAD4BA30C00BC52638269AFDEA5AECD836
          23C09E8628E4A9EC18397928004091110EDB71DE49089E206E0F01B18882B9A6
          861B604BE177B86E3F717BD552FC1D31D7367003A4EEFC0A2E7B4BD0B6FB3B72
          EBFBB6DAA30100DEAE876BDACC21B8B89E68AB3570036C7EEB08DC333F87C47E
          B6FE30D5350500283808B7E3FC2D07F9365BA0B1A9EE580080FC32789C5D4188
          F3EB8F5375466E804D79259873F5F016FC6FCA0FF6547D4B00809C1DF0CE5A42
          62373B401B37C0F3D9AF81745B17100BE0068F6B69AD3FC30DF0749A92BE8261
          B00757A5FCD6CFB55A027C090BF680225DB72DCC77CF8F9F547003246F52D000
          1EBFE344889233D1DEDA1FE04BF866E14D070308530BACDF349ECF683C5CC905
          209FA6A7CBB18441E79A49DF3DB88215E04EC51D07F80B8CCA82E42EA50F1200
          00000049454E44AE426082}
      end>
    Left = 398
    Top = 525
    Bitmap = {}
  end
  object CloseDropDownMenu: TPopupMenu
    Images = ActionImageList
    Left = 36
    Top = 397
    object MenuItem15: TMenuItem
      Action = CloseAction
      Default = True
    end
    object MenuItem20: TMenuItem
      Action = NeverShowAgainAction
      AutoCheck = True
    end
  end
end
