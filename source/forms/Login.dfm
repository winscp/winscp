object LoginDialog: TLoginDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login'
  BorderIcons = [biSystemMenu, biMinimize, biHelp]
  Caption = 'Login'
  ClientHeight = 432
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
    Height = 432
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ContentsPanel: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 391
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      DesignSize = (
        361
        391)
      object ContentsGroupBox: TGroupBox
        Left = 2
        Top = 12
        Width = 347
        Height = 376
        Anchors = [akLeft, akTop, akBottom]
        Caption = 'ContentsGroupBox'
        TabOrder = 0
        DesignSize = (
          347
          376)
        object ContentsLabel: TLabel
          Left = 12
          Top = 20
          Width = 31
          Height = 13
          Caption = 'Name:'
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
          Height = 322
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
      Height = 391
      Align = alClient
      Anchors = [akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        361
        391)
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
        object Label3: TLabel
          Left = 12
          Top = 122
          Width = 55
          Height = 13
          Caption = '&User name:'
          FocusControl = UserNameEdit
        end
        object Label4: TLabel
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
          MaxLength = 100
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
          Value = 1.000000000000000000
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
            'WebDAV')
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
          Width = 96
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
        Height = 141
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Note'
        TabOrder = 1
        DesignSize = (
          347
          141)
        object NoteMemo: TMemo
          Left = 7
          Top = 15
          Width = 333
          Height = 117
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
      Top = 391
      Width = 361
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
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
    Width = 269
    Height = 432
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      269
      432)
    object SessionTree: TTreeView
      Left = 11
      Top = 12
      Width = 247
      Height = 383
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
      Top = 375
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
      Top = 401
      Width = 98
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Manage'
      TabOrder = 3
      OnClick = ManageButtonClick
    end
    object ToolsMenuButton: TButton
      Left = 11
      Top = 401
      Width = 98
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Tools'
      TabOrder = 2
      OnClick = ToolsMenuButtonClick
    end
  end
  object ActionList: TActionList
    Images = ActionImageList
    OnUpdate = ActionListUpdate
    Left = 68
    Top = 81
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
    object GenerateUrlAction: TAction
      Category = 'Sessions'
      Caption = '&Generate Session URL...'
      OnExecute = GenerateUrlActionExecute
    end
    object CopyParamRuleAction: TAction
      Category = 'Sessions'
      Caption = 'Transfer Settings &Rule...'
      OnExecute = CopyParamRuleActionExecute
    end
  end
  object ToolsPopupMenu: TPopupMenu
    Left = 176
    Top = 161
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
        Name = 'New Site opened'
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
        Name = 'New Site closed'
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
    Left = 68
    Top = 25
    Bitmap = {}
  end
  object SaveDropDownMenu: TPopupMenu
    Left = 68
    Top = 145
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
    Left = 68
    Top = 209
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
      Action = GenerateUrlAction
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
  end
  object ManageFolderPopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 70
    Top = 265
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
  end
  object ManageNewSitePopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 175
    Top = 225
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
      Action = GenerateUrlAction
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
  end
  object ManageWorkspacePopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 174
    Top = 281
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
  end
  object SessionAdvancedPopupMenu: TPopupMenu
    Left = 176
    Top = 89
    object Session1: TMenuItem
      Caption = 'Session'
      Enabled = False
      Visible = False
    end
    object MenuItem9: TMenuItem
      Action = SessionAdvancedAction
      Default = True
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
    Left = 177
    Top = 25
    Bitmap = {}
  end
  object LoginDropDownMenu: TPopupMenu
    Images = ActionImageList
    Left = 70
    Top = 321
    object Login1: TMenuItem
      Action = LoginAction
      Default = True
    end
    object OpeninPuTTY1: TMenuItem
      Action = PuttyAction
    end
  end
end
