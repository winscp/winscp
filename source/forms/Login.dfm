object LoginDialog: TLoginDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 432
  ClientWidth = 513
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    513
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object HelpButton: TButton
    Left = 427
    Top = 401
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    Visible = False
    OnClick = HelpButtonClick
  end
  object SaveButton: TButton
    Left = 325
    Top = 401
    Width = 82
    Height = 25
    HelpKeyword = 'ui_login_save'
    Action = SaveSessionAction
    Anchors = [akRight, akBottom]
    Style = bsSplitButton
    TabOrder = 3
    OnDropDownClick = SaveButtonDropDownClick
  end
  object LoginButton: TButton
    Left = 230
    Top = 401
    Width = 82
    Height = 25
    Action = LoginAction
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CloseButton: TButton
    Left = 420
    Top = 401
    Width = 82
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
  end
  object AboutButton: TButton
    Left = 11
    Top = 401
    Width = 82
    Height = 25
    Action = AboutAction
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    TabStop = False
  end
  object LanguagesButton: TButton
    Left = 105
    Top = 401
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Lan&guages'
    TabOrder = 1
    OnClick = LanguagesButtonClick
  end
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 513
    Height = 392
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 6
    object PageControl: TPageControl
      Tag = 1
      Left = 152
      Top = 0
      Width = 361
      Height = 392
      HelpType = htKeyword
      ActivePage = BasicSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabOrder = 1
      TabStop = False
      OnChange = PageControlChange
      object BasicSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_login_session'
        Caption = 'Session'
        ImageIndex = 1
        TabVisible = False
        DesignSize = (
          353
          382)
        object BasicGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 173
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session'
          TabOrder = 0
          DesignSize = (
            345
            173)
          object Label1: TLabel
            Left = 12
            Top = 72
            Width = 55
            Height = 13
            Caption = '&Host name:'
            FocusControl = HostNameEdit
          end
          object Label2: TLabel
            Left = 253
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
            Left = 164
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
          object HostNameEdit: TEdit
            Left = 12
            Top = 89
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 3
            Text = 'HostNameEdit'
            OnChange = DataChange
          end
          object UserNameEdit: TEdit
            Left = 12
            Top = 139
            Width = 137
            Height = 21
            MaxLength = 100
            TabOrder = 5
            Text = 'UserNameEdit'
            OnChange = DataChange
          end
          object PasswordEdit: TPasswordEdit
            Left = 163
            Top = 139
            Width = 171
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 6
            Text = 'PasswordEdit'
            OnChange = DataChange
          end
          object PortNumberEdit: TUpDownEdit
            Left = 252
            Top = 88
            Width = 82
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = PortNumberEditChange
          end
          object TransferProtocolCombo: TComboBox
            Left = 12
            Top = 39
            Width = 137
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = TransferProtocolComboChange
            Items.Strings = (
              'SFTP'
              'SCP'
              'FTP')
          end
          object FtpsCombo: TComboBox
            Left = 163
            Top = 39
            Width = 171
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnChange = TransferProtocolComboChange
            Items.Strings = (
              'No encryption'
              'SSL/TLS Implicit encryptionX'
              'SSL Explicit encryptionX'
              'TLS Explicit encryptionX')
          end
          object WebDavsCombo: TComboBox
            Left = 163
            Top = 39
            Width = 171
            Height = 21
            Style = csDropDownList
            TabOrder = 2
            OnChange = TransferProtocolComboChange
            Items.Strings = (
              'No encryptionX'
              'SSL/TLS Implicit encryptionX')
          end
        end
        object BasicFtpGroup: TGroupBox
          Left = 0
          Top = 185
          Width = 345
          Height = 75
          Caption = 'Additional'
          TabOrder = 1
          object FtpAccountLabel: TLabel
            Left = 12
            Top = 22
            Width = 43
            Height = 13
            Caption = '&Account:'
            FocusControl = FtpAccountEdit
          end
          object FtpAccountEdit: TEdit
            Left = 12
            Top = 39
            Width = 137
            Height = 21
            MaxLength = 100
            TabOrder = 0
            Text = 'FtpAccountEdit'
            OnChange = DataChange
          end
          object AnonymousLoginCheck: TCheckBox
            Left = 164
            Top = 41
            Width = 170
            Height = 17
            Caption = 'A&nonymous login'
            TabOrder = 1
            OnClick = AnonymousLoginCheckClick
          end
        end
        object BasicSshGroup: TGroupBox
          Left = 0
          Top = 266
          Width = 345
          Height = 75
          Caption = 'Additional'
          TabOrder = 2
          DesignSize = (
            345
            75)
          object PrivateKeyLabel: TLabel
            Left = 12
            Top = 22
            Width = 75
            Height = 13
            Caption = 'Private &key file:'
            FocusControl = PrivateKeyEdit
          end
          object PrivateKeyEdit: TFilenameEdit
            Left = 12
            Top = 39
            Width = 322
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PrivateKeyEditAfterDialog
            Filter = 'PuTTY Private Key Files (*.ppk)|*.ppk|All files (*.*)|*.*'
            DialogOptions = [ofReadOnly, ofPathMustExist, ofFileMustExist]
            DialogTitle = 'Select private key file'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'PrivateKeyEdit'
            OnChange = DataChange
          end
        end
        object ColorButton: TButton
          Left = 273
          Top = 347
          Width = 75
          Height = 25
          Caption = 'Select c&olor'
          TabOrder = 3
          OnClick = ColorButtonClick
        end
      end
      object SessionListSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_stored_sessions'
        Caption = 'Sites'
        TabVisible = False
        DesignSize = (
          353
          382)
        object LoadButton: TButton
          Left = 258
          Top = 35
          Width = 88
          Height = 25
          Action = EditSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 2
        end
        object DeleteButton: TButton
          Left = 258
          Top = 67
          Width = 88
          Height = 25
          Action = DeleteSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 3
        end
        object SessionTree: TTreeView
          Left = 2
          Top = 3
          Width = 247
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          DoubleBuffered = True
          DragMode = dmAutomatic
          HideSelection = False
          Indent = 19
          ParentDoubleBuffered = False
          ParentShowHint = False
          RowSelect = True
          ShowHint = True
          ShowLines = False
          ShowRoot = False
          SortType = stBoth
          StateImages = SessionImageList
          TabOrder = 0
          OnChange = SessionTreeChange
          OnCollapsed = SessionTreeExpandedCollapsed
          OnCompare = SessionTreeCompare
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
          OnMouseMove = SessionTreeMouseMove
          OnStartDrag = SessionTreeStartDrag
        end
        object NewButton: TButton
          Left = 258
          Top = 3
          Width = 88
          Height = 25
          Action = NewSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 1
        end
        object SetDefaultSessionButton: TButton
          Left = 258
          Top = 195
          Width = 88
          Height = 25
          Action = SetDefaultSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 7
        end
        object ToolsMenuButton: TButton
          Left = 258
          Top = 355
          Width = 88
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Tools...'
          TabOrder = 8
          OnClick = ToolsMenuButtonClick
        end
        object ShellIconsButton: TButton
          Left = 258
          Top = 163
          Width = 88
          Height = 25
          Action = ShellIconSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 6
        end
        object RenameButton: TButton
          Left = 258
          Top = 99
          Width = 88
          Height = 25
          Action = RenameSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 4
        end
        object NewFolderButton: TButton
          Left = 258
          Top = 131
          Width = 88
          Height = 25
          Action = NewSessionFolderAction
          Anchors = [akTop, akRight]
          TabOrder = 5
        end
        object SitesIncrementalSearchLabel: TStaticText
          Left = 258
          Top = 312
          Width = 142
          Height = 17
          BorderStyle = sbsSingle
          Caption = 'SitesIncrementalSearchLabel'
          ShowAccelChar = False
          TabOrder = 9
          Visible = False
        end
      end
      object LogSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_logging'
        Caption = 'Logging'
        ImageIndex = 4
        TabVisible = False
        inline LoggingFrame: TLoggingFrame
          Left = -3
          Top = 0
          Width = 356
          Height = 302
          TabOrder = 0
          DesignSize = (
            356
            302)
        end
      end
      object EnvironmentSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_login_environment'
        Caption = 'Environment'
        ImageIndex = 6
        TabVisible = False
        DesignSize = (
          353
          382)
        object EnvironmentGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 96
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Server environment'
          TabOrder = 0
          DesignSize = (
            345
            96)
          object EOLTypeLabel: TLabel
            Left = 12
            Top = 20
            Width = 241
            Height = 13
            Caption = '&End-of-line characters (if not indicated by server):'
            FocusControl = EOLTypeCombo
          end
          object UtfLabel: TLabel
            Left = 12
            Top = 44
            Width = 144
            Height = 13
            Caption = '&UTF-8 encoding for filenames:'
            FocusControl = UtfCombo
          end
          object TimeDifferenceLabel: TLabel
            Left = 12
            Top = 68
            Width = 81
            Height = 13
            Caption = 'Time&zone offset:'
            FocusControl = TimeDifferenceEdit
          end
          object TimeDifferenceHoursLabel: TLabel
            Left = 194
            Top = 68
            Width = 27
            Height = 13
            Caption = 'hours'
            FocusControl = TimeDifferenceEdit
          end
          object TimeDifferenceMinutesLabel: TLabel
            Left = 296
            Top = 66
            Width = 37
            Height = 13
            Caption = 'minutes'
            FocusControl = TimeDifferenceMinutesEdit
          end
          object EOLTypeCombo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Items.Strings = (
              'LF'
              'CR/LF')
          end
          object UtfCombo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object TimeDifferenceEdit: TUpDownEdit
            Left = 135
            Top = 63
            Width = 54
            Height = 21
            Alignment = taRightJustify
            MaxValue = 25.000000000000000000
            MinValue = -25.000000000000000000
            Value = -13.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnChange = DataChange
          end
          object TimeDifferenceMinutesEdit: TUpDownEdit
            Left = 237
            Top = 63
            Width = 54
            Height = 21
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 45.000000000000000000
            MinValue = -45.000000000000000000
            Value = -13.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
        end
        object DSTModeGroup: TGroupBox
          Left = 0
          Top = 110
          Width = 345
          Height = 93
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Daylight saving time'
          TabOrder = 1
          DesignSize = (
            345
            93)
          object DSTModeUnixCheck: TRadioButton
            Left = 12
            Top = 19
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Adjust remote timestamp to local co&nventions'
            TabOrder = 0
            OnClick = DataChange
          end
          object DSTModeWinCheck: TRadioButton
            Left = 12
            Top = 42
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Adjust remote timestamp with &DST'
            TabOrder = 1
            OnClick = DataChange
          end
          object DSTModeKeepCheck: TRadioButton
            Left = 12
            Top = 65
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Preser&ve remote timestamp'
            TabOrder = 2
            OnClick = DataChange
          end
        end
      end
      object DirectoriesSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_directories'
        Caption = 'Directories'
        ImageIndex = 11
        TabVisible = False
        DesignSize = (
          353
          382)
        object DirectoriesGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 183
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directories'
          TabOrder = 0
          DesignSize = (
            345
            183)
          object LocalDirectoryLabel: TLabel
            Left = 12
            Top = 111
            Width = 74
            Height = 13
            Caption = '&Local directory:'
            FocusControl = LocalDirectoryEdit
          end
          object RemoteDirectoryLabel: TLabel
            Left = 12
            Top = 66
            Width = 87
            Height = 13
            Caption = '&Remote directory:'
            FocusControl = RemoteDirectoryEdit
          end
          object LocalDirectoryDescLabel: TLabel
            Left = 12
            Top = 156
            Width = 241
            Height = 13
            Caption = 'Local directory is not used with Explorer interface.'
          end
          object LocalDirectoryEdit: TDirectoryEdit
            Left = 12
            Top = 128
            Width = 323
            Height = 21
            AcceptFiles = True
            DialogText = 'Select startup local directory.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            Text = 'LocalDirectoryEdit'
            OnChange = DataChange
          end
          object RemoteDirectoryEdit: TEdit
            Left = 12
            Top = 83
            Width = 323
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 2
            Text = 'RemoteDirectoryEdit'
            OnChange = DataChange
          end
          object UpdateDirectoriesCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Re&member last used directory'
            TabOrder = 1
          end
          object SynchronizeBrowsingCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Syn&chronize browsing'
            TabOrder = 0
          end
        end
        object DirectoryOptionsGroup: TGroupBox
          Left = 0
          Top = 198
          Width = 345
          Height = 93
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory reading options'
          TabOrder = 1
          DesignSize = (
            345
            93)
          object CacheDirectoriesCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &visited remote directories'
            TabOrder = 0
            OnClick = DataChange
          end
          object CacheDirectoryChangesCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 182
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &directory changes'
            TabOrder = 1
            OnClick = DataChange
          end
          object ResolveSymlinksCheck: TCheckBox
            Left = 12
            Top = 65
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Resolve symbolic li&nks'
            TabOrder = 3
          end
          object PreserveDirectoryChangesCheck: TCheckBox
            Left = 203
            Top = 42
            Width = 139
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Permanent cache'
            TabOrder = 2
          end
        end
      end
      object RecycleBinSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_recycle_bin'
        Caption = 'Recycle bin'
        ImageIndex = 15
        TabVisible = False
        DesignSize = (
          353
          382)
        object RecycleBinGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 114
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Recycle bin'
          TabOrder = 0
          DesignSize = (
            345
            114)
          object RecycleBinPathLabel: TLabel
            Left = 12
            Top = 64
            Width = 95
            Height = 13
            Caption = '&Remote recycle bin:'
            FocusControl = RecycleBinPathEdit
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 317
            Height = 17
            Caption = '&Preserve deleted remote files to recycle bin'
            TabOrder = 0
            OnClick = DataChange
          end
          object OverwrittenToRecycleBinCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 317
            Height = 17
            Caption = 'Preserve &overwritten remote files to recycle bin (SFTP only)'
            TabOrder = 1
            OnClick = DataChange
          end
          object RecycleBinPathEdit: TEdit
            Left = 12
            Top = 81
            Width = 322
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 2
            Text = 'RecycleBinPathEdit'
            OnChange = DataChange
          end
        end
      end
      object SftpSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_sftp'
        Caption = 'SFTP'
        ImageIndex = 12
        TabVisible = False
        DesignSize = (
          353
          382)
        object SFTPBugsGroupBox: TGroupBox
          Left = 0
          Top = 108
          Width = 345
          Height = 70
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SFTP servers'
          TabOrder = 1
          DesignSize = (
            345
            70)
          object Label10: TLabel
            Left = 12
            Top = 20
            Width = 230
            Height = 13
            Caption = '&Reverses order of symlink command arguments:'
            FocusControl = SFTPBugSymlinkCombo
          end
          object Label36: TLabel
            Left = 12
            Top = 44
            Width = 205
            Height = 13
            Caption = '&Misinterprets file timestamps prior to 1970:'
            FocusControl = SFTPBugSignedTSCombo
          end
          object SFTPBugSymlinkCombo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object SFTPBugSignedTSCombo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
        end
        object SFTPProtocolGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 96
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            345
            96)
          object Label34: TLabel
            Left = 12
            Top = 44
            Width = 157
            Height = 13
            Caption = '&Preferred SFTP protocol version:'
            FocusControl = SFTPMaxVersionCombo
          end
          object Label23: TLabel
            Left = 12
            Top = 20
            Width = 62
            Height = 13
            Caption = 'SFTP ser&ver:'
            FocusControl = SftpServerEdit
          end
          object SFTPMaxVersionCombo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Items.Strings = (
              '0'
              '1'
              '2'
              '3'
              '4'
              '5')
          end
          object SftpServerEdit: TComboBox
            Left = 184
            Top = 15
            Width = 149
            Height = 21
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 0
            Text = 'SftpServerEdit'
            Items.Strings = (
              'Default'
              '/bin/sftp-server'
              'sudo su -c /bin/sftp-server')
          end
          object AllowScpFallbackCheck: TCheckBox
            Left = 12
            Top = 68
            Width = 121
            Height = 17
            Caption = 'Allow SCP &fallback'
            TabOrder = 2
            OnClick = DataChange
          end
        end
      end
      object ScpSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_scp'
        Caption = 'SCP/Shell'
        ImageIndex = 3
        TabVisible = False
        DesignSize = (
          353
          382)
        object OtherShellOptionsGroup: TGroupBox
          Left = 0
          Top = 161
          Width = 345
          Height = 69
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other options'
          TabOrder = 2
          object LookupUserGroupsCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 140
            Height = 17
            AllowGrayed = True
            Caption = 'Lookup &user groups'
            TabOrder = 0
            OnClick = DataChange
          end
          object ClearAliasesCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 140
            Height = 17
            Caption = 'Clear a&liases'
            TabOrder = 2
            OnClick = DataChange
          end
          object UnsetNationalVarsCheck: TCheckBox
            Left = 152
            Top = 19
            Width = 185
            Height = 17
            Caption = 'Clear &national variables'
            TabOrder = 1
            OnClick = DataChange
          end
          object Scp1CompatibilityCheck: TCheckBox
            Left = 152
            Top = 42
            Width = 185
            Height = 17
            Caption = 'Use scp&2 with scp1 compatibility'
            TabOrder = 3
            OnClick = DataChange
          end
        end
        object ShellGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 70
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shell'
          TabOrder = 0
          DesignSize = (
            345
            70)
          object Label19: TLabel
            Left = 12
            Top = 20
            Width = 26
            Height = 13
            Caption = 'S&hell:'
            FocusControl = ShellEdit
          end
          object Label20: TLabel
            Left = 12
            Top = 44
            Width = 104
            Height = 13
            Caption = '&Return code variable:'
            FocusControl = ReturnVarEdit
          end
          object ShellEdit: TComboBox
            Left = 152
            Top = 15
            Width = 181
            Height = 21
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 0
            Text = 'ShellEdit'
            Items.Strings = (
              'Default'
              '/bin/bash'
              '/bin/ksh'
              'sudo su -')
          end
          object ReturnVarEdit: TComboBox
            Left = 152
            Top = 39
            Width = 181
            Height = 21
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 1
            Text = 'ReturnVarEdit'
            Items.Strings = (
              'Autodetect'
              '?'
              'status')
          end
        end
        object ScpLsOptionsGroup: TGroupBox
          Left = 0
          Top = 84
          Width = 345
          Height = 69
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory listing'
          TabOrder = 1
          DesignSize = (
            345
            69)
          object Label9: TLabel
            Left = 12
            Top = 20
            Width = 82
            Height = 13
            Caption = 'Listing &command:'
            FocusControl = ListingCommandEdit
          end
          object IgnoreLsWarningsCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 140
            Height = 17
            Caption = 'Ignore LS &warnings'
            TabOrder = 1
            OnClick = DataChange
          end
          object SCPLsFullTimeAutoCheck: TCheckBox
            Left = 152
            Top = 42
            Width = 185
            Height = 17
            Caption = 'Try to get &full timestamp'
            TabOrder = 2
            OnClick = DataChange
          end
          object ListingCommandEdit: TComboBox
            Left = 152
            Top = 15
            Width = 181
            Height = 21
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 0
            Text = 'ListingCommandEdit'
            Items.Strings = (
              'ls -la'
              'ls -gla')
          end
        end
      end
      object FtpSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_ftp'
        Caption = 'FTP'
        ImageIndex = 16
        TabVisible = False
        DesignSize = (
          353
          382)
        object FtpGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 178
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            345
            178)
          object Label25: TLabel
            Left = 12
            Top = 20
            Width = 103
            Height = 13
            Caption = 'Post login &commands:'
            FocusControl = PostLoginCommandsMemo
          end
          object Label5: TLabel
            Left = 12
            Top = 102
            Width = 159
            Height = 13
            Caption = '&Support for listing of hidden files:'
            FocusControl = FtpListAllCombo
          end
          object Label24: TLabel
            Left = 12
            Top = 126
            Width = 188
            Height = 13
            Caption = 'Use &MLSD command for directory listing'
            FocusControl = FtpUseMlsdCombo
          end
          object FtpForcePasvIpLabel: TLabel
            Left = 12
            Top = 150
            Width = 226
            Height = 13
            Caption = '&Force IP address for passive mode connections'
            FocusControl = FtpForcePasvIpCombo
          end
          object PostLoginCommandsMemo: TMemo
            Left = 12
            Top = 37
            Width = 321
            Height = 53
            Anchors = [akLeft, akTop, akRight]
            ScrollBars = ssVertical
            TabOrder = 0
          end
          object FtpListAllCombo: TComboBox
            Left = 272
            Top = 97
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
          end
          object FtpForcePasvIpCombo: TComboBox
            Left = 272
            Top = 145
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
          end
          object FtpUseMlsdCombo: TComboBox
            Left = 272
            Top = 121
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
        end
      end
      object ConnSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_login_connection'
        Caption = 'Connection'
        ImageIndex = 7
        TabVisible = False
        DesignSize = (
          353
          382)
        object FtpPingGroup: TGroupBox
          Left = 0
          Top = 132
          Width = 345
          Height = 117
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 4
          DesignSize = (
            345
            117)
          object FtpPingIntervalLabel: TLabel
            Left = 12
            Top = 90
            Width = 142
            Height = 13
            Caption = 'Seconds &between keepalives:'
            FocusControl = FtpPingIntervalSecEdit
          end
          object FtpPingIntervalSecEdit: TUpDownEdit
            Left = 208
            Top = 85
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 3600.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            MaxLength = 4
            TabOrder = 3
            OnChange = DataChange
          end
          object FtpPingOffButton: TRadioButton
            Left = 12
            Top = 19
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Off'
            TabOrder = 0
            OnClick = DataChange
          end
          object FtpPingNullPacketButton: TRadioButton
            Left = 12
            Top = 42
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sending of &null SSH packets'
            Enabled = False
            TabOrder = 1
            OnClick = DataChange
          end
          object FtpPingDummyCommandButton: TRadioButton
            Left = 12
            Top = 65
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Executing &dummy protocol commands'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object TimeoutGroup: TGroupBox
          Left = 0
          Top = 80
          Width = 345
          Height = 46
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Timeouts'
          TabOrder = 1
          object Label11: TLabel
            Left = 12
            Top = 19
            Width = 122
            Height = 13
            Caption = 'Server &response timeout:'
            FocusControl = TimeoutEdit
          end
          object Label12: TLabel
            Left = 286
            Top = 19
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = TimeoutEdit
          end
          object TimeoutEdit: TUpDownEdit
            Left = 208
            Top = 14
            Width = 73
            Height = 21
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 6000.000000000000000000
            MinValue = 5.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 4
            TabOrder = 0
            OnChange = DataChange
          end
        end
        object PingGroup: TGroupBox
          Left = 0
          Top = 132
          Width = 345
          Height = 117
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 3
          DesignSize = (
            345
            117)
          object PingIntervalLabel: TLabel
            Left = 12
            Top = 90
            Width = 142
            Height = 13
            Caption = 'Seconds &between keepalives:'
            FocusControl = PingIntervalSecEdit
          end
          object PingIntervalSecEdit: TUpDownEdit
            Left = 208
            Top = 85
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 3600.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            MaxLength = 4
            TabOrder = 3
            OnChange = DataChange
          end
          object PingOffButton: TRadioButton
            Left = 12
            Top = 19
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Off'
            TabOrder = 0
            OnClick = DataChange
          end
          object PingNullPacketButton: TRadioButton
            Left = 12
            Top = 42
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sending of &null SSH packets'
            TabOrder = 1
            OnClick = DataChange
          end
          object PingDummyCommandButton: TRadioButton
            Left = 12
            Top = 65
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Executing &dummy protocol commands'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object IPvGroup: TGroupBox
          Left = 0
          Top = 255
          Width = 345
          Height = 46
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Internet protocol version'
          TabOrder = 2
          DesignSize = (
            345
            46)
          object IPAutoButton: TRadioButton
            Left = 12
            Top = 19
            Width = 101
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'A&uto'
            TabOrder = 0
            OnClick = DataChange
          end
          object IPv4Button: TRadioButton
            Left = 124
            Top = 19
            Width = 101
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'IPv&4'
            TabOrder = 1
            OnClick = DataChange
          end
          object IPv6Button: TRadioButton
            Left = 236
            Top = 19
            Width = 101
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'IPv&6'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object ConnectionGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 69
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 0
          DesignSize = (
            345
            69)
          object FtpPasvModeCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Passive mode'
            TabOrder = 0
            OnClick = DataChange
          end
          object BufferSizeCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Optimize connection &buffer size'
            TabOrder = 1
            OnClick = DataChange
          end
        end
      end
      object ProxySheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_proxy'
        Caption = 'Proxy'
        ImageIndex = 8
        TabVisible = False
        DesignSize = (
          353
          382)
        object ProxyTypeGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 136
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy'
          TabOrder = 0
          DesignSize = (
            345
            136)
          object ProxyMethodLabel: TLabel
            Left = 12
            Top = 20
            Width = 57
            Height = 13
            Caption = 'Proxy &type:'
            FocusControl = SshProxyMethodCombo
          end
          object ProxyHostLabel: TLabel
            Left = 12
            Top = 41
            Width = 85
            Height = 13
            Caption = 'Pro&xy host name:'
            FocusControl = ProxyHostEdit
          end
          object ProxyPortLabel: TLabel
            Left = 252
            Top = 41
            Width = 63
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = ProxyPortEdit
          end
          object ProxyUsernameLabel: TLabel
            Left = 12
            Top = 85
            Width = 55
            Height = 13
            Caption = '&User name:'
            FocusControl = ProxyUsernameEdit
          end
          object ProxyPasswordLabel: TLabel
            Left = 163
            Top = 85
            Width = 50
            Height = 13
            Caption = '&Password:'
            FocusControl = ProxyPasswordEdit
          end
          object SshProxyMethodCombo: TComboBox
            Left = 128
            Top = 15
            Width = 110
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = DataChange
            Items.Strings = (
              'None'
              'SOCKS4'
              'SOCKS5'
              'HTTP'
              'Telnet'
              'Local')
          end
          object ProxyPortEdit: TUpDownEdit
            Left = 252
            Top = 58
            Width = 82
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = DataChange
          end
          object ProxyHostEdit: TEdit
            Left = 12
            Top = 58
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 3
            Text = 'ProxyHostEdit'
            OnChange = DataChange
          end
          object ProxyUsernameEdit: TEdit
            Left = 12
            Top = 102
            Width = 137
            Height = 21
            MaxLength = 50
            TabOrder = 5
            Text = 'ProxyUsernameEdit'
            OnChange = DataChange
          end
          object ProxyPasswordEdit: TPasswordEdit
            Left = 163
            Top = 102
            Width = 171
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 6
            Text = 'ProxyPasswordEdit'
            OnChange = DataChange
          end
          object FtpProxyMethodCombo: TComboBox
            Left = 128
            Top = 15
            Width = 206
            Height = 21
            Style = csDropDownList
            DropDownCount = 12
            TabOrder = 1
            OnChange = DataChange
            Items.Strings = (
              'None'
              'SOCKS4'
              'SOCKS5'
              'HTTP'
              'SITE %host'
              'USER %proxyuser, USER %user@%host'
              'OPEN %host'
              'USER %proxyuser, USER %user'
              'USER %user@%host'
              'USER %proxyuser@%host'
              'USER %user@%host %proxyuser'
              'USER %user@%proxyuser@%host')
          end
          object WebDavProxyMethodCombo: TComboBox
            Left = 128
            Top = 15
            Width = 110
            Height = 21
            Style = csDropDownList
            TabOrder = 2
            OnChange = DataChange
            Items.Strings = (
              'None'
              'SOCKS4'
              'SOCKS5'
              'HTTP')
          end
        end
        object ProxySettingsGroup: TGroupBox
          Left = 0
          Top = 147
          Width = 345
          Height = 128
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy settings'
          TabOrder = 1
          DesignSize = (
            345
            128)
          object ProxyTelnetCommandLabel: TLabel
            Left = 12
            Top = 18
            Width = 82
            Height = 13
            Caption = 'Telnet co&mmand:'
            FocusControl = ProxyTelnetCommandEdit
          end
          object Label17: TLabel
            Left = 12
            Top = 99
            Width = 168
            Height = 13
            Caption = 'Do &DNS name lookup at proxy end:'
          end
          object ProxyLocalCommandLabel: TLabel
            Left = 12
            Top = 18
            Width = 107
            Height = 13
            Caption = 'Local proxy co&mmand:'
            FocusControl = ProxyLocalCommandEdit
          end
          object ProxyTelnetCommandEdit: TEdit
            Left = 12
            Top = 35
            Width = 322
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 0
            Text = 'ProxyTelnetCommandEdit'
            OnChange = DataChange
          end
          object ProxyLocalhostCheck: TCheckBox
            Left = 12
            Top = 77
            Width = 313
            Height = 17
            Caption = 'Co&nsider proxying local host connections'
            TabOrder = 5
          end
          object ProxyDNSCombo: TComboBox
            Left = 252
            Top = 94
            Width = 82
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 6
            Items.Strings = (
              'Auto'
              'No'
              'Yes')
          end
          object ProxyLocalCommandEdit: TEdit
            Left = 12
            Top = 35
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'ProxyLocalCommandEdit'
            OnChange = DataChange
          end
          object ProxyLocalCommandBrowseButton: TButton
            Left = 252
            Top = 33
            Width = 82
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Browse...'
            TabOrder = 3
            OnClick = ProxyLocalCommandBrowseButtonClick
          end
          object ProxyTelnetCommandHintText: TStaticText
            Left = 255
            Top = 58
            Width = 79
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'patterns'
            TabOrder = 1
            TabStop = True
          end
          object ProxyLocalCommandHintText: TStaticText
            Left = 159
            Top = 58
            Width = 79
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'patterns'
            TabOrder = 4
            TabStop = True
          end
        end
      end
      object TunnelSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_tunnel'
        Caption = 'Tunnel'
        ImageIndex = 14
        TabVisible = False
        DesignSize = (
          353
          382)
        object TunnelSessionGroup: TGroupBox
          Left = 0
          Top = 32
          Width = 345
          Height = 169
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Host to setup tunnel on'
          TabOrder = 1
          DesignSize = (
            345
            169)
          object Label6: TLabel
            Left = 12
            Top = 18
            Width = 55
            Height = 13
            Caption = '&Host name:'
            FocusControl = TunnelHostNameEdit
          end
          object Label14: TLabel
            Left = 252
            Top = 18
            Width = 63
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = TunnelPortNumberEdit
          end
          object Label15: TLabel
            Left = 12
            Top = 68
            Width = 55
            Height = 13
            Caption = '&User name:'
            FocusControl = TunnelUserNameEdit
          end
          object Label16: TLabel
            Left = 163
            Top = 68
            Width = 50
            Height = 13
            Caption = '&Password:'
            FocusControl = TunnelPasswordEdit
          end
          object Label18: TLabel
            Left = 12
            Top = 118
            Width = 75
            Height = 13
            Caption = 'Private &key file:'
            FocusControl = TunnelPrivateKeyEdit
          end
          object TunnelHostNameEdit: TEdit
            Left = 12
            Top = 35
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 0
            Text = 'TunnelHostNameEdit'
            OnChange = DataChange
          end
          object TunnelUserNameEdit: TEdit
            Left = 12
            Top = 85
            Width = 137
            Height = 21
            MaxLength = 50
            TabOrder = 1
            Text = 'TunnelUserNameEdit'
            OnChange = DataChange
          end
          object TunnelPasswordEdit: TPasswordEdit
            Left = 163
            Top = 85
            Width = 171
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 2
            Text = 'TunnelPasswordEdit'
            OnChange = DataChange
          end
          object TunnelPortNumberEdit: TUpDownEdit
            Left = 252
            Top = 35
            Width = 82
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object TunnelPrivateKeyEdit: TFilenameEdit
            Left = 12
            Top = 135
            Width = 322
            Height = 21
            AcceptFiles = True
            OnAfterDialog = PrivateKeyEditAfterDialog
            Filter = 'PuTTY Private Key Files (*.ppk)|*.ppk|All files (*.*)|*.*'
            DialogOptions = [ofReadOnly, ofPathMustExist, ofFileMustExist]
            DialogTitle = 'Select private key file'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
            Text = 'TunnelPrivateKeyEdit'
            OnChange = DataChange
          end
        end
        object TunnelCheck: TCheckBox
          Left = 11
          Top = 8
          Width = 307
          Height = 17
          Caption = '&Connect through SSH tunnel'
          TabOrder = 0
          OnClick = DataChange
        end
        object TunnelOptionsGroup: TGroupBox
          Left = 0
          Top = 205
          Width = 345
          Height = 47
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Tunnel options'
          TabOrder = 2
          DesignSize = (
            345
            47)
          object Label21: TLabel
            Left = 12
            Top = 20
            Width = 84
            Height = 13
            Caption = '&Local tunnel port:'
            FocusControl = TunnelLocalPortNumberEdit
          end
          object TunnelLocalPortNumberEdit: TComboBox
            Left = 252
            Top = 15
            Width = 82
            Height = 21
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 0
            Text = 'TunnelLocalPortNumberEdit'
            OnChange = DataChange
            Items.Strings = (
              'Autoselect')
          end
        end
      end
      object AdvancedSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_login_ssh'
        Caption = 'SSH'
        ImageIndex = 2
        TabVisible = False
        DesignSize = (
          353
          382)
        object ProtocolGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 87
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            345
            87)
          object Label7: TLabel
            Left = 12
            Top = 42
            Width = 152
            Height = 13
            Caption = 'Preferred SSH protocol version:'
            FocusControl = SshProt1onlyButton
          end
          object SshProt1Button: TRadioButton
            Left = 88
            Top = 59
            Width = 65
            Height = 17
            Caption = '&1'
            TabOrder = 2
            OnClick = DataChange
          end
          object SshProt2Button: TRadioButton
            Left = 160
            Top = 59
            Width = 65
            Height = 17
            Caption = '&2'
            TabOrder = 3
            OnClick = DataChange
          end
          object CompressionCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 324
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable &compression'
            TabOrder = 0
            OnClick = DataChange
          end
          object SshProt1onlyButton: TRadioButton
            Left = 16
            Top = 59
            Width = 65
            Height = 17
            Caption = '1 on&ly'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = DataChange
          end
          object SshProt2onlyButton: TRadioButton
            Left = 232
            Top = 59
            Width = 65
            Height = 17
            Caption = '2 o&nly'
            TabOrder = 4
            OnClick = DataChange
          end
        end
        object EncryptionGroup: TGroupBox
          Left = 0
          Top = 100
          Width = 345
          Height = 163
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Encryption options'
          TabOrder = 1
          DesignSize = (
            345
            163)
          object Label8: TLabel
            Left = 12
            Top = 19
            Width = 162
            Height = 13
            Caption = 'Encryption cipher selection &policy:'
            FocusControl = CipherListBox
          end
          object CipherListBox: TListBox
            Left = 12
            Top = 36
            Width = 190
            Height = 91
            DragMode = dmAutomatic
            ItemHeight = 13
            TabOrder = 0
            OnClick = DataChange
            OnDragDrop = AlgListBoxDragDrop
            OnDragOver = AlgListBoxDragOver
            OnStartDrag = AlgListBoxStartDrag
          end
          object Ssh2LegacyDESCheck: TCheckBox
            Left = 16
            Top = 134
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable legacy use of single-&DES in SSH-2'
            TabOrder = 3
          end
          object CipherUpButton: TButton
            Left = 211
            Top = 36
            Width = 70
            Height = 25
            Caption = '&Up'
            TabOrder = 1
            OnClick = CipherButtonClick
          end
          object CipherDownButton: TButton
            Left = 211
            Top = 68
            Width = 70
            Height = 25
            Caption = '&Down'
            TabOrder = 2
            OnClick = CipherButtonClick
          end
        end
      end
      object KexSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_kex'
        Caption = 'Key exchange'
        ImageIndex = 13
        TabVisible = False
        DesignSize = (
          353
          382)
        object KexOptionsGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 137
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Key exchange algorithm options'
          TabOrder = 0
          object Label28: TLabel
            Left = 12
            Top = 19
            Width = 124
            Height = 13
            Caption = 'Algorithm selection &policy:'
            FocusControl = KexListBox
          end
          object KexListBox: TListBox
            Left = 12
            Top = 36
            Width = 190
            Height = 89
            DragMode = dmAutomatic
            ItemHeight = 13
            TabOrder = 0
            OnClick = DataChange
            OnDragDrop = AlgListBoxDragDrop
            OnDragOver = AlgListBoxDragOver
            OnStartDrag = AlgListBoxStartDrag
          end
          object KexUpButton: TButton
            Left = 211
            Top = 36
            Width = 70
            Height = 25
            Caption = '&Up'
            TabOrder = 1
            OnClick = KexButtonClick
          end
          object KexDownButton: TButton
            Left = 211
            Top = 68
            Width = 70
            Height = 25
            Caption = '&Down'
            TabOrder = 2
            OnClick = KexButtonClick
          end
        end
        object KexReexchangeGroup: TGroupBox
          Left = 0
          Top = 150
          Width = 345
          Height = 69
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options controlling key re-exchange'
          TabOrder = 1
          object Label31: TLabel
            Left = 12
            Top = 20
            Width = 199
            Height = 13
            Caption = 'Max &minutes before rekey (0 for no limit):'
            Color = clBtnFace
            FocusControl = RekeyTimeEdit
            ParentColor = False
          end
          object Label32: TLabel
            Left = 12
            Top = 44
            Width = 184
            Height = 13
            Caption = 'Ma&x data before rekey (0 for no limit):'
            Color = clBtnFace
            FocusControl = RekeyDataEdit
            ParentColor = False
          end
          object RekeyTimeEdit: TUpDownEdit
            Left = 256
            Top = 15
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 1440.000000000000000000
            MaxLength = 4
            TabOrder = 0
            OnChange = DataChange
          end
          object RekeyDataEdit: TEdit
            Left = 256
            Top = 39
            Width = 73
            Height = 21
            MaxLength = 10
            TabOrder = 1
            OnChange = DataChange
          end
        end
      end
      object AuthSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_authentication'
        Caption = 'Authentication'
        ImageIndex = 10
        TabVisible = False
        DesignSize = (
          353
          382)
        object SshNoUserAuthCheck: TCheckBox
          Left = 11
          Top = 8
          Width = 307
          Height = 17
          Caption = '&Bypass authentication entirely (SSH-2)'
          TabOrder = 0
          OnClick = DataChange
        end
        object AuthenticationGroup: TGroupBox
          Left = 0
          Top = 32
          Width = 345
          Height = 117
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication options'
          TabOrder = 1
          DesignSize = (
            345
            117)
          object TryAgentCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt authentication using &Pageant'
            TabOrder = 0
            OnClick = DataChange
          end
          object AuthTISCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Atte&mpt TIS or CryptoCard authentication (SSH-1)'
            TabOrder = 1
            OnClick = DataChange
          end
          object AuthKICheck: TCheckBox
            Left = 12
            Top = 65
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt '#39'keyboard-&interactive'#39' authentication (SSH-2)'
            TabOrder = 2
            OnClick = DataChange
          end
          object AuthKIPasswordCheck: TCheckBox
            Left = 32
            Top = 88
            Width = 305
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Respond with pass&word to the first prompt'
            TabOrder = 3
            OnClick = DataChange
          end
        end
        object AuthenticationParamsGroup: TGroupBox
          Left = 0
          Top = 154
          Width = 345
          Height = 48
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication parameters'
          TabOrder = 2
          DesignSize = (
            345
            48)
          object AgentFwdCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow agent &forwarding'
            TabOrder = 0
            OnClick = DataChange
          end
        end
        object GSSAPIGroup: TGroupBox
          Left = 0
          Top = 208
          Width = 345
          Height = 71
          Anchors = [akLeft, akTop, akRight]
          Caption = 'GSSAPI'
          TabOrder = 3
          DesignSize = (
            345
            71)
          object AuthGSSAPICheck3: TCheckBox
            Left = 12
            Top = 19
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt &GSSAPI authentication (SSH-2)'
            TabOrder = 0
            OnClick = AuthGSSAPICheck3Click
          end
          object GSSAPIFwdTGTCheck: TCheckBox
            Left = 12
            Top = 42
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow GSSAPI &credential delegation'
            TabOrder = 1
            OnClick = AuthGSSAPICheck3Click
          end
        end
      end
      object BugsSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_bugs'
        Caption = 'Bugs'
        ImageIndex = 9
        TabVisible = False
        DesignSize = (
          353
          382)
        object BugsGroupBox: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 265
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SSH servers'
          TabOrder = 0
          DesignSize = (
            345
            265)
          object BugIgnore1Label: TLabel
            Left = 12
            Top = 20
            Width = 169
            Height = 13
            Caption = 'Chokes on SSH-1 &ignore messages:'
            FocusControl = BugIgnore1Combo
          end
          object BugPlainPW1Label: TLabel
            Left = 12
            Top = 44
            Width = 195
            Height = 13
            Caption = 'Refuses all SSH-1 pass&word camouflage:'
            FocusControl = BugPlainPW1Combo
          end
          object BugRSA1Label: TLabel
            Left = 12
            Top = 68
            Width = 181
            Height = 13
            Caption = 'Chokes on SSH-1 &RSA authentication:'
            FocusControl = BugRSA1Combo
          end
          object BugHMAC2Label: TLabel
            Left = 12
            Top = 116
            Width = 154
            Height = 13
            Caption = 'Miscomputes SSH-2 H&MAC keys:'
            FocusControl = BugHMAC2Combo
          end
          object BugDeriveKey2Label: TLabel
            Left = 12
            Top = 140
            Width = 176
            Height = 13
            Caption = 'Miscomputes SSH-2 &encryption keys:'
            FocusControl = BugDeriveKey2Combo
          end
          object BugRSAPad2Label: TLabel
            Left = 12
            Top = 164
            Width = 210
            Height = 13
            Caption = 'Requires &padding on SSH-2 RSA signatures:'
            FocusControl = BugRSAPad2Combo
          end
          object BugPKSessID2Label: TLabel
            Left = 12
            Top = 188
            Width = 195
            Height = 13
            Caption = 'Misuses the sessio&n ID in SSH-2 PK auth:'
            FocusControl = BugPKSessID2Combo
          end
          object BugRekey2Label: TLabel
            Left = 12
            Top = 212
            Width = 187
            Height = 13
            Caption = 'Handles SSH-2 &key re-exchange badly:'
            FocusControl = BugRekey2Combo
          end
          object BugMaxPkt2Label: TLabel
            Left = 12
            Top = 236
            Width = 176
            Height = 13
            Caption = 'Ignores SSH-2 ma&ximum packet size:'
            FocusControl = BugMaxPkt2Combo
          end
          object BugIgnore2Label: TLabel
            Left = 12
            Top = 92
            Width = 169
            Height = 13
            Caption = 'Chokes on SSH-&2 ignore messages:'
            FocusControl = BugIgnore2Combo
          end
          object BugIgnore1Combo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = DataChange
          end
          object BugPlainPW1Combo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            OnChange = DataChange
          end
          object BugRSA1Combo: TComboBox
            Left = 272
            Top = 63
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            OnChange = DataChange
          end
          object BugHMAC2Combo: TComboBox
            Left = 272
            Top = 111
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
            OnChange = DataChange
          end
          object BugDeriveKey2Combo: TComboBox
            Left = 272
            Top = 135
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 5
            OnChange = DataChange
          end
          object BugRSAPad2Combo: TComboBox
            Left = 272
            Top = 159
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 6
            OnChange = DataChange
          end
          object BugPKSessID2Combo: TComboBox
            Left = 272
            Top = 183
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 7
            OnChange = DataChange
          end
          object BugRekey2Combo: TComboBox
            Left = 272
            Top = 207
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 8
            OnChange = DataChange
          end
          object BugMaxPkt2Combo: TComboBox
            Left = 272
            Top = 231
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 9
            OnChange = DataChange
          end
          object BugIgnore2Combo: TComboBox
            Left = 272
            Top = 87
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
        end
      end
      object GeneralSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_login_preferences'
        Caption = 'Preferences'
        ImageIndex = 5
        TabVisible = False
        object Label13: TLabel
          Left = 16
          Top = 224
          Width = 109
          Height = 13
          Caption = 'Other general options:'
        end
        object PreferencesButton: TButton
          Left = 184
          Top = 218
          Width = 90
          Height = 25
          Caption = '&Preferences...'
          TabOrder = 1
          OnClick = PreferencesButtonClick
        end
        inline GeneralSettingsFrame: TGeneralSettingsFrame
          Left = 0
          Top = 6
          Width = 345
          Height = 202
          TabOrder = 0
          DesignSize = (
            345
            202)
          inherited InterfaceGroup: TGroupBox
            Width = 345
            DesignSize = (
              345
              202)
            inherited CommanderDescriptionLabel2: TLabel
              Width = 206
            end
            inherited ExplorerDescriptionLabel: TLabel
              Width = 208
            end
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 152
      Height = 392
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        152
        392)
      object NavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 136
        Height = 356
        Anchors = [akLeft, akTop, akRight, akBottom]
        DoubleBuffered = True
        HideSelection = False
        HotTrack = True
        Indent = 19
        ParentDoubleBuffered = False
        ReadOnly = True
        ShowButtons = False
        ShowRoot = False
        TabOrder = 0
        OnChange = NavigationTreeChange
        OnCollapsing = NavigationTreeCollapsing
        Items.NodeData = {
          03050000002E000000000000000000000000000000FFFFFFFF00000000000000
          00020000000108530065007300730069006F006E0058003E0000000000000000
          00000000000000FFFFFFFF0000000000000000000000000110530074006F0072
          00650064002000730065007300730069006F006E00730058002E000000000000
          000000000000000000FFFFFFFF00000000000000000000000001084C006F0067
          00670069006E006700580036000000000000000000000000000000FFFFFFFF00
          0000000000000005000000010C45006E007600690072006F006E006D0065006E
          007400580036000000000000000000000000000000FFFFFFFF00000000000000
          0000000000010C4400690072006500630074006F007200690065007300580036
          000000000000000000000000000000FFFFFFFF00000000000000000000000001
          0C520065006300790063006C0065002000620069006E00580028000000000000
          000000000000000000FFFFFFFF00000000000000000000000001055300460054
          005000580026000000000000000000000000000000FFFFFFFF00000000000000
          000000000001045300430050005800260000000000000000000000FFFFFFFFFF
          FFFFFF0000000000000000000000000104460054005000580034000000000000
          000000000000000000FFFFFFFF000000000000000002000000010B43006F006E
          006E0065006300740069006F006E0058002A0000000000000000000000000000
          00FFFFFFFF0000000000000000000000000106500072006F007800790058002C
          000000000000000000000000000000FFFFFFFF00000000000000000000000001
          07540075006E006E0065006C00580026000000000000000000000000000000FF
          FFFFFF0000000000000000030000000104530053004800580038000000000000
          000000000000000000FFFFFFFF000000000000000000000000010D4B00650078
          002000650078006300680061006E006700650058003C00000000000000000000
          0000000000FFFFFFFF000000000000000000000000010F410075007400680065
          006E007400690063006100740069006F006E0058002800000000000000000000
          0000000000FFFFFFFF0000000000000000000000000105420075006700730058
          0036000000000000000000000000000000FFFFFFFF0000000000000000000000
          00010C50007200650066006500720065006E006300650073005800}
      end
      object ShowAdvancedLoginOptionsCheck: TCheckBox
        Left = 16
        Top = 372
        Width = 120
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Advanced options'
        TabOrder = 1
        OnClick = DataChange
      end
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 12
    Top = 289
    object EditSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Edit'
      OnExecute = EditSessionActionExecute
    end
    object SaveSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Save...'
      OnExecute = SaveSessionActionExecute
    end
    object DeleteSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Delete'
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
    object NewSessionAction: TAction
      Category = 'Sessions'
      Caption = '&New'
      OnExecute = NewSessionActionExecute
    end
    object SetDefaultSessionAction: TAction
      Category = 'Sessions'
      Caption = 'Set de&faults'
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
      OnExecute = RenameSessionActionExecute
    end
    object ShellIconSessionAction: TAction
      Category = 'Sessions'
      Caption = 'Shell &icon...'
      OnExecute = ShellIconSessionActionExecute
    end
    object NewSessionFolderAction: TAction
      Category = 'Sessions'
      Caption = 'Ne&w folder...'
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
  end
  object ToolsPopupMenu: TPopupMenu
    Left = 48
    Top = 289
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
  end
  object IconsPopupMenu: TPopupMenu
    Left = 80
    Top = 289
    object Desktopicon1: TMenuItem
      Action = DesktopIconAction
    end
    object ExplorersSendtoshortcut1: TMenuItem
      Action = SendToHookAction
    end
  end
  object ColorPopupMenu: TPopupMenu
    Images = ColorImageList
    Left = 112
    Top = 289
    object ColorDefaultItem: TMenuItem
      Caption = '&Default'
      OnClick = ColorDefaultItemClick
    end
    object PickColorItem: TMenuItem
      Caption = '&Pick Color...'
      ImageIndex = 0
      OnClick = PickColorItemClick
    end
  end
  object ColorImageList: TImageList
    BkColor = clRed
    AllocBy = 1
    Left = 12
    Top = 321
    Bitmap = {
      494C010101000400440010001000FF000000FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
  object SessionImageList: TPngImageList
    PngImages = <
      item
        Background = clWindow
        Name = 'session opened'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF000000FF89C02F90000000174944415478DA63FCCF
          F09F8114C038AA6154C3F0D50000BC451FF12F5559220000000049454E44AE42
          6082}
      end
      item
        Background = clWindow
        Name = 'session closed'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF000000FF89C02F90000000174944415478DA63FCCF
          F09F8114C038AA6154C3F0D50000BC451FF12F5559220000000049454E44AE42
          6082}
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
          D0A7FB93199393FF040398F3FC63332DDB000000E84944415478DA63FCFFFF3F
          032580912A06F09A17DE01B295C9D07F1766C07F8BE07292759F58DB0977C1FF
          B5F31A48362038A901D580798B563124C585C16918C0258E62C0F3433D586DA9
          5F7898A131DE16AB9CA45D09C28047FBBB19361DBB0596F0B35203B34174CB92
          230C35313658E5E41C4B1106DCDBD385D5968EE547192A22ADB1CA29B994210C
          B8B9B39361C7A93B60090F3315301B44F7AE3AC6501C6685554EDDBD1C61C095
          6D1D586D99B4F638435EB02556391DAF0A8401E737B76355347DC30986CC000B
          AC7286BE950803D292FD494E07B3E66E241C88F8007220529617280100EE449E
          BF3EDFD5E80000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'Workspace closed'
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100802000000909168
          360000000674524E5300FF000000FF89C02F90000000174944415478DA63FCCF
          F09F8114C038AA6154C3F0D50000BC451FF12F5559220000000049454E44AE42
          6082}
      end>
    Left = 48
    Top = 321
    Bitmap = {}
  end
  object SaveDropDownMenu: TPopupMenu
    Left = 80
    Top = 321
    object Save1: TMenuItem
      Action = SaveSessionAction
      Default = True
    end
    object Setdefaults1: TMenuItem
      Action = SetDefaultSessionAction
    end
  end
end
