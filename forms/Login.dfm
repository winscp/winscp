object LoginDialog: TLoginDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 336
  ClientWidth = 513
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    513
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object HelpButton: TButton
    Left = 427
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    Visible = False
    OnClick = HelpButtonClick
  end
  object SaveButton: TButton
    Left = 339
    Top = 305
    Width = 75
    Height = 25
    HelpKeyword = 'ui_login_save'
    Action = SaveSessionAction
    Anchors = [akRight, akBottom]
    TabOrder = 3
  end
  object LoginButton: TButton
    Left = 251
    Top = 305
    Width = 75
    Height = 25
    Action = LoginAction
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CloseButton: TButton
    Left = 427
    Top = 305
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
  end
  object AboutButton: TButton
    Left = 11
    Top = 305
    Width = 82
    Height = 25
    Action = AboutAction
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    TabStop = False
  end
  object LanguagesButton: TButton
    Left = 105
    Top = 305
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
    Height = 296
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 6
    object PageControl: TPageControl
      Tag = 1
      Left = 152
      Top = 0
      Width = 361
      Height = 296
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
        Hint = 'Session'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_session'
        Caption = 'Basic'
        ImageIndex = 1
        TabVisible = False
        DesignSize = (
          353
          286)
        object BasicGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 169
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session'
          TabOrder = 0
          DesignSize = (
            345
            169)
          object Label1: TLabel
            Left = 12
            Top = 18
            Width = 54
            Height = 13
            Caption = '&Host name:'
            FocusControl = HostNameEdit
          end
          object Label2: TLabel
            Left = 253
            Top = 18
            Width = 60
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = PortNumberEdit
          end
          object Label3: TLabel
            Left = 12
            Top = 68
            Width = 54
            Height = 13
            Caption = '&User name:'
            FocusControl = UserNameEdit
          end
          object Label4: TLabel
            Left = 164
            Top = 68
            Width = 49
            Height = 13
            Caption = '&Password:'
            FocusControl = PasswordEdit
          end
          object PrivateKeyLabel: TLabel
            Left = 12
            Top = 118
            Width = 72
            Height = 13
            Caption = 'Private &key file:'
            FocusControl = PrivateKeyEdit
          end
          object HostNameEdit: TEdit
            Left = 12
            Top = 35
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 0
            Text = 'HostNameEdit'
            OnChange = DataChange
          end
          object UserNameEdit: TEdit
            Left = 13
            Top = 85
            Width = 137
            Height = 21
            MaxLength = 100
            TabOrder = 1
            Text = 'UserNameEdit'
            OnChange = DataChange
          end
          object PasswordEdit: TPasswordEdit
            Left = 163
            Top = 85
            Width = 171
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 2
            Text = 'PasswordEdit'
            OnChange = DataChange
          end
          object PortNumberEdit: TUpDownEdit
            Left = 252
            Top = 35
            Width = 82
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535
            MinValue = 1
            Value = 1
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object PrivateKeyEdit: TFilenameEdit
            Left = 12
            Top = 135
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
            TabOrder = 4
            Text = 'PrivateKeyEdit'
            OnChange = DataChange
          end
        end
        object TransferProtocolGroup: TGroupBox
          Left = 0
          Top = 182
          Width = 345
          Height = 47
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol'
          TabOrder = 1
          object Label22: TLabel
            Left = 12
            Top = 21
            Width = 60
            Height = 13
            Caption = '&File protocol:'
            FocusControl = TransferProtocolCombo
          end
          object TransferProtocolCombo: TComboBox
            Left = 108
            Top = 16
            Width = 59
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = TransferProtocolComboChange
            Items.Strings = (
              'SFTP'
              'SCP'
              'FTP')
          end
          object AllowScpFallbackCheck: TCheckBox
            Left = 175
            Top = 18
            Width = 121
            Height = 17
            Caption = 'Allow SCP &fallback'
            TabOrder = 1
            OnClick = DataChange
          end
          object FtpsCombo: TComboBox
            Left = 175
            Top = 16
            Width = 159
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
            OnChange = TransferProtocolComboChange
            Items.Strings = (
              'No encryption'
              'SSL/TLS Implicit encryptionX'
              'SSL Explicit encryptionX'
              'TLS Explicit encryptionX')
          end
        end
        object ColorButton: TButton
          Left = 271
          Top = 237
          Width = 75
          Height = 25
          Caption = 'Select c&olor'
          TabOrder = 2
          OnClick = ColorButtonClick
        end
      end
      object SessionListSheet: TTabSheet
        Tag = 2
        Hint = 'Stored sessions'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_stored_sessions'
        Caption = 'StSe'
        TabVisible = False
        DesignSize = (
          353
          286)
        object LoadButton: TButton
          Left = 259
          Top = 35
          Width = 88
          Height = 25
          Action = EditSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 2
        end
        object DeleteButton: TButton
          Left = 259
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
          Height = 281
          Anchors = [akLeft, akTop, akRight, akBottom]
          DragMode = dmAutomatic
          HideSelection = False
          Indent = 19
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
          OnExpanded = SessionTreeExpandedCollapsed
          OnKeyDown = SessionTreeKeyDown
          OnMouseMove = SessionTreeMouseMove
          OnStartDrag = SessionTreeStartDrag
        end
        object NewButton: TButton
          Left = 259
          Top = 3
          Width = 88
          Height = 25
          Action = NewSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 1
        end
        object SetDefaultSessionButton: TButton
          Left = 258
          Top = 163
          Width = 88
          Height = 25
          Action = SetDefaultSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 6
        end
        object ToolsMenuButton: TButton
          Left = 258
          Top = 259
          Width = 88
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Tools...'
          TabOrder = 8
          OnClick = ToolsMenuButtonClick
        end
        object ShellIconsButton: TButton
          Left = 258
          Top = 195
          Width = 88
          Height = 25
          Action = ShellIconSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 7
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
      end
      object LogSheet: TTabSheet
        Tag = 2
        Hint = 'Logging'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_logging'
        Caption = 'Log'
        ImageIndex = 4
        TabVisible = False
        inline LoggingFrame: TLoggingFrame
          Left = -3
          Top = 0
          Width = 356
          Height = 273
          TabOrder = 0
          DesignSize = (
            356
            273)
          inherited LoggingGroup: TGroupBox
            Width = 345
            inherited LogToFileCheck: TCheckBox
              Width = 313
            end
            inherited LogFileNameEdit2: TFilenameEdit
              Width = 291
            end
            inherited LogShowWindowCheck: TCheckBox
              Width = 321
            end
            inherited LogWindowCompleteButton: TRadioButton
              Width = 289
            end
            inherited LogFilePanel: TPanel
              Width = 291
              inherited LogFileAppendButton: TRadioButton
                Caption = 'Appe&nd'
              end
              inherited LogFileOverwriteButton: TRadioButton
                Width = 153
              end
            end
            inherited LogFileNameHintText: TStaticText
              Left = 256
            end
          end
          inherited LogGroup: TGroupBox
            Width = 345
          end
        end
      end
      object EnvironmentSheet: TTabSheet
        Tag = 1
        Hint = 'Environment'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_environment'
        Caption = 'Env'
        ImageIndex = 6
        TabVisible = False
        DesignSize = (
          353
          286)
        object EnvironmentOtherLabel: TLabel
          Left = 0
          Top = 211
          Width = 68
          Height = 13
          Caption = 'Other settings:'
        end
        object RecycleBinLinkLabel: TStaticText
          Left = 88
          Top = 211
          Width = 60
          Height = 17
          Caption = 'Recycle bin'
          TabOrder = 2
          TabStop = True
          OnClick = RecycleBinLinkLabelClick
        end
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
            Width = 230
            Height = 13
            Caption = '&End-of-line characters (if not indicated by server):'
            FocusControl = EOLTypeCombo
          end
          object UtfLabel: TLabel
            Left = 12
            Top = 44
            Width = 142
            Height = 13
            Caption = '&UTF-8 encoding for filenames:'
            FocusControl = UtfCombo
          end
          object TimeDifferenceLabel: TLabel
            Left = 12
            Top = 68
            Width = 78
            Height = 13
            Caption = 'Time&zone offset:'
            FocusControl = TimeDifferenceEdit
          end
          object TimeDifferenceHoursLabel: TLabel
            Left = 194
            Top = 68
            Width = 26
            Height = 13
            Caption = 'hours'
            FocusControl = TimeDifferenceEdit
          end
          object TimeDifferenceMinutesLabel: TLabel
            Left = 296
            Top = 66
            Width = 36
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
            ItemHeight = 13
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
            ItemHeight = 13
            TabOrder = 1
          end
          object TimeDifferenceEdit: TUpDownEdit
            Left = 135
            Top = 63
            Width = 54
            Height = 21
            Alignment = taRightJustify
            MaxValue = 25
            MinValue = -25
            Value = -13
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
            Increment = 15
            MaxValue = 45
            MinValue = -45
            Value = -13
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
        Hint = 'Directories'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_directories'
        Caption = 'Dir'
        ImageIndex = 11
        TabVisible = False
        DesignSize = (
          353
          286)
        object DirectoriesGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 152
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directories'
          TabOrder = 0
          DesignSize = (
            345
            152)
          object LocalDirectoryLabel: TLabel
            Left = 12
            Top = 84
            Width = 72
            Height = 13
            Caption = '&Local directory:'
            FocusControl = LocalDirectoryEdit
          end
          object RemoteDirectoryLabel: TLabel
            Left = 12
            Top = 41
            Width = 83
            Height = 13
            Caption = '&Remote directory:'
            FocusControl = RemoteDirectoryEdit
          end
          object LocalDirectoryDescLabel: TLabel
            Left = 12
            Top = 126
            Width = 233
            Height = 13
            Caption = 'Local directory is not used with Explorer interface.'
          end
          object LocalDirectoryEdit: TDirectoryEdit
            Left = 12
            Top = 101
            Width = 323
            Height = 21
            AcceptFiles = True
            DialogText = 'Select startup local directory.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'LocalDirectoryEdit'
            OnChange = DataChange
          end
          object RemoteDirectoryEdit: TEdit
            Left = 12
            Top = 58
            Width = 323
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 1
            Text = 'RemoteDirectoryEdit'
            OnChange = DataChange
          end
          object UpdateDirectoriesCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Re&member last used directory'
            TabOrder = 0
          end
        end
        object DirectoryOptionsGroup: TGroupBox
          Left = 0
          Top = 166
          Width = 345
          Height = 87
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory reading options'
          TabOrder = 1
          DesignSize = (
            345
            87)
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
            Top = 39
            Width = 182
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &directory changes'
            TabOrder = 1
            OnClick = DataChange
          end
          object ResolveSymlinksCheck: TCheckBox
            Left = 12
            Top = 59
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Resolve symbolic li&nks'
            TabOrder = 3
          end
          object PreserveDirectoryChangesCheck: TCheckBox
            Left = 203
            Top = 39
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
        Hint = 'Recycle bin'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_recycle_bin'
        Caption = 'Rec'
        ImageIndex = 15
        TabVisible = False
        DesignSize = (
          353
          286)
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
            Width = 94
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
        Hint = 'SFTP'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_sftp'
        Caption = 'Sftp'
        ImageIndex = 12
        TabVisible = False
        DesignSize = (
          353
          286)
        object SFTPBugsGroupBox: TGroupBox
          Left = 0
          Top = 84
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
            Width = 225
            Height = 13
            Caption = '&Reverses order of symlink command arguments:'
            FocusControl = SFTPBugSymlinkCombo
          end
          object Label36: TLabel
            Left = 12
            Top = 44
            Width = 195
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
            ItemHeight = 13
            TabOrder = 0
          end
          object SFTPBugSignedTSCombo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 1
          end
        end
        object SFTPProtocolGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 70
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            345
            70)
          object Label34: TLabel
            Left = 12
            Top = 44
            Width = 154
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
            ItemHeight = 13
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
            ItemHeight = 13
            MaxLength = 255
            TabOrder = 0
            Text = 'SftpServerEdit'
            Items.Strings = (
              'Default'
              '/bin/sftp-server'
              'sudo su -c /bin/sftp-server')
          end
        end
      end
      object ScpSheet: TTabSheet
        Tag = 2
        Hint = 'SCP/Shell'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_scp'
        Caption = 'Shl'
        ImageIndex = 3
        TabVisible = False
        DesignSize = (
          353
          286)
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
            Width = 102
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
            ItemHeight = 13
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
            ItemHeight = 13
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
            ItemHeight = 13
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
        Hint = 'FTP'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_ftp'
        Caption = 'FtpSheet'
        ImageIndex = 16
        TabVisible = False
        DesignSize = (
          353
          286)
        object FtpGroup: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 154
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            345
            154)
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
            Width = 152
            Height = 13
            Caption = '&Support for listing of hidden files:'
            FocusControl = FtpListAllCombo
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
            ItemHeight = 13
            TabOrder = 1
          end
          object FtpForcePasvIpCheck: TCheckBox
            Left = 12
            Top = 125
            Width = 317
            Height = 17
            Caption = '&Force IP address for passive mode connections'
            TabOrder = 2
            OnClick = DataChange
          end
        end
      end
      object ConnSheet: TTabSheet
        Tag = 1
        Hint = 'Connection'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_connection'
        Caption = 'Conn'
        ImageIndex = 7
        TabVisible = False
        DesignSize = (
          353
          286)
        object FtpPingGroup: TGroupBox
          Left = 0
          Top = 108
          Width = 345
          Height = 117
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 3
          DesignSize = (
            345
            117)
          object FtpPingIntervalLabel: TLabel
            Left = 12
            Top = 90
            Width = 143
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
            MaxValue = 3600
            MinValue = 1
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
          Top = 57
          Width = 345
          Height = 46
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Timeouts'
          TabOrder = 1
          object Label11: TLabel
            Left = 12
            Top = 19
            Width = 117
            Height = 13
            Caption = 'Server &response timeout:'
            FocusControl = TimeoutEdit
          end
          object Label12: TLabel
            Left = 286
            Top = 19
            Width = 40
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
            Increment = 5
            MaxValue = 6000
            MinValue = 5
            MaxLength = 4
            TabOrder = 0
            OnChange = DataChange
          end
        end
        object PingGroup: TGroupBox
          Left = 0
          Top = 108
          Width = 345
          Height = 117
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 2
          DesignSize = (
            345
            117)
          object PingIntervalLabel: TLabel
            Left = 12
            Top = 90
            Width = 143
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
            MaxValue = 3600
            MinValue = 1
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
          Top = 230
          Width = 345
          Height = 46
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Internet protocol version'
          TabOrder = 4
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
          Height = 46
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 0
          DesignSize = (
            345
            46)
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
        end
      end
      object ProxySheet: TTabSheet
        Tag = 2
        Hint = 'Proxy'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_proxy'
        Caption = 'Proxy'
        ImageIndex = 8
        TabVisible = False
        DesignSize = (
          353
          286)
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
            Width = 52
            Height = 13
            Caption = 'Proxy &type:'
            FocusControl = SshProxyMethodCombo
          end
          object ProxyHostLabel: TLabel
            Left = 12
            Top = 41
            Width = 81
            Height = 13
            Caption = 'Pro&xy host name:'
            FocusControl = ProxyHostEdit
          end
          object ProxyPortLabel: TLabel
            Left = 252
            Top = 41
            Width = 60
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = ProxyPortEdit
          end
          object ProxyUsernameLabel: TLabel
            Left = 12
            Top = 85
            Width = 54
            Height = 13
            Caption = '&User name:'
            FocusControl = ProxyUsernameEdit
          end
          object ProxyPasswordLabel: TLabel
            Left = 163
            Top = 85
            Width = 49
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
            ItemHeight = 13
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
            MaxValue = 65535
            MinValue = 1
            Value = 1
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object ProxyHostEdit: TEdit
            Left = 12
            Top = 58
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 2
            Text = 'ProxyHostEdit'
            OnChange = DataChange
          end
          object ProxyUsernameEdit: TEdit
            Left = 12
            Top = 102
            Width = 137
            Height = 21
            MaxLength = 50
            TabOrder = 4
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
            TabOrder = 5
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
            ItemHeight = 13
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
            Width = 106
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
            ItemHeight = 13
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
        Hint = 'Tunnel'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_tunnel'
        Caption = 'Tun'
        ImageIndex = 14
        TabVisible = False
        DesignSize = (
          353
          286)
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
            Width = 54
            Height = 13
            Caption = '&Host name:'
            FocusControl = TunnelHostNameEdit
          end
          object Label14: TLabel
            Left = 252
            Top = 18
            Width = 60
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = TunnelPortNumberEdit
          end
          object Label15: TLabel
            Left = 12
            Top = 68
            Width = 54
            Height = 13
            Caption = '&User name:'
            FocusControl = TunnelUserNameEdit
          end
          object Label16: TLabel
            Left = 163
            Top = 68
            Width = 49
            Height = 13
            Caption = '&Password:'
            FocusControl = TunnelPasswordEdit
          end
          object Label18: TLabel
            Left = 12
            Top = 118
            Width = 72
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
            MaxValue = 65535
            MinValue = 1
            Value = 1
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
            Width = 82
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
            ItemHeight = 13
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
        Hint = 'SSH'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_ssh'
        Caption = 'SSH'
        ImageIndex = 2
        TabVisible = False
        DesignSize = (
          353
          286)
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
            Width = 149
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
            Width = 160
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
        Hint = 'Key exchange'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_kex'
        Caption = 'KEX'
        ImageIndex = 13
        TabVisible = False
        DesignSize = (
          353
          286)
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
            Width = 121
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
            Width = 189
            Height = 13
            Caption = 'Max &minutes before rekey (0 for no limit):'
            Color = clBtnFace
            FocusControl = RekeyTimeEdit
            ParentColor = False
          end
          object Label32: TLabel
            Left = 12
            Top = 44
            Width = 174
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
            MaxValue = 1440
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
        Hint = 'Authentication'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_authentication'
        Caption = 'Auth'
        ImageIndex = 10
        TabVisible = False
        DesignSize = (
          353
          286)
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
          Height = 140
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication options'
          TabOrder = 1
          DesignSize = (
            345
            140)
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
          object AuthGSSAPICheck2: TCheckBox
            Left = 12
            Top = 111
            Width = 325
            Height = 17
            Caption = 'Attempt &GSSAPI/SSPI authentication (SSH-2)'
            TabOrder = 4
            OnClick = AuthGSSAPICheck2Click
          end
        end
        object AuthenticationParamsGroup: TGroupBox
          Left = 0
          Top = 177
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
      end
      object BugsSheet: TTabSheet
        Tag = 2
        Hint = 'Bugs'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_bugs'
        Caption = 'Bugs'
        ImageIndex = 9
        TabVisible = False
        DesignSize = (
          353
          286)
        object BugsGroupBox: TGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 241
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SSH servers'
          TabOrder = 0
          DesignSize = (
            345
            241)
          object BugIgnore1Label: TLabel
            Left = 12
            Top = 20
            Width = 170
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
            Width = 183
            Height = 13
            Caption = 'Chokes on SSH-1 &RSA authentication:'
            FocusControl = BugRSA1Combo
          end
          object BugHMAC2Label: TLabel
            Left = 12
            Top = 92
            Width = 158
            Height = 13
            Caption = 'Miscomputes SSH-2 H&MAC keys:'
            FocusControl = BugHMAC2Combo
          end
          object BugDeriveKey2Label: TLabel
            Left = 12
            Top = 116
            Width = 176
            Height = 13
            Caption = 'Miscomputes SSH-2 &encryption keys:'
            FocusControl = BugDeriveKey2Combo
          end
          object BugRSAPad2Label: TLabel
            Left = 12
            Top = 140
            Width = 211
            Height = 13
            Caption = 'Requires &padding on SSH-2 RSA signatures:'
            FocusControl = BugRSAPad2Combo
          end
          object BugPKSessID2Label: TLabel
            Left = 12
            Top = 164
            Width = 197
            Height = 13
            Caption = 'Misuses the sessio&n ID in SSH-2 PK auth:'
            FocusControl = BugPKSessID2Combo
          end
          object BugRekey2Label: TLabel
            Left = 12
            Top = 188
            Width = 186
            Height = 13
            Caption = 'Handles SSH-2 &key re-exchange badly:'
            FocusControl = BugRekey2Combo
          end
          object BugMaxPkt2Label: TLabel
            Left = 12
            Top = 212
            Width = 175
            Height = 13
            Caption = 'Ignores SSH-2 ma&ximum packet size:'
            FocusControl = BugMaxPkt2Combo
          end
          object BugIgnore1Combo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
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
            ItemHeight = 13
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
            ItemHeight = 13
            TabOrder = 2
            OnChange = DataChange
          end
          object BugHMAC2Combo: TComboBox
            Left = 272
            Top = 87
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 3
            OnChange = DataChange
          end
          object BugDeriveKey2Combo: TComboBox
            Left = 272
            Top = 111
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 4
            OnChange = DataChange
          end
          object BugRSAPad2Combo: TComboBox
            Left = 272
            Top = 135
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 5
            OnChange = DataChange
          end
          object BugPKSessID2Combo: TComboBox
            Left = 272
            Top = 159
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 6
            OnChange = DataChange
          end
          object BugRekey2Combo: TComboBox
            Left = 272
            Top = 183
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 7
            OnChange = DataChange
          end
          object BugMaxPkt2Combo: TComboBox
            Left = 272
            Top = 207
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 8
            OnChange = DataChange
          end
        end
      end
      object GeneralSheet: TTabSheet
        Tag = 1
        Hint = 'Preferences'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_preferences'
        Caption = 'Int'
        ImageIndex = 5
        TabVisible = False
        object Label13: TLabel
          Left = 16
          Top = 224
          Width = 104
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
      Height = 296
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        152
        296)
      object NavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 136
        Height = 260
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        ShowButtons = False
        ShowRoot = False
        TabOrder = 0
        OnChange = NavigationTreeChange
        OnCollapsing = NavigationTreeCollapsing
        Items.Data = {
          0500000021000000000000000000000000000000FFFFFFFF0000000002000000
          0853657373696F6E5829000000000000000000000000000000FFFFFFFF000000
          00000000001053746F7265642073657373696F6E735821000000000000000000
          000000000000FFFFFFFF0000000000000000084C6F6767696E67582500000000
          0000000000000000000000FFFFFFFF00000000050000000C456E7669726F6E6D
          656E745825000000000000000000000000000000FFFFFFFF0000000000000000
          0C4469726563746F726965735825000000000000000000000000000000FFFFFF
          FF00000000000000000C52656379636C652062696E581E000000000000000000
          000000000000FFFFFFFF00000000000000000553465450581D00000000000000
          0000000000000000FFFFFFFF000000000000000004534350581D000000000000
          0000000000FFFFFFFFFFFFFFFF00000000000000000446545058240000000000
          00000000000000000000FFFFFFFF00000000020000000B436F6E6E656374696F
          6E581F000000000000000000000000000000FFFFFFFF00000000000000000650
          726F78795820000000000000000000000000000000FFFFFFFF00000000000000
          000754756E6E656C581D000000000000000000000000000000FFFFFFFF000000
          0003000000045353485826000000000000000000000000000000FFFFFFFF0000
          0000000000000D4B65782065786368616E676558280000000000000000000000
          00000000FFFFFFFF00000000000000000F41757468656E7469636174696F6E58
          1E000000000000000000000000000000FFFFFFFF000000000000000005427567
          735825000000000000000000000000000000FFFFFFFF00000000000000000C50
          7265666572656E63657358}
      end
      object ShowAdvancedLoginOptionsCheck: TCheckBox
        Left = 16
        Top = 276
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
    Top = 257
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
      Caption = '&Import...'
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
  end
  object ToolsPopupMenu: TPopupMenu
    Left = 48
    Top = 257
    object Import1: TMenuItem
      Action = ImportSessionsAction
    end
    object Cleanup1: TMenuItem
      Action = CleanUpAction
    end
    object CheckForUpdates1: TMenuItem
      Action = CheckForUpdatesAction
    end
  end
  object IconsPopupMenu: TPopupMenu
    Left = 80
    Top = 257
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
    Top = 257
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
    Top = 289
    Bitmap = {
      494C010101000400040010001000FF000000FF10FFFFFFFFFFFFFFFF424D3600
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
  object SessionImageList: TImageList
    Left = 48
    Top = 289
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001821
      2100182121001821210018212100182121001821210018212100182121001821
      2100182121001821210018212100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000006363000063
      6300006363000063630000636300006363000063630000636300006363000063
      63000063630018212100182121000000000000000000006363009CFFFF0000CE
      FF0000CEFF00009CCE0000CEFF00009CCE0000CEFF00009CCE00009CCE00009C
      CE00639C9C00009CCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000006363009CFF
      FF00009CCE0000CEFF00009CCE0000CEFF00009CCE00009CCE00009CCE00009C
      CE000063630018212100182121000000000000000000006363009CFFFF0000CE
      FF0000CEFF0000CEFF00009CCE0000CEFF00009CCE0000CEFF00009CCE00009C
      CE00009CCE00639C9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000006363009CFFFF0000CE
      FF0000CEFF00009CCE0000CEFF00009CCE0000CEFF00009CCE00009CCE00639C
      9C001821210000636300182121000000000000000000006363009CFFFF00FFFF
      FF0000CEFF0000CEFF0000CEFF00009CCE0000CEFF00009CCE0000CEFF00009C
      CE00009CCE00009CCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000006363009CFFFF00FFFF
      FF0000CEFF0000CEFF0000CEFF0000CEFF00009CCE0000CEFF00009CCE00009C
      CE001821210000636300182121000000000000000000006363009CFFFF0000CE
      FF00FFFFFF0000CEFF0000CEFF0000CEFF0000CEFF0000CEFF00009CCE0000CE
      FF00009CCE00009CCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000006363009CFFFF00FFFFFF0000CE
      FF00FFFFFF0000CEFF00009CCE0000CEFF0000CEFF00009CCE0000CEFF001821
      210000CEFF0000CEFF00182121000000000000000000006363009CFFFF00FFFF
      FF0000CEFF00FFFFFF0000CEFF0000CEFF0000CEFF0000CEFF0000CEFF00009C
      CE0000CEFF00009CCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000006363009CFFFF0000CEFF00FFFF
      FF0000CEFF00FFFFFF0000CEFF0000CEFF00009CCE0000CEFF00009CCE001821
      210000CEFF0000CEFF00182121000000000000000000006363009CFFFF0000CE
      FF00FFFFFF0000CEFF00FFFFFF0000CEFF00FFFFFF0000CEFF0000CEFF0000CE
      FF0000CEFF0000CEFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000063630000636300006363000063
      63000063630000636300006363000063630000636300006363000063630000CE
      FF0000CEFF0000CEFF00182121000000000000000000006363009CFFFF00FFFF
      FF0000CEFF00FFFFFF0000CEFF00FFFFFF0000CEFF00FFFFFF0000CEFF0000CE
      FF0000CEFF00009CCE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000006363009CFFFF0000CE
      FF00FFFFFF0000CEFF0000CEFF0000CEFF0000CEFF0000CEFF0000CEFF0000CE
      FF0000CEFF0000CEFF00182121000000000000000000006363009CFFFF0000CE
      FF00FFFFFF0000CEFF00FFFFFF0000CEFF00FFFFFF0000CEFF00FFFFFF0000CE
      FF0000CEFF0000CEFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000006363009CFFFF00FFFF
      FF0000CEFF00FFFFFF0000CEFF0000CEFF0000CEFF0000CEFF009CFFFF009CFF
      FF009CFFFF009CFFFF00182121000000000000000000006363009CFFFF009CFF
      FF009CFFFF009CFFFF009CFFFF009CFFFF009CFFFF009CFFFF009CFFFF009CFF
      FF009CFFFF009CFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000006363009CFFFF0000CE
      FF00FFFFFF0000CEFF00FFFFFF0000CEFF009CFFFF0000636300006363000063
      6300006363000063630000000000000000000000000000636300006363000063
      6300006363000063630000636300006363000063630000636300006363000063
      6300006363000063630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000006363009CFF
      FF009CFFFF009CFFFF009CFFFF009CFFFF000063630000000000000000000000
      0000000000000000000000000000000000000000000000000000006363009CFF
      FF009CFFFF009CCECE009CCECE00009C9C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000063
      6300006363000063630000636300006363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000063
      6300006363000063630000636300006363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFE0018001FFFFFFFFC0018001FFFFFFFFC0018001FFFFFFFF80018001
      FFFFFFFF80018001FFFFFFFF00018001FFFFFFFF00018001FFFFFFFF00018001
      FFFFFFFF80018001FFFFFFFF80018001FFFFFFFF80038001FFFFFFFFC07FC07F
      FFFFFFFFE0FFE0FFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
