object LoginDialog: TLoginDialog
  Left = 353
  Top = 185
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 358
  ClientWidth = 522
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    522
    358)
  PixelsPerInch = 96
  TextHeight = 13
  object SaveButton: TButton
    Left = 260
    Top = 327
    Width = 75
    Height = 25
    Action = SaveSessionAction
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object LoginButton: TButton
    Left = 348
    Top = 327
    Width = 75
    Height = 25
    Action = LoginAction
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CloseButton: TButton
    Left = 436
    Top = 327
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
    Top = 327
    Width = 82
    Height = 25
    Action = AboutAction
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    TabStop = False
  end
  object LanguagesButton: TButton
    Left = 105
    Top = 327
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
    Width = 522
    Height = 318
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 5
    object PageControl: TPageControl
      Tag = 6
      Left = 161
      Top = 0
      Width = 361
      Height = 318
      ActivePage = GeneralSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabIndex = 6
      TabOrder = 1
      OnChange = PageControlChange
      object SessionListSheet: TTabSheet
        Tag = 1
        Hint = 'Stored sessions'
        Caption = 'StSe'
        DesignSize = (
          353
          263)
        object LoadButton: TButton
          Left = 258
          Top = 35
          Width = 88
          Height = 25
          Action = LoadSessionAction
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
        object SessionListView: TListView
          Left = 2
          Top = 3
          Width = 247
          Height = 258
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          ShowColumnHeaders = False
          ShowHint = True
          TabOrder = 0
          ViewStyle = vsReport
          OnCustomDrawItem = SessionListViewCustomDrawItem
          OnDblClick = SessionListViewDblClick
          OnInfoTip = SessionListViewInfoTip
          OnKeyDown = SessionListViewKeyDown
          OnSelectItem = SessionListViewSelectItem
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
          Top = 99
          Width = 88
          Height = 25
          Action = SetDefaultSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 4
        end
        object ToolsMenuButton: TButton
          Left = 258
          Top = 236
          Width = 88
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Tools...'
          TabOrder = 6
          OnClick = ToolsMenuButtonClick
        end
        object ShellIconsButton: TButton
          Left = 258
          Top = 131
          Width = 88
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Shell &icon'
          TabOrder = 5
          OnClick = ShellIconsButtonClick
        end
      end
      object BasicSheet: TTabSheet
        Tag = 2
        Hint = 'Session'
        Caption = 'Basic'
        ImageIndex = 1
        DesignSize = (
          353
          263)
        object BasicGroup: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 169
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session'
          TabOrder = 0
          DesignSize = (
            345
            169)
          object Label1: TLabel
            Left = 11
            Top = 18
            Width = 51
            Height = 13
            Caption = '&Host name'
            FocusControl = HostNameEdit
          end
          object Label2: TLabel
            Left = 252
            Top = 18
            Width = 57
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number'
            FocusControl = PortNumberEdit
          end
          object Label3: TLabel
            Left = 11
            Top = 68
            Width = 51
            Height = 13
            Caption = '&User name'
            FocusControl = UserNameEdit
          end
          object Label4: TLabel
            Left = 163
            Top = 68
            Width = 46
            Height = 13
            Caption = '&Password'
            FocusControl = PasswordEdit
          end
          object Label5: TLabel
            Left = 11
            Top = 118
            Width = 69
            Height = 13
            Caption = 'Private &key file'
            FocusControl = PrivateKeyEdit
          end
          object HostNameEdit: TEdit
            Left = 11
            Top = 35
            Width = 226
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 0
            Text = 'HostNameEdit'
            OnChange = DataChange
          end
          object UserNameEdit: TEdit
            Left = 11
            Top = 85
            Width = 137
            Height = 21
            MaxLength = 50
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
            MaxLength = 50
            TabOrder = 2
            Text = 'PasswordEdit'
            OnChange = DataChange
          end
          object PortNumberEdit: TUpDownEdit
            Left = 252
            Top = 35
            Width = 82
            Height = 21
            MaxValue = 65535
            MinValue = 1
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object PrivateKeyEdit: TFilenameEdit
            Left = 11
            Top = 135
            Width = 323
            Height = 21
            AcceptFiles = True
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
        object TransferProtocolGroup: TXPGroupBox
          Left = 0
          Top = 184
          Width = 345
          Height = 48
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol'
          TabOrder = 1
          object SFTPButton: TRadioButton
            Left = 88
            Top = 19
            Width = 153
            Height = 17
            Caption = 'SFTP (allow SCP &fallback)'
            TabOrder = 1
            OnClick = DataChange
          end
          object SCPonlyButton: TRadioButton
            Left = 16
            Top = 19
            Width = 65
            Height = 17
            Caption = 'S&CP'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = DataChange
          end
          object SFTPonlyButton: TRadioButton
            Left = 264
            Top = 19
            Width = 65
            Height = 17
            Caption = 'SF&TP'
            TabOrder = 2
            OnClick = DataChange
          end
        end
      end
      object AdvancedSheet: TTabSheet
        Tag = 3
        Hint = 'SSH'
        Caption = 'SSH'
        ImageIndex = 2
        DesignSize = (
          353
          263)
        object ProtocolGroup: TXPGroupBox
          Left = 0
          Top = 8
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
        object EncryptionGroup: TXPGroupBox
          Left = 0
          Top = 102
          Width = 345
          Height = 149
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Encryption options'
          TabOrder = 1
          DesignSize = (
            345
            149)
          object Label8: TLabel
            Left = 12
            Top = 19
            Width = 160
            Height = 13
            Caption = 'Encryption cipher selection &policy:'
            FocusControl = CipherListBox
          end
          object CipherListBox: TListBox
            Left = 11
            Top = 36
            Width = 190
            Height = 77
            DragMode = dmAutomatic
            ItemHeight = 13
            TabOrder = 0
            OnClick = DataChange
            OnDragDrop = CipherListBoxDragDrop
            OnDragOver = CipherListBoxDragOver
            OnStartDrag = CipherListBoxStartDrag
          end
          object Ssh2LegacyDESCheck: TCheckBox
            Left = 16
            Top = 120
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable legacy use of single-&DES in SSH 2'
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
      object EnvironmentSheet: TTabSheet
        Tag = 4
        Hint = 'Environment'
        Caption = 'Env'
        ImageIndex = 6
        DesignSize = (
          353
          263)
        object DirectoriesGroup: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 196
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directories'
          TabOrder = 0
          DesignSize = (
            345
            196)
          object Label9: TLabel
            Left = 11
            Top = 128
            Width = 121
            Height = 13
            Caption = '&Local directory (left panel)'
            FocusControl = LocalDirectoryEdit
          end
          object Label10: TLabel
            Left = 11
            Top = 84
            Width = 138
            Height = 13
            Caption = '&Remote directory (right panel)'
            FocusControl = RemoteDirectoryEdit
          end
          object Label16: TLabel
            Left = 11
            Top = 172
            Width = 251
            Height = 13
            Caption = 'Local directory is not used with explorer-like interface.'
          end
          object LocalDirectoryEdit: TDirectoryEdit
            Left = 11
            Top = 145
            Width = 323
            Height = 21
            AcceptFiles = True
            DialogText = 'Select startup local directory.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
            Text = 'LocalDirectoryEdit'
            OnChange = DataChange
          end
          object RemoteDirectoryEdit: TEdit
            Left = 11
            Top = 101
            Width = 323
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 3
            Text = 'RemoteDirectoryEdit'
            OnChange = DataChange
          end
          object UpdateDirectoriesCheck: TCheckBox
            Left = 11
            Top = 20
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Re&member last used directory'
            TabOrder = 0
          end
          object CacheDirectoriesCheck: TCheckBox
            Left = 11
            Top = 41
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &visited remote directories'
            TabOrder = 1
          end
          object ResolveSymlinksCheck: TCheckBox
            Left = 11
            Top = 62
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Resolve symbolic li&nks'
            TabOrder = 2
          end
        end
        object EOLTypeGroup: TXPGroupBox
          Left = 0
          Top = 211
          Width = 345
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          Caption = 'EOL (end-of-line) characters (SCP and SFTP version < 4)'
          TabOrder = 1
          object EOLTypeLFButton: TRadioButton
            Left = 12
            Top = 18
            Width = 133
            Height = 17
            Caption = 'L&F [10] (Unix)'
            TabOrder = 0
            OnClick = DataChange
          end
          object EOLTypeCRLFButton: TRadioButton
            Left = 152
            Top = 18
            Width = 185
            Height = 17
            Caption = '&CR/LF [13/10] (Windows)'
            TabOrder = 1
            OnClick = DataChange
          end
        end
      end
      object ScpSheet: TTabSheet
        Tag = 5
        Hint = 'SCP'
        Caption = 'Scp'
        ImageIndex = 3
        DesignSize = (
          353
          263)
        object OtherShellOptionsGroup: TXPGroupBox
          Left = 0
          Top = 108
          Width = 345
          Height = 113
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other options'
          TabOrder = 2
          DesignSize = (
            345
            113)
          object Label29: TLabel
            Left = 13
            Top = 88
            Width = 105
            Height = 13
            Caption = 'Server time&zone offset'
            FocusControl = TimeDifferenceEdit
          end
          object Label30: TLabel
            Left = 204
            Top = 88
            Width = 26
            Height = 13
            Caption = 'hours'
            FocusControl = TimeDifferenceEdit
          end
          object LookupUserGroupsCheck: TCheckBox
            Left = 12
            Top = 18
            Width = 140
            Height = 17
            Caption = 'Lookup &user groups'
            TabOrder = 0
            OnClick = DataChange
          end
          object ClearAliasesCheck: TCheckBox
            Left = 12
            Top = 38
            Width = 140
            Height = 17
            Caption = 'Clear a&liases'
            TabOrder = 2
            OnClick = DataChange
          end
          object UnsetNationalVarsCheck: TCheckBox
            Left = 152
            Top = 17
            Width = 185
            Height = 17
            Caption = 'Clear &national variables'
            TabOrder = 1
            OnClick = DataChange
          end
          object AliasGroupListCheck: TCheckBox
            Left = 152
            Top = 38
            Width = 185
            Height = 17
            Caption = 'Alias LS to display g&roup name'
            TabOrder = 3
            OnClick = DataChange
          end
          object IgnoreLsWarningsCheck: TCheckBox
            Left = 12
            Top = 59
            Width = 140
            Height = 17
            Caption = 'Ignore LS &warnings'
            TabOrder = 4
            OnClick = DataChange
          end
          object Scp1CompatibilityCheck: TCheckBox
            Left = 152
            Top = 59
            Width = 185
            Height = 17
            Caption = 'Use scp&2 with scp1 compat.'
            TabOrder = 5
            OnClick = DataChange
          end
          object TimeDifferenceEdit: TUpDownEdit
            Left = 137
            Top = 83
            Width = 61
            Height = 21
            Alignment = taRightJustify
            Decimal = 1
            MaxValue = 12
            MinValue = -12
            Value = 1
            Anchors = [akTop, akRight]
            TabOrder = 6
            OnChange = DataChange
          end
        end
        object ReturnVarGroup: TXPGroupBox
          Left = 0
          Top = 57
          Width = 345
          Height = 44
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Return code variable'
          TabOrder = 1
          DesignSize = (
            345
            44)
          object ReturnVarAutodetectButton: TRadioButton
            Left = 12
            Top = 18
            Width = 101
            Height = 17
            Caption = 'Aut&odetect'
            TabOrder = 0
            OnClick = DataChange
          end
          object ReturnVarEnterButton: TRadioButton
            Left = 112
            Top = 18
            Width = 73
            Height = 17
            Caption = 'En&ter:'
            TabOrder = 1
            OnClick = DataChange
          end
          object ReturnVarEdit: TComboBox
            Left = 184
            Top = 14
            Width = 151
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            MaxLength = 50
            TabOrder = 2
            Text = 'ReturnVarEdit'
            Items.Strings = (
              '?'
              'status')
          end
        end
        object ShellGroup: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 44
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shell'
          TabOrder = 0
          DesignSize = (
            345
            44)
          object DefaultShellButton: TRadioButton
            Left = 12
            Top = 18
            Width = 101
            Height = 17
            Caption = '&Default'
            TabOrder = 0
            OnClick = DataChange
          end
          object ShellEnterButton: TRadioButton
            Left = 112
            Top = 18
            Width = 73
            Height = 17
            Caption = '&Enter:'
            TabOrder = 1
            OnClick = DataChange
          end
          object ShellEdit: TComboBox
            Left = 184
            Top = 14
            Width = 151
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            MaxLength = 50
            TabOrder = 2
            Text = 'ShellEdit'
            Items.Strings = (
              '/bin/bash'
              '/bin/ksh')
          end
        end
      end
      object LogSheet: TTabSheet
        Tag = 6
        Hint = 'Logging'
        Caption = 'Log'
        ImageIndex = 4
        inline LoggingFrame: TLoggingFrame
          Left = -3
          Top = 0
          Width = 356
          Height = 212
          TabOrder = 0
          DesignSize = (
            356
            212)
          inherited LoggingCheck: TCheckBox
            Width = 307
          end
          inherited LoggingGroup: TXPGroupBox
            Width = 345
            inherited LogToFileCheck: TCheckBox
              Width = 313
            end
            inherited LogFileNameEdit: TFilenameEdit
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
                Caption = 'A&ppend'
              end
              inherited LogFileOverwriteButton: TRadioButton
                Width = 153
              end
            end
          end
        end
      end
      object GeneralSheet: TTabSheet
        Tag = 7
        Hint = 'Preferences'
        Caption = 'Int'
        ImageIndex = 5
        object Label13: TLabel
          Left = 16
          Top = 226
          Width = 104
          Height = 13
          Caption = 'Other general options:'
        end
        object PreferencesButton: TButton
          Left = 184
          Top = 220
          Width = 90
          Height = 25
          Caption = '&Preferences...'
          TabOrder = 1
          OnClick = PreferencesButtonClick
        end
        inline GeneralSettingsFrame: TGeneralSettingsFrame
          Left = 0
          Top = 8
          Width = 345
          Height = 202
          TabOrder = 0
          inherited InterfaceGroup: TXPGroupBox
            Width = 345
            inherited CommanderDescriptionLabel: TLabel
              Width = 206
            end
            inherited ExplorerDescriptionLabel: TLabel
              Width = 208
            end
          end
        end
      end
      object ConnSheet: TTabSheet
        Tag = 8
        Hint = 'Connection'
        Caption = 'Conn'
        ImageIndex = 7
        DesignSize = (
          353
          263)
        object TimeoutGroup: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 95
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Timeouts'
          TabOrder = 0
          DesignSize = (
            345
            95)
          object Label6: TLabel
            Left = 32
            Top = 68
            Width = 140
            Height = 13
            Caption = 'Seconds &between keepalives'
          end
          object Label11: TLabel
            Left = 12
            Top = 19
            Width = 114
            Height = 13
            Caption = 'Server &response timeout'
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
          object PingIntervalCheck: TCheckBox
            Left = 12
            Top = 43
            Width = 324
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sending of null packets to &keep session alive'
            TabOrder = 1
            OnClick = DataChange
          end
          object PingIntervalSecEdit: TUpDownEdit
            Left = 208
            Top = 63
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 60
            MinValue = 1
            MaxLength = 2
            TabOrder = 2
            OnChange = DataChange
          end
          object TimeoutEdit: TUpDownEdit
            Left = 208
            Top = 14
            Width = 73
            Height = 21
            Alignment = taRightJustify
            Increment = 5
            MaxValue = 300
            MinValue = 5
            MaxLength = 2
            TabOrder = 0
            OnChange = DataChange
          end
        end
      end
      object ProxySheet: TTabSheet
        Tag = 9
        Hint = 'Proxy'
        Caption = 'Proxy'
        ImageIndex = 8
        DesignSize = (
          353
          263)
        object ProxyTypeGroup: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 136
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy'
          TabOrder = 0
          DesignSize = (
            345
            136)
          object Label15: TLabel
            Left = 11
            Top = 41
            Width = 78
            Height = 13
            Caption = 'Pro&xy host name'
            FocusControl = ProxyHostEdit
          end
          object Label18: TLabel
            Left = 240
            Top = 41
            Width = 57
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number'
            FocusControl = ProxyPortEdit
          end
          object Label19: TLabel
            Left = 11
            Top = 85
            Width = 51
            Height = 13
            Caption = '&User name'
            FocusControl = ProxyUsernameEdit
          end
          object Label20: TLabel
            Left = 163
            Top = 85
            Width = 46
            Height = 13
            Caption = '&Password'
            FocusControl = ProxyPasswordEdit
          end
          object ProxyNoneButton: TRadioButton
            Left = 12
            Top = 19
            Width = 77
            Height = 17
            Caption = '&None'
            TabOrder = 0
            OnClick = DataChange
          end
          object ProxyPortEdit: TUpDownEdit
            Left = 240
            Top = 58
            Width = 94
            Height = 21
            MaxValue = 65535
            MinValue = 1
            Anchors = [akTop, akRight]
            TabOrder = 5
            OnChange = DataChange
          end
          object ProxyHostEdit: TEdit
            Left = 11
            Top = 58
            Width = 214
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 4
            Text = 'ProxyHostEdit'
            OnChange = DataChange
          end
          object ProxyUsernameEdit: TEdit
            Left = 11
            Top = 102
            Width = 137
            Height = 21
            MaxLength = 50
            TabOrder = 6
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
            TabOrder = 7
            Text = 'PasswordEdit'
            OnChange = DataChange
          end
          object ProxySocks4Button: TRadioButton
            Left = 81
            Top = 19
            Width = 77
            Height = 17
            Caption = 'SOCKS&4'
            TabOrder = 2
            OnClick = DataChange
          end
          object ProxySocks5Button: TRadioButton
            Left = 148
            Top = 19
            Width = 67
            Height = 17
            Caption = 'SOCKS&5'
            TabOrder = 8
            OnClick = DataChange
          end
          object ProxyHTTPButton: TRadioButton
            Left = 215
            Top = 19
            Width = 66
            Height = 17
            Caption = '&HTTP'
            TabOrder = 1
            OnClick = DataChange
          end
          object ProxyTelnetButton: TRadioButton
            Left = 282
            Top = 19
            Width = 61
            Height = 17
            Caption = '&Telnet'
            TabOrder = 3
            OnClick = DataChange
          end
        end
        object ProxySettingsGroup: TXPGroupBox
          Left = 0
          Top = 149
          Width = 345
          Height = 108
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy settings'
          TabOrder = 1
          DesignSize = (
            345
            108)
          object Label21: TLabel
            Left = 11
            Top = 19
            Width = 79
            Height = 13
            Caption = 'Telnet co&mmand'
            FocusControl = ProxyTelnetCommandEdit
          end
          object Label17: TLabel
            Left = 11
            Top = 64
            Width = 168
            Height = 13
            Caption = 'Do &DNS name lookup at proxy end:'
            FocusControl = ProxyDNSOffButton
          end
          object ProxyTelnetCommandEdit: TEdit
            Left = 128
            Top = 14
            Width = 205
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 0
            Text = 'ProxyTelnetCommandEdit'
            OnChange = DataChange
          end
          object ProxyLocalhostCheck: TCheckBox
            Left = 13
            Top = 42
            Width = 313
            Height = 17
            Caption = 'Co&nsider proxying local host connections'
            TabOrder = 1
          end
          object ProxyDNSOffButton: TRadioButton
            Left = 12
            Top = 83
            Width = 85
            Height = 17
            Caption = 'No'
            TabOrder = 2
          end
          object ProxyDNSAutoButton: TRadioButton
            Left = 100
            Top = 83
            Width = 85
            Height = 17
            Caption = 'Auto'
            TabOrder = 3
          end
          object ProxyDNSOnButton: TRadioButton
            Left = 188
            Top = 83
            Width = 85
            Height = 17
            Caption = 'Yes'
            TabOrder = 4
          end
        end
      end
      object BugsSheet: TTabSheet
        Tag = 10
        Hint = 'Bugs'
        Caption = 'Bugs'
        ImageIndex = 9
        DesignSize = (
          353
          263)
        object BugsGroupBox: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 217
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SSH servers'
          TabOrder = 0
          DesignSize = (
            345
            217)
          object Label22: TLabel
            Left = 12
            Top = 20
            Width = 164
            Height = 13
            Caption = 'Chokes on SSH1 &ignore messages'
            FocusControl = BugIgnore1Combo
          end
          object Label23: TLabel
            Left = 12
            Top = 44
            Width = 189
            Height = 13
            Caption = 'Refuses all SSH1 pass&word camouflage'
            FocusControl = BugPlainPW1Combo
          end
          object Label24: TLabel
            Left = 12
            Top = 68
            Width = 177
            Height = 13
            Caption = 'Chokes on SSH1 &RSA authentication'
            FocusControl = BugRSA1Combo
          end
          object Label25: TLabel
            Left = 12
            Top = 92
            Width = 152
            Height = 13
            Caption = 'Miscomputes SSH2 H&MAC keys'
            FocusControl = BugHMAC2Combo
          end
          object Label26: TLabel
            Left = 12
            Top = 116
            Width = 170
            Height = 13
            Caption = 'Miscomputes SSH2 &encryption keys'
            FocusControl = BugDeriveKey2Combo
          end
          object Label27: TLabel
            Left = 12
            Top = 140
            Width = 205
            Height = 13
            Caption = 'Requires &padding on SSH2 RSA signatures'
            FocusControl = BugRSAPad2Combo
          end
          object Label28: TLabel
            Left = 12
            Top = 164
            Width = 199
            Height = 13
            Caption = 'Chokes on &Diffie-Hellman group exchange'
            FocusControl = BugDHGEx2Combo
          end
          object Label14: TLabel
            Left = 12
            Top = 188
            Width = 160
            Height = 13
            Caption = 'Misuses the sessio&n ID in PK auth'
            FocusControl = BugPKSessID2Combo
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
          end
          object BugDHGEx2Combo: TComboBox
            Left = 272
            Top = 159
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 6
          end
          object BugPKSessID2Combo: TComboBox
            Left = 272
            Top = 183
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 7
          end
        end
      end
      object AuthSheet: TTabSheet
        Tag = 11
        Hint = 'Authentication'
        Caption = 'Auth'
        ImageIndex = 10
        DesignSize = (
          353
          263)
        object AuthenticationGroup: TXPGroupBox
          Left = 0
          Top = 8
          Width = 345
          Height = 97
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication options'
          TabOrder = 0
          DesignSize = (
            345
            97)
          object AuthTISCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt &TIS or CryptoCard authentication (SSH1)'
            TabOrder = 0
            OnClick = DataChange
          end
          object AgentFwdCheck: TCheckBox
            Left = 12
            Top = 67
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow agent &forwarding'
            TabOrder = 1
            OnClick = DataChange
          end
          object AuthKICheck: TCheckBox
            Left = 12
            Top = 43
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt '#39'keyboard-&interactive'#39' authentication (SSH2)'
            TabOrder = 2
            OnClick = DataChange
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 161
      Height = 318
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        161
        318)
      object SimpleNavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 145
        Height = 282
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        RightClickSelect = True
        TabOrder = 0
        OnChange = NavigationTreeChange
        Items.Data = {
          04000000210000000000000002000000FFFFFFFFFFFFFFFF0000000001000000
          0853657373696F6E58290000000000000001000000FFFFFFFFFFFFFFFF000000
          00000000001053746F7265642073657373696F6E735825000000000000000400
          0000FFFFFFFFFFFFFFFF00000000000000000C456E7669726F6E6D656E74581D
          0000000000000003000000FFFFFFFFFFFFFFFF00000000000000000453534858
          250000000000000087010000FFFFFFFFFFFFFFFF00000000000000000C507265
          666572656E63657358}
      end
      object AdvancedNavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 145
        Height = 282
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnChange = NavigationTreeChange
        Items.Data = {
          05000000210000000000000002000000FFFFFFFFFFFFFFFF0000000002000000
          0853657373696F6E58290000000000000001000000FFFFFFFFFFFFFFFF000000
          00000000001053746F7265642073657373696F6E735821000000000000000600
          0000FFFFFFFFFFFFFFFF0000000000000000084C6F6767696E67582500000000
          00000004000000FFFFFFFFFFFFFFFF00000000010000000C456E7669726F6E6D
          656E74581D0000000000000005000000FFFFFFFFFFFFFFFF0000000000000000
          0453435058240000000000000008000000FFFFFFFFFFFFFFFF00000000010000
          000B436F6E6E656374696F6E581F0000000000000009000000FFFFFFFFFFFFFF
          FF00000000000000000650726F7879581D0000000000000003000000FFFFFFFF
          FFFFFFFF0000000002000000045353485828000000000000000B000000FFFFFF
          FFFFFFFFFF00000000000000000F41757468656E7469636174696F6E581E0000
          00000000000A000000FFFFFFFFFFFFFFFF000000000000000005427567735825
          0000000000000087010000FFFFFFFFFFFFFFFF00000000000000000C50726566
          6572656E63657358}
      end
      object ShowAdvancedLoginOptionsCheck: TCheckBox
        Left = 16
        Top = 298
        Width = 129
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Advanced options'
        TabOrder = 2
        OnClick = DataChange
      end
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 20
    Top = 257
    object LoadSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Load'
      OnExecute = LoadSessionActionExecute
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
      Caption = '&Clean up...'
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
      Caption = 'Desktop &icon'
      OnExecute = DesktopIconActionExecute
    end
    object SendToHookAction: TAction
      Category = 'Sessions'
      Caption = 'Explorer'#39's '#39'Send to'#39' shortcut'
      OnExecute = SendToHookActionExecute
    end
    object CheckForUpdatesAction: TAction
      Tag = 15
      Category = 'Other'
      Caption = 'Check For &Updates'
      ImageIndex = 63
      OnExecute = CheckForUpdatesActionExecute
    end
  end
  object ToolsPopupMenu: TPopupMenu
    Left = 64
    Top = 256
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
    Left = 104
    Top = 256
    object Desktopicon1: TMenuItem
      Action = DesktopIconAction
    end
    object ExplorersSendtoshortcut1: TMenuItem
      Action = SendToHookAction
    end
  end
end
