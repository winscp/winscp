object LoginDialog: TLoginDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 364
  ClientWidth = 532
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    532
    364)
  PixelsPerInch = 96
  TextHeight = 13
  object SaveButton: TButton
    Left = 270
    Top = 333
    Width = 75
    Height = 25
    HelpKeyword = 'ui_login_save'
    Action = SaveSessionAction
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object LoginButton: TButton
    Left = 358
    Top = 333
    Width = 75
    Height = 25
    Action = LoginAction
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CloseButton: TButton
    Left = 446
    Top = 333
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
    Visible = False
  end
  object AboutButton: TButton
    Left = 11
    Top = 333
    Width = 82
    Height = 25
    Action = AboutAction
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    TabStop = False
  end
  object LanguagesButton: TButton
    Left = 105
    Top = 333
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
    Width = 532
    Height = 324
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 6
    object PageControl: TPageControl
      Tag = 6
      Left = 171
      Top = 0
      Width = 361
      Height = 324
      HelpType = htKeyword
      ActivePage = SessionListSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabIndex = 0
      TabOrder = 1
      TabStop = False
      OnChange = PageControlChange
      object SessionListSheet: TTabSheet
        Tag = 1
        Hint = 'Stored sessions'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_stored_sessions'
        Caption = 'StSe'
        DesignSize = (
          353
          269)
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
          Height = 264
          Anchors = [akLeft, akTop, akRight, akBottom]
          Columns = <
            item
            end>
          HideSelection = False
          RowSelect = True
          ParentShowHint = False
          ShowColumnHeaders = False
          ShowHint = True
          TabOrder = 0
          ViewStyle = vsReport
          OnCompare = SessionListViewCompare
          OnCustomDrawItem = SessionListViewCustomDrawItem
          OnDblClick = SessionListViewDblClick
          OnEdited = SessionListViewEdited
          OnEditing = SessionListViewEditing
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
          Top = 131
          Width = 88
          Height = 25
          Action = SetDefaultSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 5
        end
        object ToolsMenuButton: TButton
          Left = 258
          Top = 242
          Width = 88
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Tools...'
          TabOrder = 7
          OnClick = ToolsMenuButtonClick
        end
        object ShellIconsButton: TButton
          Left = 258
          Top = 163
          Width = 88
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Shell &icon...'
          TabOrder = 6
          OnClick = ShellIconsButtonClick
        end
        object RenameButton: TButton
          Left = 257
          Top = 99
          Width = 88
          Height = 25
          Action = RenameSessionAction
          Anchors = [akTop, akRight]
          TabOrder = 4
        end
      end
      object BasicSheet: TTabSheet
        Tag = 2
        Hint = 'Session'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_session'
        Caption = 'Basic'
        ImageIndex = 1
        DesignSize = (
          353
          269)
        object BasicGroup: TXPGroupBox
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
            MaxLength = 100
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
        object TransferProtocolGroup: TXPGroupBox
          Left = 0
          Top = 182
          Width = 345
          Height = 71
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
            Left = 264
            Top = 19
            Width = 65
            Height = 17
            Caption = 'S&CP'
            Checked = True
            TabOrder = 2
            TabStop = True
            OnClick = DataChange
          end
          object SFTPonlyButton: TRadioButton
            Left = 16
            Top = 19
            Width = 65
            Height = 17
            Caption = 'SF&TP'
            TabOrder = 0
            OnClick = DataChange
          end
          object ExternalSSHButton: TRadioButton
            Left = 16
            Top = 42
            Width = 129
            Height = 17
            Caption = 'SSH &Terminal'
            TabOrder = 3
            OnClick = DataChange
          end
          object ExternalSFTPButton: TRadioButton
            Left = 168
            Top = 42
            Width = 129
            Height = 17
            Caption = 'SFTP Ter&minal'
            TabOrder = 4
            OnClick = DataChange
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
      object AdvancedSheet: TTabSheet
        Tag = 3
        Hint = 'SSH'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_ssh'
        Caption = 'SSH'
        ImageIndex = 2
        DesignSize = (
          353
          269)
        object ProtocolGroup: TXPGroupBox
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
        object EncryptionGroup: TXPGroupBox
          Left = 0
          Top = 100
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
            OnDragDrop = AlgListBoxDragDrop
            OnDragOver = AlgListBoxDragOver
            OnStartDrag = AlgListBoxStartDrag
          end
          object Ssh2LegacyDESCheck: TCheckBox
            Left = 16
            Top = 120
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
      object EnvironmentSheet: TTabSheet
        Tag = 4
        Hint = 'Environment'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_environment'
        Caption = 'Env'
        ImageIndex = 6
        DesignSize = (
          353
          269)
        object EOLTypeGroup: TXPGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Server EOL (end-of-line) characters (SCP and SFTP version < 4)'
          TabOrder = 0
          object EOLTypeLFButton: TRadioButton
            Left = 12
            Top = 19
            Width = 133
            Height = 17
            Caption = 'L&F [10] (Unix)'
            TabOrder = 0
            OnClick = DataChange
          end
          object EOLTypeCRLFButton: TRadioButton
            Left = 152
            Top = 19
            Width = 185
            Height = 17
            Caption = '&CR/LF [13/10] (Windows)'
            TabOrder = 1
            OnClick = DataChange
          end
        end
        object ConsiderDSTGroup: TXPGroupBox
          Left = 0
          Top = 59
          Width = 345
          Height = 70
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Daylight saving time'
          TabOrder = 1
          DesignSize = (
            345
            70)
          object ConsiderDSTOnCheck: TRadioButton
            Left = 12
            Top = 19
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Server works in &UTC time (adjust DST locally)'
            TabOrder = 0
            OnClick = DataChange
          end
          object ConsiderDSTOffCheck: TRadioButton
            Left = 12
            Top = 42
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Server adjusts &DST'
            TabOrder = 1
            OnClick = DataChange
          end
        end
        object RecycleBinGroup: TXPGroupBox
          Left = 0
          Top = 137
          Width = 345
          Height = 114
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Recycle bin'
          TabOrder = 2
          DesignSize = (
            345
            114)
          object RecycleBinPathLabel: TLabel
            Left = 11
            Top = 64
            Width = 91
            Height = 13
            Caption = '&Remote recycle bin'
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
            Left = 11
            Top = 81
            Width = 323
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 2
            Text = 'RecycleBinPathEdit'
            OnChange = DataChange
          end
        end
      end
      object DirectoriesSheet: TTabSheet
        Tag = 12
        Hint = 'Directories'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_directories'
        Caption = 'Dir'
        ImageIndex = 11
        DesignSize = (
          353
          269)
        object DirectoriesGroup: TXPGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 153
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directories'
          TabOrder = 0
          DesignSize = (
            345
            153)
          object LocalDirectoryLabel: TLabel
            Left = 11
            Top = 85
            Width = 69
            Height = 13
            Caption = '&Local directory'
            FocusControl = LocalDirectoryEdit
          end
          object RemoteDirectoryLabel: TLabel
            Left = 11
            Top = 42
            Width = 80
            Height = 13
            Caption = '&Remote directory'
            FocusControl = RemoteDirectoryEdit
          end
          object LocalDirectoryDescLabel: TLabel
            Left = 11
            Top = 127
            Width = 251
            Height = 13
            Caption = 'Local directory is not used with explorer-like interface.'
          end
          object LocalDirectoryEdit: TDirectoryEdit
            Left = 11
            Top = 102
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
            Left = 11
            Top = 59
            Width = 323
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 1
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
        end
        object DirectoryOptionsGroup: TXPGroupBox
          Left = 0
          Top = 167
          Width = 345
          Height = 89
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory reading options'
          TabOrder = 1
          DesignSize = (
            345
            89)
          object CacheDirectoriesCheck: TCheckBox
            Left = 11
            Top = 20
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &visited remote directories'
            TabOrder = 0
            OnClick = DataChange
          end
          object CacheDirectoryChangesCheck: TCheckBox
            Left = 11
            Top = 40
            Width = 182
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &directory changes'
            TabOrder = 1
            OnClick = DataChange
          end
          object ResolveSymlinksCheck: TCheckBox
            Left = 11
            Top = 60
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Resolve symbolic li&nks'
            TabOrder = 3
          end
          object PreserveDirectoryChangesCheck: TCheckBox
            Left = 202
            Top = 40
            Width = 139
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Permanent cache'
            TabOrder = 2
          end
        end
      end
      object ScpSheet: TTabSheet
        Tag = 5
        Hint = 'SCP'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_scp'
        Caption = 'Scp'
        ImageIndex = 3
        DesignSize = (
          353
          269)
        object OtherShellOptionsGroup: TXPGroupBox
          Left = 0
          Top = 171
          Width = 345
          Height = 92
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other options'
          TabOrder = 3
          DesignSize = (
            345
            92)
          object Label29: TLabel
            Left = 13
            Top = 67
            Width = 105
            Height = 13
            Caption = 'Server time&zone offset'
            FocusControl = TimeDifferenceEdit
          end
          object Label30: TLabel
            Left = 196
            Top = 67
            Width = 26
            Height = 13
            Caption = 'hours'
            FocusControl = TimeDifferenceEdit
          end
          object Label9: TLabel
            Left = 298
            Top = 67
            Width = 36
            Height = 13
            Caption = 'minutes'
            FocusControl = TimeDifferenceMinutesEdit
          end
          object LookupUserGroupsCheck: TCheckBox
            Left = 12
            Top = 17
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
          object Scp1CompatibilityCheck: TCheckBox
            Left = 152
            Top = 38
            Width = 185
            Height = 17
            Caption = 'Use scp&2 with scp1 compat.'
            TabOrder = 3
            OnClick = DataChange
          end
          object TimeDifferenceEdit: TUpDownEdit
            Left = 137
            Top = 62
            Width = 54
            Height = 21
            Alignment = taRightJustify
            MaxValue = 13
            MinValue = -13
            Value = -13
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = DataChange
          end
          object TimeDifferenceMinutesEdit: TUpDownEdit
            Left = 239
            Top = 62
            Width = 54
            Height = 21
            Alignment = taRightJustify
            Increment = 15
            MaxValue = 45
            MinValue = -45
            Value = -13
            Anchors = [akTop, akRight]
            TabOrder = 5
            OnChange = DataChange
          end
        end
        object ReturnVarGroup: TXPGroupBox
          Left = 0
          Top = 54
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
          Top = 6
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
        object ScpLsOptionsGroup: TXPGroupBox
          Left = 0
          Top = 103
          Width = 345
          Height = 63
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory listing'
          TabOrder = 2
          object IgnoreLsWarningsCheck: TCheckBox
            Left = 12
            Top = 17
            Width = 140
            Height = 17
            Caption = 'Ignore LS &warnings'
            TabOrder = 0
            OnClick = DataChange
          end
          object AliasGroupListCheck: TCheckBox
            Left = 152
            Top = 17
            Width = 185
            Height = 17
            Caption = 'Alias LS to display g&roup name'
            TabOrder = 1
            OnClick = DataChange
          end
          object SCPLsFullTimeAutoCheck: TCheckBox
            Left = 12
            Top = 38
            Width = 325
            Height = 17
            Caption = 'Try to get &full timestamp'
            TabOrder = 2
            OnClick = DataChange
          end
        end
      end
      object SftpSheet: TTabSheet
        Tag = 13
        Hint = 'SFTP'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_sftp'
        Caption = 'Sftp'
        ImageIndex = 12
        DesignSize = (
          353
          269)
        object SFTPBugsGroupBox: TXPGroupBox
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
            Width = 222
            Height = 13
            Caption = '&Reverses order of symlink command arguments'
            FocusControl = SFTPBugSymlinkCombo
          end
          object Label36: TLabel
            Left = 12
            Top = 44
            Width = 192
            Height = 13
            Caption = '&Misinterprets file timestamps prior to 1970'
            FocusControl = SFTPBugSignedTSCombo
          end
          object SFTPBugSymlinkCombo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 0
          end
          object SFTPBugSignedTSCombo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 1
          end
        end
        object SFTPProtocolGroup: TXPGroupBox
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
            Top = 20
            Width = 151
            Height = 13
            Caption = '&Preferred SFTP protocol version'
            FocusControl = SFTPMaxVersionCombo
          end
          object Label35: TLabel
            Left = 12
            Top = 44
            Width = 128
            Height = 13
            Caption = 'Server does not use &UTF-8'
            FocusControl = SFTPBugUtfCombo
          end
          object SFTPMaxVersionCombo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 0
            Items.Strings = (
              '0'
              '1'
              '2'
              '3'
              '4'
              '5')
          end
          object SFTPBugUtfCombo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 1
          end
        end
      end
      object LogSheet: TTabSheet
        Tag = 6
        Hint = 'Logging'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_logging'
        Caption = 'Log'
        ImageIndex = 4
        inline LoggingFrame: TLoggingFrame
          Left = -3
          Top = 0
          Width = 356
          Height = 241
          TabOrder = 0
          DesignSize = (
            356
            241)
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
        HelpType = htKeyword
        HelpKeyword = 'ui_login_preferences'
        Caption = 'Int'
        ImageIndex = 5
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
          inherited InterfaceGroup: TXPGroupBox
            Width = 345
            inherited CommanderDescriptionLabel2: TLabel
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
        HelpType = htKeyword
        HelpKeyword = 'ui_login_connection'
        Caption = 'Conn'
        ImageIndex = 7
        DesignSize = (
          353
          269)
        object TimeoutGroup: TXPGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 46
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Timeouts'
          TabOrder = 0
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
        object PingGroup: TXPGroupBox
          Left = 0
          Top = 58
          Width = 345
          Height = 111
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 1
          DesignSize = (
            345
            111)
          object PingIntervalLabel: TLabel
            Left = 12
            Top = 84
            Width = 140
            Height = 13
            Caption = 'Seconds &between keepalives'
            FocusControl = PingIntervalSecEdit
          end
          object PingIntervalSecEdit: TUpDownEdit
            Left = 208
            Top = 79
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
            Top = 39
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sending of &null SSH packets'
            TabOrder = 1
            OnClick = DataChange
          end
          object PingDummyCommandButton: TRadioButton
            Left = 12
            Top = 59
            Width = 317
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Executing &dummy protocol commands'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object IPvGroup: TXPGroupBox
          Left = 0
          Top = 176
          Width = 345
          Height = 45
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Internet protocol version'
          TabOrder = 2
          DesignSize = (
            345
            45)
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
      end
      object ProxySheet: TTabSheet
        Tag = 9
        Hint = 'Proxy'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_proxy'
        Caption = 'Proxy'
        ImageIndex = 8
        DesignSize = (
          353
          269)
        object ProxyTypeGroup: TXPGroupBox
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
          object ProxyHostLabel: TLabel
            Left = 11
            Top = 41
            Width = 78
            Height = 13
            Caption = 'Pro&xy host name'
            FocusControl = ProxyHostEdit
          end
          object ProxyPortLabel: TLabel
            Left = 240
            Top = 41
            Width = 57
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number'
            FocusControl = ProxyPortEdit
          end
          object ProxyUsernameLabel: TLabel
            Left = 11
            Top = 85
            Width = 51
            Height = 13
            Caption = '&User name'
            FocusControl = ProxyUsernameEdit
          end
          object ProxyPasswordLabel: TLabel
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
            TabOrder = 6
            OnChange = DataChange
          end
          object ProxyHostEdit: TEdit
            Left = 11
            Top = 58
            Width = 214
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 5
            Text = 'ProxyHostEdit'
            OnChange = DataChange
          end
          object ProxyUsernameEdit: TEdit
            Left = 11
            Top = 102
            Width = 137
            Height = 21
            MaxLength = 50
            TabOrder = 7
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
            TabOrder = 8
            Text = 'ProxyPasswordEdit'
            OnChange = DataChange
          end
          object ProxySocks4Button: TRadioButton
            Left = 81
            Top = 19
            Width = 77
            Height = 17
            Caption = 'SOCKS&4'
            TabOrder = 1
            OnClick = DataChange
          end
          object ProxySocks5Button: TRadioButton
            Left = 148
            Top = 19
            Width = 67
            Height = 17
            Caption = 'SOCKS&5'
            TabOrder = 2
            OnClick = DataChange
          end
          object ProxyHTTPButton: TRadioButton
            Left = 215
            Top = 19
            Width = 66
            Height = 17
            Caption = '&HTTP'
            TabOrder = 3
            OnClick = DataChange
          end
          object ProxyTelnetButton: TRadioButton
            Left = 282
            Top = 19
            Width = 61
            Height = 17
            Caption = '&Telnet'
            TabOrder = 4
            OnClick = DataChange
          end
        end
        object ProxySettingsGroup: TXPGroupBox
          Left = 0
          Top = 147
          Width = 345
          Height = 108
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy settings'
          TabOrder = 1
          DesignSize = (
            345
            108)
          object ProxyTelnetCommandLabel: TLabel
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
        HelpType = htKeyword
        HelpKeyword = 'ui_login_bugs'
        Caption = 'Bugs'
        ImageIndex = 9
        DesignSize = (
          353
          269)
        object BugsGroupBox: TXPGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 217
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SSH servers'
          TabOrder = 0
          DesignSize = (
            345
            217)
          object BugIgnore1Label: TLabel
            Left = 12
            Top = 20
            Width = 167
            Height = 13
            Caption = 'Chokes on SSH-1 &ignore messages'
            FocusControl = BugIgnore1Combo
          end
          object BugPlainPW1Label: TLabel
            Left = 12
            Top = 44
            Width = 192
            Height = 13
            Caption = 'Refuses all SSH-1 pass&word camouflage'
            FocusControl = BugPlainPW1Combo
          end
          object BugRSA1Label: TLabel
            Left = 12
            Top = 68
            Width = 180
            Height = 13
            Caption = 'Chokes on SSH-1 &RSA authentication'
            FocusControl = BugRSA1Combo
          end
          object BugHMAC2Label: TLabel
            Left = 12
            Top = 92
            Width = 155
            Height = 13
            Caption = 'Miscomputes SSH-2 H&MAC keys'
            FocusControl = BugHMAC2Combo
          end
          object BugDeriveKey2Label: TLabel
            Left = 12
            Top = 116
            Width = 173
            Height = 13
            Caption = 'Miscomputes SSH-2 &encryption keys'
            FocusControl = BugDeriveKey2Combo
          end
          object BugRSAPad2Label: TLabel
            Left = 12
            Top = 140
            Width = 208
            Height = 13
            Caption = 'Requires &padding on SSH-2 RSA signatures'
            FocusControl = BugRSAPad2Combo
          end
          object BugPKSessID2Label: TLabel
            Left = 12
            Top = 164
            Width = 194
            Height = 13
            Caption = 'Misuses the sessio&n ID in SSH-2 PK auth'
            FocusControl = BugPKSessID2Combo
          end
          object BugRekey2Label: TLabel
            Left = 12
            Top = 188
            Width = 183
            Height = 13
            Caption = 'Handles SSH-2 &key re-exchange badly'
            FocusControl = BugRekey2Combo
          end
          object BugIgnore1Combo: TComboBox
            Left = 272
            Top = 15
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 0
          end
          object BugPlainPW1Combo: TComboBox
            Left = 272
            Top = 39
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 1
          end
          object BugRSA1Combo: TComboBox
            Left = 272
            Top = 63
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 2
          end
          object BugHMAC2Combo: TComboBox
            Left = 272
            Top = 87
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 3
          end
          object BugDeriveKey2Combo: TComboBox
            Left = 272
            Top = 111
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 4
          end
          object BugRSAPad2Combo: TComboBox
            Left = 272
            Top = 135
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 5
          end
          object BugPKSessID2Combo: TComboBox
            Left = 272
            Top = 159
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 6
          end
          object BugRekey2Combo: TComboBox
            Left = 272
            Top = 183
            Width = 61
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 0
            TabOrder = 7
          end
        end
      end
      object AuthSheet: TTabSheet
        Tag = 11
        Hint = 'Authentication'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_authentication'
        Caption = 'Auth'
        ImageIndex = 10
        DesignSize = (
          353
          269)
        object AuthenticationGroup: TXPGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 145
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication options'
          TabOrder = 0
          DesignSize = (
            345
            145)
          object AuthTISCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 321
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt &TIS or CryptoCard authentication (SSH-1)'
            TabOrder = 0
            OnClick = DataChange
          end
          object AgentFwdCheck: TCheckBox
            Left = 12
            Top = 115
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow agent &forwarding'
            TabOrder = 4
            OnClick = DataChange
          end
          object AuthKICheck: TCheckBox
            Left = 12
            Top = 43
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt '#39'keyboard-&interactive'#39' authentication (SSH-2)'
            TabOrder = 1
            OnClick = DataChange
          end
          object AuthKIPasswordCheck: TCheckBox
            Left = 32
            Top = 67
            Width = 305
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Respond with &password to the first prompt'
            TabOrder = 2
            OnClick = DataChange
          end
          object AuthGSSAPICheck: TCheckBox
            Left = 12
            Top = 91
            Width = 325
            Height = 17
            Caption = 'Attempt MIT Kerberos 5 &GSSAPI authentication (SSH-2)'
            TabOrder = 3
            OnClick = AuthGSSAPICheckClick
          end
        end
      end
      object KexSheet: TTabSheet
        Tag = 14
        Hint = 'Key exchange'
        HelpType = htKeyword
        HelpKeyword = 'ui_login_kex'
        Caption = 'KEX'
        ImageIndex = 13
        DesignSize = (
          353
          269)
        object KexOptionsGroup: TXPGroupBox
          Left = 0
          Top = 6
          Width = 345
          Height = 117
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
            Left = 11
            Top = 36
            Width = 190
            Height = 69
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
        object KexReexchangeGroup: TXPGroupBox
          Left = 0
          Top = 130
          Width = 345
          Height = 69
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options controlling key re-exchange'
          TabOrder = 1
          object Label31: TLabel
            Left = 12
            Top = 20
            Width = 186
            Height = 13
            Caption = 'Max &minutes before rekey (0 for no limit)'
            Color = clBtnFace
            FocusControl = RekeyTimeEdit
            ParentColor = False
          end
          object Label32: TLabel
            Left = 12
            Top = 44
            Width = 171
            Height = 13
            Caption = 'Ma&x data before rekey (0 for no limit)'
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
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 171
      Height = 324
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        171
        324)
      object SimpleNavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 155
        Height = 288
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        RightClickSelect = True
        TabOrder = 0
        OnChange = NavigationTreeChange
        Items.Data = {
          0400000021000000000000000200000000000000FFFFFFFF0000000001000000
          0853657373696F6E5829000000000000000100000000000000FFFFFFFF000000
          00000000001053746F7265642073657373696F6E735825000000000000000400
          000000000000FFFFFFFF00000000010000000C456E7669726F6E6D656E745825
          000000000000000C000000FFFFFFFFFFFFFFFF00000000000000000C44697265
          63746F72696573581D000000000000000300000000000000FFFFFFFF00000000
          00000000045353485825000000000000000700000020000000FFFFFFFF000000
          00000000000C507265666572656E63657358}
      end
      object AdvancedNavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 155
        Height = 288
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnChange = NavigationTreeChange
        Items.Data = {
          0500000021000000000000000200000000000000FFFFFFFF0000000002000000
          0853657373696F6E5829000000000000000100000000000000FFFFFFFF000000
          00000000001053746F7265642073657373696F6E735821000000000000000600
          000000000000FFFFFFFF0000000000000000084C6F6767696E67582500000000
          0000000400000000000000FFFFFFFF00000000030000000C456E7669726F6E6D
          656E745825000000000000000C00000000000000FFFFFFFF0000000000000000
          0C4469726563746F72696573581E000000000000000D00000000000000FFFFFF
          FF00000000000000000553465450581D000000000000000500000000000000FF
          FFFFFF0000000000000000045343505824000000000000000800000000000000
          FFFFFFFF00000000010000000B436F6E6E656374696F6E581F00000000000000
          0900000000000000FFFFFFFF00000000000000000650726F7879581D00000000
          0000000300000000000000FFFFFFFF0000000003000000045353485826000000
          000000000E00000000000000FFFFFFFF00000000000000000D4B657820657863
          68616E67655828000000000000000B00000000000000FFFFFFFF000000000000
          00000F41757468656E7469636174696F6E581E000000000000000A0000000000
          0000FFFFFFFF0000000000000000054275677358250000000000000007000000
          20000000FFFFFFFF00000000000000000C507265666572656E63657358}
      end
      object ShowAdvancedLoginOptionsCheck: TCheckBox
        Left = 16
        Top = 304
        Width = 139
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = '&Advanced options'
        TabOrder = 2
        OnClick = DataChange
      end
    end
  end
  object HelpButton: TButton
    Left = 446
    Top = 333
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = HelpButtonClick
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 12
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
    object RenameSessionAction: TAction
      Category = 'Sessions'
      Caption = '&Rename'
      OnExecute = RenameSessionActionExecute
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
    Left = 144
    Top = 257
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
end
