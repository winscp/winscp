object SiteAdvancedDialog: TSiteAdvancedDialog
  Left = 351
  Top = 167
  HelpType = htKeyword
  HelpKeyword = 'ui_login_advanced'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Advanced Site Settings'
  ClientHeight = 429
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    619
    429)
  TextHeight = 15
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 619
    Height = 398
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 161
      Top = 0
      Width = 458
      Height = 398
      HelpType = htKeyword
      ActivePage = EnvironmentSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabOrder = 1
      TabStop = False
      OnChange = PageControlChange
      object EnvironmentSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_login_environment'
        Caption = 'Environment'
        ImageIndex = 6
        TabVisible = False
        DesignSize = (
          450
          388)
        object EnvironmentGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 156
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Server environment'
          TabOrder = 0
          DesignSize = (
            438
            156)
          object EOLTypeLabel: TLabel
            Left = 9
            Top = 22
            Width = 261
            Height = 15
            Caption = 'End-of-line &characters (if not indicated by server):'
            FocusControl = EOLTypeCombo
          end
          object UtfLabel: TLabel
            Left = 9
            Top = 51
            Width = 160
            Height = 15
            Caption = '&UTF-8 encoding for filenames:'
            FocusControl = UtfCombo
          end
          object TimeDifferenceLabel: TLabel
            Left = 9
            Top = 80
            Width = 91
            Height = 15
            Caption = 'Time &zone offset:'
            FocusControl = TimeDifferenceEdit
          end
          object TimeDifferenceHoursLabel: TLabel
            Left = 245
            Top = 80
            Width = 30
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'hours'
            FocusControl = TimeDifferenceEdit
          end
          object TimeDifferenceMinutesLabel: TLabel
            Left = 376
            Top = 80
            Width = 43
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'minutes'
            FocusControl = TimeDifferenceMinutesEdit
          end
          object EOLTypeCombo: TComboBox
            Left = 362
            Top = 19
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
            Items.Strings = (
              'LF'
              'CR/LF')
          end
          object UtfCombo: TComboBox
            Left = 362
            Top = 48
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
          end
          object TimeDifferenceEdit: TUpDownEdit
            Left = 163
            Top = 77
            Width = 76
            Height = 23
            Alignment = taRightJustify
            MaxValue = 25.000000000000000000
            MinValue = -25.000000000000000000
            Value = -13.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnChange = DataChange
          end
          object TimeDifferenceMinutesEdit: TUpDownEdit
            Left = 294
            Top = 77
            Width = 76
            Height = 23
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 45.000000000000000000
            MinValue = -45.000000000000000000
            Value = -13.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object TimeDifferenceAutoCheck: TCheckBox
            Left = 165
            Top = 106
            Width = 264
            Height = 17
            Anchors = [akTop, akRight]
            Caption = 'Detect &automatically'
            TabOrder = 4
            OnClick = DataChange
          end
          object TrimVMSVersionsCheck: TCheckBox
            Left = 11
            Top = 129
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Trim VMS version numbers'
            TabOrder = 5
            OnClick = DataChange
          end
        end
        object DSTModeGroup: TGroupBox
          Left = 8
          Top = 164
          Width = 438
          Height = 93
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Daylight saving time'
          TabOrder = 1
          DesignSize = (
            438
            93)
          object DSTModeUnixCheck: TRadioButton
            Left = 11
            Top = 23
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Adjust remote timestamp to local co&nventions'
            TabOrder = 0
            OnClick = DataChange
          end
          object DSTModeWinCheck: TRadioButton
            Left = 11
            Top = 46
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Adjust remote timestamp with &DST'
            TabOrder = 1
            OnClick = DataChange
          end
          object DSTModeKeepCheck: TRadioButton
            Left = 11
            Top = 69
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Preser&ve remote timestamp'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object PuttyGroup: TGroupBox
          Left = 8
          Top = 263
          Width = 438
          Height = 103
          Anchors = [akLeft, akTop, akRight]
          Caption = 'PuTTY'
          TabOrder = 2
          DesignSize = (
            438
            103)
          object PuttySettingsLabel: TLabel
            Left = 9
            Top = 22
            Width = 129
            Height = 15
            Caption = '&PuTTY terminal settings:'
            FocusControl = EncryptKeyPasswordEdit
          end
          object PuttySettingsButton: TButton
            Left = 9
            Top = 69
            Width = 138
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Edit in PuTTY...'
            TabOrder = 1
            OnClick = PuttySettingsButtonClick
          end
          object PuttySettingsEdit: TEdit
            Left = 9
            Top = 40
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 64
            TabOrder = 0
            Text = 'PuttySettingsEdit'
            OnChange = DataChange
            OnExit = EncryptKeyEditExit
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
          450
          388)
        object DirectoriesGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 185
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directories'
          TabOrder = 0
          DesignSize = (
            438
            185)
          object LocalDirectoryLabel: TLabel
            Left = 9
            Top = 115
            Width = 81
            Height = 15
            Caption = '&Local directory:'
            FocusControl = LocalDirectoryEdit
          end
          object RemoteDirectoryLabel: TLabel
            Left = 9
            Top = 68
            Width = 94
            Height = 15
            Caption = '&Remote directory:'
            FocusControl = RemoteDirectoryEdit
          end
          object LocalDirectoryDescLabel: TLabel
            Left = 9
            Top = 160
            Width = 261
            Height = 15
            Caption = 'Local directory is not used with Explorer interface.'
            ShowAccelChar = False
          end
          object LocalDirectoryEdit: TDirectoryEdit
            Left = 9
            Top = 133
            Width = 420
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
            Left = 9
            Top = 86
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 2
            Text = 'RemoteDirectoryEdit'
            OnChange = DataChange
          end
          object UpdateDirectoriesCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Re&member last used directory'
            TabOrder = 1
          end
          object SynchronizeBrowsingCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Syn&chronize browsing'
            TabOrder = 0
          end
        end
        object DirectoryOptionsGroup: TGroupBox
          Left = 8
          Top = 193
          Width = 438
          Height = 116
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory reading options'
          TabOrder = 1
          DesignSize = (
            438
            116)
          object CacheDirectoriesCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &visited remote directories'
            TabOrder = 0
            OnClick = DataChange
          end
          object CacheDirectoryChangesCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 275
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Cache &directory changes'
            TabOrder = 1
            OnClick = DataChange
          end
          object ResolveSymlinksCheck: TCheckBox
            Left = 11
            Top = 68
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Resolve symbolic li&nks'
            TabOrder = 3
          end
          object PreserveDirectoryChangesCheck: TCheckBox
            Left = 266
            Top = 45
            Width = 163
            Height = 17
            Anchors = [akTop, akRight]
            Caption = '&Permanent cache'
            TabOrder = 2
          end
          object FollowDirectorySymlinksCheck: TCheckBox
            Left = 11
            Top = 91
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Follow symbolic links to directories'
            TabOrder = 4
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
          450
          388)
        object RecycleBinGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 119
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Recycle bin'
          TabOrder = 0
          DesignSize = (
            438
            119)
          object RecycleBinPathLabel: TLabel
            Left = 9
            Top = 68
            Width = 104
            Height = 15
            Caption = '&Remote recycle bin:'
            FocusControl = RecycleBinPathEdit
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Preserve deleted remote files to recycle bin'
            TabOrder = 0
            OnClick = DataChange
          end
          object OverwrittenToRecycleBinCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Preserve &overwritten remote files to recycle bin (SFTP only)'
            TabOrder = 1
            OnClick = DataChange
          end
          object RecycleBinPathEdit: TEdit
            Left = 9
            Top = 86
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 2
            Text = 'RecycleBinPathEdit'
            OnChange = DataChange
          end
        end
      end
      object EncryptionSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_encryption'
        Caption = 'Encryption'
        TabVisible = False
        DesignSize = (
          450
          388)
        object EncryptFilesCheck: TCheckBox
          Left = 8
          Top = 8
          Width = 438
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = '&Encrypt files'
          TabOrder = 0
          OnClick = DataChange
        end
        object EncryptFilesGroup: TGroupBox
          Left = 8
          Top = 31
          Width = 438
          Height = 127
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Encryption options'
          TabOrder = 1
          DesignSize = (
            438
            127)
          object Label13: TLabel
            Left = 9
            Top = 22
            Width = 81
            Height = 15
            Caption = 'Encryption &key:'
            FocusControl = EncryptKeyPasswordEdit
          end
          object EncryptKeyVisibleEdit: TEdit
            Left = 9
            Top = 40
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 64
            TabOrder = 1
            Text = 'EncryptKeyVisibleEdit'
            Visible = False
            OnChange = DataChange
            OnExit = EncryptKeyEditExit
          end
          object EncryptKeyPasswordEdit: TPasswordEdit
            Left = 9
            Top = 40
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 64
            TabOrder = 0
            Text = 'EncryptKeyPasswordEdit'
            OnChange = DataChange
            OnExit = EncryptKeyEditExit
          end
          object ShowEncryptionKeyCheck: TCheckBox
            Left = 11
            Top = 69
            Width = 418
            Height = 17
            Caption = '&Show key'
            TabOrder = 2
            OnClick = ShowEncryptionKeyCheckClick
          end
          object GenerateKeyButton: TButton
            Left = 9
            Top = 92
            Width = 129
            Height = 25
            Caption = '&Generate Key'
            TabOrder = 3
            OnClick = GenerateKeyButtonClick
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
          450
          388)
        object SFTPBugsGroupBox: TGroupBox
          Left = 8
          Top = 164
          Width = 438
          Height = 77
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SFTP servers'
          TabOrder = 1
          DesignSize = (
            438
            77)
          object Label10: TLabel
            Left = 9
            Top = 22
            Width = 233
            Height = 15
            Caption = '&Reverses order of link command arguments:'
            FocusControl = SFTPBugSymlinkCombo
          end
          object Label36: TLabel
            Left = 9
            Top = 47
            Width = 225
            Height = 15
            Caption = '&Misinterprets file timestamps prior to 1970:'
            FocusControl = SFTPBugSignedTSCombo
          end
          object SFTPBugSymlinkCombo: TComboBox
            Left = 362
            Top = 15
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
          end
          object SFTPBugSignedTSCombo: TComboBox
            Left = 362
            Top = 44
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
          end
        end
        object SFTPProtocolGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 156
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            438
            156)
          object Label34: TLabel
            Left = 9
            Top = 51
            Width = 169
            Height = 15
            Caption = '&Preferred SFTP protocol version:'
            FocusControl = SFTPMaxVersionCombo
          end
          object Label23: TLabel
            Left = 9
            Top = 22
            Width = 63
            Height = 15
            Caption = 'SFTP ser&ver:'
            FocusControl = SftpServerEdit
          end
          object Label5: TLabel
            Left = 9
            Top = 80
            Width = 170
            Height = 15
            Caption = '&Canonicalize paths on the server'
            FocusControl = SFTPRealPathCombo
          end
          object SFTPMaxVersionCombo: TComboBox
            Left = 362
            Top = 48
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            Items.Strings = (
              'Auto'
              '0'
              '1'
              '2'
              '3'
              '4'
              '5'
              '6')
          end
          object SftpServerEdit: TComboBox
            Left = 152
            Top = 19
            Width = 277
            Height = 23
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 0
            Text = 'SftpServerEdit'
            OnChange = DataChange
            Items.Strings = (
              'Default'
              '/bin/sftp-server'
              'sudo su -c /bin/sftp-server')
          end
          object AllowScpFallbackCheck: TCheckBox
            Left = 11
            Top = 106
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow SCP &fallback'
            TabOrder = 3
            OnClick = DataChange
          end
          object SFTPRealPathCombo: TComboBox
            Left = 362
            Top = 77
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 2
          end
          object UsePosixRenameCheck: TCheckBox
            Left = 11
            Top = 129
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use POSIX rename'
            TabOrder = 4
            OnClick = DataChange
          end
        end
      end
      object ScpSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_scp'
        Caption = 'SCP/ShellX'
        ImageIndex = 3
        TabVisible = False
        DesignSize = (
          450
          388)
        object OtherShellOptionsGroup: TGroupBox
          Left = 8
          Top = 170
          Width = 438
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other options'
          TabOrder = 2
          DesignSize = (
            438
            72)
          object LookupUserGroupsCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 172
            Height = 17
            AllowGrayed = True
            Caption = 'Lookup &user groups'
            TabOrder = 0
            OnClick = DataChange
          end
          object ClearAliasesCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 172
            Height = 17
            Caption = 'Clear a&liases'
            TabOrder = 2
            OnClick = DataChange
          end
          object UnsetNationalVarsCheck: TCheckBox
            Left = 189
            Top = 22
            Width = 240
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Clear &national variables'
            TabOrder = 1
            OnClick = DataChange
          end
          object Scp1CompatibilityCheck: TCheckBox
            Left = 189
            Top = 45
            Width = 240
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use scp&2 with scp1 compatibility'
            TabOrder = 3
            OnClick = DataChange
          end
        end
        object ShellGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 81
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shell'
          TabOrder = 0
          DesignSize = (
            438
            81)
          object Label19: TLabel
            Left = 9
            Top = 22
            Width = 28
            Height = 15
            Caption = 'S&hell:'
            FocusControl = ShellEdit
          end
          object Label20: TLabel
            Left = 9
            Top = 51
            Width = 111
            Height = 15
            Caption = '&Return code variable:'
            FocusControl = ReturnVarEdit
          end
          object ShellEdit: TComboBox
            Left = 187
            Top = 19
            Width = 242
            Height = 23
            AutoComplete = False
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 0
            Text = 'ShellEdit'
            Items.Strings = (
              'Default'
              '/bin/bash'
              '/bin/ksh'
              '/bin/sh'
              'sudo su -')
          end
          object ReturnVarEdit: TComboBox
            Left = 187
            Top = 48
            Width = 242
            Height = 23
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
          Left = 8
          Top = 89
          Width = 438
          Height = 75
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Directory listing'
          TabOrder = 1
          DesignSize = (
            438
            75)
          object Label9: TLabel
            Left = 9
            Top = 22
            Width = 96
            Height = 15
            Caption = 'Listing &command:'
            FocusControl = ListingCommandEdit
          end
          object IgnoreLsWarningsCheck: TCheckBox
            Left = 11
            Top = 48
            Width = 172
            Height = 17
            Caption = 'Ignore LS &warnings'
            TabOrder = 1
            OnClick = DataChange
          end
          object SCPLsFullTimeAutoCheck: TCheckBox
            Left = 189
            Top = 48
            Width = 240
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Try to get &full timestamp'
            TabOrder = 2
            OnClick = DataChange
          end
          object ListingCommandEdit: TComboBox
            Left = 187
            Top = 19
            Width = 242
            Height = 23
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
          450
          388)
        object FtpGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 274
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            438
            274)
          object Label25: TLabel
            Left = 9
            Top = 48
            Width = 119
            Height = 15
            Caption = 'Post login &commands:'
            FocusControl = PostLoginCommandsMemo
          end
          object FtpListAllLabel: TLabel
            Left = 9
            Top = 157
            Width = 176
            Height = 15
            Caption = '&Support for listing of hidden files:'
            FocusControl = FtpListAllCombo
          end
          object Label24: TLabel
            Left = 9
            Top = 128
            Width = 214
            Height = 15
            Caption = 'Use &MLSD command for directory listing'
            FocusControl = FtpUseMlsdCombo
          end
          object FtpForcePasvIpLabel: TLabel
            Left = 9
            Top = 186
            Width = 249
            Height = 15
            Caption = '&Force IP address for passive mode connections:'
            FocusControl = FtpForcePasvIpCombo
          end
          object FtpAccountLabel: TLabel
            Left = 9
            Top = 22
            Width = 48
            Height = 15
            Caption = '&Account:'
            FocusControl = FtpAccountEdit
          end
          object Label3: TLabel
            Left = 9
            Top = 215
            Width = 255
            Height = 15
            Caption = 'Use &HOST command to select host on the server'
            FocusControl = FtpHostCombo
          end
          object PostLoginCommandsMemo: TMemo
            Left = 9
            Top = 66
            Width = 420
            Height = 53
            Anchors = [akLeft, akTop, akRight]
            ScrollBars = ssVertical
            TabOrder = 1
          end
          object FtpListAllCombo: TComboBox
            Left = 362
            Top = 154
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object FtpForcePasvIpCombo: TComboBox
            Left = 362
            Top = 183
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = DataChange
          end
          object FtpUseMlsdCombo: TComboBox
            Left = 362
            Top = 125
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnChange = DataChange
          end
          object FtpAccountEdit: TEdit
            Left = 168
            Top = 19
            Width = 261
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 0
            Text = 'FtpAccountEdit'
            OnChange = DataChange
          end
          object FtpHostCombo: TComboBox
            Left = 362
            Top = 212
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 5
            OnChange = DataChange
          end
          object VMSAllRevisionsCheck: TCheckBox
            Left = 11
            Top = 241
            Width = 418
            Height = 17
            Caption = 'Display all file &revisions on VMS servers'
            TabOrder = 6
          end
        end
      end
      object S3Sheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_s3'
        Caption = 'S3'
        ImageIndex = 16
        TabVisible = False
        DesignSize = (
          450
          388)
        object S3Group: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 104
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            438
            104)
          object Label27: TLabel
            Left = 9
            Top = 22
            Width = 78
            Height = 15
            Caption = '&Default region:'
            FocusControl = S3DefaultReqionCombo
          end
          object S3UrlStyleLabel: TLabel
            Left = 9
            Top = 51
            Width = 51
            Height = 15
            Caption = '&URL style:'
            FocusControl = S3UrlStyleCombo
          end
          object S3DefaultReqionCombo: TComboBox
            Left = 170
            Top = 19
            Width = 258
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            MaxLength = 32
            TabOrder = 0
            Text = 'S3DefaultRegionCombo'
            OnChange = DataChange
            Items.Strings = (
              'af-south-1'
              'ap-east-1'
              'ap-east-2'
              'ap-northeast-1'
              'ap-northeast-2'
              'ap-northeast-3'
              'ap-south-1'
              'ap-south-2'
              'ap-southeast-1'
              'ap-southeast-2'
              'ap-southeast-3'
              'ap-southeast-4'
              'ap-southeast-5'
              'ap-southeast-7'
              'ca-central-1'
              'ca-west-1'
              'cn-north-1'
              'cn-northwest-1'
              'eu-central-1'
              'eu-central-2'
              'eu-north-1'
              'eu-south-1'
              'eu-south-2'
              'eu-west-1'
              'eu-west-2'
              'eu-west-3'
              'il-central-1'
              'me-central-1'
              'me-south-1'
              'mx-central-1'
              'sa-east-1'
              'us-east-1'
              'us-east-2'
              'us-gov-east-1'
              'us-gov-west-1'
              'us-west-1'
              'us-west-2')
          end
          object S3UrlStyleCombo: TComboBox
            Left = 170
            Top = 48
            Width = 258
            Height = 23
            AutoComplete = False
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 1
            Items.Strings = (
              'Virtual Host'
              'Path')
          end
          object S3RequesterPaysCheck: TCheckBox
            Left = 11
            Top = 77
            Width = 420
            Height = 17
            Caption = 'Requester &pays'
            TabOrder = 2
          end
        end
        object S3AuthenticationGroup: TGroupBox
          Left = 8
          Top = 112
          Width = 438
          Height = 187
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication'
          TabOrder = 1
          DesignSize = (
            438
            187)
          object S3SessionTokenLabel: TLabel
            Left = 9
            Top = 20
            Width = 75
            Height = 15
            Caption = '&Session token:'
            FocusControl = S3SessionTokenMemo
          end
          object S3RoleArnLabel: TLabel
            Left = 9
            Top = 135
            Width = 53
            Height = 15
            Caption = '&Role ARN:'
            FocusControl = S3RoleArnEdit
          end
          object S3SessionTokenMemo: TMemo
            Left = 9
            Top = 36
            Width = 419
            Height = 93
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 10000
            TabOrder = 0
            OnChange = DataChange
            OnKeyDown = NoteMemoKeyDown
          end
          object S3RoleArnEdit: TEdit
            Left = 9
            Top = 153
            Width = 419
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Text = 'S3RoleArnEdit'
            OnChange = DataChange
          end
        end
      end
      object WebDavSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_webdav'
        Caption = 'WebDAV'
        ImageIndex = 17
        TabVisible = False
        DesignSize = (
          450
          388)
        object WebdavGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            438
            49)
          object WebDavLiberalEscapingCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Tolerate non-encoded special characters in filenames'
            TabOrder = 0
            OnClick = DataChange
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
          450
          388)
        object FtpPingGroup: TGroupBox
          Left = 8
          Top = 139
          Width = 438
          Height = 124
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 3
          DesignSize = (
            438
            124)
          object FtpPingIntervalLabel: TLabel
            Left = 9
            Top = 94
            Width = 152
            Height = 15
            Caption = 'Seconds &between keepalives:'
            FocusControl = FtpPingIntervalSecEdit
          end
          object FtpPingIntervalSecEdit: TUpDownEdit
            Left = 288
            Top = 91
            Width = 80
            Height = 23
            Alignment = taRightJustify
            MaxValue = 3600.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 4
            TabOrder = 3
            OnChange = DataChange
          end
          object FtpPingOffButton: TRadioButton
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Off'
            TabOrder = 0
            OnClick = DataChange
          end
          object FtpPingDummyCommandButton: TRadioButton
            Left = 11
            Top = 45
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Executing &dummy protocol commands'
            TabOrder = 1
            OnClick = DataChange
          end
          object FtpPingDirectoryListingButton: TRadioButton
            Left = 11
            Top = 68
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&And additionally reading the current directory'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object TimeoutGroup: TGroupBox
          Left = 8
          Top = 81
          Width = 438
          Height = 52
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Timeouts'
          TabOrder = 1
          DesignSize = (
            438
            52)
          object Label11: TLabel
            Left = 9
            Top = 22
            Width = 130
            Height = 15
            Caption = 'Server &response timeout:'
            FocusControl = TimeoutEdit
          end
          object Label12: TLabel
            Left = 374
            Top = 22
            Width = 43
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'seconds'
            FocusControl = TimeoutEdit
          end
          object TimeoutEdit: TUpDownEdit
            Left = 288
            Top = 19
            Width = 80
            Height = 23
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 6000.000000000000000000
            MinValue = 5.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 4
            TabOrder = 0
            OnChange = DataChange
          end
        end
        object PingGroup: TGroupBox
          Left = 8
          Top = 139
          Width = 438
          Height = 124
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Keepalives'
          TabOrder = 2
          DesignSize = (
            438
            124)
          object PingIntervalLabel: TLabel
            Left = 9
            Top = 94
            Width = 152
            Height = 15
            Caption = 'Seconds &between keepalives:'
            FocusControl = PingIntervalSecEdit
          end
          object PingIntervalSecEdit: TUpDownEdit
            Left = 288
            Top = 91
            Width = 80
            Height = 23
            Alignment = taRightJustify
            MaxValue = 3600.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 4
            TabOrder = 3
            OnChange = DataChange
          end
          object PingOffButton: TRadioButton
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Off'
            TabOrder = 0
            OnClick = DataChange
          end
          object PingNullPacketButton: TRadioButton
            Left = 11
            Top = 45
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sending of &null SSH packets'
            TabOrder = 1
            OnClick = DataChange
          end
          object PingDummyCommandButton: TRadioButton
            Left = 11
            Top = 68
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Executing &dummy protocol commands'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object IPvGroup: TGroupBox
          Left = 8
          Top = 269
          Width = 438
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Internet protocol version'
          TabOrder = 4
          object IPAutoButton: TRadioButton
            Left = 11
            Top = 22
            Width = 101
            Height = 17
            Caption = 'A&uto'
            TabOrder = 0
            OnClick = DataChange
          end
          object IPv4Button: TRadioButton
            Left = 133
            Top = 22
            Width = 101
            Height = 17
            Caption = 'IPv&4'
            TabOrder = 1
            OnClick = DataChange
          end
          object IPv6Button: TRadioButton
            Left = 255
            Top = 22
            Width = 101
            Height = 17
            Caption = 'IPv&6'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object ConnectionGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 0
          DesignSize = (
            438
            73)
          object FtpPasvModeCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Passive mode'
            TabOrder = 0
            OnClick = DataChange
          end
          object BufferSizeCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 418
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
          450
          388)
        object ProxyTypeGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 176
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy'
          TabOrder = 0
          DesignSize = (
            438
            176)
          object ProxyMethodLabel: TLabel
            Left = 9
            Top = 22
            Width = 58
            Height = 15
            Caption = 'Proxy &type:'
            FocusControl = SshProxyMethodCombo
          end
          object ProxyHostLabel: TLabel
            Left = 9
            Top = 48
            Width = 91
            Height = 15
            Caption = 'Pro&xy host name:'
            FocusControl = ProxyHostEdit
          end
          object ProxyPortLabel: TLabel
            Left = 327
            Top = 48
            Width = 70
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = ProxyPortEdit
          end
          object ProxyUsernameLabel: TLabel
            Left = 9
            Top = 95
            Width = 59
            Height = 15
            Caption = '&User name:'
            FocusControl = ProxyUsernameEdit
          end
          object ProxyPasswordLabel: TLabel
            Left = 222
            Top = 95
            Width = 53
            Height = 15
            Caption = '&Password:'
            FocusControl = ProxyPasswordEdit
          end
          object SshProxyMethodCombo: TComboBox
            Left = 140
            Top = 19
            Width = 121
            Height = 23
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
            Left = 327
            Top = 66
            Width = 102
            Height = 23
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = DataChange
          end
          object ProxyHostEdit: TEdit
            Left = 9
            Top = 66
            Width = 312
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 3
            Text = 'ProxyHostEdit'
            OnChange = DataChange
          end
          object ProxyUsernameEdit: TEdit
            Left = 9
            Top = 113
            Width = 207
            Height = 23
            MaxLength = 100
            TabOrder = 5
            Text = 'ProxyUsernameEdit'
            OnChange = DataChange
          end
          object ProxyPasswordEdit: TPasswordEdit
            Left = 222
            Top = 113
            Width = 207
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 6
            Text = 'ProxyPasswordEdit'
            OnChange = DataChange
          end
          object FtpProxyMethodCombo: TComboBox
            Left = 140
            Top = 19
            Width = 289
            Height = 23
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
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
          object NeonProxyMethodCombo: TComboBox
            Left = 140
            Top = 19
            Width = 121
            Height = 23
            Style = csDropDownList
            TabOrder = 2
            OnChange = DataChange
            Items.Strings = (
              'None'
              'SOCKS4'
              'SOCKS5'
              'HTTP')
          end
          object ProxyAutodetectButton: TButton
            Left = 9
            Top = 142
            Width = 100
            Height = 25
            Caption = '&Autodetect'
            TabOrder = 7
            OnClick = ProxyAutodetectButtonClick
          end
        end
        object ProxySettingsGroup: TGroupBox
          Left = 8
          Top = 184
          Width = 438
          Height = 134
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Proxy settings'
          TabOrder = 1
          DesignSize = (
            438
            134)
          object ProxyTelnetCommandLabel: TLabel
            Left = 9
            Top = 22
            Width = 93
            Height = 15
            Caption = 'Telnet co&mmand:'
            FocusControl = ProxyTelnetCommandEdit
          end
          object Label17: TLabel
            Left = 9
            Top = 104
            Width = 185
            Height = 15
            Caption = 'Do &DNS name lookup at proxy end:'
            FocusControl = ProxyDNSCombo
          end
          object ProxyLocalCommandLabel: TLabel
            Left = 9
            Top = 22
            Width = 121
            Height = 15
            Caption = 'Local proxy co&mmand:'
            FocusControl = ProxyLocalCommandEdit
          end
          object ProxyTelnetCommandEdit: TEdit
            Left = 9
            Top = 40
            Width = 420
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 0
            Text = 'ProxyTelnetCommandEdit'
            OnChange = DataChange
          end
          object ProxyLocalhostCheck: TCheckBox
            Left = 11
            Top = 78
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Co&nsider proxying local host connections'
            TabOrder = 5
          end
          object ProxyDNSCombo: TComboBox
            Left = 362
            Top = 101
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 6
            Items.Strings = (
              'Auto'
              'No'
              'Yes')
          end
          object ProxyLocalCommandEdit: TEdit
            Left = 9
            Top = 40
            Width = 332
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'ProxyLocalCommandEdit'
            OnChange = DataChange
          end
          object ProxyLocalCommandBrowseButton: TButton
            Left = 347
            Top = 39
            Width = 82
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Browse...'
            TabOrder = 3
            OnClick = ProxyLocalCommandBrowseButtonClick
          end
          object ProxyTelnetCommandHintText: TStaticText
            Left = 350
            Top = 63
            Width = 79
            Height = 17
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'patterns'
            TabOrder = 1
            TabStop = True
          end
          object ProxyLocalCommandHintText: TStaticText
            Left = 262
            Top = 63
            Width = 79
            Height = 17
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
          450
          388)
        object TunnelSessionGroup: TGroupBox
          Left = 8
          Top = 25
          Width = 438
          Height = 120
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Host to setup tunnel on'
          TabOrder = 1
          DesignSize = (
            438
            120)
          object Label6: TLabel
            Left = 9
            Top = 22
            Width = 61
            Height = 15
            Caption = '&Host name:'
            FocusControl = TunnelHostNameEdit
          end
          object Label14: TLabel
            Left = 327
            Top = 22
            Width = 70
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = TunnelPortNumberEdit
          end
          object Label15: TLabel
            Left = 9
            Top = 69
            Width = 59
            Height = 15
            Caption = '&User name:'
            FocusControl = TunnelUserNameEdit
          end
          object Label16: TLabel
            Left = 222
            Top = 69
            Width = 53
            Height = 15
            Caption = '&Password:'
            FocusControl = TunnelPasswordEdit
          end
          object TunnelHostNameEdit: TEdit
            Left = 9
            Top = 40
            Width = 312
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 0
            Text = 'TunnelHostNameEdit'
            OnChange = DataChange
          end
          object TunnelUserNameEdit: TEdit
            Left = 9
            Top = 87
            Width = 207
            Height = 23
            MaxLength = 100
            TabOrder = 2
            Text = 'TunnelUserNameEdit'
            OnChange = DataChange
          end
          object TunnelPasswordEdit: TPasswordEdit
            Left = 222
            Top = 87
            Width = 207
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 100
            TabOrder = 3
            Text = 'TunnelPasswordEdit'
            OnChange = DataChange
          end
          object TunnelPortNumberEdit: TUpDownEdit
            Left = 327
            Top = 40
            Width = 102
            Height = 23
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnChange = DataChange
          end
        end
        object TunnelCheck: TCheckBox
          Left = 10
          Top = 2
          Width = 436
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = '&Connect through SSH tunnel'
          TabOrder = 0
          OnClick = DataChange
        end
        object TunnelOptionsGroup: TGroupBox
          Left = 8
          Top = 151
          Width = 438
          Height = 52
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Tunnel options'
          TabOrder = 2
          DesignSize = (
            438
            52)
          object Label21: TLabel
            Left = 9
            Top = 22
            Width = 93
            Height = 15
            Caption = '&Local tunnel port:'
            FocusControl = TunnelLocalPortNumberEdit
          end
          object TunnelLocalPortNumberEdit: TComboBox
            Left = 327
            Top = 19
            Width = 102
            Height = 23
            AutoComplete = False
            Anchors = [akTop, akRight]
            MaxLength = 50
            TabOrder = 0
            Text = 'TunnelLocalPortNumberEdit'
            OnChange = DataChange
            Items.Strings = (
              'Autoselect')
          end
        end
        object TunnelAuthenticationParamsGroup: TGroupBox
          Left = 8
          Top = 209
          Width = 438
          Height = 71
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Tunnel authentication parameters'
          TabOrder = 3
          DesignSize = (
            438
            71)
          object Label18: TLabel
            Left = 9
            Top = 22
            Width = 79
            Height = 15
            Caption = 'Private &key file:'
            FocusControl = TunnelPrivateKeyEdit3
          end
          object TunnelPrivateKeyEdit3: TFilenameEdit
            Left = 9
            Top = 40
            Width = 420
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PrivateKeyEdit3AfterDialog
            Filter = 
              'PuTTY Private Key Files (*.ppk)|*.ppk|All Private Key Files (*.p' +
              'pk;*.pem;*.key;id_*)|*.ppk;*.pem;*.key;id_*|All Files (*.*)|*.*'
            DialogOptions = [ofReadOnly, ofPathMustExist, ofFileMustExist]
            DialogTitle = 'Select private key file'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'TunnelPrivateKeyEdit3'
            OnChange = DataChange
          end
        end
      end
      object SslSheet: TTabSheet
        Tag = 2
        HelpType = htKeyword
        HelpKeyword = 'ui_login_tls'
        Caption = 'TLS/SSL'
        ImageIndex = 13
        TabVisible = False
        DesignSize = (
          450
          388)
        object TlsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 104
          Anchors = [akLeft, akTop, akRight]
          Caption = 'TLS options'
          TabOrder = 0
          DesignSize = (
            438
            104)
          object MinTlsVersionLabel: TLabel
            Left = 9
            Top = 22
            Width = 119
            Height = 15
            Caption = 'Mi&nimum TLS version:'
            FocusControl = MinTlsVersionCombo
          end
          object MaxTlsVersionLabel: TLabel
            Left = 9
            Top = 51
            Width = 120
            Height = 15
            Caption = 'Ma&ximum TLS version:'
            FocusControl = MaxTlsVersionCombo
          end
          object MinTlsVersionCombo: TComboBox
            Left = 345
            Top = 19
            Width = 84
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
            OnChange = MinTlsVersionComboChange
            Items.Strings = (
              'TLS 1.0'
              'TLS 1.1'
              'TLS 1.2'
              'TLS 1.3')
          end
          object MaxTlsVersionCombo: TComboBox
            Left = 345
            Top = 48
            Width = 84
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnChange = MaxTlsVersionComboChange
            Items.Strings = (
              'TLS 1.0'
              'TLS 1.1'
              'TLS 1.2'
              'TLS 1.3')
          end
          object SslSessionReuseCheck2: TCheckBox
            Left = 11
            Top = 77
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Reuse TLS session ID for data connections'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object TlsAuthenticationGroup: TGroupBox
          Left = 9
          Top = 112
          Width = 438
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication parameters'
          TabOrder = 1
          DesignSize = (
            438
            73)
          object Label4: TLabel
            Left = 9
            Top = 22
            Width = 108
            Height = 15
            Caption = '&Client certificate file:'
            FocusControl = TlsCertificateFileEdit
          end
          object TlsCertificateFileEdit: TFilenameEdit
            Left = 9
            Top = 40
            Width = 420
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = TlsCertificateFileEditAfterDialog
            Filter = 
              'Certificates and private key files (*.pfx;*.p12;*.key;*.pem)|*.p' +
              'fx;*.p12;*.key;*.pem|All Files (*.*)|*.*'
            DialogOptions = [ofReadOnly, ofPathMustExist, ofFileMustExist]
            DialogTitle = 'Select client certificate file'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'TlsCertificateFileEdit'
            OnChange = DataChange
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
          450
          388)
        object ProtocolGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Protocol options'
          TabOrder = 0
          DesignSize = (
            438
            49)
          object CompressionCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 418
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable &compression'
            TabOrder = 0
            OnClick = DataChange
          end
        end
        object EncryptionGroup: TGroupBox
          Left = 8
          Top = 57
          Width = 438
          Height = 204
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Encryption options'
          TabOrder = 1
          DesignSize = (
            438
            204)
          object Label8: TLabel
            Left = 9
            Top = 22
            Width = 181
            Height = 15
            Caption = 'Encryption cipher selection &policy:'
            FocusControl = CipherListBox
          end
          object CipherListBox: TListBox
            Left = 9
            Top = 40
            Width = 334
            Height = 131
            Anchors = [akLeft, akTop, akRight, akBottom]
            DragMode = dmAutomatic
            ItemHeight = 15
            TabOrder = 0
            OnClick = DataChange
            OnDragDrop = AlgListBoxDragDrop
            OnDragOver = AlgListBoxDragOver
            OnStartDrag = AlgListBoxStartDrag
          end
          object Ssh2LegacyDESCheck: TCheckBox
            Left = 11
            Top = 177
            Width = 418
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = 'Enable legacy use of single-&DES'
            TabOrder = 3
          end
          object CipherUpButton: TButton
            Left = 349
            Top = 40
            Width = 80
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Up'
            TabOrder = 1
            OnClick = CipherButtonClick
          end
          object CipherDownButton: TButton
            Left = 349
            Top = 71
            Width = 80
            Height = 25
            Anchors = [akTop, akRight]
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
          450
          388)
        object KexOptionsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 280
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Key exchange algorithm options'
          TabOrder = 0
          DesignSize = (
            438
            280)
          object Label28: TLabel
            Left = 9
            Top = 22
            Width = 142
            Height = 15
            Caption = 'Algorithm selection &policy:'
            FocusControl = KexListBox
          end
          object KexListBox: TListBox
            Left = 9
            Top = 40
            Width = 334
            Height = 207
            Anchors = [akLeft, akTop, akRight, akBottom]
            DragMode = dmAutomatic
            ItemHeight = 15
            TabOrder = 0
            OnClick = DataChange
            OnDragDrop = AlgListBoxDragDrop
            OnDragOver = AlgListBoxDragOver
            OnStartDrag = AlgListBoxStartDrag
          end
          object KexUpButton: TButton
            Left = 349
            Top = 40
            Width = 80
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Up'
            TabOrder = 1
            OnClick = KexButtonClick
          end
          object KexDownButton: TButton
            Left = 349
            Top = 68
            Width = 80
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Down'
            TabOrder = 2
            OnClick = KexButtonClick
          end
          object AuthGSSAPIKEXCheck: TCheckBox
            Left = 11
            Top = 253
            Width = 285
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Attempt &GSSAPI key exchange'
            TabOrder = 3
            OnClick = DataChange
          end
        end
        object KexReexchangeGroup: TGroupBox
          Left = 8
          Top = 288
          Width = 438
          Height = 81
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options controlling key re-exchange'
          TabOrder = 1
          DesignSize = (
            438
            81)
          object Label31: TLabel
            Left = 9
            Top = 22
            Width = 218
            Height = 15
            Caption = 'Max &minutes before rekey (0 for no limit):'
            Color = clBtnFace
            FocusControl = RekeyTimeEdit
            ParentColor = False
          end
          object Label32: TLabel
            Left = 9
            Top = 51
            Width = 198
            Height = 15
            Caption = 'Ma&x data before rekey (0 for no limit):'
            Color = clBtnFace
            FocusControl = RekeyDataEdit
            ParentColor = False
          end
          object RekeyTimeEdit: TUpDownEdit
            Left = 349
            Top = 19
            Width = 80
            Height = 23
            Alignment = taRightJustify
            MaxValue = 1440.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 4
            TabOrder = 0
            OnChange = DataChange
          end
          object RekeyDataEdit: TEdit
            Left = 349
            Top = 48
            Width = 80
            Height = 23
            Anchors = [akTop, akRight]
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
          450
          388)
        object SshNoUserAuthCheck: TCheckBox
          Left = 10
          Top = 2
          Width = 436
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = '&Bypass authentication entirely'
          TabOrder = 0
          OnClick = DataChange
        end
        object AuthenticationGroup: TGroupBox
          Left = 8
          Top = 25
          Width = 438
          Height = 95
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication options'
          TabOrder = 1
          DesignSize = (
            438
            95)
          object TryAgentCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 416
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt authentication using &Pageant'
            TabOrder = 0
            OnClick = DataChange
          end
          object AuthKICheck: TCheckBox
            Left = 11
            Top = 45
            Width = 416
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt '#39'keyboard-&interactive'#39' authentication'
            TabOrder = 1
            OnClick = DataChange
          end
          object AuthKIPasswordCheck: TCheckBox
            Left = 26
            Top = 68
            Width = 401
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Respond with a pass&word to the first prompt'
            TabOrder = 2
            OnClick = DataChange
          end
        end
        object AuthenticationParamsGroup: TGroupBox
          Left = 8
          Top = 126
          Width = 438
          Height = 172
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Authentication parameters'
          TabOrder = 2
          DesignSize = (
            438
            172)
          object PrivateKeyLabel: TLabel
            Left = 9
            Top = 45
            Width = 79
            Height = 15
            Caption = 'Private &key file:'
            FocusControl = PrivateKeyEdit3
          end
          object DetachedCertificateLabel: TLabel
            Left = 9
            Top = 121
            Width = 198
            Height = 15
            Caption = 'Certificate to &use with the private key:'
            FocusControl = DetachedCertificateEdit
          end
          object AgentFwdCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 416
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow agent &forwarding'
            TabOrder = 0
            OnClick = DataChange
          end
          object PrivateKeyEdit3: TFilenameEdit
            Left = 9
            Top = 63
            Width = 418
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PrivateKeyEdit3AfterDialog
            Filter = 
              'PuTTY Private Key Files (*.ppk)|*.ppk|All Private Key Files (*.p' +
              'pk;*.pem;*.key;id_*)|*.ppk;*.pem;*.key;id_*|All Files (*.*)|*.*'
            DialogOptions = [ofReadOnly, ofPathMustExist, ofFileMustExist]
            DialogTitle = 'Select private key file'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Text = 'PrivateKeyEdit3'
            OnChange = DataChange
          end
          object PrivateKeyToolsButton: TButton
            Left = 215
            Top = 90
            Width = 94
            Height = 25
            Caption = '&Tools'
            TabOrder = 3
            OnClick = PrivateKeyToolsButtonClick
          end
          object PrivateKeyViewButton: TButton
            Left = 9
            Top = 90
            Width = 200
            Height = 25
            Caption = '&Display Public Key'
            TabOrder = 2
            OnClick = PrivateKeyViewButtonClick
          end
          object DetachedCertificateEdit: TFilenameEdit
            Left = 9
            Top = 139
            Width = 418
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PrivateKeyEdit3AfterDialog
            Filter = 'Public key files (*.pub)|*.pub|All Files (*.*)|*.*'
            DialogOptions = [ofReadOnly, ofPathMustExist, ofFileMustExist]
            DialogTitle = 'Select certificate file'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
            Text = 'DetachedCertificateEdit'
            OnChange = DataChange
          end
        end
        object GSSAPIGroup: TGroupBox
          Left = 8
          Top = 304
          Width = 438
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'GSSAPI'
          TabOrder = 3
          DesignSize = (
            438
            72)
          object AuthGSSAPICheck3: TCheckBox
            Left = 11
            Top = 22
            Width = 416
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Attempt &GSSAPI authentication'
            TabOrder = 0
            OnClick = AuthGSSAPICheck3Click
          end
          object GSSAPIFwdTGTCheck: TCheckBox
            Left = 26
            Top = 45
            Width = 401
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
          450
          388)
        object BugsGroupBox: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 257
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Detection of known bugs in SSH servers'
          TabOrder = 0
          DesignSize = (
            438
            257)
          object BugHMAC2Label: TLabel
            Left = 9
            Top = 80
            Width = 164
            Height = 15
            Caption = 'Miscomputes SSH H&MAC keys:'
            FocusControl = BugHMAC2Combo
          end
          object BugDeriveKey2Label: TLabel
            Left = 9
            Top = 109
            Width = 185
            Height = 15
            Caption = 'Miscomputes SSH &encryption keys:'
            FocusControl = BugDeriveKey2Combo
          end
          object BugRSAPad2Label: TLabel
            Left = 9
            Top = 138
            Width = 217
            Height = 15
            Caption = 'Requires &padding on SSH RSA signatures:'
            FocusControl = BugRSAPad2Combo
          end
          object BugPKSessID2Label: TLabel
            Left = 9
            Top = 167
            Width = 201
            Height = 15
            Caption = 'Misuses the sessio&n ID in SSH PK auth:'
            FocusControl = BugPKSessID2Combo
          end
          object BugRekey2Label: TLabel
            Left = 9
            Top = 196
            Width = 191
            Height = 15
            Caption = 'Handles SSH &key re-exchange badly:'
            FocusControl = BugRekey2Combo
          end
          object BugMaxPkt2Label: TLabel
            Left = 9
            Top = 225
            Width = 183
            Height = 15
            Caption = 'Ignores SSH ma&ximum packet size:'
            FocusControl = BugMaxPkt2Combo
          end
          object BugIgnore2Label: TLabel
            Left = 9
            Top = 22
            Width = 174
            Height = 15
            Caption = 'Chokes on SSH i&gnore messages:'
            FocusControl = BugIgnore2Combo
          end
          object BugWinAdjLabel: TLabel
            Left = 9
            Top = 51
            Width = 224
            Height = 15
            Caption = 'Chokes on WinSCP'#39's SSH '#39'&winadj'#39' requests'
            FocusControl = BugWinAdjCombo
          end
          object BugHMAC2Combo: TComboBox
            Left = 362
            Top = 77
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 2
            OnChange = DataChange
          end
          object BugDeriveKey2Combo: TComboBox
            Left = 362
            Top = 106
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 3
            OnChange = DataChange
          end
          object BugRSAPad2Combo: TComboBox
            Left = 362
            Top = 135
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = DataChange
          end
          object BugPKSessID2Combo: TComboBox
            Left = 362
            Top = 164
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 5
            OnChange = DataChange
          end
          object BugRekey2Combo: TComboBox
            Left = 362
            Top = 193
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 6
            OnChange = DataChange
          end
          object BugMaxPkt2Combo: TComboBox
            Left = 362
            Top = 222
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 7
            OnChange = DataChange
          end
          object BugIgnore2Combo: TComboBox
            Left = 362
            Top = 19
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
            OnChange = DataChange
          end
          object BugWinAdjCombo: TComboBox
            Left = 362
            Top = 48
            Width = 67
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnChange = DataChange
          end
        end
      end
      object NoteSheet: TTabSheet
        HelpType = htKeyword
        HelpKeyword = 'ui_login_note'
        Caption = 'Note'
        ImageIndex = 14
        TabVisible = False
        DesignSize = (
          450
          388)
        object NoteGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 438
          Height = 382
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Note'
          TabOrder = 0
          DesignSize = (
            438
            382)
          object NoteMemo: TMemo
            Left = 9
            Top = 22
            Width = 420
            Height = 350
            Anchors = [akLeft, akTop, akRight, akBottom]
            MaxLength = 4000
            TabOrder = 0
            OnChange = DataChange
            OnKeyDown = NoteMemoKeyDown
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 161
      Height = 398
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        161
        398)
      object NavigationPanel: TPanel
        Left = 8
        Top = 8
        Width = 151
        Height = 382
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 0
        object NavigationTree: TTreeView
          Left = 0
          Top = 0
          Width = 151
          Height = 382
          Align = alClient
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
            030400000036000000000000000000000000000000FFFFFFFF00000000000000
            0008000000010C45006E007600690072006F006E006D0065006E007400580036
            000000000000000000000000000000FFFFFFFF00000000000000000000000001
            0C4400690072006500630074006F007200690065007300580036000000000000
            000000000000000000FFFFFFFF000000000000000000000000010C5200650063
            00790063006C0065002000620069006E005800340000000000000000000000FF
            FFFFFFFFFFFFFF000000000000000000000000010B45006E0063007200790070
            00740069006F006E00580028000000000000000000000000000000FFFFFFFF00
            0000000000000000000000010553004600540050005800260000000000000000
            00000000000000FFFFFFFF000000000000000000000000010453004300500058
            00260000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
            000104460054005000580024000000000000000000000000000000FFFFFFFF00
            000000000000000000000001035300330058002C0000000000000000000000FF
            FFFFFFFFFFFFFF00000000000000000000000001075700650062004400410056
            00580034000000000000000000000000000000FFFFFFFF000000000000000002
            000000010B43006F006E006E0065006300740069006F006E0058002A00000000
            0000000000000000000000FFFFFFFF0000000000000000000000000106500072
            006F007800790058002C000000000000000000000000000000FFFFFFFF000000
            0000000000000000000107540075006E006E0065006C00580026000000000000
            000000000000000000FFFFFFFF00000000000000000300000001045300530048
            00580038000000000000000000000000000000FFFFFFFF000000000000000000
            000000010D4B00650078002000650078006300680061006E006700650058003C
            000000000000000000000000000000FFFFFFFF00000000000000000000000001
            0F410075007400680065006E007400690063006100740069006F006E00580028
            000000000000000000000000000000FFFFFFFF00000000000000000000000001
            054200750067007300580028000000000000000000000000000000FFFFFFFF00
            000000000000000000000001054E006F00740065005800}
        end
      end
    end
  end
  object OKBtn: TButton
    Left = 359
    Top = 396
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 445
    Top = 396
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 531
    Top = 396
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object ColorButton: TButton
    Left = 8
    Top = 396
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Color'
    TabOrder = 4
    OnClick = ColorButtonClick
  end
  object ColorImageList: TImageList
    AllocBy = 1
    Left = 36
    Top = 289
    Bitmap = {
      494C010101000400500010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      3500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004B3F35004B3F35004B3F35004B3F3500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000645951004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F350062584F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500736052007360
      5200736052007360520073605200736052007360520073605200736052007360
      520073605200736052004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35001DE6B5001DE6
      B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6B5001DE6
      B5001DE6B5001DE6B5004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000062584F004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F350062584F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000F00F000000000000
      FC3F000000000000800100000000000080010000000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
  object ColorImageList120: TImageList
    AllocBy = 1
    Height = 20
    Width = 20
    Left = 132
    Top = 289
    Bitmap = {
      494C0101010010001C0014001400FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000001400000001002000000000000019
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000062584F004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F350062584F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004B3F350099816F0099816F0099816F0099816F0099816F009981
      6F0099816F0099816F004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000062584F004B3F35004B3F35003028220030282200302822003028
      22004B3F35004B3F350062584F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000027211C0027211C0027211C002721
      1C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000071675F004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F350071675F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35008C8078008C80
      78008C8078008C8078008C8078008C8078008C8078008C8078008C8078008C80
      78008C8078008C8078008C80780029BD3E0029BD3E008C8078004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004B3F35000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006F655D004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35006F655D000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000050000000140000000100010000000000F00000000000000000000000
      000000000000000000000000FFFFFF00FFFFF0000000000000000000F801F000
      0000000000000000F801F0000000000000000000F801F0000000000000000000
      FF0FF00000000000000000008000100000000000000000008000100000000000
      0000000080001000000000000000000080001000000000000000000080001000
      0000000000000000800010000000000000000000800010000000000000000000
      8000100000000000000000008000100000000000000000008000100000000000
      0000000080001000000000000000000080001000000000000000000080001000
      0000000000000000FFFFF0000000000000000000FFFFF0000000000000000000
      00000000000000000000000000000000000000000000}
  end
  object ColorImageList144: TImageList
    AllocBy = 1
    Height = 24
    Width = 24
    Left = 36
    Top = 337
    Bitmap = {
      494C010101006800740018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000001800000001002000000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000062584F004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F350062584F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004B3F350099816F0099816F0099816F0099816F009981
      6F0099816F0099816F0099816F0099816F0099816F004B3F3500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004B3F35007360520073605200736052004B3F35004B3F
      35004B3F35004B3F35007360520073605200736052004B3F3500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000062584F004B3F35004B3F35004B3F350042372F004237
      2F0042372F0042372F004B3F35004B3F35004B3F350062584F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000302822003028
      2200302822003028220000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000746A63004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F3500746A6300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35004E4238004E42
      38004E4238004E4238004E4238004E4238004E4238004E4238004E4238004E42
      38004E4238004E4238004E4238004E4238004E4238004E4238004E42380029BD
      3E0029BD3E004E4238004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350051443B008C80
      78008C8078008C8078008C8078008C8078008C8078008C8078008C8078008C80
      78008C8078008C8078008C8078008C8078008C8078008C8078008C8078008C80
      78008C80780051443B004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350055473D00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055473D004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F3500584A3F00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00584A3F004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35005B4D4200FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005B4D42004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35005E504400FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005E5044004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350062534700FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00625347004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350065554900FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00655549004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350069584C00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0069584C004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35006C5B4E00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF006C5B4E004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F35006F5E5100FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF006F5E51004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350073615300FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00736153004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350076645600FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00766456004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B3F350079665800855D
      6700855D6700855D6700855D6700855D6700855D6700855D6700855D6700855D
      6700855D6700855D6700855D6700855D6700855D6700855D6700855D6700855D
      6700855D6700796658004B3F3500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000726961004F4338004F43
      38004F4338004F4338004F4338004F4338004F4338004F4338004F4338004F43
      38004F4338004F4338004F4338004F4338004F4338004F4338004F4338004F43
      38004F4338004F43380072696100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00FFFFFF000000000000000000FC003F00
      0000000000000000FC003F000000000000000000FC003F000000000000000000
      FC003F000000000000000000FFC3FF0000000000000000008000010000000000
      0000000080000100000000000000000080000100000000000000000080000100
      0000000000000000800001000000000000000000800001000000000000000000
      8000010000000000000000008000010000000000000000008000010000000000
      0000000080000100000000000000000080000100000000000000000080000100
      0000000000000000800001000000000000000000800001000000000000000000
      800001000000000000000000800001000000000000000000FFFFFF0000000000
      00000000FFFFFF00000000000000000000000000000000000000000000000000
      000000000000}
  end
  object ColorImageList192: TImageList
    AllocBy = 1
    Height = 32
    Width = 32
    Left = 132
    Top = 337
    Bitmap = {
      494C0101010060006C0020002000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000002000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000695F57004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F3500695F57000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004B3F350099816F0099816F009981
      6F0099816F0099816F0099816F0099816F0099816F0099816F0099816F009981
      6F0099816F0099816F0099816F004B3F35000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004B3F350073605200736052007360
      5200736052004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35007360
      52007360520073605200736052004B3F35000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000695F57004B3F35004B3F35004B3F
      35004B3F3500453A3100453A3100453A3100453A3100453A3100453A31004B3F
      35004B3F35004B3F35004B3F3500695F57000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003930280039302800393028003930280039302800393028000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002D2620002D2620002D2620002D2620002D2620002D2620000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C857E004C40
      36004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004C4036008F88810000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C4036004D41
      38004D4138004D4138004D4138004D4138004D4138004D4138004D4138004D41
      38004D4138004D4138004D4138004D4138004D4138004D4138004D4138004D41
      38004D4138004D4138004D4138004D4138004D4138004D4138004D4138004D41
      38004D4138004C40360000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35005043
      3A0050433A0050433A0050433A0050433A0050433A0050433A0050433A005043
      3A0050433A0050433A0050433A0050433A0050433A0050433A0050433A005043
      3A0050433A0050433A0050433A0050433A0050433A0029BD3E0029BD3E0029BD
      3E0050433A004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35005245
      3B008D8178008D8178008D8178008D8178008D8178008D8178008D8178008D81
      78008D8178008D8178008D8178008D8178008D8178008D8178008D8178008D81
      78008D8178008D8178008D8178008D8178008D8178008D8178008D8178008D81
      780052453B004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35005447
      3D00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0054473D004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35005749
      3F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0057493F004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F3500594B
      4000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00594B40004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35005B4D
      4200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005B4D42004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35005E4F
      4400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005E4F44004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006051
      4600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00605146004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006253
      4700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00625347004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006555
      4900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00655549004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006757
      4B00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0067574B004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006A59
      4C00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF006A594C004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006C5B
      4E00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF006C5B4E004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35006F5D
      5000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF006F5D50004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F3500715F
      5200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00715F52004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35007361
      5300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00736153004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35007563
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00756355004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004B3F35007865
      5700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00786557004B3F350000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C4036007A67
      59007A6759007A6759007A6759007A6759007A6759007A6759007A6759007A67
      59007A6759007A6759007A6759007A6759007A6759007A6759007A6759007A67
      59007A6759007A6759007A6759007A6759007A6759007A6759007A6759007A67
      59007A6759004C40360000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008B837D004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F35004B3F
      35004C4036008C857E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000200000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF000000000000000000000000
      FFFFFFFF000000000000000000000000FF0000FF000000000000000000000000
      FF0000FF000000000000000000000000FF0000FF000000000000000000000000
      FF0000FF000000000000000000000000FFF81FFF000000000000000000000000
      FFF81FFF000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000C0000003000000000000000000000000
      C0000003000000000000000000000000FFFFFFFF000000000000000000000000
      FFFFFFFF00000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object PrivateKeyMenu: TPopupMenu
    Left = 128
    Top = 384
    object PrivateKeyGenerateItem: TMenuItem
      Caption = '&Generate New Key Pair with PuTTYgen...'
      OnClick = PrivateKeyGenerateItemClick
    end
    object PrivateKeyUploadItem: TMenuItem
      Caption = '&Install Public Key into Server...'
      OnClick = PrivateKeyUploadItemClick
    end
  end
end
