object PreferencesDialog: TPreferencesDialog
  Left = 400
  Top = 161
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 427
  ClientWidth = 527
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    527
    427)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 352
    Top = 396
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CloseButton: TButton
    Left = 440
    Top = 396
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 390
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    object PageControl: TPageControl
      Left = 141
      Top = 0
      Width = 386
      Height = 390
      ActivePage = CustomCommandsSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabIndex = 9
      TabOrder = 0
      OnChange = PageControlChange
      object PreferencesSheet: TTabSheet
        Tag = 1
        Hint = 'Environment'
        Caption = 'Gen'
        ImageIndex = 2
        DesignSize = (
          378
          335)
        object RandomSeedFileLabel: TLabel
          Left = 16
          Top = 276
          Width = 82
          Height = 13
          Caption = '&Random seed file'
          FocusControl = RandomSeedFileEdit
        end
        object CommonPreferencesGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 181
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Confirmations'
          TabOrder = 0
          DesignSize = (
            362
            181)
          object CopyOnDoubleClickCheck: TCheckBox
            Left = 16
            Top = 109
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Copy files using double-click'
            TabOrder = 4
            OnClick = ControlChange
          end
          object CopyOnDoubleClickConfirmationCheck: TCheckBox
            Left = 32
            Top = 131
            Width = 314
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Co&nfirm copy on double-click operation'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ConfirmOverwritingCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Overwriting of files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ConfirmDeletingCheck: TCheckBox
            Left = 16
            Top = 43
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Deleting of files (recommended)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ConfirmClosingSessionCheck: TCheckBox
            Left = 16
            Top = 65
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Closing &session'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DDTransferConfirmationCheck: TCheckBox
            Left = 16
            Top = 87
            Width = 338
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Drag && drop operations'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ContinueOnErrorCheck: TCheckBox
            Left = 16
            Top = 153
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Continue on &error (advanced users)'
            TabOrder = 6
            OnClick = ControlChange
          end
        end
        object RandomSeedFileEdit: TFilenameEdit
          Left = 136
          Top = 272
          Width = 234
          Height = 21
          AcceptFiles = True
          DefaultExt = 'log'
          Filter = 'Random seed files (*.rnd)|*.rnd|All files (*.*)|*.*'
          DialogOptions = [ofHideReadOnly, ofPathMustExist]
          DialogTitle = 'Select file for random seed'
          ClickKey = 16397
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = 'RandomSeedFileEdit'
          OnChange = ControlChange
        end
        object StorageGroup: TXPGroupBox
          Left = 8
          Top = 195
          Width = 362
          Height = 68
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Configuration storage'
          TabOrder = 1
          object RegistryStorageButton: TRadioButton
            Left = 16
            Top = 19
            Width = 289
            Height = 17
            Caption = 'Windows re&gistry'
            TabOrder = 0
            OnClick = ControlChange
          end
          object IniFileStorageButton: TRadioButton
            Left = 16
            Top = 43
            Width = 289
            Height = 17
            Caption = '&INI file (winscp3.ini)'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
      end
      object LogSheet: TTabSheet
        Tag = 2
        Hint = 'Logging'
        Caption = 'Log'
        ImageIndex = 4
        DesignSize = (
          378
          335)
        inline LoggingFrame: TLoggingFrame
          Left = 5
          Top = 0
          Width = 377
          Height = 212
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          DesignSize = (
            377
            212)
          inherited LoggingGroup: TXPGroupBox
            Width = 362
            DesignSize = (
              362
              177)
            inherited LogFileNameEdit: TFilenameEdit
              Width = 308
            end
            inherited LogFilePanel: TPanel
              Width = 308
            end
          end
        end
      end
      object GeneralSheet: TTabSheet
        Tag = 3
        Hint = 'Interface'
        Caption = 'Int'
        ImageIndex = 5
        DesignSize = (
          378
          335)
        object Label1: TLabel
          Left = 8
          Top = 226
          Width = 361
          Height = 33
          AutoSize = False
          Caption = 
            'Note: a change to this setting will only take effect the next ti' +
            'me you start the application.'
          WordWrap = True
        end
        inline GeneralSettingsFrame: TGeneralSettingsFrame
          Left = 8
          Top = 8
          Width = 362
          Height = 202
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          inherited InterfaceGroup: TXPGroupBox
            Width = 362
            inherited CommanderDescriptionLabel: TLabel
              Width = 223
            end
            inherited ExplorerDescriptionLabel: TLabel
              Width = 225
            end
          end
        end
      end
      object PanelsSheet: TTabSheet
        Tag = 4
        Hint = 'Panels'
        Caption = 'Pan'
        ImageIndex = 3
        DesignSize = (
          378
          335)
        object PanelsRemoteDirectoryGroup: TXPGroupBox
          Left = 8
          Top = 88
          Width = 362
          Height = 51
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Remote directory'
          TabOrder = 1
          DesignSize = (
            362
            51)
          object ShowInaccesibleDirectoriesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show inaccesible directories'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
        object PanelsCommonGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Common'
          TabOrder = 0
          DesignSize = (
            362
            72)
          object ShowHiddenFilesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show hidden files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DefaultDirIsHomeCheck: TCheckBox
            Left = 16
            Top = 42
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default directory is &home directory'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object DragDropPreferencesGroup: TXPGroupBox
          Left = 8
          Top = 147
          Width = 362
          Height = 166
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Drag && Drop'
          TabOrder = 2
          DesignSize = (
            362
            166)
          object Label5: TLabel
            Left = 16
            Top = 47
            Width = 337
            Height = 39
            AutoSize = False
            Caption = 
              'When downloading files using drag && drop, they are stored first' +
              ' to temporary directory.'
            WordWrap = True
          end
          object DDAllowMoveCheck: TCheckBox
            Left = 16
            Top = 23
            Width = 338
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &move from remote directory (not recommended)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDSystemTemporaryDirectoryButton: TRadioButton
            Left = 32
            Top = 83
            Width = 297
            Height = 17
            Caption = '&Use temporary directory of system'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDCustomTemporaryDirectoryButton: TRadioButton
            Left = 32
            Top = 107
            Width = 129
            Height = 17
            Caption = 'Use this &directory:'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DDTemporaryDirectoryEdit: TDirectoryEdit
            Left = 168
            Top = 103
            Width = 181
            Height = 21
            AcceptFiles = True
            DialogText = 'Select directory for temporary drag && drop files.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            Text = 'DDTemporaryDirectoryEdit'
            OnClick = ControlChange
          end
          object DDWarnLackOfTempSpaceCheck: TCheckBox
            Left = 32
            Top = 136
            Width = 321
            Height = 17
            Caption = '&Warn when there is not enough free space'
            TabOrder = 4
            OnClick = ControlChange
          end
        end
      end
      object CommanderSheet: TTabSheet
        Tag = 5
        Hint = 'Commander'
        Caption = 'Cmd'
        ImageIndex = 3
        DesignSize = (
          378
          335)
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 366
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Preferences on this tab applies to Norton Commander interface on' +
            'ly.'
          WordWrap = True
        end
        object PanelsGroup: TXPGroupBox
          Left = 8
          Top = 38
          Width = 362
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panels'
          TabOrder = 0
          DesignSize = (
            362
            99)
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete local files to recycle bin'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ExplorerStyleSelectionCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Explorer style selection'
            TabOrder = 1
            OnClick = ControlChange
          end
          object PreserveLocalDirectoryCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do not &change local directory when switching sessions'
            TabOrder = 2
            OnClick = ControlChange
          end
        end
        object CommanderMiscGroup: TXPGroupBox
          Left = 8
          Top = 146
          Width = 362
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 1
          DesignSize = (
            362
            53)
          object UseLocationProfilesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use Location Profiles instead of Directory Bookmarks'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
        object CompareCriterionsGroup: TXPGroupBox
          Left = 8
          Top = 209
          Width = 362
          Height = 74
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Compare directory criterions'
          TabOrder = 2
          DesignSize = (
            362
            74)
          object CompareByTimeCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Compare by &time'
            TabOrder = 0
            OnClick = CompareByTimeCheckClick
          end
          object CompareBySizeCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Compare by &size'
            TabOrder = 1
            OnClick = CompareBySizeCheckClick
          end
        end
      end
      object ExplorerSheet: TTabSheet
        Tag = 6
        Hint = 'Explorer'
        Caption = 'Exp'
        ImageIndex = 5
        DesignSize = (
          378
          335)
        object Label4: TLabel
          Left = 8
          Top = 8
          Width = 366
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this tab applies to Explorer-like interface only.'
          WordWrap = True
        end
        object GroupBox2: TXPGroupBox
          Left = 8
          Top = 38
          Width = 362
          Height = 54
          Anchors = [akLeft, akTop, akRight]
          Caption = 'View'
          TabOrder = 0
          DesignSize = (
            362
            54)
          object ShowFullAddressCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show full path on address bar'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object TransferSheet: TTabSheet
        Tag = 7
        Hint = 'Transfer'
        Caption = 'Tran'
        ImageIndex = 6
        inline CopyParamsFrame: TCopyParamsFrame
          Left = 0
          Top = 0
          Width = 529
          Height = 345
          TabOrder = 0
          inherited CommonPropertiesGroup: TXPGroupBox
            Left = 198
            Top = 156
            Height = 66
            Caption = 'Common options'
            inherited CommonPreserveTimestampCheck: TCheckBox
              Top = 18
            end
            inherited CommonCalculateSizeCheck: TCheckBox
              Top = 40
            end
          end
          inherited LocalPropertiesGroup: TXPGroupBox
            Left = 8
            Top = 284
            Width = 182
            Height = 45
            Caption = 'Download options'
            DesignSize = (
              182
              45)
            inherited PreserveReadOnlyCheck: TCheckBox
              Top = 18
              Width = 164
            end
            inherited LocalPreserveTimeCheck: TCheckBox
              Top = 92
              Width = 164
            end
          end
          inherited RemotePropertiesGroup: TXPGroupBox
            Left = 8
            Top = 156
            Width = 182
            Height = 126
            Caption = 'Upload options'
            inherited RightsFrame: TRightsFrame
              Height = 88
              PopupMenu = CopyParamsFrame.RightsFrame.RightsPopup
            end
            inherited RemotePreserveTimeCheck: TCheckBox
              Top = 161
            end
          end
          inherited ChangeCaseGroup: TXPGroupBox
            Left = 247
            Top = 8
            Width = 123
            inherited CCNoChangeButton: TRadioButton
              Width = 110
            end
            inherited CCUpperCaseButton: TRadioButton
              Width = 110
            end
            inherited CCLowerCaseButton: TRadioButton
              Width = 110
            end
            inherited CCFirstUpperCaseButton: TRadioButton
              Width = 110
            end
          end
          inherited TransferModeGroup: TXPGroupBox
            Left = 8
            Top = 8
            Width = 231
            inherited TMTextButton: TRadioButton
              Width = 219
            end
            inherited TMBinaryButton: TRadioButton
              Width = 218
            end
            inherited TMAutomaticButton: TRadioButton
              Width = 218
            end
            inherited AsciiFileMaskCombo: THistoryComboBox
              Width = 213
            end
          end
        end
        object ResumeBox: TXPGroupBox
          Left = 198
          Top = 224
          Width = 172
          Height = 105
          Caption = 'Enable transfer resume for'
          TabOrder = 1
          object ResumeThresholdUnitLabel: TLabel
            Left = 136
            Top = 61
            Width = 13
            Height = 13
            Caption = 'kB'
            FocusControl = ResumeThresholdEdit
          end
          object ResumeOnButton: TRadioButton
            Left = 11
            Top = 18
            Width = 156
            Height = 17
            Caption = 'A&ll files (not recommended)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ResumeSmartButton: TRadioButton
            Left = 11
            Top = 38
            Width = 156
            Height = 17
            Caption = 'Files abo&ve'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ResumeOffButton: TRadioButton
            Left = 12
            Top = 82
            Width = 156
            Height = 17
            Caption = 'Di&sable'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ResumeThresholdEdit: TUpDownEdit
            Left = 45
            Top = 57
            Width = 84
            Height = 21
            Alignment = taRightJustify
            Increment = 10
            MaxValue = 4194304
            TabOrder = 2
            OnClick = ControlChange
          end
        end
      end
      object EditorSheet: TTabSheet
        Tag = 8
        Hint = 'Editor'
        Caption = 'Edit'
        ImageIndex = 7
        DesignSize = (
          378
          335)
        object EditorGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 103
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Default editor'
          TabOrder = 0
          DesignSize = (
            362
            103)
          object EditorInternalButton: TRadioButton
            Left = 16
            Top = 21
            Width = 113
            Height = 17
            Caption = '&Internal editor'
            TabOrder = 0
          end
          object EditorExternalButton: TRadioButton
            Left = 16
            Top = 45
            Width = 113
            Height = 17
            Caption = '&External editor'
            TabOrder = 1
          end
          object ExternalEditorEdit: TFilenameEdit
            Left = 32
            Top = 69
            Width = 315
            Height = 21
            OnAfterDialog = ExternalEditorEditAfterDialog
            Filter = 'Executable files (*.exe)|*.exe|All files (*.*)|*.*'
            DialogTitle = 'Select editor application.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'ExternalEditorEdit'
            OnChange = ExternalEditorEditChange
            OnExit = ExternalEditorEditExit
          end
        end
        object EditorFontGroup: TXPGroupBox
          Left = 8
          Top = 117
          Width = 362
          Height = 56
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Editor font'
          TabOrder = 1
          DesignSize = (
            362
            56)
          object EditorFontLabel: TLabel
            Left = 16
            Top = 25
            Width = 249
            Height = 13
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'EditorFontLabel'
          end
          object EditorFontButton: TButton
            Left = 271
            Top = 18
            Width = 75
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Select...'
            TabOrder = 0
            OnClick = EditorFontButtonClick
          end
        end
        object EditorOptionsGroup: TXPGroupBox
          Left = 8
          Top = 179
          Width = 362
          Height = 51
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          TabOrder = 2
          DesignSize = (
            362
            51)
          object EditorWordWrapCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Wrap long lines'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object IntegrationSheet: TTabSheet
        Tag = 9
        Hint = 'Integration'
        Caption = 'Integ'
        ImageIndex = 8
        DesignSize = (
          378
          335)
        object ShellIconsGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 209
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shell icons'
          TabOrder = 0
          DesignSize = (
            362
            209)
          object ShellIconsLabel: TLabel
            Left = 16
            Top = 155
            Width = 329
            Height = 46
            AutoSize = False
            Caption = 
              'To add shortcuts, which directly open stored session, use button' +
              ' '#39'Shell icon'#39' on '#39'Stored sessions'#39' tab of login dialog.'
            WordWrap = True
          end
          object DesktopIconButton: TButton
            Left = 16
            Top = 24
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &desktop icon'
            TabOrder = 0
            OnClick = IconButtonClick
          end
          object QuickLaunchIconButton: TButton
            Left = 16
            Top = 88
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &Quick Launch icon'
            TabOrder = 1
            OnClick = IconButtonClick
          end
          object DesktopIconAllUsersButton: TButton
            Left = 16
            Top = 56
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a desktop icon (&all users)'
            TabOrder = 2
            OnClick = IconButtonClick
          end
          object SendToHookButton: TButton
            Left = 16
            Top = 120
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add upload shortcut to Explorer'#39's '#39'&Send to'#39' context menu'
            TabOrder = 3
            OnClick = IconButtonClick
          end
        end
        object XPGroupBox1: TXPGroupBox
          Left = 8
          Top = 224
          Width = 362
          Height = 78
          Anchors = [akLeft, akTop, akRight]
          Caption = 'External applications'
          TabOrder = 1
          DesignSize = (
            362
            78)
          object Label2: TLabel
            Left = 16
            Top = 24
            Width = 61
            Height = 13
            Caption = '&PuTTY path:'
          end
          object PuttyPathEdit: TFilenameEdit
            Left = 16
            Top = 41
            Width = 330
            Height = 21
            OnAfterDialog = ExternalEditorEditAfterDialog
            Filter = 
              'PuTTY executable (putty.exe)|putty.exe|Executable files (*.exe)|' +
              '*.exe|All files (*.*)|*.*'
            DialogTitle = 'Find PuTTY executable.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'PuttyPathEdit'
            OnChange = ExternalEditorEditChange
            OnExit = ExternalEditorEditExit
          end
        end
      end
      object CustomCommandsSheet: TTabSheet
        Tag = 10
        Hint = 'Commands'
        Caption = 'Cmds'
        ImageIndex = 9
        DesignSize = (
          378
          335)
        object CustomCommandsGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 317
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom commands (SCP only)'
          TabOrder = 0
          DesignSize = (
            362
            317)
          object LocalDirectoryLabel: TLabel
            Left = 16
            Top = 24
            Width = 56
            Height = 13
            Caption = 'Descri&ption:'
            FocusControl = CustomCommandDescEdit
          end
          object RemoteDirectoryLabel: TLabel
            Left = 184
            Top = 24
            Width = 84
            Height = 13
            Caption = '&Custom command'
            FocusControl = CustomCommandEdit
          end
          object CustomCommandsPatternsLabel: TLabel
            Left = 16
            Top = 92
            Width = 329
            Height = 45
            AutoSize = False
            Caption = 
              'Patterns: !! - exclamation mark; ! - file name; !?prompt?default' +
              '!'#160'-'#160'prompts user for parameter value'
            WordWrap = True
          end
          object CustomCommandDescEdit: TEdit
            Left = 16
            Top = 41
            Width = 153
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'CustomCommandDescEdit'
            OnChange = ControlChange
          end
          object CustomCommandEdit: TEdit
            Left = 184
            Top = 41
            Width = 161
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 1000
            TabOrder = 1
            Text = 'CustomCommandEdit'
            OnChange = ControlChange
          end
          object CustomCommandsView: TListView
            Left = 16
            Top = 136
            Width = 238
            Height = 165
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Description'
                Width = 75
              end
              item
                Caption = 'Command'
                Width = 115
              end
              item
                Caption = 'D/R'
                Tag = 1
                Width = 40
              end>
            ColumnClick = False
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 4
            ViewStyle = vsReport
            OnData = CustomCommandsViewData
            OnDragDrop = CustomCommandsViewDragDrop
            OnDragOver = CustomCommandsViewDragOver
            OnKeyDown = CustomCommandsViewKeyDown
            OnSelectItem = CustomCommandsViewSelectItem
            OnStartDrag = CustomCommandsViewStartDrag
          end
          object AddCommandButton: TButton
            Left = 263
            Top = 136
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Add'
            TabOrder = 5
            OnClick = AddCommandButtonClick
          end
          object RemoveCommandButton: TButton
            Left = 263
            Top = 200
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Remove'
            TabOrder = 7
            OnClick = RemoveCommandButtonClick
          end
          object UpCommandButton: TButton
            Left = 263
            Top = 244
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 8
            OnClick = UpDownCommandButtonClick
          end
          object DownCommandButton: TButton
            Left = 263
            Top = 276
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 9
            OnClick = UpDownCommandButtonClick
          end
          object SaveCommandButton: TButton
            Left = 263
            Top = 168
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Save'
            TabOrder = 6
            OnClick = SaveCommandButtonClick
          end
          object CustomCommandApplyToDirectoriesCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 145
            Height = 17
            Caption = '&Apply to directories'
            TabOrder = 2
            OnClick = ControlChange
          end
          object CustomCommandRecursiveCheck: TCheckBox
            Left = 184
            Top = 69
            Width = 145
            Height = 17
            Caption = '&Execute recursively'
            TabOrder = 3
            OnClick = ControlChange
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 141
      Height = 390
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        141
        390)
      object NavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 125
        Height = 380
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = NavigationTreeChange
        Items.Data = {
          06000000250000000000000001000000FFFFFFFFFFFFFFFF0000000004000000
          0C456E7669726F6E6D656E7458230000000000000003000000FFFFFFFFFFFFFF
          FF00000000000000000A496E7465726661636558200000000000000004000000
          FFFFFFFFFFFFFFFF00000000000000000750616E656C73582300000000000000
          05000000FFFFFFFFFFFFFFFF00000000000000000A436F6D6D616E6465725822
          0000000000000006000000FFFFFFFFFFFFFFFF0000000000000000094578706C
          6F72657258200000000000000008000000FFFFFFFFFFFFFFFF00000000000000
          0007456469746F7258220000000000000007000000FFFFFFFFFFFFFFFF000000
          0000000000095472616E7366657258210000000000000002000000FFFFFFFFFF
          FFFFFF0000000000000000084C6F6767696E6758250000000000000009000000
          FFFFFFFFFFFFFFFF00000000000000000C496E746567726174696F6E58220000
          00000000000A000000FFFFFFFFFFFFFFFF000000000000000009436F6D6D616E
          647358}
      end
    end
  end
end
