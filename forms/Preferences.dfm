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
    TabOrder = 1
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
    TabOrder = 2
  end
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 390
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 141
      Top = 0
      Width = 386
      Height = 390
      ActivePage = PreferencesSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabIndex = 0
      TabOrder = 1
      OnChange = PageControlChange
      object PreferencesSheet: TTabSheet
        Tag = 1
        Hint = 'Environment'
        Caption = 'Gen'
        ImageIndex = 2
        DesignSize = (
          378
          335)
        object CommonPreferencesGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 245
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Confirmations'
          TabOrder = 0
          DesignSize = (
            362
            245)
          object CopyOnDoubleClickCheck: TCheckBox
            Left = 16
            Top = 153
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Copy files using double-click'
            TabOrder = 6
            OnClick = ControlChange
          end
          object CopyOnDoubleClickConfirmationCheck: TCheckBox
            Left = 32
            Top = 175
            Width = 314
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Co&nfirm copy on double-click operation'
            TabOrder = 7
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
            Top = 87
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'E&xiting application'
            TabOrder = 3
            OnClick = ControlChange
          end
          object DDTransferConfirmationCheck: TCheckBox
            Left = 16
            Top = 131
            Width = 338
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'D&rag && drop operations'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ContinueOnErrorCheck: TCheckBox
            Left = 16
            Top = 219
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Continue on &error (advanced users)'
            TabOrder = 9
            OnClick = ControlChange
          end
          object ConfirmExitOnCompletionCheck: TCheckBox
            Left = 16
            Top = 109
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Exiting application on o&peration completion'
            TabOrder = 4
            OnClick = ControlChange
          end
          object ConfirmResumeCheck: TCheckBox
            Left = 16
            Top = 65
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Transfer resuming'
            TabOrder = 2
            OnClick = ControlChange
          end
          object ConfirmCommandSessionCheck: TCheckBox
            Left = 16
            Top = 197
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Opening separate &shell session'
            TabOrder = 8
            OnClick = ControlChange
          end
        end
        object NotificationsGroup: TXPGroupBox
          Left = 8
          Top = 256
          Width = 362
          Height = 51
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Notifications'
          TabOrder = 1
          DesignSize = (
            362
            51)
          object BeepOnFinishAfterText: TLabel
            Left = 344
            Top = 22
            Width = 5
            Height = 13
            Caption = 's'
          end
          object BeepOnFinishCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 265
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Beep when work finishes, if it lasted more then'
            TabOrder = 0
            OnClick = ControlChange
          end
          object BeepOnFinishAfterEdit: TUpDownEdit
            Left = 283
            Top = 18
            Width = 57
            Height = 21
            Alignment = taRightJustify
            Increment = 15
            MaxValue = 999
            MaxLength = 3
            TabOrder = 1
            OnChange = ControlChange
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
          Top = 111
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
          Height = 97
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Common'
          TabOrder = 0
          DesignSize = (
            362
            97)
          object ShowHiddenFilesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show hidden files (Ctrl+Alt+H)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DefaultDirIsHomeCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default directory is &home directory'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete local files to recycle bin'
            TabOrder = 2
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
          Height = 75
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panels'
          TabOrder = 0
          DesignSize = (
            362
            75)
          object ExplorerStyleSelectionCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Explorer style selection'
            TabOrder = 0
            OnClick = ControlChange
          end
          object PreserveLocalDirectoryCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do not &change local directory when switching sessions'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object CommanderMiscGroup: TXPGroupBox
          Left = 8
          Top = 122
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
          Top = 185
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
            Left = 197
            Top = 209
            Height = 73
            Caption = 'Common options'
            DesignSize = (
              173
              73)
            inherited CommonPreserveTimestampCheck: TCheckBox
              Top = 19
            end
          end
          inherited LocalPropertiesGroup: TXPGroupBox
            Left = 197
            Top = 156
            Height = 48
            Caption = 'Download options'
            DesignSize = (
              173
              48)
            inherited PreserveReadOnlyCheck: TCheckBox
              Top = 20
            end
            inherited LocalPreserveTimeCheck: TCheckBox
              Top = 92
            end
          end
          inherited RemotePropertiesGroup: TXPGroupBox
            Left = 8
            Top = 156
            Width = 182
            Height = 126
            Caption = 'Upload options'
            inherited RemotePreserveTimeCheck: TCheckBox
              Top = 161
            end
          end
          inherited ChangeCaseGroup: TXPGroupBox
            Left = 247
            Top = 8
            Width = 123
            DesignSize = (
              123
              146)
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
            DesignSize = (
              231
              146)
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
          inherited FilterGroup: TXPGroupBox
            Left = 8
            Top = 284
            Width = 362
            DesignSize = (
              362
              41)
            inherited ExcludeFileMaskCombo: THistoryComboBox
              Width = 254
            end
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
          Top = 87
          Width = 362
          Height = 122
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Default editor'
          TabOrder = 1
          DesignSize = (
            362
            122)
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
            OnKeyDown = PathEditsKeyDown
          end
          object ExternalEditorTextCheck: TCheckBox
            Left = 40
            Top = 96
            Width = 305
            Height = 17
            Caption = 'Force &text transfer mode for files edited in external editor'
            TabOrder = 3
          end
        end
        object EditorFontGroup: TXPGroupBox
          Left = 8
          Top = 215
          Width = 362
          Height = 56
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Editor font'
          TabOrder = 2
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
          Top = 277
          Width = 362
          Height = 51
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          TabOrder = 3
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
        object SingleEditorGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Allow multiple opened files (editors)'
          TabOrder = 0
          object EditorSingleEditorOnCheck: TRadioButton
            Left = 16
            Top = 21
            Width = 329
            Height = 17
            Caption = '&One file only, use main session to upload changed files'
            TabOrder = 0
          end
          object EditorSingleEditorOffCheck: TRadioButton
            Left = 16
            Top = 45
            Width = 337
            Height = 17
            Caption = '&Multiple files, use background transfer to upload changed files'
            TabOrder = 1
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
          Height = 233
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shell icons'
          TabOrder = 0
          DesignSize = (
            362
            233)
          object ShellIconsLabel: TLabel
            Left = 16
            Top = 184
            Width = 329
            Height = 44
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
            TabOrder = 2
            OnClick = IconButtonClick
          end
          object DesktopIconAllUsersButton: TButton
            Left = 16
            Top = 56
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a desktop icon (&all users)'
            TabOrder = 1
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
          object RegisterAsUrlHandlerButton: TButton
            Left = 16
            Top = 152
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Register to handle scp:// and sftp:// addresses'
            TabOrder = 4
            OnClick = RegisterAsUrlHandlerButtonClick
          end
        end
        object ExternalAppsGroup: TXPGroupBox
          Left = 8
          Top = 248
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
            FocusControl = PuttyPathEdit
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
            OnKeyDown = PathEditsKeyDown
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
          Caption = 'Custom commands'
          TabOrder = 0
          DesignSize = (
            362
            317)
          object CustomCommandsView: TListView
            Left = 16
            Top = 24
            Width = 329
            Height = 209
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Description'
                Width = 85
              end
              item
                Caption = 'Command'
                Width = 140
              end
              item
                Caption = 'L/R'
                Width = 35
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
            TabOrder = 0
            ViewStyle = vsReport
            OnData = CustomCommandsViewData
            OnDblClick = CustomCommandsViewDblClick
            OnDragDrop = CustomCommandsViewDragDrop
            OnDragOver = CustomCommandsViewDragOver
            OnKeyDown = CustomCommandsViewKeyDown
            OnSelectItem = CustomCommandsViewSelectItem
            OnStartDrag = CustomCommandsViewStartDrag
          end
          object AddCommandButton: TButton
            Left = 15
            Top = 244
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditCommandButtonClick
          end
          object RemoveCommandButton: TButton
            Left = 15
            Top = 276
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCommandButtonClick
          end
          object UpCommandButton: TButton
            Left = 263
            Top = 244
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Up'
            TabOrder = 4
            OnClick = UpDownCommandButtonClick
          end
          object DownCommandButton: TButton
            Left = 263
            Top = 276
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Down'
            TabOrder = 5
            OnClick = UpDownCommandButtonClick
          end
          object EditCommandButton: TButton
            Left = 111
            Top = 244
            Width = 83
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Edit ...'
            TabOrder = 2
            OnClick = AddEditCommandButtonClick
          end
        end
      end
      object DragDropSheet: TTabSheet
        Tag = 11
        Hint = 'Drag & Drop'
        Caption = 'DragDrop'
        ImageIndex = 10
        DesignSize = (
          378
          335)
        object DragDropDownloadsGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 241
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Drag && Drop downloads'
          TabOrder = 0
          DesignSize = (
            362
            241)
          object DDExtEnabledLabel: TLabel
            Left = 35
            Top = 68
            Width = 318
            Height = 41
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'Allows direct downloads to regular local folders (e.g. Window Ex' +
              'plorer). Does not allow downloads to other destinations (ZIP arc' +
              'hives,  FTP, etc.)'
            WordWrap = True
            OnClick = DDExtLabelClick
          end
          object DDExtDisabledLabel: TLabel
            Left = 35
            Top = 140
            Width = 319
            Height = 41
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'Allows downloads to any destinations (regular folders, ZIP archi' +
              'ves,  FTP, etc.). Files are downloaded first to temporary folder' +
              ', from where they are delivered to the destination.'
            WordWrap = True
            OnClick = DDExtLabelClick
          end
          object DDExtEnabledButton: TRadioButton
            Left = 16
            Top = 48
            Width = 337
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &shell extension'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDExtDisabledButton: TRadioButton
            Left = 16
            Top = 120
            Width = 329
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &temporary folder'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDExtDisabledPanel: TPanel
            Left = 34
            Top = 184
            Width = 325
            Height = 51
            BevelOuter = bvNone
            TabOrder = 2
            DesignSize = (
              325
              51)
            object DDWarnLackOfTempSpaceCheck: TCheckBox
              Left = 0
              Top = 5
              Width = 321
              Height = 17
              Caption = '&Warn when there is not enough free space'
              TabOrder = 0
              OnClick = ControlChange
            end
            object DDWarnOnMoveCheck: TCheckBox
              Left = 0
              Top = 28
              Width = 303
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Warn when mo&ving via temporary directory'
              TabOrder = 1
              OnClick = ControlChange
            end
          end
          object DDAllowMoveInitCheck: TCheckBox
            Left = 16
            Top = 24
            Width = 319
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &moving from remote directory to other applications'
            TabOrder = 3
            OnClick = ControlChange
          end
        end
      end
      object QueueSheet: TTabSheet
        Tag = 12
        Hint = 'Background'
        Caption = 'Queue'
        ImageIndex = 11
        DesignSize = (
          378
          335)
        object QueueGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 126
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Background transfers'
          TabOrder = 0
          object Label5: TLabel
            Left = 16
            Top = 25
            Width = 211
            Height = 13
            Caption = '&Maximal number of transfers at the same time'
            FocusControl = QueueTransferLimitEdit
          end
          object QueueTransferLimitEdit: TUpDownEdit
            Left = 272
            Top = 21
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 9
            MinValue = 1
            MaxLength = 1
            TabOrder = 0
          end
          object QueueAutoPopupCheck: TCheckBox
            Left = 16
            Top = 74
            Width = 337
            Height = 17
            Caption = '&Automatically popup prompts of background transfers when idle'
            TabOrder = 2
          end
          object QueueCheck: TCheckBox
            Left = 16
            Top = 50
            Width = 337
            Height = 17
            Caption = '&Transfer on background by default'
            TabOrder = 1
          end
          object RememberPasswordCheck: TCheckBox
            Left = 16
            Top = 98
            Width = 337
            Height = 17
            Caption = 'Remember &password of main session for background transfers'
            TabOrder = 3
          end
        end
        object QueueViewGroup: TXPGroupBox
          Left = 8
          Top = 140
          Width = 362
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Queue list'
          TabOrder = 1
          object QueueViewShowButton: TRadioButton
            Left = 16
            Top = 21
            Width = 337
            Height = 17
            Caption = '&Show'
            TabOrder = 0
          end
          object QueueViewHideWhenEmptyButton: TRadioButton
            Left = 16
            Top = 45
            Width = 337
            Height = 17
            Caption = 'Hide when &empty'
            TabOrder = 1
          end
          object QueueViewHideButton: TRadioButton
            Left = 16
            Top = 69
            Width = 337
            Height = 17
            Caption = '&Hide'
            TabOrder = 2
          end
        end
      end
      object StorageSheet: TTabSheet
        Tag = 13
        Hint = 'Storage'
        Caption = 'Storage'
        ImageIndex = 12
        DesignSize = (
          378
          335)
        object RandomSeedFileLabel: TLabel
          Left = 16
          Top = 250
          Width = 82
          Height = 13
          Caption = '&Random seed file'
          FocusControl = RandomSeedFileEdit
        end
        object StorageGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Configuration storage'
          TabOrder = 0
          object RegistryStorageButton: TRadioButton
            Left = 16
            Top = 21
            Width = 289
            Height = 17
            Caption = 'Windows re&gistry'
            TabOrder = 0
            OnClick = ControlChange
          end
          object IniFileStorageButton: TRadioButton
            Left = 16
            Top = 45
            Width = 289
            Height = 17
            Caption = '&INI file (winscp3.ini)'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object RandomSeedFileEdit: TFilenameEdit
          Left = 136
          Top = 246
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
          OnKeyDown = PathEditsKeyDown
        end
        object TemporaryDirectoryGrouo: TXPGroupBox
          Left = 8
          Top = 88
          Width = 362
          Height = 148
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Temporary directory'
          TabOrder = 1
          DesignSize = (
            362
            148)
          object Label6: TLabel
            Left = 16
            Top = 22
            Width = 329
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Specify where to temporarily store edited and downloaded files.'
            WordWrap = True
            OnClick = DDExtLabelClick
          end
          object DDSystemTemporaryDirectoryButton: TRadioButton
            Left = 16
            Top = 45
            Width = 297
            Height = 17
            Caption = '&Use temporary directory of system'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDCustomTemporaryDirectoryButton: TRadioButton
            Left = 16
            Top = 69
            Width = 129
            Height = 17
            Caption = 'Use this &directory:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDTemporaryDirectoryEdit: TDirectoryEdit
            Left = 152
            Top = 65
            Width = 197
            Height = 21
            AcceptFiles = True
            DialogText = 'Select directory for temporary drag && drop files.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'DDTemporaryDirectoryEdit'
            OnClick = ControlChange
            OnKeyDown = PathEditsKeyDown
          end
          object TemporaryDirectoryCleanupCheck: TCheckBox
            Left = 16
            Top = 94
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Cleanup obsolete temporary directories on startup'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ConfirmTemporaryDirectoryCleanupCheck: TCheckBox
            Left = 32
            Top = 119
            Width = 314
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Ask before cleanup'
            TabOrder = 4
            OnClick = ControlChange
          end
        end
      end
      object TransferResumeSheet: TTabSheet
        Tag = 14
        Hint = 'Resume'
        Caption = 'Resume'
        ImageIndex = 13
        DesignSize = (
          378
          335)
        object ResumeBox: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 121
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable transfer resume for'
          TabOrder = 0
          object ResumeThresholdUnitLabel: TLabel
            Left = 136
            Top = 71
            Width = 13
            Height = 13
            Caption = 'kB'
            FocusControl = ResumeThresholdEdit
          end
          object ResumeOnButton: TRadioButton
            Left = 16
            Top = 21
            Width = 329
            Height = 17
            Caption = 'A&ll files (not recommended)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ResumeSmartButton: TRadioButton
            Left = 16
            Top = 45
            Width = 156
            Height = 17
            Caption = 'Files abo&ve'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ResumeOffButton: TRadioButton
            Left = 16
            Top = 93
            Width = 329
            Height = 17
            Caption = 'Di&sable'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ResumeThresholdEdit: TUpDownEdit
            Left = 45
            Top = 67
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
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 141
      Height = 390
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
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
          07000000250000000000000001000000FFFFFFFFFFFFFFFF0000000004000000
          0C456E7669726F6E6D656E7458230000000000000003000000FFFFFFFFFFFFFF
          FF00000000000000000A496E7465726661636558200000000000000004000000
          FFFFFFFFFFFFFFFF00000000000000000750616E656C73582300000000000000
          05000000FFFFFFFFFFFFFFFF00000000000000000A436F6D6D616E6465725822
          0000000000000006000000FFFFFFFFFFFFFFFF0000000000000000094578706C
          6F72657258200000000000000008000000FFFFFFFFFFFFFFFF00000000000000
          0007456469746F7258220000000000000007000000FFFFFFFFFFFFFFFF000000
          0003000000095472616E736665725822000000000000000B000000FFFFFFFFFF
          FFFFFF0000000000000000094472616744726F705824000000000000000C0000
          00FFFFFFFFFFFFFFFF00000000000000000B4261636B67726F756E6458200000
          00000000000E000000FFFFFFFFFFFFFFFF000000000000000007526573756D65
          58210000000000000002000000FFFFFFFFFFFFFFFF0000000000000000084C6F
          6767696E6758250000000000000009000000FFFFFFFFFFFFFFFF000000000000
          00000C496E746567726174696F6E5822000000000000000A000000FFFFFFFFFF
          FFFFFF000000000000000009436F6D6D616E64735821000000000000000D0000
          00FFFFFFFFFFFFFFFF00000000000000000853746F7261676558}
      end
    end
  end
end
