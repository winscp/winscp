object PreferencesDialog: TPreferencesDialog
  Left = 400
  Top = 161
  HelpType = htKeyword
  HelpKeyword = 'ui_preferences'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 444
  ClientWidth = 527
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    527
    444)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 264
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CloseButton: TButton
    Left = 352
    Top = 413
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
    Height = 407
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 141
      Top = 0
      Width = 386
      Height = 407
      ActivePage = PreferencesSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabIndex = 0
      TabOrder = 1
      TabStop = False
      OnChange = PageControlChange
      object PreferencesSheet: TTabSheet
        Tag = 1
        Hint = 'Environment'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_environment'
        Caption = 'Gen'
        ImageIndex = 2
        DesignSize = (
          378
          352)
        object CommonPreferencesGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 210
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Confirmations'
          TabOrder = 0
          DesignSize = (
            362
            210)
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
            Top = 44
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Deleting of files (recommended)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ConfirmClosingSessionCheck: TCheckBox
            Left = 16
            Top = 90
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'E&xiting application'
            TabOrder = 3
            OnClick = ControlChange
          end
          object DDTransferConfirmationCheck: TCheckBox
            Left = 16
            Top = 136
            Width = 338
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'D&rag && drop operations'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ContinueOnErrorCheck: TCheckBox
            Left = 16
            Top = 182
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Continue on &error (advanced users)'
            TabOrder = 7
            OnClick = ControlChange
          end
          object ConfirmExitOnCompletionCheck: TCheckBox
            Left = 16
            Top = 113
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Exiting application on o&peration completion'
            TabOrder = 4
            OnClick = ControlChange
          end
          object ConfirmResumeCheck: TCheckBox
            Left = 16
            Top = 67
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Transfer resuming'
            TabOrder = 2
            OnClick = ControlChange
          end
          object ConfirmCommandSessionCheck: TCheckBox
            Left = 16
            Top = 159
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Opening separate &shell session'
            TabOrder = 6
            OnClick = ControlChange
          end
        end
        object NotificationsGroup: TXPGroupBox
          Left = 8
          Top = 225
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_logging'
        Caption = 'Log'
        ImageIndex = 4
        DesignSize = (
          378
          352)
        inline LoggingFrame: TLoggingFrame
          Left = 5
          Top = 0
          Width = 377
          Height = 241
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          DesignSize = (
            377
            241)
          inherited LoggingGroup: TXPGroupBox
            Width = 362
            DesignSize = (
              362
              198)
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_interface'
        Caption = 'Int'
        ImageIndex = 5
        DesignSize = (
          378
          352)
        object Label1: TLabel
          Left = 8
          Top = 218
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
            inherited CommanderDescriptionLabel2: TLabel
              Width = 223
            end
            inherited ExplorerDescriptionLabel: TLabel
              Width = 225
            end
          end
        end
        object ThemeGroup: TXPGroupBox
          Left = 8
          Top = 256
          Width = 361
          Height = 52
          Caption = 'Theme'
          TabOrder = 1
          DesignSize = (
            361
            52)
          object Label7: TLabel
            Left = 16
            Top = 23
            Width = 74
            Height = 13
            Caption = 'Interface &theme'
            FocusControl = ThemeCombo
          end
          object ThemeCombo: TComboBox
            Left = 120
            Top = 18
            Width = 113
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 0
            Items.Strings = (
              'System'
              'Office XP')
          end
        end
      end
      object PanelsSheet: TTabSheet
        Tag = 4
        Hint = 'Panels'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_panels'
        Caption = 'Pan'
        ImageIndex = 3
        DesignSize = (
          378
          352)
        object PanelsRemoteDirectoryGroup: TXPGroupBox
          Left = 8
          Top = 202
          Width = 362
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Remote directory'
          TabOrder = 2
          DesignSize = (
            362
            49)
          object ShowInaccesibleDirectoriesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show in&accesible directories'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
        object PanelsCommonGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 115
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Common'
          TabOrder = 0
          DesignSize = (
            362
            115)
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
            Top = 65
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default directory is &home directory'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 16
            Top = 43
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete local files to recycle bin'
            TabOrder = 1
            OnClick = ControlChange
          end
          object PreservePanelStateCheck: TCheckBox
            Left = 16
            Top = 87
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Remember panels state when switching sessions'
            TabOrder = 3
            OnClick = ControlChange
          end
        end
        object PathInCaptionGroup: TXPGroupBox
          Left = 8
          Top = 255
          Width = 362
          Height = 90
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Path in window title'
          TabOrder = 3
          object PathInCaptionFullButton: TRadioButton
            Left = 16
            Top = 20
            Width = 337
            Height = 17
            Caption = 'Show &full path'
            TabOrder = 0
          end
          object PathInCaptionShortButton: TRadioButton
            Left = 16
            Top = 42
            Width = 337
            Height = 17
            Caption = 'Sho&w short path'
            TabOrder = 1
          end
          object PathInCaptionNoneButton: TRadioButton
            Left = 16
            Top = 64
            Width = 337
            Height = 17
            Caption = 'Do &not show'
            TabOrder = 2
          end
        end
        object DoubleClickGroup: TXPGroupBox
          Left = 8
          Top = 126
          Width = 362
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Double click'
          TabOrder = 1
          DesignSize = (
            362
            72)
          object DoubleClickActionLabel: TLabel
            Left = 16
            Top = 21
            Width = 171
            Height = 13
            Caption = '&Operation to perform on double-click'
            FocusControl = DoubleClickActionCombo
          end
          object CopyOnDoubleClickConfirmationCheck: TCheckBox
            Left = 32
            Top = 44
            Width = 313
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Confirm copy on double-click operation'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DoubleClickActionCombo: TComboBox
            Left = 232
            Top = 17
            Width = 113
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 0
            OnChange = ControlChange
            Items.Strings = (
              'Open'
              'Copy'
              'Edit')
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
          352)
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
          object Label8: TLabel
            Left = 16
            Top = 21
            Width = 107
            Height = 13
            Caption = '&Explorer style selection'
            FocusControl = NortonLikeModeCombo
          end
          object PreserveLocalDirectoryCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do not &change state of local panel when switching sessions'
            TabOrder = 1
            OnClick = ControlChange
          end
          object SwappedPanelsCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'S&wap panels (local on right, remote on left)'
            TabOrder = 2
            OnClick = ControlChange
          end
          object NortonLikeModeCombo: TComboBox
            Left = 208
            Top = 17
            Width = 137
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 0
            OnChange = ControlChange
            Items.Strings = (
              'Never'
              'Mouse only'
              'Mouse and Keyboard')
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
            OnClick = ControlChange
          end
          object CompareBySizeCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 330
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Compare by &size'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
      end
      object ExplorerSheet: TTabSheet
        Tag = 6
        Hint = 'Explorer'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_explorer'
        Caption = 'Exp'
        ImageIndex = 5
        DesignSize = (
          378
          352)
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_transfer'
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
            inherited CCLowerCaseShortButton: TRadioButton
              Width = 110
            end
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
              Width = 219
            end
            inherited TMAutomaticButton: TRadioButton
              Width = 219
            end
            inherited AsciiFileMaskCombo: THistoryComboBox
              Width = 213
            end
          end
          inherited OtherGroup: TXPGroupBox
            Left = 8
            Top = 284
            Width = 362
            DesignSize = (
              362
              61)
            inherited ExcludeFileMaskCombo: THistoryComboBox
              Width = 217
            end
            inherited ExcludeFileMaskHintText: TStaticText
              Left = 256
            end
          end
        end
      end
      object EditorSheet: TTabSheet
        Tag = 8
        Hint = 'Editors'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_editor'
        Caption = 'Edit'
        ImageIndex = 7
        DesignSize = (
          378
          352)
        object InternalEditorGroup: TXPGroupBox
          Left = 8
          Top = 268
          Width = 362
          Height = 79
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Internal editor options'
          TabOrder = 2
          DesignSize = (
            362
            79)
          object EditorFontLabel: TLabel
            Left = 149
            Top = 19
            Width = 204
            Height = 50
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'EditorFontLabel'
            Color = clBtnFace
            ParentColor = False
            OnDblClick = EditorFontLabelDblClick
          end
          object EditorFontButton: TButton
            Left = 16
            Top = 18
            Width = 105
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Select font...'
            TabOrder = 0
            OnClick = EditorFontButtonClick
          end
          object EditorWordWrapCheck: TCheckBox
            Left = 16
            Top = 52
            Width = 131
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Wrap long lines'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object SingleEditorGroup: TXPGroupBox
          Left = 8
          Top = 191
          Width = 362
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Allow multiple remote opened files (editors)'
          TabOrder = 1
          object EditorSingleEditorOnCheck: TRadioButton
            Left = 16
            Top = 21
            Width = 329
            Height = 17
            Caption = '&One file only, use main session to upload changed files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object EditorSingleEditorOffCheck: TRadioButton
            Left = 16
            Top = 45
            Width = 337
            Height = 17
            Caption = '&Multiple files, use background transfer to upload changed files'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object EditorPreferenceGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 178
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Editor preference'
          TabOrder = 0
          DesignSize = (
            362
            178)
          object EditorListView: TListView
            Left = 16
            Top = 24
            Width = 329
            Height = 80
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Mask'
              end
              item
                Caption = 'Editor'
                Width = 175
              end
              item
                Caption = 'MDI'
                Tag = 1
                Width = 45
              end
              item
                Caption = 'Text'
                Tag = 1
                Width = 45
              end>
            ColumnClick = False
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnData = EditorListViewData
            OnDblClick = EditorListViewDblClick
            OnDragDrop = EditorListViewDragDrop
            OnDragOver = ListViewDragOver
            OnKeyDown = EditorListViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddEditorButton: TButton
            Left = 15
            Top = 111
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditEditorButtonClick
          end
          object EditEditorButton: TButton
            Left = 111
            Top = 111
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Edit ...'
            TabOrder = 2
            OnClick = AddEditEditorButtonClick
          end
          object UpEditorButton: TButton
            Left = 263
            Top = 111
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 3
            OnClick = UpDownEditorButtonClick
          end
          object DownEditorButton: TButton
            Left = 263
            Top = 142
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 4
            OnClick = UpDownEditorButtonClick
          end
          object RemoveEditorButton: TButton
            Left = 15
            Top = 142
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Remove'
            TabOrder = 5
            OnClick = RemoveEditorButtonClick
          end
        end
      end
      object IntegrationSheet: TTabSheet
        Tag = 9
        Hint = 'Integration'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_integration'
        Caption = 'Integ'
        ImageIndex = 8
        DesignSize = (
          378
          352)
        object ShellIconsGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 206
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Shell icons'
          TabOrder = 0
          DesignSize = (
            362
            206)
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
            Top = 56
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &Quick Launch icon'
            TabOrder = 1
            OnClick = IconButtonClick
          end
          object SendToHookButton: TButton
            Left = 16
            Top = 88
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add upload shortcut to Explorer'#39's '#39'&Send to'#39' context menu'
            TabOrder = 2
            OnClick = IconButtonClick
          end
          object RegisterAsUrlHandlerButton: TButton
            Left = 16
            Top = 135
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Register to &handle scp:// and sftp:// addresses'
            TabOrder = 4
            OnClick = RegisterAsUrlHandlerButtonClick
          end
          object AddSearchPathButton: TButton
            Left = 16
            Top = 167
            Width = 330
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add WinSCP to &search path'
            TabOrder = 5
            OnClick = AddSearchPathButtonClick
          end
          object ShellIconsText: TStaticText
            Left = 16
            Top = 116
            Width = 330
            Height = 17
            Hint = 
              'To add shortcuts, which directly open stored session, use button' +
              ' '#39'Shell icon'#39' on '#39'Stored sessions'#39' tab of Login dialog.'
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'Associate the icons with stored session'
            TabOrder = 3
            TabStop = True
          end
        end
        object ExternalAppsGroup: TXPGroupBox
          Left = 8
          Top = 222
          Width = 362
          Height = 119
          Anchors = [akLeft, akTop, akRight]
          Caption = 'External applications'
          TabOrder = 1
          DesignSize = (
            362
            119)
          object Label2: TLabel
            Left = 16
            Top = 21
            Width = 61
            Height = 13
            Caption = '&PuTTY path:'
            FocusControl = PuttyPathEdit
          end
          object PuttyPathEdit: TFilenameEdit
            Left = 16
            Top = 38
            Width = 330
            Height = 21
            Filter = 
              'PuTTY executable (putty.exe)|putty.exe|Executable files (*.exe)|' +
              '*.exe|All files (*.*)|*.*'
            DialogTitle = 'Find PuTTY executable.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'PuttyPathEdit'
            OnChange = ControlChange
          end
          object PuttyPasswordCheck: TCheckBox
            Left = 24
            Top = 65
            Width = 329
            Height = 17
            Caption = '&Remember session password and pass it to PuTTY'
            TabOrder = 1
          end
          object AutoOpenInPuttyCheck: TCheckBox
            Left = 24
            Top = 90
            Width = 314
            Height = 17
            Caption = 'Automatically &open new sessions in PuTTY'
            TabOrder = 2
          end
        end
      end
      object CustomCommandsSheet: TTabSheet
        Tag = 10
        Hint = 'Commands'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_commands'
        Caption = 'Cmds'
        ImageIndex = 9
        DesignSize = (
          378
          352)
        object CustomCommandsGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 334
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom commands'
          TabOrder = 0
          DesignSize = (
            362
            334)
          object CustomCommandsView: TListView
            Left = 16
            Top = 24
            Width = 329
            Height = 226
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
                Tag = 1
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
            OnDragOver = ListViewDragOver
            OnKeyDown = CustomCommandsViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddCommandButton: TButton
            Left = 15
            Top = 261
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditCommandButtonClick
          end
          object RemoveCommandButton: TButton
            Left = 15
            Top = 293
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCommandButtonClick
          end
          object UpCommandButton: TButton
            Left = 263
            Top = 261
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 4
            OnClick = UpDownCommandButtonClick
          end
          object DownCommandButton: TButton
            Left = 263
            Top = 293
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 5
            OnClick = UpDownCommandButtonClick
          end
          object EditCommandButton: TButton
            Left = 111
            Top = 261
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Edit ...'
            TabOrder = 2
            OnClick = AddEditCommandButtonClick
          end
        end
      end
      object DragDropSheet: TTabSheet
        Tag = 11
        Hint = 'Drag & Drop'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_dragdrop'
        Caption = 'DragDrop'
        ImageIndex = 10
        DesignSize = (
          378
          352)
        object DragDropDownloadsGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 252
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Drag && Drop downloads'
          TabOrder = 0
          DesignSize = (
            362
            252)
          object DDExtEnabledLabel: TLabel
            Left = 35
            Top = 68
            Width = 318
            Height = 53
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
            Top = 144
            Width = 319
            Height = 54
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
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDExtDisabledButton: TRadioButton
            Left = 16
            Top = 124
            Width = 329
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &temporary folder'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DDExtDisabledPanel: TPanel
            Left = 34
            Top = 195
            Width = 325
            Height = 51
            BevelOuter = bvNone
            TabOrder = 3
            DesignSize = (
              325
              51)
            object DDWarnLackOfTempSpaceCheck: TCheckBox
              Left = 0
              Top = 5
              Width = 321
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = '&Warn when there is not enough free space'
              TabOrder = 0
              OnClick = ControlChange
            end
            object DDWarnOnMoveCheck: TCheckBox
              Left = 0
              Top = 28
              Width = 321
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
            Width = 337
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &moving from remote directory to other applications'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object QueueSheet: TTabSheet
        Tag = 12
        Hint = 'Background'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_background'
        Caption = 'Queue'
        ImageIndex = 11
        DesignSize = (
          378
          352)
        object QueueGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 150
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
            Top = 98
            Width = 337
            Height = 17
            Caption = '&Automatically popup prompts of background transfers when idle'
            TabOrder = 3
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
            Top = 122
            Width = 337
            Height = 17
            Caption = 'Remember &password of main session for background transfers'
            TabOrder = 4
          end
          object QueueNoConfirmationCheck: TCheckBox
            Left = 16
            Top = 74
            Width = 337
            Height = 17
            Caption = '&No confirmations for background transfers'
            TabOrder = 2
          end
        end
        object QueueViewGroup: TXPGroupBox
          Left = 8
          Top = 164
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_storage'
        Caption = 'Stor'
        ImageIndex = 12
        DesignSize = (
          378
          352)
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
        object OtherStorageGroup: TXPGroupBox
          Left = 8
          Top = 243
          Width = 362
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other'
          TabOrder = 2
          DesignSize = (
            362
            53)
          object RandomSeedFileLabel: TLabel
            Left = 16
            Top = 24
            Width = 82
            Height = 13
            Caption = '&Random seed file'
            FocusControl = RandomSeedFileEdit
          end
          object RandomSeedFileEdit: TFilenameEdit
            Left = 128
            Top = 19
            Width = 221
            Height = 21
            AcceptFiles = True
            DefaultExt = 'log'
            Filter = 'Random seed files (*.rnd)|*.rnd|All files (*.*)|*.*'
            DialogOptions = [ofHideReadOnly, ofPathMustExist]
            DialogTitle = 'Select file for random seed'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'RandomSeedFileEdit'
            OnChange = ControlChange
          end
        end
      end
      object TransferResumeSheet: TTabSheet
        Tag = 14
        Hint = 'Resume'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_resume'
        Caption = 'Resum'
        ImageIndex = 13
        DesignSize = (
          378
          352)
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
      object UpdatesSheet: TTabSheet
        Tag = 15
        Hint = 'Updates'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_updates'
        Caption = 'Upd'
        ImageIndex = 14
        DesignSize = (
          378
          352)
        object UpdatesGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 123
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatically check for application updates'
          TabOrder = 0
          object UpdatesNeverButton: TRadioButton
            Left = 16
            Top = 21
            Width = 329
            Height = 17
            Caption = '&Never'
            TabOrder = 0
            OnClick = ControlChange
          end
          object UpdatesDailyButton: TRadioButton
            Left = 16
            Top = 45
            Width = 156
            Height = 17
            Caption = '&Daily'
            TabOrder = 1
            OnClick = ControlChange
          end
          object UpdatesWeeklyButton: TRadioButton
            Left = 16
            Top = 69
            Width = 156
            Height = 17
            Caption = '&Weekly'
            TabOrder = 2
            OnClick = ControlChange
          end
          object UpdatesMonthlyButton: TRadioButton
            Left = 16
            Top = 93
            Width = 156
            Height = 17
            Caption = '&Monthly'
            TabOrder = 3
            OnClick = ControlChange
          end
        end
        object UpdatesProxyGroup: TXPGroupBox
          Left = 8
          Top = 139
          Width = 362
          Height = 93
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 1
          DesignSize = (
            362
            93)
          object Label15: TLabel
            Left = 30
            Top = 41
            Width = 78
            Height = 13
            Caption = 'Pro&xy host name'
            FocusControl = UpdatesProxyHostEdit
          end
          object Label18: TLabel
            Left = 257
            Top = 41
            Width = 57
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number'
            FocusControl = UpdatesProxyPortEdit
          end
          object UpdatesProxyPortEdit: TUpDownEdit
            Left = 257
            Top = 58
            Width = 94
            Height = 21
            MaxValue = 65535
            MinValue = 1
            Anchors = [akTop, akRight]
            TabOrder = 2
          end
          object UpdatesProxyHostEdit: TEdit
            Left = 30
            Top = 58
            Width = 214
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 1
            Text = 'UpdatesProxyHostEdit'
          end
          object UpdatesProxyCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 333
            Height = 17
            Caption = '&Use proxy server'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object CopyParamListSheet: TTabSheet
        Tag = 16
        Hint = 'Presets'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_presets'
        Caption = 'Pres'
        ImageIndex = 15
        DesignSize = (
          378
          352)
        object CopyParamListGroup: TXPGroupBox
          Left = 8
          Top = 8
          Width = 362
          Height = 277
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Transfer settings presets'
          TabOrder = 0
          DesignSize = (
            362
            277)
          object CopyParamListView: TListView
            Left = 16
            Top = 24
            Width = 329
            Height = 169
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Preset description'
                Width = 100
              end
              item
                Caption = 'Auto'
                Tag = 1
                Width = 40
              end>
            ColumnClick = False
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            ViewStyle = vsReport
            OnData = CopyParamListViewData
            OnDblClick = CopyParamListViewDblClick
            OnDragDrop = CopyParamListViewDragDrop
            OnDragOver = ListViewDragOver
            OnInfoTip = CopyParamListViewInfoTip
            OnKeyDown = CopyParamListViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddCopyParamButton: TButton
            Left = 15
            Top = 204
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditCopyParamButtonClick
          end
          object RemoveCopyParamButton: TButton
            Left = 15
            Top = 236
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCopyParamButtonClick
          end
          object UpCopyParamButton: TButton
            Left = 263
            Top = 204
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 5
            OnClick = UpDownCopyParamButtonClick
          end
          object DownCopyParamButton: TButton
            Left = 263
            Top = 236
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 6
            OnClick = UpDownCopyParamButtonClick
          end
          object EditCopyParamButton: TButton
            Left = 111
            Top = 204
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Edit ...'
            TabOrder = 2
            OnClick = AddEditCopyParamButtonClick
          end
          object DuplicateCopyParamButton: TButton
            Left = 111
            Top = 236
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = 'Du&plicate ...'
            TabOrder = 4
            OnClick = AddEditCopyParamButtonClick
          end
        end
        object CopyParamListOptionsGroup: TXPGroupBox
          Left = 8
          Top = 292
          Width = 362
          Height = 50
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Options'
          TabOrder = 1
          object CopyParamAutoSelectNoticeCheck: TCheckBox
            Left = 12
            Top = 19
            Width = 341
            Height = 17
            Caption = '&Show announcement when transfer settings preset is autoselected'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 141
      Height = 407
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        141
        407)
      object NavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 125
        Height = 397
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = NavigationTreeChange
        Items.Data = {
          08000000250000000000000001000000FFFFFFFFFFFFFFFF0000000004000000
          0C456E7669726F6E6D656E7458230000000000000003000000FFFFFFFFFFFFFF
          FF00000000000000000A496E7465726661636558200000000000000004000000
          FFFFFFFFFFFFFFFF00000000000000000750616E656C73582300000000000000
          05000000FFFFFFFFFFFFFFFF00000000000000000A436F6D6D616E6465725822
          0000000000000006000000FFFFFFFFFFFFFFFF0000000000000000094578706C
          6F72657258200000000000000008000000FFFFFFFFFFFFFFFF00000000000000
          0007456469746F7258220000000000000007000000FFFFFFFFFFFFFFFF000000
          0004000000095472616E7366657258210000000000000010000000FFFFFFFFFF
          FFFFFF000000000000000008507265736574735822000000000000000B000000
          FFFFFFFFFFFFFFFF0000000000000000094472616744726F7058240000000000
          00000C000000FFFFFFFFFFFFFFFF00000000000000000B4261636B67726F756E
          645820000000000000000E000000FFFFFFFFFFFFFFFF00000000000000000752
          6573756D6558210000000000000002000000FFFFFFFFFFFFFFFF000000000000
          0000084C6F6767696E6758250000000000000009000000FFFFFFFFFFFFFFFF00
          000000000000000C496E746567726174696F6E5822000000000000000A000000
          FFFFFFFFFFFFFFFF000000000000000009436F6D6D616E647358210000000000
          00000D000000FFFFFFFFFFFFFFFF00000000000000000853746F726167655821
          000000000000000F000000FFFFFFFFFFFFFFFF00000000000000000855706461
          74657358}
      end
    end
  end
  object HelpButton: TButton
    Left = 440
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
