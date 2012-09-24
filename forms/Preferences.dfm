object PreferencesDialog: TPreferencesDialog
  Left = 400
  Top = 161
  HelpType = htKeyword
  HelpKeyword = 'ui_preferences'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 400
  ClientWidth = 513
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    513
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 250
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CloseButton: TButton
    Left = 338
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 513
    Height = 363
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 132
      Top = 0
      Width = 381
      Height = 363
      ActivePage = PreferencesSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object CommonPreferencesGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 256
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Confirmations'
          TabOrder = 0
          DesignSize = (
            357
            256)
          object ConfirmOverwritingCheck: TCheckBox
            Left = 16
            Top = 44
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Overwriting of files'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ConfirmDeletingCheck: TCheckBox
            Left = 16
            Top = 67
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Deleting of files (recommended)'
            TabOrder = 2
            OnClick = ControlChange
          end
          object ConfirmClosingSessionCheck: TCheckBox
            Left = 16
            Top = 136
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Exiting appli&cation'
            TabOrder = 5
            OnClick = ControlChange
          end
          object DDTransferConfirmationCheck: TCheckBox
            Left = 16
            Top = 182
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'D&rag && drop operations'
            TabOrder = 7
            OnClick = ControlChange
          end
          object ContinueOnErrorCheck: TCheckBox
            Left = 16
            Top = 228
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Continue on &error (advanced users)'
            TabOrder = 9
            OnClick = ControlChange
          end
          object ConfirmExitOnCompletionCheck: TCheckBox
            Left = 16
            Top = 159
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Exiting application on o&peration completion'
            TabOrder = 6
            OnClick = ControlChange
          end
          object ConfirmResumeCheck: TCheckBox
            Left = 16
            Top = 113
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Trans&fer resuming'
            TabOrder = 4
            OnClick = ControlChange
          end
          object ConfirmCommandSessionCheck: TCheckBox
            Left = 16
            Top = 205
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Opening separate &shell session'
            TabOrder = 8
            OnClick = ControlChange
          end
          object ConfirmRecyclingCheck: TCheckBox
            Left = 16
            Top = 90
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Moving files to Recycle bin'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ConfirmTransferringCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Transferring of files'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
        object NotificationsGroup: TGroupBox
          Left = 8
          Top = 271
          Width = 357
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Notifications'
          TabOrder = 1
          DesignSize = (
            357
            73)
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
            Width = 260
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Beep when work finishes, if it lasted more then'
            TabOrder = 0
            OnClick = ControlChange
          end
          object BeepOnFinishAfterEdit: TUpDownEdit
            Left = 282
            Top = 19
            Width = 57
            Height = 21
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 999.000000000000000000
            MaxLength = 3
            TabOrder = 1
            OnChange = ControlChange
          end
          object BalloonNotificationsCheck: TCheckBox
            Left = 16
            Top = 46
            Width = 309
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show balloon &notifications in taskbar status area (system tray)'
            TabOrder = 2
            OnClick = ControlChange
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
        TabVisible = False
        DesignSize = (
          373
          353)
        inline LoggingFrame: TLoggingFrame
          Left = 5
          Top = 5
          Width = 372
          Height = 309
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          DesignSize = (
            372
            309)
        end
      end
      object GeneralSheet: TTabSheet
        Tag = 3
        Hint = 'Interface'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_interface'
        Caption = 'Int'
        ImageIndex = 5
        TabVisible = False
        DesignSize = (
          373
          353)
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
          Width = 357
          Height = 202
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          inherited InterfaceGroup: TGroupBox
            Width = 362
            inherited CommanderDescriptionLabel2: TLabel
              Width = 223
            end
            inherited ExplorerDescriptionLabel: TLabel
              Width = 225
            end
          end
        end
        object ThemeGroup: TGroupBox
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
            Width = 82
            Height = 13
            Caption = 'Interface &theme:'
            FocusControl = ThemeCombo
          end
          object ThemeCombo: TComboBox
            Left = 120
            Top = 18
            Width = 113
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Items.Strings = (
              'System'
              'Office XP'
              'Office 2003')
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object PanelsCommonGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 195
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Common'
          TabOrder = 0
          DesignSize = (
            357
            195)
          object ShowHiddenFilesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show hidden files (Ctrl+Alt+H)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DefaultDirIsHomeCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default directory is &home directory'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete local files to recycle bin'
            TabOrder = 1
            OnClick = ControlChange
          end
          object PreservePanelStateCheck: TCheckBox
            Left = 16
            Top = 93
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Remember panels'#39' state when switching sessions'
            TabOrder = 3
            OnClick = ControlChange
          end
          object RenameWholeNameCheck: TCheckBox
            Left = 16
            Top = 117
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Select &whole name when renaming file'
            TabOrder = 4
            OnClick = ControlChange
          end
          object FormatSizeBytesCheck: TCheckBox
            Left = 16
            Top = 141
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show file sizes in short &format'
            TabOrder = 5
            OnClick = ControlChange
          end
          object FullRowSelectCheck: TCheckBox
            Left = 16
            Top = 165
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Full row select'
            TabOrder = 6
            OnClick = ControlChange
          end
        end
        object DoubleClickGroup: TGroupBox
          Left = 8
          Top = 208
          Width = 357
          Height = 74
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Double-click'
          TabOrder = 1
          DesignSize = (
            357
            74)
          object DoubleClickActionLabel: TLabel
            Left = 16
            Top = 21
            Width = 179
            Height = 13
            Caption = '&Operation to perform on double-click:'
            FocusControl = DoubleClickActionCombo
          end
          object CopyOnDoubleClickConfirmationCheck: TCheckBox
            Left = 32
            Top = 45
            Width = 308
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Confirm copy on double-click operation'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DoubleClickActionCombo: TComboBox
            Left = 232
            Top = 17
            Width = 108
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_commander'
        Caption = 'Cmd'
        ImageIndex = 3
        TabVisible = False
        DesignSize = (
          373
          353)
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 361
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this tab applies to Commander interface only.'
          WordWrap = True
        end
        object PanelsGroup: TGroupBox
          Left = 8
          Top = 38
          Width = 357
          Height = 123
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panels'
          TabOrder = 0
          DesignSize = (
            357
            123)
          object Label8: TLabel
            Left = 16
            Top = 21
            Width = 116
            Height = 13
            Caption = '&Explorer-style selection:'
            FocusControl = NortonLikeModeCombo
          end
          object PreserveLocalDirectoryCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do not &change state of local panel when switching sessions'
            TabOrder = 1
            OnClick = ControlChange
          end
          object SwappedPanelsCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'S&wap panels (local on right, remote on left)'
            TabOrder = 2
            OnClick = ControlChange
          end
          object NortonLikeModeCombo: TComboBox
            Left = 208
            Top = 17
            Width = 132
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ControlChange
            Items.Strings = (
              'Never'
              'Mouse only'
              'Mouse and Keyboard')
          end
          object TreeOnLeftCheck: TCheckBox
            Left = 16
            Top = 93
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show &directory tree left of file list'
            TabOrder = 3
            OnClick = ControlChange
          end
        end
        object CommanderMiscGroup: TGroupBox
          Left = 8
          Top = 170
          Width = 357
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 1
          DesignSize = (
            357
            53)
          object UseLocationProfilesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use Location Profiles instead of Directory Bookmarks'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
        object CompareCriterionsGroup: TGroupBox
          Left = 8
          Top = 233
          Width = 357
          Height = 74
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Compare directory criterions'
          TabOrder = 2
          DesignSize = (
            357
            74)
          object CompareByTimeCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Compare by &time'
            TabOrder = 0
            OnClick = ControlChange
          end
          object CompareBySizeCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 325
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object Label4: TLabel
          Left = 8
          Top = 8
          Width = 361
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this tab applies to Explorer interface only.'
          WordWrap = True
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 38
          Width = 357
          Height = 54
          Anchors = [akLeft, akTop, akRight]
          Caption = 'View'
          TabOrder = 0
          DesignSize = (
            357
            54)
          object ShowFullAddressCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
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
        TabVisible = False
        object CopyParamGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 94
          Caption = 'Transfer settings'
          TabOrder = 0
          OnClick = CopyParamGroupClick
          DesignSize = (
            357
            94)
          object CopyParamLabel: TLabel
            Left = 7
            Top = 15
            Width = 343
            Height = 70
            Anchors = [akLeft, akTop, akRight, akBottom]
            AutoSize = False
            Caption = 'CopyParamLabel'
            WordWrap = True
            OnClick = CopyParamGroupClick
          end
        end
        object TransferSettingsButton: TButton
          Left = 8
          Top = 108
          Width = 87
          Height = 25
          Caption = '&Edit...'
          TabOrder = 1
          OnClick = CopyParamGroupClick
        end
      end
      object EditorSheet: TTabSheet
        Tag = 8
        Hint = 'Editors'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_editor'
        Caption = 'Edit'
        ImageIndex = 7
        TabVisible = False
        DesignSize = (
          373
          353)
        object InternalEditorGroup: TGroupBox
          Left = 8
          Top = 205
          Width = 357
          Height = 146
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Internal editor options'
          TabOrder = 1
          DesignSize = (
            357
            146)
          object EditorFontLabel: TLabel
            Left = 148
            Top = 49
            Width = 199
            Height = 88
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'EditorFontLabel'
            Color = clBtnFace
            ParentColor = False
            Transparent = False
            OnDblClick = EditorFontLabelDblClick
          end
          object Label9: TLabel
            Left = 16
            Top = 47
            Width = 71
            Height = 13
            Caption = '&Tabulator size:'
            FocusControl = EditorTabSizeEdit
          end
          object Label11: TLabel
            Left = 16
            Top = 95
            Width = 85
            Height = 13
            Caption = 'Default &encoding:'
            FocusControl = EditorEncodingCombo
          end
          object EditorFontButton: TButton
            Left = 148
            Top = 18
            Width = 105
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Select font...'
            TabOrder = 3
            OnClick = EditorFontButtonClick
          end
          object EditorWordWrapCheck: TCheckBox
            Left = 16
            Top = 20
            Width = 126
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Wrap long lines'
            TabOrder = 0
            OnClick = ControlChange
          end
          object EditorTabSizeEdit: TUpDownEdit
            Left = 16
            Top = 64
            Width = 89
            Height = 21
            Alignment = taRightJustify
            MaxValue = 99.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            MaxLength = 2
            TabOrder = 1
            OnChange = ControlChange
          end
          object EditorEncodingCombo: TComboBox
            Left = 16
            Top = 112
            Width = 113
            Height = 21
            Style = csDropDownList
            MaxLength = 2
            TabOrder = 2
            OnChange = ControlChange
          end
        end
        object EditorPreferenceGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 192
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Editor preference'
          TabOrder = 0
          DesignSize = (
            357
            192)
          object EditorListView2: TListView
            Left = 16
            Top = 24
            Width = 324
            Height = 94
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Mask'
                Width = 70
              end
              item
                Caption = 'Editor'
                Width = 190
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
            OnData = EditorListView2Data
            OnDblClick = EditorListView2DblClick
            OnEndDrag = ListViewEndDrag
            OnDragDrop = EditorListView2DragDrop
            OnDragOver = ListViewDragOver
            OnKeyDown = EditorListView2KeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddEditorButton: TButton
            Left = 16
            Top = 125
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditEditorButtonClick
          end
          object EditEditorButton: TButton
            Left = 112
            Top = 125
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Edit ...'
            TabOrder = 2
            OnClick = AddEditEditorButtonClick
          end
          object UpEditorButton: TButton
            Left = 258
            Top = 125
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 3
            OnClick = UpDownEditorButtonClick
          end
          object DownEditorButton: TButton
            Left = 258
            Top = 156
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 4
            OnClick = UpDownEditorButtonClick
          end
          object RemoveEditorButton: TButton
            Left = 16
            Top = 156
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object ShellIconsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 206
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Windows Shell'
          TabOrder = 0
          DesignSize = (
            357
            206)
          object DesktopIconButton: TButton
            Left = 16
            Top = 24
            Width = 325
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &desktop icon'
            TabOrder = 0
            OnClick = IconButtonClick
          end
          object QuickLaunchIconButton: TButton
            Left = 16
            Top = 56
            Width = 325
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &Quick Launch icon'
            TabOrder = 1
            OnClick = IconButtonClick
          end
          object SendToHookButton: TButton
            Left = 16
            Top = 88
            Width = 325
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add upload shortcut to Explorer'#39's '#39'&Send to'#39' context menu'
            TabOrder = 2
            OnClick = IconButtonClick
          end
          object RegisterAsUrlHandlerButton: TButton
            Left = 16
            Top = 135
            Width = 325
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Register to &handle sftp:// and scp:// addresses'
            TabOrder = 4
            OnClick = RegisterAsUrlHandlerButtonClick
          end
          object AddSearchPathButton: TButton
            Left = 16
            Top = 167
            Width = 325
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add WinSCP to &search path'
            TabOrder = 5
            OnClick = AddSearchPathButtonClick
          end
          object ShellIconsText: TStaticText
            Left = 11
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
      end
      object CustomCommandsSheet: TTabSheet
        Tag = 10
        Hint = 'Commands'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_commands'
        Caption = 'Cmds'
        ImageIndex = 9
        TabVisible = False
        DesignSize = (
          373
          353)
        object CustomCommandsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 335
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom commands'
          TabOrder = 0
          DesignSize = (
            357
            335)
          object CustomCommandsView: TListView
            Left = 16
            Top = 24
            Width = 324
            Height = 227
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
            OnEndDrag = ListViewEndDrag
            OnDragDrop = CustomCommandsViewDragDrop
            OnDragOver = ListViewDragOver
            OnKeyDown = CustomCommandsViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddCommandButton: TButton
            Left = 16
            Top = 262
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditCommandButtonClick
          end
          object RemoveCommandButton: TButton
            Left = 16
            Top = 294
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCommandButtonClick
          end
          object UpCommandButton: TButton
            Left = 258
            Top = 262
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 4
            OnClick = UpDownCommandButtonClick
          end
          object DownCommandButton: TButton
            Left = 258
            Top = 294
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 5
            OnClick = UpDownCommandButtonClick
          end
          object EditCommandButton: TButton
            Left = 112
            Top = 262
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object DragDropDownloadsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 252
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Drag && Drop downloads'
          TabOrder = 0
          DesignSize = (
            357
            252)
          object DDExtEnabledLabel: TLabel
            Left = 35
            Top = 68
            Width = 313
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
            Width = 314
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
            Width = 332
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &shell extension'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDExtDisabledButton: TRadioButton
            Left = 16
            Top = 124
            Width = 324
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &temporary folder'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DDExtDisabledPanel: TPanel
            Left = 34
            Top = 195
            Width = 315
            Height = 51
            BevelOuter = bvNone
            TabOrder = 3
            DesignSize = (
              315
              51)
            object DDWarnLackOfTempSpaceCheck: TCheckBox
              Left = 0
              Top = 5
              Width = 315
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = '&Warn when there is not enough free space'
              TabOrder = 0
              OnClick = ControlChange
            end
            object DDWarnOnMoveCheck: TCheckBox
              Left = 0
              Top = 28
              Width = 315
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
            Width = 332
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
        Caption = 'Que'
        ImageIndex = 11
        TabVisible = False
        DesignSize = (
          373
          353)
        object QueueGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 198
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Background transfers'
          TabOrder = 0
          object Label5: TLabel
            Left = 16
            Top = 25
            Width = 224
            Height = 13
            Caption = '&Maximal number of transfers at the same time:'
            FocusControl = QueueTransferLimitEdit
          end
          object QueueTransferLimitEdit: TUpDownEdit
            Left = 272
            Top = 21
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 9.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            MaxLength = 1
            TabOrder = 0
          end
          object QueueAutoPopupCheck: TCheckBox
            Left = 16
            Top = 146
            Width = 337
            Height = 17
            Caption = '&Automatically popup prompts of background transfers when idle'
            TabOrder = 5
          end
          object QueueCheck: TCheckBox
            Left = 16
            Top = 74
            Width = 337
            Height = 17
            Caption = 'Transfer on &background by default'
            TabOrder = 2
          end
          object RememberPasswordCheck: TCheckBox
            Left = 16
            Top = 170
            Width = 337
            Height = 17
            Caption = 'Remember &password of main session for background transfers'
            TabOrder = 6
          end
          object QueueNoConfirmationCheck: TCheckBox
            Left = 16
            Top = 122
            Width = 337
            Height = 17
            Caption = '&No confirmations for background transfers'
            TabOrder = 4
          end
          object QueueIndividuallyCheck: TCheckBox
            Left = 16
            Top = 98
            Width = 337
            Height = 17
            Caption = '&Transfer each file individually on background by default'
            TabOrder = 3
          end
          object EnableQueueByDefaultCheck: TCheckBox
            Left = 16
            Top = 50
            Width = 337
            Height = 17
            Caption = '&Enable queue processing by default'
            TabOrder = 1
          end
        end
        object QueueViewGroup: TGroupBox
          Left = 8
          Top = 212
          Width = 357
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
            Caption = 'Hide &when empty'
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object StorageGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
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
          object IniFileStorageButton2: TRadioButton
            Left = 16
            Top = 45
            Width = 289
            Height = 17
            Caption = '&INI file (winscp.ini)'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object TemporaryDirectoryGrouo: TGroupBox
          Left = 8
          Top = 88
          Width = 357
          Height = 198
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Temporary directory'
          TabOrder = 1
          DesignSize = (
            357
            198)
          object Label6: TLabel
            Left = 16
            Top = 22
            Width = 324
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Specify where to temporarily store edited and downloaded files.'
            WordWrap = True
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
            Width = 192
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PathEditAfterDialog
            DialogText = 'Select directory for temporary drag && drop files.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'DDTemporaryDirectoryEdit'
            OnClick = ControlChange
          end
          object TemporaryDirectoryCleanupCheck: TCheckBox
            Left = 16
            Top = 144
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Cleanup obsolete temporary directories on startup'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ConfirmTemporaryDirectoryCleanupCheck: TCheckBox
            Left = 32
            Top = 169
            Width = 309
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Ask before cleanup'
            TabOrder = 6
            OnClick = ControlChange
          end
          object TemporaryDirectoryAppendSessionCheck: TCheckBox
            Left = 16
            Top = 94
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Append &session name to temporary path'
            TabOrder = 3
            OnClick = ControlChange
          end
          object TemporaryDirectoryAppendPathCheck: TCheckBox
            Left = 16
            Top = 119
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Append remote &path to temporary path'
            TabOrder = 4
            OnClick = ControlChange
          end
        end
        object OtherStorageGroup: TGroupBox
          Left = 8
          Top = 293
          Width = 357
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other'
          TabOrder = 2
          DesignSize = (
            357
            53)
          object RandomSeedFileLabel: TLabel
            Left = 16
            Top = 24
            Width = 86
            Height = 13
            Caption = '&Random seed file:'
            FocusControl = RandomSeedFileEdit
          end
          object RandomSeedFileEdit: TFilenameEdit
            Left = 128
            Top = 19
            Width = 216
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PathEditAfterDialog
            DefaultExt = 'log'
            Filter = 'Random seed files (*.rnd)|*.rnd|All files (*.*)|*.*'
            DialogOptions = [ofHideReadOnly, ofPathMustExist]
            DialogTitle = 'Select file for random seed'
            OnCreateEditDialog = RandomSeedFileEditCreateEditDialog
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'RandomSeedFileEdit'
            OnChange = ControlChange
          end
        end
      end
      object TransferEnduranceSheet: TTabSheet
        Tag = 14
        Hint = 'Endurance'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_resume'
        Caption = 'Endur'
        ImageIndex = 13
        TabVisible = False
        DesignSize = (
          373
          353)
        object ResumeBox: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 123
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable transfer resume/transfer to temporary filename for'
          TabOrder = 0
          object ResumeThresholdUnitLabel: TLabel
            Left = 136
            Top = 71
            Width = 14
            Height = 13
            Caption = 'KiB'
            FocusControl = ResumeThresholdEdit
          end
          object ResumeOnButton: TRadioButton
            Left = 16
            Top = 21
            Width = 329
            Height = 17
            Caption = 'A&ll files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ResumeSmartButton: TRadioButton
            Left = 16
            Top = 45
            Width = 156
            Height = 17
            Caption = 'Files abo&ve:'
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
            Increment = 10.000000000000000000
            MaxValue = 4194304.000000000000000000
            TabOrder = 2
            OnClick = ControlChange
          end
        end
        object SessionReopenGroup: TGroupBox
          Left = 8
          Top = 139
          Width = 357
          Height = 176
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatic reconnect'
          TabOrder = 1
          object SessionReopenAutoLabel: TLabel
            Left = 34
            Top = 72
            Width = 82
            Height = 13
            Caption = '&Reconnect after:'
            FocusControl = SessionReopenAutoEdit
          end
          object SessionReopenAutoSecLabel: TLabel
            Left = 254
            Top = 72
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenAutoEdit
          end
          object SessionReopenTimeoutLabel: TLabel
            Left = 16
            Top = 149
            Width = 110
            Height = 13
            Caption = '&Keep reconnecting for:'
            FocusControl = SessionReopenTimeoutEdit
          end
          object SessionReopenTimeoutSecLabel: TLabel
            Left = 254
            Top = 149
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenTimeoutEdit
          end
          object SessionReopenAutoStallLabel: TLabel
            Left = 34
            Top = 122
            Width = 82
            Height = 13
            Caption = 'Re&connect after:'
            FocusControl = SessionReopenAutoStallEdit
          end
          object SessionReopenAutoStallSecLabel: TLabel
            Left = 254
            Top = 122
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenAutoStallEdit
          end
          object SessionReopenAutoCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 337
            Height = 17
            Caption = '&Automatically reconnect session, if it breaks during transfer'
            TabOrder = 0
            OnClick = ControlChange
          end
          object SessionReopenAutoEdit: TUpDownEdit
            Left = 168
            Top = 67
            Width = 81
            Height = 21
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 2
          end
          object SessionReopenAutoIdleCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 337
            Height = 17
            Caption = 'Automatically reconnect session, if it breaks &while idle'
            TabOrder = 1
            OnClick = ControlChange
          end
          object SessionReopenTimeoutEdit: TUpDownEdit
            Left = 168
            Top = 144
            Width = 81
            Height = 21
            Alignment = taRightJustify
            Increment = 30.000000000000000000
            MaxValue = 86400.000000000000000000
            MaxLength = 5
            TabOrder = 5
            OnGetValue = SessionReopenTimeoutEditGetValue
            OnSetValue = SessionReopenTimeoutEditSetValue
          end
          object SessionReopenAutoStallCheck: TCheckBox
            Left = 16
            Top = 95
            Width = 337
            Height = 17
            Caption = 'Automatically reconnect session, if it &stalls'
            TabOrder = 3
            OnClick = ControlChange
          end
          object SessionReopenAutoStallEdit: TUpDownEdit
            Left = 168
            Top = 117
            Width = 81
            Height = 21
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 4
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object UpdatesGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 51
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatic check for application updates'
          TabOrder = 0
          DesignSize = (
            357
            51)
          object Label12: TLabel
            Left = 16
            Top = 23
            Width = 115
            Height = 13
            Caption = 'Automatic check &period:'
            FocusControl = UpdatesPeriodCombo
          end
          object UpdatesPeriodCombo: TComboBox
            Left = 256
            Top = 18
            Width = 88
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Items.Strings = (
              'Never'
              'Daily'
              'Weekly'
              'Monthly')
          end
        end
        object UpdatesProxyGroup: TGroupBox
          Left = 8
          Top = 152
          Width = 357
          Height = 142
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 2
          DesignSize = (
            357
            142)
          object UpdatesProxyHostLabel: TLabel
            Left = 34
            Top = 91
            Width = 85
            Height = 13
            Caption = 'Proxy &host name:'
            FocusControl = UpdatesProxyHostEdit
          end
          object UpdatesProxyPortLabel: TLabel
            Left = 252
            Top = 91
            Width = 63
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = UpdatesProxyPortEdit
          end
          object UpdatesProxyPortEdit: TUpDownEdit
            Left = 252
            Top = 108
            Width = 94
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
          end
          object UpdatesProxyHostEdit: TEdit
            Left = 34
            Top = 108
            Width = 205
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 3
            Text = 'UpdatesProxyHostEdit'
          end
          object UpdatesProxyCheck: TRadioButton
            Left = 16
            Top = 69
            Width = 333
            Height = 17
            Caption = '&Use proxy server'
            TabOrder = 2
            OnClick = ControlChange
          end
          object UpdatesDirectCheck: TRadioButton
            Left = 16
            Top = 21
            Width = 333
            Height = 17
            Caption = 'No &proxy'
            TabOrder = 0
            OnClick = ControlChange
          end
          object UpdatesAutoCheck: TRadioButton
            Left = 16
            Top = 45
            Width = 333
            Height = 17
            Caption = '&Automatically detect proxy settings'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object UpdatesOptionsGroup: TGroupBox
          Left = 8
          Top = 65
          Width = 357
          Height = 81
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          TabOrder = 1
          DesignSize = (
            357
            81)
          object UpdatesBetaVersionsLabel: TLabel
            Left = 16
            Top = 23
            Width = 118
            Height = 13
            Caption = 'Check for &beta versions:'
            FocusControl = UpdatesBetaVersionsCombo
          end
          object UpdatesBetaVersionsCombo: TComboBox
            Left = 256
            Top = 18
            Width = 88
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object CollectUsageCheck: TCheckBox
            Left = 16
            Top = 49
            Width = 234
            Height = 17
            Caption = 'Allow &anonymous usage statistics'
            TabOrder = 1
            OnClick = ControlChange
          end
          object UsageViewButton: TButton
            Left = 255
            Top = 45
            Width = 90
            Height = 25
            Caption = 'View &statistics'
            TabOrder = 2
            OnClick = UsageViewButtonClick
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
        TabVisible = False
        DesignSize = (
          373
          353)
        object CopyParamListGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 277
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Transfer settings presets'
          TabOrder = 0
          DesignSize = (
            357
            277)
          object CopyParamListView: TListView
            Left = 16
            Top = 24
            Width = 324
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
            OnEndDrag = ListViewEndDrag
            OnDragDrop = CopyParamListViewDragDrop
            OnDragOver = ListViewDragOver
            OnInfoTip = CopyParamListViewInfoTip
            OnKeyDown = CopyParamListViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddCopyParamButton: TButton
            Left = 16
            Top = 204
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Add ...'
            TabOrder = 1
            OnClick = AddEditCopyParamButtonClick
          end
          object RemoveCopyParamButton: TButton
            Left = 16
            Top = 236
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCopyParamButtonClick
          end
          object UpCopyParamButton: TButton
            Left = 258
            Top = 204
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 5
            OnClick = UpDownCopyParamButtonClick
          end
          object DownCopyParamButton: TButton
            Left = 258
            Top = 236
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 6
            OnClick = UpDownCopyParamButtonClick
          end
          object EditCopyParamButton: TButton
            Left = 112
            Top = 204
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Edit ...'
            TabOrder = 2
            OnClick = AddEditCopyParamButtonClick
          end
          object DuplicateCopyParamButton: TButton
            Left = 112
            Top = 236
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = 'Du&plicate ...'
            TabOrder = 4
            OnClick = AddEditCopyParamButtonClick
          end
        end
        object CopyParamListOptionsGroup: TGroupBox
          Left = 8
          Top = 292
          Width = 357
          Height = 51
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Options'
          TabOrder = 1
          object CopyParamAutoSelectNoticeCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 337
            Height = 17
            Caption = '&Show announcement when transfer settings preset is autoselected'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object WindowSheet: TTabSheet
        Tag = 17
        Hint = 'Window'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_window'
        Caption = 'Win'
        ImageIndex = 16
        TabVisible = False
        DesignSize = (
          373
          353)
        object PathInCaptionGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 94
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Path in window title'
          TabOrder = 0
          object PathInCaptionFullButton: TRadioButton
            Left = 16
            Top = 21
            Width = 337
            Height = 17
            Caption = 'Show &full path'
            TabOrder = 0
          end
          object PathInCaptionShortButton: TRadioButton
            Left = 16
            Top = 44
            Width = 337
            Height = 17
            Caption = 'Sho&w short path'
            TabOrder = 1
          end
          object PathInCaptionNoneButton: TRadioButton
            Left = 16
            Top = 67
            Width = 337
            Height = 17
            Caption = 'Do &not show'
            TabOrder = 2
          end
        end
        object WindowMiscellaneousGroup: TGroupBox
          Left = 8
          Top = 108
          Width = 357
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 1
          DesignSize = (
            357
            53)
          object MinimizeToTrayCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Minimize main window to taskbar status area (system tray)'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object SecuritySheet: TTabSheet
        Tag = 19
        Hint = 'Security'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_security'
        Caption = 'Security'
        ImageIndex = 18
        TabVisible = False
        DesignSize = (
          373
          353)
        object MasterPasswordGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 92
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Master password'
          TabOrder = 0
          DesignSize = (
            357
            92)
          object SetMasterPasswordButton: TButton
            Left = 16
            Top = 51
            Width = 325
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Change master password...'
            TabOrder = 1
            OnClick = SetMasterPasswordButtonClick
          end
          object UseMasterPasswordCheck: TCheckBox
            Left = 17
            Top = 24
            Width = 331
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use master password'
            TabOrder = 0
            OnClick = UseMasterPasswordCheckClick
          end
        end
      end
      object IntegrationAppSheet: TTabSheet
        Tag = 18
        Hint = 'Applications'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_integration_app'
        Caption = 'IntgApp'
        ImageIndex = 17
        TabVisible = False
        DesignSize = (
          373
          353)
        object ExternalAppsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 177
          Anchors = [akLeft, akTop, akRight]
          Caption = 'External applications'
          TabOrder = 0
          DesignSize = (
            357
            177)
          object Label2: TLabel
            Left = 16
            Top = 21
            Width = 59
            Height = 13
            Caption = '&PuTTY path:'
            FocusControl = PuttyPathEdit
          end
          object PuttyPathEdit: TEdit
            Left = 16
            Top = 38
            Width = 330
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = 'PuttyPathEdit'
            OnChange = ControlChange
          end
          object PuttyPasswordCheck2: TCheckBox
            Left = 24
            Top = 97
            Width = 321
            Height = 17
            Caption = '&Remember session password and pass it to PuTTY (SSH)'
            TabOrder = 3
          end
          object AutoOpenInPuttyCheck: TCheckBox
            Left = 24
            Top = 147
            Width = 321
            Height = 17
            Caption = 'Automatically &open new sessions in PuTTY'
            TabOrder = 5
          end
          object PuttyPathBrowseButton: TButton
            Left = 200
            Top = 65
            Width = 75
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 1
            OnClick = PuttyPathBrowseButtonClick
          end
          object PuttyPathResetButton: TButton
            Left = 281
            Top = 65
            Width = 65
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'R&eset'
            TabOrder = 2
            OnClick = PuttyPathResetButtonClick
          end
          object TelnetForFtpInPuttyCheck: TCheckBox
            Left = 24
            Top = 122
            Width = 321
            Height = 17
            Caption = 'Open &Telnet sessions in PuTTY for FTP sessions'
            TabOrder = 4
          end
        end
      end
      object NetworkSheet: TTabSheet
        Tag = 20
        Hint = 'Network'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_network'
        Caption = 'Network'
        ImageIndex = 20
        TabVisible = False
        DesignSize = (
          373
          353)
        object ExternalIpAddressGroupBox: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 98
          Anchors = [akLeft, akTop, akRight]
          Caption = 'External IP address'
          TabOrder = 0
          object RetrieveExternalIpAddressButton: TRadioButton
            Left = 16
            Top = 21
            Width = 329
            Height = 17
            Caption = 'Retrieve external IP address from &operating system'
            TabOrder = 0
            OnClick = ControlChange
          end
          object CustomExternalIpAddressButton: TRadioButton
            Left = 16
            Top = 45
            Width = 233
            Height = 17
            Caption = 'Use &following external IP address:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object CustomExternalIpAddressEdit: TEdit
            Left = 45
            Top = 67
            Width = 108
            Height = 21
            TabOrder = 2
            OnClick = ControlChange
          end
        end
      end
      object PanelRemoteSheet: TTabSheet
        Tag = 21
        Hint = 'Remote'
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_panels_remote'
        Caption = 'PanRemote'
        ImageIndex = 20
        TabVisible = False
        DesignSize = (
          373
          353)
        object PanelsRemoteDirectoryGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 357
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Remote panel'
          TabOrder = 0
          DesignSize = (
            357
            99)
          object RefreshRemoteDirectoryUnitLabel: TLabel
            Left = 336
            Top = 69
            Width = 5
            Height = 13
            Caption = 's'
          end
          object ShowInaccesibleDirectoriesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show in&accessible directories'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutoReadDirectoryAfterOpCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Auto&matically refresh directory after operation (Ctrl+Alt+R)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object RefreshRemotePanelCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 234
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Refresh remote panel &every'
            TabOrder = 2
            OnClick = ControlChange
          end
          object RefreshRemotePanelIntervalEdit: TUpDownEdit
            Left = 256
            Top = 67
            Width = 75
            Height = 21
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 9999.000000000000000000
            MaxLength = 3
            TabOrder = 3
            OnChange = ControlChange
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 132
      Height = 363
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        132
        363)
      object NavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 116
        Height = 353
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
          030A000000360000000000000001000000FFFFFFFFFFFFFFFF00000000000000
          0005000000010C45006E007600690072006F006E006D0065006E007400580032
          0000000000000003000000FFFFFFFFFFFFFFFF00000000000000000000000001
          0A49006E00740065007200660061006300650058002C00000000000000110000
          00FFFFFFFFFFFFFFFF0000000000000000000000000107570069006E0064006F
          00770058002C0000000000000004000000FFFFFFFFFFFFFFFF00000000000000
          00010000000107500061006E0065006C00730058002C00000000000000150000
          00FFFFFFFFFFFFFFFF0000000000000000000000000107520065006D006F0074
          0065005800320000000000000005000000FFFFFFFFFFFFFFFF00000000000000
          0000000000010A43006F006D006D0061006E0064006500720058003000000000
          00000006000000FFFFFFFFFFFFFFFF0000000000000000000000000109450078
          0070006C006F0072006500720058002C0000000000000008000000FFFFFFFFFF
          FFFFFF000000000000000000000000010745006400690074006F007200580030
          0000000000000007000000FFFFFFFFFFFFFFFF00000000000000000400000001
          095400720061006E00730066006500720058002E0000000000000010000000FF
          FFFFFFFFFFFFFF00000000000000000000000001085000720065007300650074
          007300580030000000000000000B000000FFFFFFFFFFFFFFFF00000000000000
          000000000001094400720061006700440072006F007000580034000000000000
          000C000000FFFFFFFFFFFFFFFF000000000000000000000000010B4200610063
          006B00670072006F0075006E00640058002C000000000000000E000000FFFFFF
          FFFFFFFFFF000000000000000000000000010752006500730075006D00650058
          002E0000000000000014000000FFFFFFFFFFFFFFFF0000000000000000000000
          0001084E006500740077006F0072006B005800300000000000000013000000FF
          FFFFFFFFFFFFFF00000000000000000000000001095300650063007500720069
          007400790058002E0000000000000002000000FFFFFFFFFFFFFFFF0000000000
          0000000000000001084C006F006700670069006E006700580036000000000000
          0009000000FFFFFFFFFFFFFFFF000000000000000001000000010C49006E0074
          006500670072006100740069006F006E005800380000000000000012000000FF
          FFFFFFFFFFFFFF000000000000000000000000010D4100700070006C00690063
          006100740069006F006E007300580030000000000000000A000000FFFFFFFFFF
          FFFFFF000000000000000000000000010943006F006D006D0061006E00640073
          0058002E000000000000000D000000FFFFFFFFFFFFFFFF000000000000000000
          0000000108530074006F00720061006700650058002E000000000000000F0000
          00FFFFFFFFFFFFFFFF0000000000000000000000000108550070006400610074
          00650073005800}
      end
    end
  end
  object HelpButton: TButton
    Left = 426
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object ExportButton: TButton
    Left = 8
    Top = 369
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'E&xport...'
    TabOrder = 1
    OnClick = ExportButtonClick
  end
end
