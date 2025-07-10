object PreferencesDialog: TPreferencesDialog
  Left = 400
  Top = 161
  HelpType = htKeyword
  HelpKeyword = 'ui_preferences'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 569
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnShortCut = FormShortCut
  OnShow = FormShow
  DesignSize = (
    605
    569)
  TextHeight = 15
  object OKButton: TButton
    Left = 345
    Top = 486
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CloseButton: TButton
    Left = 431
    Top = 486
    Width = 80
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
    Width = 605
    Height = 485
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 140
      Top = 0
      Width = 465
      Height = 485
      ActivePage = PreferencesSheet
      Align = alClient
      MultiLine = True
      Style = tsButtons
      TabOrder = 1
      TabStop = False
      OnChange = PageControlChange
      object PreferencesSheet: TTabSheet
        Tag = 1
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_environment'
        Caption = 'Environment'
        ImageIndex = 2
        TabVisible = False
        DesignSize = (
          457
          475)
        object CommonPreferencesGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 304
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Confirmations'
          TabOrder = 0
          DesignSize = (
            445
            304)
          object SynchronizeSummaryCheck: TCheckBox
            Left = 11
            Top = 254
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sync&hronization summary'
            TabOrder = 10
            OnClick = ControlChange
          end
          object ConfirmOverwritingCheck: TCheckBox
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Overwriting of files'
            TabOrder = 2
            OnClick = ControlChange
          end
          object ConfirmDeletingCheck: TCheckBox
            Left = 11
            Top = 139
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Deleting of files (recommended)'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ConfirmClosingSessionCheck2: TCheckBox
            Left = 11
            Top = 185
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Closing sessions when exiting appli&cation'
            TabOrder = 7
            OnClick = ControlChange
          end
          object DDTransferConfirmationCheck2: TCheckBox
            Left = 26
            Top = 45
            Width = 410
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'D&rag && drop operations and paste to other applications'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ContinueOnErrorCheck: TCheckBox
            Left = 11
            Top = 277
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Continue on &error (advanced users)'
            TabOrder = 11
            OnClick = ControlChange
          end
          object ConfirmExitOnCompletionCheck: TCheckBox
            Left = 11
            Top = 208
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Exiting application on o&peration completion'
            TabOrder = 8
            OnClick = ControlChange
          end
          object ConfirmResumeCheck: TCheckBox
            Left = 11
            Top = 91
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Trans&fer resuming'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ConfirmCommandSessionCheck: TCheckBox
            Left = 11
            Top = 231
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Opening separate &shell session'
            TabOrder = 9
            OnClick = ControlChange
          end
          object ConfirmRecyclingCheck: TCheckBox
            Left = 11
            Top = 162
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Moving files to Recycle bin'
            TabOrder = 6
            OnClick = ControlChange
          end
          object ConfirmTransferringCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Transferring of files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object BackgroundConfirmationsLink: TStaticText
            Left = 11
            Top = 114
            Width = 425
            Height = 19
            Alignment = taRightJustify
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Change confirmations of background transfers'
            TabOrder = 4
            TabStop = True
            OnClick = BackgroundConfirmationsLinkClick
          end
        end
        object NotificationsGroup: TGroupBox
          Left = 8
          Top = 312
          Width = 445
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Notifications'
          TabOrder = 1
          DesignSize = (
            445
            72)
          object BeepOnFinishAfterText: TLabel
            Left = 429
            Top = 22
            Width = 5
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 's'
            ShowAccelChar = False
          end
          object BeepOnFinishCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 333
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Beep when work finishes, if it lasted more than'
            TabOrder = 0
            OnClick = ControlChange
          end
          object BeepOnFinishAfterEdit: TUpDownEdit
            Left = 350
            Top = 19
            Width = 73
            Height = 23
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 999.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 3
            TabOrder = 1
            OnChange = ControlChange
          end
          object BalloonNotificationsCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 425
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_logging'
        Caption = 'Logging'
        ImageIndex = 4
        TabVisible = False
        DesignSize = (
          457
          475)
        object LogProtocolHintLabel: TLabel
          Left = 8
          Top = 299
          Width = 445
          Height = 43
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'The selected logging level severely degrades performance. Use it' +
            ' when troubleshooting only.'
          ShowAccelChar = False
          WordWrap = True
        end
        object LoggingGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 195
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session log'
          TabOrder = 0
          DesignSize = (
            445
            195)
          object LogMaxSizeCountFilesLabel: TLabel
            Left = 383
            Top = 142
            Width = 21
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'files'
            FocusControl = LogMaxSizeCountEdit
            ShowAccelChar = False
          end
          object LogFileNameLabel: TLabel
            Left = 27
            Top = 45
            Width = 50
            Height = 15
            Caption = '&Log path:'
            FocusControl = LogFileNameEdit3
            OnClick = ControlChange
          end
          object LogFileNameEdit3: TFilenameEdit
            Left = 27
            Top = 63
            Width = 409
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PathEditAfterDialog
            DialogKind = dkSave
            DefaultExt = 'log'
            Filter = 'Session log files (*.log)|*.log|All files (*.*)|*.*'
            DialogOptions = [ofHideReadOnly, ofPathMustExist]
            DialogTitle = 'Select file for session log'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'LogFileNameEdit3'
            OnChange = ControlChange
          end
          object LogFilePanel: TPanel
            Left = 29
            Top = 86
            Width = 321
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            TabOrder = 4
            object LogFileAppendButton: TRadioButton
              Left = 0
              Top = 4
              Width = 116
              Height = 17
              Caption = 'Appe&nd'
              TabOrder = 0
              OnClick = ControlChange
            end
            object LogFileOverwriteButton: TRadioButton
              Left = 122
              Top = 4
              Width = 116
              Height = 17
              Caption = '&Overwrite'
              TabOrder = 1
              OnClick = ControlChange
            end
          end
          object LogProtocolCombo2: TComboBox
            Left = 305
            Top = 19
            Width = 131
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 1
            OnChange = ControlChange
            Items.Strings = (
              'Reduced'
              'Normal'
              'Debug 1'
              'Debug 2')
          end
          object LogFileNameHintText: TStaticText
            Left = 305
            Top = 84
            Width = 131
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = '&patterns'
            TabOrder = 3
            TabStop = True
          end
          object EnableLoggingCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 288
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable &session logging on level:'
            TabOrder = 0
            OnClick = ControlChange
          end
          object LogSensitiveCheck: TCheckBox
            Left = 29
            Top = 168
            Width = 391
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Log pass&words and other sensitive information'
            TabOrder = 9
            OnClick = ControlChange
          end
          object LogMaxSizeCheck: TCheckBox
            Left = 29
            Top = 113
            Width = 270
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Rotate log files after reaching'
            TabOrder = 5
            OnClick = ControlChange
          end
          object LogMaxSizeCombo: TComboBox
            Left = 305
            Top = 110
            Width = 131
            Height = 23
            Anchors = [akTop, akRight]
            MaxLength = 20
            TabOrder = 6
            OnChange = ControlChange
            OnExit = SizeComboExit
            Items.Strings = (
              '1M'
              '10M'
              '100M'
              '1G')
          end
          object LogMaxSizeCountCheck: TCheckBox
            Left = 45
            Top = 142
            Width = 254
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete old log files, keep'
            TabOrder = 7
            OnClick = ControlChange
          end
          object LogMaxSizeCountEdit: TUpDownEdit
            Left = 305
            Top = 139
            Width = 72
            Height = 23
            MaxValue = 999.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 8
            OnChange = ControlChange
          end
        end
        object ActionsLoggingGroup: TGroupBox
          Left = 8
          Top = 203
          Width = 445
          Height = 90
          Anchors = [akLeft, akTop, akRight]
          Caption = 'XML log'
          TabOrder = 1
          DesignSize = (
            445
            90)
          object ActionsLogFileNameEdit: TFilenameEdit
            Left = 27
            Top = 45
            Width = 409
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PathEditAfterDialog
            DialogKind = dkSave
            DefaultExt = 'xml'
            Filter = 'XML log files (*.xml)|*.xml|All files (*.*)|*.*'
            DialogOptions = [ofHideReadOnly, ofPathMustExist]
            DialogTitle = 'Select file for XML log.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Text = 'ActionsLogFileNameEdit'
            OnChange = ControlChange
          end
          object ActionsLogFileNameHintText: TStaticText
            Left = 305
            Top = 66
            Width = 131
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'pa&tterns'
            TabOrder = 2
            TabStop = True
          end
          object EnableActionsLoggingCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable &XML logging to file:'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object GeneralSheet: TTabSheet
        Tag = 3
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_interface'
        Caption = 'Interface'
        ImageIndex = 5
        TabVisible = False
        DesignSize = (
          457
          475)
        object InterfaceChangeLabel: TLabel
          Left = 8
          Top = 284
          Width = 190
          Height = 15
          Caption = 'Changes will apply on the next start.'
          ShowAccelChar = False
        end
        object InterfaceGroup: TGroupBox
          Left = 8
          Top = 60
          Width = 445
          Height = 218
          Anchors = [akLeft, akTop, akRight]
          Caption = 'User Interface'
          TabOrder = 1
          DesignSize = (
            445
            218)
          object CommanderDescriptionLabel2: TLabel
            Left = 147
            Top = 22
            Width = 289
            Height = 115
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              '- two panels (left for local directory, right for remote directo' +
              'ry)'#13#10'- keyboard shortcuts like in Norton Commander (and other si' +
              'milar programs as Total Commander, Midnight Commander...)'#13#10'- dra' +
              'g && drop to/from both panels'
            FocusControl = CommanderInterfaceButton2
            WordWrap = True
            OnClick = CommanderClick
          end
          object ExplorerDescriptionLabel: TLabel
            Left = 147
            Top = 138
            Width = 289
            Height = 67
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              '- only remote directory'#13#10'- keyboard shortcuts like in Windows Fi' +
              'le Explorer'#13#10'- drag && drop'
            FocusControl = ExplorerInterfaceButton2
            WordWrap = True
            OnClick = ExplorerClick
          end
          object CommanderInterfacePicture: TImage
            Left = 55
            Top = 45
            Width = 32
            Height = 32
            AutoSize = True
            OnClick = CommanderClick
          end
          object ExplorerInterfacePicture: TImage
            Left = 55
            Top = 161
            Width = 32
            Height = 32
            AutoSize = True
            OnClick = ExplorerClick
          end
          object CommanderInterfaceButton2: TRadioButton
            Left = 11
            Top = 22
            Width = 116
            Height = 17
            Caption = '&Commander'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ControlChange
          end
          object ExplorerInterfaceButton2: TRadioButton
            Left = 11
            Top = 138
            Width = 111
            Height = 17
            Caption = '&Explorer'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object ThemeGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 52
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Theme'
          TabOrder = 0
          DesignSize = (
            445
            52)
          object Label7: TLabel
            Left = 9
            Top = 22
            Width = 86
            Height = 15
            Caption = 'Interface &theme:'
            FocusControl = ThemeCombo
          end
          object ThemeCombo: TComboBox
            Left = 147
            Top = 19
            Width = 197
            Height = 23
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Items.Strings = (
              'Automatic'
              'Light'
              'Dark')
          end
        end
      end
      object PanelsSheet: TTabSheet
        Tag = 4
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_panels'
        Caption = 'Panels'
        ImageIndex = 3
        TabVisible = False
        DesignSize = (
          457
          475)
        object PanelsCommonGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 245
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Common'
          TabOrder = 0
          DesignSize = (
            445
            245)
          object Label1: TLabel
            Left = 9
            Top = 186
            Width = 91
            Height = 15
            Caption = 'Show file si&zes in:'
            FocusControl = FormatSizeBytesCombo
            OnClick = ControlChange
          end
          object Label2: TLabel
            Left = 9
            Top = 215
            Width = 103
            Height = 15
            Caption = '&Incremental search:'
            FocusControl = PanelSearchCombo
            OnClick = ControlChange
          end
          object ShowHiddenFilesCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show hidden files (Ctrl+Alt+H)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DefaultDirIsHomeCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default directory is &home directory'
            TabOrder = 1
            OnClick = ControlChange
          end
          object PreservePanelStateCheck: TCheckBox
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Remember panels'#39' state when switching sessions'
            TabOrder = 2
            OnClick = ControlChange
          end
          object RenameWholeNameCheck: TCheckBox
            Left = 11
            Top = 91
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Select &whole name when renaming file'
            TabOrder = 3
            OnClick = ControlChange
          end
          object FullRowSelectCheck: TCheckBox
            Left = 11
            Top = 114
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Full row select'
            TabOrder = 4
            OnClick = ControlChange
          end
          object FormatSizeBytesCombo: TComboBox
            Left = 316
            Top = 183
            Width = 120
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            MaxLength = 1
            TabOrder = 6
            OnChange = ControlChange
            Items.Strings = (
              'Bytes'
              'Kilobytes'
              'Short format')
          end
          object NaturalOrderNumericalSortingCheck: TCheckBox
            Left = 11
            Top = 137
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &natural order numerical sorting'
            TabOrder = 5
            OnClick = ControlChange
          end
          object PanelSearchCombo: TComboBox
            Left = 241
            Top = 212
            Width = 195
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            MaxLength = 1
            TabOrder = 7
            OnChange = ControlChange
            Items.Strings = (
              'Beginning of name only'
              'Any part of name'
              'All columns')
          end
          object AlwaysSortDirectoriesByNameCheck: TCheckBox
            Left = 11
            Top = 160
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Always sort &directories by name'
            TabOrder = 8
            OnClick = ControlChange
          end
        end
        object DoubleClickGroup: TGroupBox
          Left = 8
          Top = 253
          Width = 445
          Height = 75
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Double-click'
          TabOrder = 1
          DesignSize = (
            445
            75)
          object DoubleClickActionLabel: TLabel
            Left = 9
            Top = 22
            Width = 202
            Height = 15
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Operation to perform on double-click:'
            FocusControl = DoubleClickActionCombo
          end
          object CopyOnDoubleClickConfirmationCheck: TCheckBox
            Left = 11
            Top = 48
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Confirm copy on double-click operation'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DoubleClickActionCombo: TComboBox
            Left = 316
            Top = 19
            Width = 120
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
            OnChange = ControlChange
            Items.Strings = (
              'Open'
              'Copy'
              'Edit')
          end
        end
        object PanelFontGroup: TGroupBox
          Left = 8
          Top = 334
          Width = 445
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panel font'
          TabOrder = 2
          DesignSize = (
            445
            80)
          object PanelFontLabel: TLabel
            Left = 160
            Top = 22
            Width = 276
            Height = 48
            Anchors = [akLeft, akTop, akRight, akBottom]
            AutoSize = False
            Caption = 'PanelFontLabel'
            Color = clWindow
            ParentColor = False
            ShowAccelChar = False
            Transparent = False
            WordWrap = True
            OnDblClick = PanelFontLabelDblClick
          end
          object PanelFontButton: TButton
            Left = 9
            Top = 45
            Width = 145
            Height = 25
            Caption = 'Select fon&t...'
            TabOrder = 1
            OnClick = PanelFontButtonClick
          end
          object PanelFontCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 143
            Height = 17
            Caption = '&Use custom font'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object CommanderSheet: TTabSheet
        Tag = 5
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_commander'
        Caption = 'Commander'
        ImageIndex = 3
        TabVisible = False
        DesignSize = (
          457
          475)
        object Label3: TLabel
          Left = 8
          Top = 2
          Width = 445
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this page apply to Commander interface only.'
          ShowAccelChar = False
          WordWrap = True
        end
        object PanelsGroup: TGroupBox
          Left = 8
          Top = 34
          Width = 445
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panels'
          TabOrder = 0
          DesignSize = (
            445
            99)
          object Label8: TLabel
            Left = 9
            Top = 22
            Width = 124
            Height = 15
            Caption = '&Explorer-style selection:'
            FocusControl = NortonLikeModeCombo
          end
          object SwappedPanelsCheck: TCheckBox
            Left = 11
            Top = 48
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'S&wap panels (local on right, remote on left)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object NortonLikeModeCombo: TComboBox
            Left = 210
            Top = 19
            Width = 226
            Height = 23
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
            Left = 11
            Top = 71
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show &directory tree left of file list'
            TabOrder = 2
            OnClick = ControlChange
          end
        end
        object CommanderMiscGroup: TGroupBox
          Left = 8
          Top = 139
          Width = 445
          Height = 75
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 1
          DesignSize = (
            445
            75)
          object Label10: TLabel
            Left = 9
            Top = 22
            Width = 102
            Height = 15
            Caption = '&Keyboard shortcuts'
            FocusControl = ExplorerKeyboardShortcutsCombo
          end
          object UseLocationProfilesCheck: TCheckBox
            Left = 11
            Top = 48
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use Location Profiles instead of Directory Bookmarks'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ExplorerKeyboardShortcutsCombo: TComboBox
            Left = 210
            Top = 19
            Width = 226
            Height = 23
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ControlChange
            Items.Strings = (
              'Commander'
              'Explorer')
          end
        end
        object CompareCriterionsGroup: TGroupBox
          Left = 8
          Top = 220
          Width = 445
          Height = 72
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Compare directory criteria'
          TabOrder = 2
          DesignSize = (
            445
            72)
          object CompareByTimeCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Compare by &time'
            TabOrder = 0
            OnClick = ControlChange
          end
          object CompareBySizeCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 425
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
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_explorer'
        Caption = 'Explorer'
        ImageIndex = 5
        TabVisible = False
        DesignSize = (
          457
          475)
        object Label4: TLabel
          Left = 8
          Top = 2
          Width = 445
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this page apply to Explorer interface only.'
          ShowAccelChar = False
          WordWrap = True
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 34
          Width = 445
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'View'
          TabOrder = 0
          DesignSize = (
            445
            49)
          object ShowFullAddressCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show full path on address bar'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object EditorSheet: TTabSheet
        Tag = 8
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_editor'
        Caption = 'Editors'
        ImageIndex = 7
        TabVisible = False
        DesignSize = (
          457
          475)
        object EditorPreferenceGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 416
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Editor preference'
          TabOrder = 0
          DesignSize = (
            445
            416)
          object EditorListView3: TListView
            Left = 9
            Top = 22
            Width = 427
            Height = 322
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Editor'
                Width = 190
              end
              item
                Caption = 'Mask'
                Width = 70
              end
              item
                Caption = 'Text'
                Width = 45
              end>
            ColumnClick = False
            DoubleBuffered = True
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            TabOrder = 0
            ViewStyle = vsReport
            OnData = EditorListView3Data
            OnDblClick = EditorListView3DblClick
            OnEndDrag = ListViewEndDrag
            OnDragDrop = EditorListView3DragDrop
            OnDragOver = ListViewDragOver
            OnKeyDown = EditorListView3KeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddEditorButton: TButton
            Left = 9
            Top = 350
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = AddEditEditorButtonClick
          end
          object EditEditorButton: TButton
            Left = 105
            Top = 350
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = AddEditEditorButtonClick
          end
          object UpEditorButton: TButton
            Left = 346
            Top = 350
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 3
            OnClick = UpDownEditorButtonClick
          end
          object DownEditorButton: TButton
            Left = 346
            Top = 381
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 4
            OnClick = UpDownEditorButtonClick
          end
          object RemoveEditorButton: TButton
            Left = 9
            Top = 381
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 5
            OnClick = RemoveEditorButtonClick
          end
        end
        object EditingOptionsGroup: TGroupBox
          Left = 8
          Top = 424
          Width = 445
          Height = 49
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Editing options'
          TabOrder = 1
          DesignSize = (
            445
            49)
          object EditorCheckNotModifiedCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 413
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Check that edited remote file has not changed before saving it'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object IntegrationSheet: TTabSheet
        Tag = 9
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_integration'
        Caption = 'Integration'
        ImageIndex = 8
        TabVisible = False
        DesignSize = (
          457
          475)
        object ShellIconsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 170
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Windows Shell'
          TabOrder = 0
          DesignSize = (
            445
            170)
          object DesktopIconButton: TButton
            Left = 9
            Top = 22
            Width = 427
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &desktop icon'
            TabOrder = 0
            OnClick = IconButtonClick
          end
          object SendToHookButton: TButton
            Left = 9
            Top = 53
            Width = 427
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add upload shortcut to Explorer'#39's '#39'&Send to'#39' context menu'
            TabOrder = 1
            OnClick = IconButtonClick
          end
          object RegisterAsUrlHandlersButton: TButton
            Left = 9
            Top = 104
            Width = 427
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Register to handle &URL addresses'
            TabOrder = 3
            OnClick = RegisterAsUrlHandlersButtonClick
          end
          object AddSearchPathButton: TButton
            Left = 9
            Top = 135
            Width = 427
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add WinSCP to &search path'
            TabOrder = 4
            OnClick = AddSearchPathButtonClick
          end
          object ShellIconsText2: TStaticText
            Left = 106
            Top = 81
            Width = 330
            Height = 17
            Hint = 
              'To add shortcuts, which directly open site, use icon commands in' +
              ' '#39'Manage'#39' menu on Login dialog.'
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'Associate the icons with site'
            TabOrder = 2
            TabStop = True
          end
        end
      end
      object CustomCommandsSheet: TTabSheet
        Tag = 10
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_commands'
        Caption = 'Commands'
        ImageIndex = 9
        TabVisible = False
        DesignSize = (
          457
          475)
        object CustomCommandsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 471
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom commands'
          TabOrder = 0
          DesignSize = (
            445
            471)
          object CustomCommandsView: TListView
            Left = 9
            Top = 22
            Width = 427
            Height = 377
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
              end>
            ColumnClick = False
            DoubleBuffered = True
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            ViewStyle = vsReport
            OnData = CustomCommandsViewData
            OnDblClick = CustomCommandsViewDblClick
            OnEndDrag = ListViewEndDrag
            OnDragDrop = CustomCommandsViewDragDrop
            OnDragOver = CustomCommandsViewDragOver
            OnKeyDown = CustomCommandsViewKeyDown
            OnMouseMove = CustomCommandsViewMouseMove
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddCommandButton: TButton
            Left = 9
            Top = 405
            Width = 100
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            Style = bsSplitButton
            TabOrder = 1
            OnClick = AddCommandButtonClick
            OnDropDownClick = AddCommandButtonDropDownClick
          end
          object RemoveCommandButton: TButton
            Left = 9
            Top = 436
            Width = 100
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 4
            OnClick = RemoveCommandButtonClick
          end
          object UpCommandButton: TButton
            Left = 346
            Top = 405
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 5
            OnClick = UpDownCommandButtonClick
          end
          object DownCommandButton: TButton
            Left = 346
            Top = 436
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 6
            OnClick = UpDownCommandButtonClick
          end
          object EditCommandButton: TButton
            Left = 115
            Top = 405
            Width = 100
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = EditCommandButtonClick
          end
          object ConfigureCommandButton: TButton
            Left = 115
            Top = 405
            Width = 100
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Configure...'
            TabOrder = 3
            OnClick = ConfigureCommandButtonClick
          end
        end
      end
      object DragDropSheet: TTabSheet
        Tag = 11
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_dragdrop'
        Caption = 'Drag & Drop'
        ImageIndex = 10
        TabVisible = False
        DesignSize = (
          457
          475)
        object DragDropDownloadsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 329
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Drag && Drop downloads'
          TabOrder = 0
          DesignSize = (
            445
            329)
          object DDFakeFileEnabledLabel: TLabel
            Left = 27
            Top = 45
            Width = 409
            Height = 59
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'Allows direct downloads to regular local folders (e.g. Windows F' +
              'ile Explorer). Does not allow downloads to other destinations (Z' +
              'IP archives,  FTP, etc.). Uses drag&&drop shell extension, when ' +
              'available.'
            FocusControl = DDFakeFileEnabledButton
            WordWrap = True
            OnClick = DDLabelClick
          end
          object DDFakeFileDisabledLabel: TLabel
            Left = 27
            Top = 239
            Width = 409
            Height = 60
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'Allows downloads to any destinations (regular folders, ZIP archi' +
              'ves,  FTP, etc.). Files are downloaded first to temporary folder' +
              ', from where they are delivered to the destination.'
            FocusControl = DDFakeFileDisabledButton
            WordWrap = True
            OnClick = DDLabelClick
          end
          object DragExtStatusLabel: TLabel
            Left = 27
            Top = 108
            Width = 100
            Height = 15
            Caption = 'DragExtStatusLabel'
            FocusControl = DDFakeFileEnabledButton
            ShowAccelChar = False
            OnClick = DDLabelClick
          end
          object DDDrivesLabel: TLabel
            Left = 27
            Top = 126
            Width = 234
            Height = 15
            Caption = 'Allow dropping files to these &network drives:'
            FocusControl = DDDrivesMemo
          end
          object DDFakeFileEnabledButton: TRadioButton
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Determine drop target by dragging a &fake file'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDFakeFileDisabledButton: TRadioButton
            Left = 11
            Top = 216
            Width = 412
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Download files via &temporary folder'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDWarnLackOfTempSpaceCheck: TCheckBox
            Left = 27
            Top = 302
            Width = 315
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Warn when there is not enough free space'
            TabOrder = 3
            OnClick = ControlChange
          end
          object DDDrivesMemo: TMemo
            Left = 27
            Top = 144
            Width = 409
            Height = 66
            Anchors = [akLeft, akTop, akRight]
            Lines.Strings = (
              'DDDrivesMemo')
            ScrollBars = ssVertical
            TabOrder = 2
          end
        end
      end
      object QueueSheet: TTabSheet
        Tag = 12
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_background'
        Caption = 'Background'
        ImageIndex = 11
        TabVisible = False
        DesignSize = (
          457
          475)
        object QueueGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 242
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Background transfers'
          TabOrder = 0
          DesignSize = (
            445
            242)
          object Label5: TLabel
            Left = 9
            Top = 22
            Width = 246
            Height = 15
            Caption = '&Maximal number of transfers at the same time:'
            FocusControl = QueueTransferLimitEdit
          end
          object QueueKeepDoneItemsCheck: TLabel
            Left = 11
            Top = 214
            Width = 216
            Height = 15
            Caption = 'Display &completed transfers in queue for:'
            FocusControl = QueueKeepDoneItemsForCombo
            OnClick = ControlChange
          end
          object ParallelTransferThresholdUnitLabel: TLabel
            Left = 151
            Top = 141
            Width = 28
            Height = 15
            Caption = 'bytes'
            FocusControl = ParallelTransferThresholdCombo
            OnClick = ControlChange
          end
          object QueueTransferLimitEdit: TUpDownEdit
            Left = 357
            Top = 19
            Width = 79
            Height = 23
            Alignment = taRightJustify
            MaxValue = 9.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 1
            TabOrder = 0
          end
          object QueueAutoPopupCheck: TCheckBox
            Left = 11
            Top = 188
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically popup prompts of background transfers when idle'
            TabOrder = 7
          end
          object QueueCheck: TCheckBox
            Left = 11
            Top = 69
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Transfer in &background by default'
            TabOrder = 2
          end
          object QueueNoConfirmationCheck: TCheckBox
            Left = 11
            Top = 165
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&No confirmations for background transfers'
            TabOrder = 6
          end
          object QueueParallelCheck: TCheckBox
            Left = 11
            Top = 92
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use multiple connections for single transfer'
            TabOrder = 3
            OnClick = ControlChange
          end
          object EnableQueueByDefaultCheck: TCheckBox
            Left = 11
            Top = 46
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Enable queue processing by default'
            TabOrder = 1
          end
          object QueueKeepDoneItemsForCombo: TComboBox
            Left = 329
            Top = 211
            Width = 107
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            MaxLength = 1
            TabOrder = 8
            OnChange = ControlChange
            Items.Strings = (
              'Never'
              '15 seconds'
              '1 minute'
              '15 minutes'
              '1 hour'
              'Forever')
          end
          object ParallelTransferCheck: TCheckBox
            Left = 27
            Top = 115
            Width = 409
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use multiple connections for single files abo&ve:'
            TabOrder = 4
            OnClick = ControlChange
          end
          object ParallelTransferThresholdCombo: TComboBox
            Left = 43
            Top = 138
            Width = 102
            Height = 23
            Anchors = [akTop, akRight]
            MaxLength = 20
            TabOrder = 5
            OnChange = ControlChange
            OnExit = SizeComboExit
            Items.Strings = (
              '1M'
              '10M'
              '100M'
              '1G')
          end
        end
        object QueueViewGroup: TGroupBox
          Left = 8
          Top = 250
          Width = 445
          Height = 95
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Queue list'
          TabOrder = 1
          DesignSize = (
            445
            95)
          object QueueViewShowButton: TRadioButton
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show'
            TabOrder = 0
          end
          object QueueViewHideWhenEmptyButton: TRadioButton
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Hide &when empty'
            TabOrder = 1
          end
          object QueueViewHideButton: TRadioButton
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Hide'
            TabOrder = 2
          end
        end
      end
      object StorageSheet: TTabSheet
        Tag = 13
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_storage'
        Caption = 'Storage'
        ImageIndex = 12
        TabVisible = False
        DesignSize = (
          457
          475)
        object StorageGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 96
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Configuration storage'
          TabOrder = 0
          DesignSize = (
            445
            96)
          object AutomaticIniFileStorageLabel: TPathLabel
            Left = 184
            Top = 45
            Width = 254
            Height = 15
            ActiveTextColor = clWindowText
            IndentHorizontal = 0
            IndentVertical = 0
            InactiveTextColor = clGrayText
            OnGetStatus = AutomaticIniFileStorageLabelGetStatus
            Align = alNone
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
          end
          object RegistryStorageButton: TRadioButton
            Left = 11
            Top = 22
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Windows re&gistry'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutomaticIniFileStorageButton: TRadioButton
            Left = 11
            Top = 45
            Width = 167
            Height = 17
            Caption = '&Automatic INI file'
            TabOrder = 1
            OnClick = ControlChange
          end
          object CustomIniFileStorageButton: TRadioButton
            Left = 11
            Top = 68
            Width = 167
            Height = 17
            Caption = 'Custo&m INI file:'
            TabOrder = 2
            OnClick = CustomIniFileStorageButtonClick
          end
          object CustomIniFileStorageEdit: TFilenameEdit
            Left = 184
            Top = 65
            Width = 254
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = CustomIniFileStorageEditAfterDialog
            DialogKind = dkSave
            DefaultExt = 'ini'
            Filter = 'INI files (*.ini)|*.ini|All files (*.*)|*.*'
            DialogOptions = [ofHideReadOnly, ofPathMustExist]
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            Text = 'CustomIniFileStorageEdit'
            OnChange = ControlChange
            OnExit = CustomIniFileStorageEditExit
          end
        end
        object TemporaryDirectoryGrouo: TGroupBox
          Left = 8
          Top = 104
          Width = 445
          Height = 216
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Temporary directory'
          TabOrder = 1
          DesignSize = (
            445
            216)
          object Label6: TLabel
            Left = 9
            Top = 22
            Width = 429
            Height = 31
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Specify where to temporarily store edited and downloaded files.'
            FocusControl = DDSystemTemporaryDirectoryButton
            ShowAccelChar = False
            WordWrap = True
          end
          object DDSystemTemporaryDirectoryButton: TRadioButton
            Left = 11
            Top = 53
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use temporary directory of system'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDCustomTemporaryDirectoryButton: TRadioButton
            Left = 11
            Top = 76
            Width = 148
            Height = 17
            Caption = 'Use this &directory:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDTemporaryDirectoryEdit: TDirectoryEdit
            Left = 184
            Top = 73
            Width = 254
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PathEditAfterDialog
            DialogText = 'Select directory for temporary drag && drop files.'
            ClickKey = 16397
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = 'DDTemporaryDirectoryEdit'
            OnChange = ControlChange
          end
          object TemporaryDirectoryCleanupCheck: TCheckBox
            Left = 11
            Top = 168
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Cleanup obsolete temporary directories on startup'
            TabOrder = 6
            OnClick = ControlChange
          end
          object ConfirmTemporaryDirectoryCleanupCheck: TCheckBox
            Left = 27
            Top = 191
            Width = 411
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Ask before cleanup'
            TabOrder = 7
            OnClick = ControlChange
          end
          object TemporaryDirectoryAppendSessionCheck: TCheckBox
            Left = 11
            Top = 99
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Append &session name to temporary path'
            TabOrder = 3
            OnClick = ControlChange
          end
          object TemporaryDirectoryAppendPathCheck: TCheckBox
            Left = 11
            Top = 122
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Append remote &path to temporary path'
            TabOrder = 4
            OnClick = ControlChange
          end
          object TemporaryDirectoryDeterministicCheck: TCheckBox
            Left = 11
            Top = 145
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Keep temporary copies of remote files in &deterministic paths'
            TabOrder = 5
            OnClick = ControlChange
          end
        end
        object OtherStorageGroup: TGroupBox
          Left = 8
          Top = 326
          Width = 445
          Height = 50
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other'
          TabOrder = 2
          DesignSize = (
            445
            50)
          object RandomSeedFileLabel: TLabel
            Left = 9
            Top = 22
            Width = 94
            Height = 15
            Caption = '&Random seed file:'
            FocusControl = RandomSeedFileEdit
          end
          object RandomSeedFileEdit: TFilenameEdit
            Left = 184
            Top = 19
            Width = 254
            Height = 21
            AcceptFiles = True
            OnBeforeDialog = PathEditBeforeDialog
            OnAfterDialog = PathEditAfterDialog
            DialogKind = dkSave
            DefaultExt = 'rnd'
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
      object TransferEnduranceSheet: TTabSheet
        Tag = 14
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_resume'
        Caption = 'Endurance'
        ImageIndex = 13
        TabVisible = False
        DesignSize = (
          457
          475)
        object ResumeBox: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 124
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable transfer resume/transfer to temporary filename for'
          TabOrder = 0
          object ResumeThresholdUnitLabel2: TLabel
            Left = 125
            Top = 71
            Width = 14
            Height = 15
            Caption = 'KB'
            FocusControl = ResumeThresholdEdit
          end
          object ResumeOnButton: TRadioButton
            Left = 11
            Top = 22
            Width = 165
            Height = 17
            Caption = 'A&ll files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ResumeSmartButton: TRadioButton
            Left = 11
            Top = 45
            Width = 165
            Height = 17
            Caption = 'Files abo&ve:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ResumeOffButton: TRadioButton
            Left = 11
            Top = 97
            Width = 165
            Height = 17
            Caption = 'Di&sable'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ResumeThresholdEdit: TUpDownEdit
            Left = 27
            Top = 68
            Width = 92
            Height = 23
            Alignment = taRightJustify
            Increment = 10.000000000000000000
            MaxValue = 4194304.000000000000000000
            TabOrder = 2
            OnClick = ControlChange
          end
        end
        object SessionReopenGroup: TGroupBox
          Left = 8
          Top = 132
          Width = 445
          Height = 211
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatic reconnect'
          TabOrder = 1
          DesignSize = (
            445
            211)
          object SessionReopenAutoLabel: TLabel
            Left = 27
            Top = 48
            Width = 86
            Height = 15
            Caption = '&Reconnect after:'
            FocusControl = SessionReopenAutoEdit
          end
          object SessionReopenAutoSecLabel: TLabel
            Left = 286
            Top = 48
            Width = 43
            Height = 15
            Caption = 'seconds'
            FocusControl = SessionReopenAutoEdit
          end
          object SessionReopenTimeoutLabel: TLabel
            Left = 9
            Top = 181
            Width = 120
            Height = 15
            Caption = '&Keep reconnecting for:'
            FocusControl = SessionReopenTimeoutEdit
          end
          object SessionReopenTimeoutSecLabel: TLabel
            Left = 286
            Top = 181
            Width = 43
            Height = 15
            Caption = 'seconds'
            FocusControl = SessionReopenTimeoutEdit
          end
          object SessionReopenAutoStallLabel: TLabel
            Left = 27
            Top = 152
            Width = 86
            Height = 15
            Caption = 'Re&connect after:'
            FocusControl = SessionReopenAutoStallEdit
          end
          object SessionReopenAutoStallSecLabel: TLabel
            Left = 286
            Top = 152
            Width = 43
            Height = 15
            Caption = 'seconds'
            FocusControl = SessionReopenAutoStallEdit
          end
          object SessionReopenAutoIdleLabel: TLabel
            Left = 27
            Top = 100
            Width = 86
            Height = 15
            Caption = 'Reco&nnect after:'
            FocusControl = SessionReopenAutoIdleEdit
          end
          object SessionReopenAutoIdleSecLabel: TLabel
            Left = 286
            Top = 100
            Width = 43
            Height = 15
            Caption = 'seconds'
            FocusControl = SessionReopenAutoIdleEdit
          end
          object SessionReopenAutoCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically reconnect session, if it breaks during transfer'
            TabOrder = 0
            OnClick = ControlChange
          end
          object SessionReopenAutoEdit: TUpDownEdit
            Left = 183
            Top = 45
            Width = 97
            Height = 23
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 1
          end
          object SessionReopenAutoIdleCheck: TCheckBox
            Left = 11
            Top = 74
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Automatically reconnect session, if it breaks &while idle'
            TabOrder = 2
            OnClick = ControlChange
          end
          object SessionReopenTimeoutEdit: TUpDownEdit
            Left = 183
            Top = 178
            Width = 97
            Height = 23
            Alignment = taRightJustify
            Increment = 30.000000000000000000
            MaxValue = 86400.000000000000000000
            MaxLength = 5
            TabOrder = 6
            OnGetValue = SessionReopenTimeoutEditGetValue
            OnSetValue = SessionReopenTimeoutEditSetValue
          end
          object SessionReopenAutoStallCheck: TCheckBox
            Left = 11
            Top = 126
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Automatically reconnect session, if it &stalls'
            TabOrder = 4
            OnClick = ControlChange
          end
          object SessionReopenAutoStallEdit: TUpDownEdit
            Left = 183
            Top = 149
            Width = 97
            Height = 23
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 5
          end
          object SessionReopenAutoIdleEdit: TUpDownEdit
            Left = 183
            Top = 97
            Width = 97
            Height = 23
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 3
          end
        end
      end
      object UpdatesSheet: TTabSheet
        Tag = 15
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_updates'
        Caption = 'Updates'
        ImageIndex = 14
        TabVisible = False
        DesignSize = (
          457
          475)
        object UpdatesGroup2: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 123
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatic updates'
          TabOrder = 0
          DesignSize = (
            445
            123)
          object Label12: TLabel
            Left = 9
            Top = 72
            Width = 130
            Height = 15
            Caption = 'Automatic check &period:'
            FocusControl = UpdatesPeriodCombo
          end
          object UpdatesAuthenticationEmailLabel: TLabel
            Left = 9
            Top = 22
            Width = 250
            Height = 15
            Caption = '&Email address authorized to automatic updates:'
            FocusControl = UpdatesAuthenticationEmailEdit
          end
          object UpdatesPeriodCombo: TComboBox
            Left = 326
            Top = 69
            Width = 110
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 2
            Items.Strings = (
              'Never'
              'Daily'
              'Weekly'
              'Monthly')
          end
          object UpdatesShowOnStartup: TCheckBox
            Left = 11
            Top = 98
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Display information about update on startup'
            TabOrder = 3
            OnClick = ControlChange
          end
          object UpdatesAuthenticationEmailEdit: TEdit
            Left = 9
            Top = 40
            Width = 312
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ControlChange
            OnExit = UpdatesAuthenticationEmailEditExit
          end
          object UpdatesLink: TStaticText
            Left = 326
            Top = 44
            Width = 64
            Height = 19
            Anchors = [akTop, akRight]
            Caption = 'Learn more'
            TabOrder = 1
            TabStop = True
            OnClick = UpdatesLinkClick
          end
        end
        object UpdatesProxyGroup: TGroupBox
          Left = 8
          Top = 219
          Width = 445
          Height = 142
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 2
          DesignSize = (
            445
            142)
          object UpdatesProxyHostLabel: TLabel
            Left = 27
            Top = 91
            Width = 91
            Height = 15
            Caption = 'Proxy &host name:'
            FocusControl = UpdatesProxyHostEdit
          end
          object UpdatesProxyPortLabel: TLabel
            Left = 326
            Top = 91
            Width = 70
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = UpdatesProxyPortEdit
          end
          object UpdatesProxyPortEdit: TUpDownEdit
            Left = 326
            Top = 109
            Width = 110
            Height = 23
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
          end
          object UpdatesProxyHostEdit: TEdit
            Left = 27
            Top = 109
            Width = 294
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 3
            Text = 'UpdatesProxyHostEdit'
          end
          object UpdatesProxyCheck: TRadioButton
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use proxy server'
            TabOrder = 2
            OnClick = ControlChange
          end
          object UpdatesDirectCheck: TRadioButton
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&No proxy'
            TabOrder = 0
            OnClick = ControlChange
          end
          object UpdatesAutoCheck: TRadioButton
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically detect proxy settings'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object UpdatesOptionsGroup: TGroupBox
          Left = 8
          Top = 131
          Width = 445
          Height = 82
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          TabOrder = 1
          DesignSize = (
            445
            82)
          object UpdatesBetaVersionsLabel: TLabel
            Left = 8
            Top = 22
            Width = 126
            Height = 15
            Caption = 'Check for &beta versions:'
            FocusControl = UpdatesBetaVersionsCombo
          end
          object UpdatesBetaVersionsCombo: TComboBox
            Left = 326
            Top = 19
            Width = 110
            Height = 23
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
          end
          object CollectUsageCheck: TCheckBox
            Left = 11
            Top = 52
            Width = 318
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &anonymous usage statistics'
            TabOrder = 1
            OnClick = ControlChange
          end
          object UsageViewButton: TButton
            Left = 326
            Top = 48
            Width = 110
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'View &statistics'
            TabOrder = 2
            OnClick = UsageViewButtonClick
          end
        end
      end
      object CopyParamListSheet: TTabSheet
        Tag = 16
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_transfer'
        Caption = 'Transfer'
        ImageIndex = 15
        TabVisible = False
        DesignSize = (
          457
          475)
        object CopyParamListGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 471
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Transfer settings presets'
          TabOrder = 0
          DesignSize = (
            445
            471)
          object CopyParamLabel: TLabel
            Left = 9
            Top = 326
            Width = 427
            Height = 53
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Caption = 'CopyParamLabel'
            ShowAccelChar = False
            WordWrap = True
            OnClick = CopyParamLabelClick
          end
          object CopyParamListView: TListView
            Left = 9
            Top = 22
            Width = 427
            Height = 298
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Preset description'
                Width = 100
              end
              item
                Caption = 'Auto'
                Width = 40
              end>
            ColumnClick = False
            DoubleBuffered = True
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = CopyParamListViewCustomDrawItem
            OnData = CopyParamListViewData
            OnDblClick = CopyParamListViewDblClick
            OnEndDrag = ListViewEndDrag
            OnDragDrop = CopyParamListViewDragDrop
            OnDragOver = CopyParamListViewDragOver
            OnKeyDown = CopyParamListViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddCopyParamButton: TButton
            Left = 9
            Top = 382
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = AddCopyParamButtonClick
          end
          object RemoveCopyParamButton: TButton
            Left = 9
            Top = 413
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCopyParamButtonClick
          end
          object UpCopyParamButton: TButton
            Left = 346
            Top = 382
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 5
            OnClick = UpDownCopyParamButtonClick
          end
          object DownCopyParamButton: TButton
            Left = 346
            Top = 413
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 6
            OnClick = UpDownCopyParamButtonClick
          end
          object EditCopyParamButton: TButton
            Left = 105
            Top = 382
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = EditCopyParamButtonClick
          end
          object DuplicateCopyParamButton: TButton
            Left = 105
            Top = 413
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Du&plicate...'
            TabOrder = 4
            OnClick = DuplicateCopyParamButtonClick
          end
          object CopyParamAutoSelectNoticeCheck: TCheckBox
            Left = 11
            Top = 444
            Width = 425
            Height = 17
            Anchors = [akLeft, akRight, akBottom]
            Caption = '&Announce when transfer settings preset is autoselected'
            TabOrder = 7
            OnClick = ControlChange
          end
        end
      end
      object WindowSheet: TTabSheet
        Tag = 17
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_window'
        Caption = 'Window'
        ImageIndex = 16
        TabVisible = False
        DesignSize = (
          457
          475)
        object PathInCaptionGroup: TGroupBox
          Left = 8
          Top = 127
          Width = 445
          Height = 95
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Path in window title'
          TabOrder = 1
          DesignSize = (
            445
            95)
          object PathInCaptionFullButton: TRadioButton
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show &full path'
            TabOrder = 0
          end
          object PathInCaptionShortButton: TRadioButton
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sho&w short path'
            TabOrder = 1
          end
          object PathInCaptionNoneButton: TRadioButton
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do &not show'
            TabOrder = 2
          end
        end
        object WindowMiscellaneousGroup: TGroupBox
          Left = 8
          Top = 228
          Width = 445
          Height = 164
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 2
          DesignSize = (
            445
            164)
          object MinimizeToTrayCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Minimize main window to taskbar status area (system tray)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ExternalSessionInExistingInstanceCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Open new externally initiated sessions in &existing window'
            TabOrder = 1
            OnClick = ControlChange
          end
          object KeepOpenWhenNoSessionCheck: TCheckBox
            Left = 11
            Top = 91
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Keep main window open when the last session is closed'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ShowTipsCheck: TCheckBox
            Left = 11
            Top = 137
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Display tips on startup'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ShowLoginWhenNoSessionCheck: TCheckBox
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 
              '&Show Login dialog on startup and when the last session is close' +
              'd'
            TabOrder = 2
            OnClick = ControlChange
          end
          object SessionTabCaptionTruncationCheck: TCheckBox
            Left = 11
            Top = 114
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Truncate tab titles when they do not fit to window'
            TabOrder = 4
            OnClick = ControlChange
          end
        end
        object WorkspacesGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 119
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Workspaces'
          TabOrder = 0
          DesignSize = (
            445
            119)
          object AutoWorkspaceLabel: TLabel
            Left = 27
            Top = 45
            Width = 133
            Height = 15
            Caption = '&Default workspace name:'
            FocusControl = AutoWorkspaceCombo
          end
          object AutoSaveWorkspaceCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically save workspace on exit'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutoWorkspaceCombo: TComboBox
            Left = 27
            Top = 63
            Width = 409
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            TabOrder = 1
            OnClick = ControlChange
          end
          object AutoSaveWorkspacePasswordsCheck: TCheckBox
            Left = 29
            Top = 92
            Width = 408
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Save &passwords (not recommended) X'
            TabOrder = 2
            OnClick = ControlChange
          end
        end
      end
      object SecuritySheet: TTabSheet
        Tag = 19
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_security'
        Caption = 'Security'
        ImageIndex = 18
        TabVisible = False
        DesignSize = (
          457
          475)
        object MasterPasswordGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Master password'
          TabOrder = 0
          DesignSize = (
            445
            80)
          object SetMasterPasswordButton: TButton
            Left = 9
            Top = 45
            Width = 282
            Height = 25
            Caption = '&Change master password...'
            TabOrder = 1
            OnClick = SetMasterPasswordButtonClick
          end
          object UseMasterPasswordCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use master password'
            TabOrder = 0
            OnClick = UseMasterPasswordCheckClick
          end
        end
        object PasswordGroupBox: TGroupBox
          Left = 8
          Top = 88
          Width = 445
          Height = 48
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session password'
          TabOrder = 1
          DesignSize = (
            445
            48)
          object SessionRememberPasswordCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Remember &password for duration of session'
            TabOrder = 0
          end
        end
        object SshHostCAsGroup: TGroupBox
          Left = 8
          Top = 142
          Width = 445
          Height = 331
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Trusted host certification authorities'
          TabOrder = 2
          DesignSize = (
            445
            331)
          object SshHostCAsView: TListView
            Left = 9
            Top = 45
            Width = 427
            Height = 245
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                Caption = 'Name'
                Width = 100
              end
              item
                Caption = 'Hosts'
                Width = 100
              end>
            ColumnClick = False
            DoubleBuffered = True
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            TabOrder = 1
            ViewStyle = vsReport
            OnData = SshHostCAsViewData
            OnDblClick = SshHostCAsViewDblClick
            OnKeyDown = SshHostCAsViewKeyDown
            OnSelectItem = ListViewSelectItem
          end
          object AddSshHostCAButton: TButton
            Left = 9
            Top = 296
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 2
            OnClick = AddSshHostCAButtonClick
          end
          object RemoveSshHostCAButton: TButton
            Left = 201
            Top = 296
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 4
            OnClick = RemoveSshHostCAButtonClick
          end
          object EditSshHostCAButton: TButton
            Left = 105
            Top = 296
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 3
            OnClick = EditSshHostCAButtonClick
          end
          object SshHostCAsFromPuTTYCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Caption = '&Load authorities from PuTTY'
            TabOrder = 0
            OnClick = SshHostCAsFromPuTTYCheckClick
          end
          object ConfigureSshHostCAsButton: TButton
            Left = 9
            Top = 296
            Width = 138
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit in PuTTY...'
            TabOrder = 5
            OnClick = ConfigureSshHostCAsButtonClick
          end
        end
      end
      object IntegrationAppSheet: TTabSheet
        Tag = 18
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_integration_app'
        Caption = 'Applications'
        ImageIndex = 17
        TabVisible = False
        DesignSize = (
          457
          475)
        object ExternalAppsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 205
          Anchors = [akLeft, akTop, akRight]
          Caption = 'External applications'
          TabOrder = 0
          DesignSize = (
            445
            205)
          object PuttyPathLabel: TLabel
            Left = 9
            Top = 22
            Width = 148
            Height = 15
            Caption = 'PuTTY/Terminal &client path:'
            FocusControl = PuttyPathEdit
          end
          object PuttyRegistryStorageKeyLabel: TLabel
            Left = 9
            Top = 154
            Width = 101
            Height = 15
            Caption = 'PuTTY registry &key:'
            FocusControl = PuttyRegistryStorageKeyEdit
          end
          object PuttyPathEdit: THistoryComboBox
            Left = 9
            Top = 40
            Width = 341
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = PuttyPathEditChange
            OnExit = PuttyPathEditExit
          end
          object PuttyPasswordCheck2: TCheckBox
            Left = 11
            Top = 85
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Remember session password and pass it to PuTTY (SSH)'
            TabOrder = 3
          end
          object AutoOpenInPuttyCheck: TCheckBox
            Left = 11
            Top = 131
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Automatically &open new sessions in PuTTY'
            TabOrder = 5
          end
          object PuttyPathBrowseButton: TButton
            Left = 356
            Top = 39
            Width = 80
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 1
            OnClick = PuttyPathBrowseButtonClick
          end
          object TelnetForFtpInPuttyCheck: TCheckBox
            Left = 11
            Top = 108
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Open &Telnet sessions in PuTTY for FTP sessions'
            TabOrder = 4
          end
          object PuttyPathHintText: TStaticText
            Left = 224
            Top = 63
            Width = 126
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = '&patterns'
            TabOrder = 2
            TabStop = True
          end
          object PuttyRegistryStorageKeyEdit: THistoryComboBox
            Left = 9
            Top = 172
            Width = 427
            Height = 23
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 6
            OnChange = ControlChange
          end
        end
      end
      object NetworkSheet: TTabSheet
        Tag = 20
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_network'
        Caption = 'Network'
        ImageIndex = 20
        TabVisible = False
        DesignSize = (
          457
          475)
        object ExternalIpAddressGroupBox2: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 152
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Incoming FTP connections (active mode)'
          TabOrder = 0
          DesignSize = (
            445
            152)
          object LocalPortNumberRangeLabel: TLabel
            Left = 124
            Top = 122
            Width = 6
            Height = 15
            Caption = #8211
          end
          object RetrieveExternalIpAddressButton: TRadioButton
            Left = 11
            Top = 22
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Retrieve the external IP address from &operating system'
            TabOrder = 0
            OnClick = ControlChange
          end
          object CustomExternalIpAddressButton: TRadioButton
            Left = 11
            Top = 45
            Width = 427
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use the &following external IP address:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object CustomExternalIpAddressEdit: TEdit
            Left = 27
            Top = 67
            Width = 200
            Height = 23
            TabOrder = 2
            OnClick = ControlChange
          end
          object LocalPortNumberCheck: TCheckBox
            Left = 11
            Top = 96
            Width = 417
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Limit listening &ports to:'
            TabOrder = 3
            OnClick = ControlChange
          end
          object LocalPortNumberMinEdit: TUpDownEdit
            Left = 27
            Top = 119
            Width = 91
            Height = 23
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1024.000000000000000000
            TabOrder = 4
            OnChange = ControlChange
            OnExit = LocalPortNumberMinEditExit
          end
          object LocalPortNumberMaxEdit: TUpDownEdit
            Left = 136
            Top = 119
            Width = 91
            Height = 23
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1024.000000000000000000
            TabOrder = 5
            OnChange = ControlChange
            OnExit = LocalPortNumberMaxEditExit
          end
        end
        object ConnectionsGroup: TGroupBox
          Left = 8
          Top = 160
          Width = 445
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connections'
          TabOrder = 1
          DesignSize = (
            445
            49)
          object TryFtpWhenSshFailsCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 417
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'When SFTP connection is &rejected, knock FTP port'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object PanelRemoteSheet: TTabSheet
        Tag = 21
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_panels_remote'
        Caption = 'Remote'
        ImageIndex = 20
        TabVisible = False
        DesignSize = (
          457
          475)
        object PanelsRemoteDirectoryGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 98
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Remote panel'
          TabOrder = 0
          DesignSize = (
            445
            98)
          object RefreshRemoteDirectoryUnitLabel: TLabel
            Left = 430
            Top = 68
            Width = 5
            Height = 15
            Anchors = [akTop, akRight]
            Caption = 's'
            ShowAccelChar = False
          end
          object ShowInaccesibleDirectoriesCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show in&accessible directories'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutoReadDirectoryAfterOpCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Auto&matically refresh directory after operation (Ctrl+Alt+R)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object RefreshRemotePanelCheck: TCheckBox
            Left = 11
            Top = 68
            Width = 322
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Refresh remote panel &every'
            TabOrder = 2
            OnClick = ControlChange
          end
          object RefreshRemotePanelIntervalEdit: TUpDownEdit
            Left = 343
            Top = 65
            Width = 81
            Height = 23
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 9999.000000000000000000
            MinValue = 10.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 3
            TabOrder = 3
            OnChange = ControlChange
          end
        end
      end
      object PanelLocalSheet: TTabSheet
        Tag = 22
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_panels_local'
        Caption = 'Local'
        ImageIndex = 20
        TabVisible = False
        DesignSize = (
          457
          475)
        object LocalPanelGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 95
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Local panel'
          TabOrder = 0
          DesignSize = (
            445
            95)
          object PreserveLocalDirectoryCheck: TCheckBox
            Left = 11
            Top = 45
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do not &change state when switching sessions'
            TabOrder = 1
            OnClick = ControlChange
          end
          object SystemContextMenuCheck: TCheckBox
            Left = 11
            Top = 68
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &system file context menu'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete files to recycle bin'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object LanguagesSheet: TTabSheet
        Tag = 23
        Caption = 'Languages'
        ImageIndex = 21
        TabVisible = False
        DesignSize = (
          457
          475)
        object LanguagesGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 471
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Languages'
          TabOrder = 0
          DesignSize = (
            445
            471)
          object LanguageChangeLabel: TLabel
            Left = 9
            Top = 440
            Width = 190
            Height = 15
            Anchors = [akLeft, akBottom]
            Caption = 'Changes will apply on the next start.'
            ShowAccelChar = False
          end
          object LanguagesView: TListView
            Left = 9
            Top = 22
            Width = 427
            Height = 408
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                AutoSize = True
              end>
            DoubleBuffered = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            ShowColumnHeaders = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = LanguagesViewCustomDrawItem
            OnSelectItem = ListViewSelectItem
          end
          object LanguagesGetMoreButton: TButton
            Left = 326
            Top = 436
            Width = 110
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = 'Get &more...'
            TabOrder = 1
            OnClick = LanguagesGetMoreButtonClick
          end
        end
      end
      object EditorInternalSheet: TTabSheet
        Tag = 24
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_editor_internal'
        Caption = 'Internal editor'
        TabVisible = False
        DesignSize = (
          457
          475)
        object InternalEditorGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 146
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Display'
          TabOrder = 0
          DesignSize = (
            445
            146)
          object Label9: TLabel
            Left = 9
            Top = 45
            Width = 75
            Height = 15
            Caption = '&Tabulator size:'
            FocusControl = EditorTabSizeEdit
          end
          object Label11: TLabel
            Left = 9
            Top = 92
            Width = 94
            Height = 15
            Caption = 'Default en&coding:'
            FocusControl = EditorEncodingCombo
          end
          object EditorWordWrapCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Wrap long lines'
            TabOrder = 0
            OnClick = ControlChange
          end
          object EditorTabSizeEdit: TUpDownEdit
            Left = 9
            Top = 63
            Width = 161
            Height = 23
            Alignment = taRightJustify
            MaxValue = 99.000000000000000000
            MinValue = 1.000000000000000000
            MaxLength = 2
            TabOrder = 1
            OnChange = ControlChange
          end
          object EditorEncodingCombo: TComboBox
            Left = 9
            Top = 110
            Width = 161
            Height = 23
            Style = csDropDownList
            MaxLength = 2
            TabOrder = 2
            OnChange = ControlChange
          end
        end
        object FontGroup: TGroupBox
          Left = 8
          Top = 154
          Width = 445
          Height = 119
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Font'
          TabOrder = 1
          DesignSize = (
            445
            119)
          object EditorFontLabel: TLabel
            Left = 176
            Top = 22
            Width = 260
            Height = 87
            Anchors = [akLeft, akTop, akRight, akBottom]
            AutoSize = False
            Caption = 'EditorFontLabel'
            Color = clWhite
            ParentColor = False
            ShowAccelChar = False
            Transparent = False
            OnDblClick = EditorFontLabelDblClick
          end
          object EditorFontButton: TButton
            Left = 9
            Top = 22
            Width = 161
            Height = 25
            Caption = '&Select font...'
            TabOrder = 0
            OnClick = EditorFontButtonClick
          end
          object EditorFontColorButton: TButton
            Left = 9
            Top = 53
            Width = 161
            Height = 25
            Caption = '&Text color'
            TabOrder = 1
            OnClick = EditorFontColorButtonClick
          end
          object EditorBackgroundColorButton: TButton
            Left = 9
            Top = 84
            Width = 161
            Height = 25
            Caption = 'Default &background'
            TabOrder = 2
            OnClick = EditorBackgroundColorButtonClick
          end
        end
        object InternalEditorBehaviourGroup: TGroupBox
          Left = 8
          Top = 278
          Width = 445
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Behaviour'
          TabOrder = 2
          DesignSize = (
            445
            49)
          object EditorDisableSmoothScrollCheck: TCheckBox
            Left = 11
            Top = 22
            Width = 425
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Disable s&mooth scrolling'
            TabOrder = 0
            OnClick = ControlChange
          end
        end
      end
      object FileColorsSheet: TTabSheet
        Tag = 25
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_file_colors'
        Caption = 'File colors'
        ImageIndex = 23
        TabVisible = False
        DesignSize = (
          457
          475)
        object FileColorsGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 471
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'File colors'
          TabOrder = 0
          DesignSize = (
            445
            471)
          object FileColorsView: TListView
            Left = 9
            Top = 22
            Width = 427
            Height = 377
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
              end>
            ColumnClick = False
            DoubleBuffered = True
            DragMode = dmAutomatic
            HideSelection = False
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            ParentDoubleBuffered = False
            ParentShowHint = False
            ShowColumnHeaders = False
            ShowHint = False
            TabOrder = 0
            ViewStyle = vsReport
            OnCustomDrawItem = FileColorsViewCustomDrawItem
            OnData = FileColorsViewData
            OnDblClick = FileColorsViewDblClick
            OnEndDrag = ListViewEndDrag
            OnDragDrop = FileColorsViewDragDrop
            OnDragOver = ListViewDragOver
            OnKeyDown = FileColorsViewKeyDown
            OnSelectItem = ListViewSelectItem
            OnStartDrag = ListViewStartDrag
          end
          object AddFileColorButton: TButton
            Left = 9
            Top = 405
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = AddEditFileColorButtonClick
          end
          object RemoveFileColorButton: TButton
            Left = 9
            Top = 436
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveFileColorButtonClick
          end
          object UpFileColorButton: TButton
            Left = 346
            Top = 405
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 4
            OnClick = UpDownFileColorButtonClick
          end
          object DownFileColorButton: TButton
            Left = 346
            Top = 436
            Width = 90
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 5
            OnClick = UpDownFileColorButtonClick
          end
          object EditFileColorButton: TButton
            Left = 105
            Top = 405
            Width = 90
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = AddEditFileColorButtonClick
          end
        end
      end
      object SearchSheet: TTabSheet
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_search'
        Caption = 'SearchSheet'
        ImageIndex = 24
        TabVisible = False
        DesignSize = (
          457
          475)
        object SearchGroup: TGroupBox
          Left = 8
          Top = 2
          Width = 445
          Height = 471
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Search'
          TabOrder = 0
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 140
      Height = 485
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        140
        485)
      object NavigationPanel: TPanel
        Left = 8
        Top = 36
        Width = 130
        Height = 443
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 1
        object NavigationTree: TTreeView
          Left = 0
          Top = 0
          Width = 130
          Height = 443
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
          OnChanging = NavigationTreeChanging
          OnCollapsing = NavigationTreeCollapsing
          OnEnter = NavigationTreeEnter
          Items.NodeData = {
            030B000000360000000000000001000000FFFFFFFFFFFFFFFF00000000000000
            0005000000010C45006E007600690072006F006E006D0065006E007400580032
            0000000000000003000000FFFFFFFFFFFFFFFF00000000000000000000000001
            0A49006E00740065007200660061006300650058002C00000000000000110000
            00FFFFFFFFFFFFFFFF0000000000000000000000000107570069006E0064006F
            0077005800320000000000000005000000FFFFFFFFFFFFFFFF00000000000000
            0000000000010A43006F006D006D0061006E0064006500720058003000000000
            00000006000000FFFFFFFFFFFFFFFF0000000000000000000000000109450078
            0070006C006F007200650072005800320000000000000017000000FFFFFFFFFF
            FFFFFF000000000000000000000000010A4C0061006E00670075006100670065
            00730058002C0000000000000004000000FFFFFFFFFFFFFFFF00000000000000
            00030000000107500061006E0065006C00730058003600000000000000190000
            00FFFFFFFFFFFFFFFF000000000000000000000000010C460069006C00650020
            0063006F006C006F007200730058002C0000000000000015000000FFFFFFFFFF
            FFFFFF0000000000000000000000000107520065006D006F007400650058002A
            0000000000000016000000FFFFFFFFFFFFFFFF00000000000000000000000001
            064C006F00630061006C0058002C0000000000000008000000FFFFFFFFFFFFFF
            FF000000000000000001000000010745006400690074006F00720058003E0000
            000000000018000000FFFFFFFFFFFFFFFF000000000000000000000000011049
            006E007400650072006E0061006C00200065006400690074006F007200580030
            0000000000000010000000FFFFFFFFFFFFFFFF00000000000000000300000001
            095400720061006E007300660065007200580030000000000000000B000000FF
            FFFFFFFFFFFFFF00000000000000000000000001094400720061006700440072
            006F007000580034000000000000000C000000FFFFFFFFFFFFFFFF0000000000
            00000000000000010B4200610063006B00670072006F0075006E00640058002C
            000000000000000E000000FFFFFFFFFFFFFFFF00000000000000000000000001
            0752006500730075006D00650058002E0000000000000014000000FFFFFFFFFF
            FFFFFF00000000000000000000000001084E006500740077006F0072006B0058
            00300000000000000013000000FFFFFFFFFFFFFFFF0000000000000000000000
            0001095300650063007500720069007400790058002E00000000000000020000
            00FFFFFFFFFFFFFFFF00000000000000000000000001084C006F006700670069
            006E0067005800360000000000000009000000FFFFFFFFFFFFFFFF0000000000
            00000001000000010C49006E0074006500670072006100740069006F006E0058
            00380000000000000012000000FFFFFFFFFFFFFFFF0000000000000000000000
            00010D4100700070006C00690063006100740069006F006E0073005800300000
            00000000000A000000FFFFFFFFFFFFFFFF000000000000000000000000010943
            006F006D006D0061006E006400730058002E000000000000000D000000FFFFFF
            FFFFFFFFFF0000000000000000000000000108530074006F0072006100670065
            0058002E000000000000000F000000FFFFFFFFFFFFFFFF000000000000000000
            000000010855007000640061007400650073005800}
        end
      end
      object SearchEdit: TComboEdit
        Left = 8
        Top = 8
        Width = 130
        Height = 21
        HelpType = htKeyword
        HelpKeyword = 'ui_pref_search'
        ButtonTabStop = False
        ButtonCaption = #10005
        ClickKey = 0
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnButtonClick = SearchEditButtonClick
        OnChange = SearchEditChangeEnter
        OnEnter = SearchEditChangeEnter
      end
    end
  end
  object HelpButton: TButton
    Left = 517
    Top = 486
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object ComponentsPanel: TPanel
    Left = 0
    Top = 519
    Width = 605
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 4
  end
  object RegisterAsUrlHandlerMenu: TPopupMenu
    Left = 56
    Top = 470
    object RegisterAsUrlHandlerItem: TMenuItem
      Caption = 'Register'
      OnClick = RegisterAsUrlHandlerItemClick
    end
    object MakeDefaultHandlerItem: TMenuItem
      Caption = 'Make WinSCP &default handler...'
      OnClick = MakeDefaultHandlerItemClick
    end
    object UnregisterForDefaultProtocolsItem: TMenuItem
      Caption = 'Unregister'
      OnClick = UnregisterForDefaultProtocolsItemClick
    end
  end
  object AddCommandMenu: TPopupMenu
    Left = 208
    Top = 470
    object AddCustomCommandMenuItem: TMenuItem
      Caption = 'Add &Custom Command...'
      OnClick = AddCustomCommandMenuItemClick
    end
    object AddExtensionMenuItem: TMenuItem
      Caption = 'Add &Extension...'
      OnClick = AddExtensionMenuItemClick
    end
  end
end
