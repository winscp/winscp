object PreferencesDialog: TPreferencesDialog
  Left = 400
  Top = 161
  HelpType = htKeyword
  HelpKeyword = 'ui_preferences'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 519
  ClientWidth = 545
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    545
    519)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 282
    Top = 438
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CloseButton: TButton
    Left = 370
    Top = 438
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
    Width = 545
    Height = 432
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl: TPageControl
      Left = 132
      Top = 0
      Width = 413
      Height = 432
      ActivePage = LogSheet
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
          405
          422)
        object CommonPreferencesGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 305
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Confirmations'
          TabOrder = 0
          DesignSize = (
            389
            305)
          object SynchronizeSummaryCheck: TCheckBox
            Left = 16
            Top = 253
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sync&hronization summary'
            TabOrder = 10
            OnClick = ControlChange
          end
          object ConfirmOverwritingCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Overwriting of files'
            TabOrder = 2
            OnClick = ControlChange
          end
          object ConfirmDeletingCheck: TCheckBox
            Left = 16
            Top = 133
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Deleting of files (recommended)'
            TabOrder = 5
            OnClick = ControlChange
          end
          object ConfirmClosingSessionCheck2: TCheckBox
            Left = 16
            Top = 181
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Closing sessions when exiting appli&cation'
            TabOrder = 7
            OnClick = ControlChange
          end
          object DDTransferConfirmationCheck2: TCheckBox
            Left = 32
            Top = 45
            Width = 341
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'D&rag && drop operations and paste to other applications'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ContinueOnErrorCheck: TCheckBox
            Left = 16
            Top = 277
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Continue on &error (advanced users)'
            TabOrder = 11
            OnClick = ControlChange
          end
          object ConfirmExitOnCompletionCheck: TCheckBox
            Left = 16
            Top = 205
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Exiting application on o&peration completion'
            TabOrder = 8
            OnClick = ControlChange
          end
          object ConfirmResumeCheck: TCheckBox
            Left = 16
            Top = 93
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Trans&fer resuming'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ConfirmCommandSessionCheck: TCheckBox
            Left = 16
            Top = 229
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Opening separate &shell session'
            TabOrder = 9
            OnClick = ControlChange
          end
          object ConfirmRecyclingCheck: TCheckBox
            Left = 16
            Top = 157
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Moving files to Recycle bin'
            TabOrder = 6
            OnClick = ControlChange
          end
          object ConfirmTransferringCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Transferring of files'
            TabOrder = 0
            OnClick = ControlChange
          end
          object BackgroundConfirmationsLink: TStaticText
            Left = 32
            Top = 114
            Width = 345
            Height = 17
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Change confirmations of background transfers'
            TabOrder = 4
            TabStop = True
            OnClick = BackgroundConfirmationsLinkClick
          end
        end
        object NotificationsGroup: TGroupBox
          Left = 8
          Top = 319
          Width = 389
          Height = 73
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Notifications'
          TabOrder = 1
          DesignSize = (
            389
            73)
          object BeepOnFinishAfterText: TLabel
            Left = 376
            Top = 22
            Width = 5
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 's'
            ShowAccelChar = False
          end
          object BeepOnFinishCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 292
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Beep when work finishes, if it lasted more than'
            TabOrder = 0
            OnClick = ControlChange
          end
          object BeepOnFinishAfterEdit: TUpDownEdit
            Left = 314
            Top = 19
            Width = 57
            Height = 21
            Alignment = taRightJustify
            Increment = 15.000000000000000000
            MaxValue = 999.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 3
            TabOrder = 1
            OnChange = ControlChange
          end
          object BalloonNotificationsCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 364
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
          405
          422)
        object LogProtocolHintLabel: TLabel
          Left = 8
          Top = 303
          Width = 389
          Height = 33
          AutoSize = False
          Caption = 
            'The selected logging level severely degrades performance. Use it' +
            ' when troubleshooting only.'
          ShowAccelChar = False
          WordWrap = True
        end
        object LoggingGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 196
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session log'
          TabOrder = 0
          DesignSize = (
            389
            196)
          object LogMaxSizeCountFilesLabel: TLabel
            Left = 327
            Top = 143
            Width = 19
            Height = 13
            Caption = 'files'
            FocusControl = LogMaxSizeCountEdit
            ShowAccelChar = False
          end
          object LogFileNameLabel: TLabel
            Left = 40
            Top = 46
            Width = 46
            Height = 13
            Caption = '&Log path:'
            FocusControl = LogFileNameEdit3
            OnClick = ControlChange
          end
          object LogFileNameEdit3: TFilenameEdit
            Left = 40
            Top = 62
            Width = 335
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
            Left = 40
            Top = 86
            Width = 265
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            TabOrder = 4
            object LogFileAppendButton: TRadioButton
              Left = 0
              Top = 4
              Width = 106
              Height = 17
              Caption = 'Appe&nd'
              TabOrder = 0
              OnClick = ControlChange
            end
            object LogFileOverwriteButton: TRadioButton
              Left = 112
              Top = 4
              Width = 106
              Height = 17
              Caption = '&Overwrite'
              TabOrder = 1
              OnClick = ControlChange
            end
          end
          object LogProtocolCombo2: TComboBox
            Left = 256
            Top = 21
            Width = 119
            Height = 21
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
            Left = 264
            Top = 84
            Width = 111
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = '&patterns'
            TabOrder = 3
            TabStop = True
          end
          object EnableLoggingCheck: TCheckBox
            Left = 16
            Top = 23
            Width = 234
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable &session logging on level:'
            TabOrder = 0
            OnClick = ControlChange
          end
          object LogSensitiveCheck: TCheckBox
            Left = 40
            Top = 167
            Width = 335
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Log pass&words and other sensitive information'
            TabOrder = 9
            OnClick = ControlChange
          end
          object LogMaxSizeCheck: TCheckBox
            Left = 40
            Top = 115
            Width = 210
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Rotate log files after reaching'
            TabOrder = 5
            OnClick = ControlChange
          end
          object LogMaxSizeCombo: TComboBox
            Left = 256
            Top = 113
            Width = 119
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 6
            OnChange = ControlChange
            OnExit = LogMaxSizeComboExit
            Items.Strings = (
              '1M'
              '10M'
              '100M'
              '1G')
          end
          object LogMaxSizeCountCheck: TCheckBox
            Left = 64
            Top = 142
            Width = 186
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Delete old log files, keep'
            TabOrder = 7
            OnClick = ControlChange
          end
          object LogMaxSizeCountEdit: TUpDownEdit
            Left = 256
            Top = 140
            Width = 65
            Height = 21
            MaxValue = 999.000000000000000000
            MinValue = 1.000000000000000000
            TabOrder = 8
            OnChange = ControlChange
          end
        end
        object ActionsLoggingGroup: TGroupBox
          Left = 8
          Top = 210
          Width = 389
          Height = 86
          Anchors = [akLeft, akTop, akRight]
          Caption = 'XML log'
          TabOrder = 1
          DesignSize = (
            389
            86)
          object ActionsLogFileNameEdit: TFilenameEdit
            Left = 40
            Top = 43
            Width = 335
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
            Left = 264
            Top = 65
            Width = 111
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'pa&tterns'
            TabOrder = 2
            TabStop = True
          end
          object EnableActionsLoggingCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 359
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
          405
          422)
        object InterfaceChangeLabel: TLabel
          Left = 8
          Top = 282
          Width = 177
          Height = 13
          Caption = 'Changes will apply on the next start.'
          ShowAccelChar = False
        end
        object InterfaceGroup: TGroupBox
          Left = 8
          Top = 66
          Width = 389
          Height = 208
          Anchors = [akLeft, akTop, akRight]
          Caption = 'User Interface'
          TabOrder = 1
          DesignSize = (
            389
            208)
          object CommanderDescriptionLabel2: TLabel
            Left = 132
            Top = 20
            Width = 250
            Height = 115
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              '- two panels (left for local directory, right for remote directo' +
              'ry)'#13#10'- keyboard shortcuts like in Norton Commander (and other si' +
              'milar programs as Total Commander, Midnight Commander...)'#13#10'- dra' +
              'g && drop to/from both panels'
            WordWrap = True
            OnClick = CommanderClick
          end
          object ExplorerDescriptionLabel: TLabel
            Left = 132
            Top = 134
            Width = 252
            Height = 62
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              '- only remote directory'#13#10'- keyboard shortcuts like in Windows Fi' +
              'le Explorer'#13#10'- drag && drop'
            WordWrap = True
            OnClick = ExplorerClick
          end
          object CommanderInterfacePicture: TImage
            Left = 55
            Top = 41
            Width = 32
            Height = 32
            AutoSize = True
            OnClick = CommanderClick
          end
          object ExplorerInterfacePicture: TImage
            Left = 55
            Top = 155
            Width = 32
            Height = 32
            AutoSize = True
            OnClick = ExplorerClick
          end
          object CommanderInterfaceButton2: TRadioButton
            Left = 16
            Top = 19
            Width = 116
            Height = 17
            Caption = '&Commander'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ControlChange
          end
          object ExplorerInterfaceButton2: TRadioButton
            Left = 16
            Top = 133
            Width = 111
            Height = 17
            Caption = '&Explorer'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object ThemeGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 52
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Theme'
          TabOrder = 0
          DesignSize = (
            389
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
            Left = 132
            Top = 18
            Width = 141
            Height = 21
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
          405
          422)
        object PanelsCommonGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 246
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Common'
          TabOrder = 0
          DesignSize = (
            389
            246)
          object Label1: TLabel
            Left = 16
            Top = 191
            Width = 84
            Height = 13
            Caption = 'Show file si&zes in:'
            FocusControl = FormatSizeBytesCombo
            OnClick = ControlChange
          end
          object Label2: TLabel
            Left = 16
            Top = 218
            Width = 96
            Height = 13
            Caption = '&Incremental search:'
            FocusControl = PanelSearchCombo
            OnClick = ControlChange
          end
          object ShowHiddenFilesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show hidden files (Ctrl+Alt+H)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DefaultDirIsHomeCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Default directory is &home directory'
            TabOrder = 1
            OnClick = ControlChange
          end
          object PreservePanelStateCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Remember panels'#39' state when switching sessions'
            TabOrder = 2
            OnClick = ControlChange
          end
          object RenameWholeNameCheck: TCheckBox
            Left = 16
            Top = 93
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Select &whole name when renaming file'
            TabOrder = 3
            OnClick = ControlChange
          end
          object FullRowSelectCheck: TCheckBox
            Left = 16
            Top = 117
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Full row select'
            TabOrder = 4
            OnClick = ControlChange
          end
          object FormatSizeBytesCombo: TComboBox
            Left = 264
            Top = 188
            Width = 108
            Height = 21
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
            Left = 16
            Top = 141
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &natural order numerical sorting'
            TabOrder = 5
            OnClick = ControlChange
          end
          object PanelSearchCombo: TComboBox
            Left = 196
            Top = 215
            Width = 176
            Height = 21
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
            Left = 16
            Top = 165
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Always sort &directories by name'
            TabOrder = 8
            OnClick = ControlChange
          end
        end
        object DoubleClickGroup: TGroupBox
          Left = 8
          Top = 260
          Width = 389
          Height = 74
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Double-click'
          TabOrder = 1
          DesignSize = (
            389
            74)
          object DoubleClickActionLabel: TLabel
            Left = 16
            Top = 21
            Width = 179
            Height = 13
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Operation to perform on double-click:'
            FocusControl = DoubleClickActionCombo
          end
          object CopyOnDoubleClickConfirmationCheck: TCheckBox
            Left = 32
            Top = 45
            Width = 340
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Confirm copy on double-click operation'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DoubleClickActionCombo: TComboBox
            Left = 264
            Top = 17
            Width = 108
            Height = 21
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
          Top = 340
          Width = 389
          Height = 82
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panel font'
          TabOrder = 2
          DesignSize = (
            389
            82)
          object PanelFontLabel: TLabel
            Left = 160
            Top = 18
            Width = 213
            Height = 52
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
            Left = 16
            Top = 44
            Width = 129
            Height = 25
            Caption = 'Select fon&t...'
            TabOrder = 1
            OnClick = PanelFontButtonClick
          end
          object PanelFontCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 129
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
          405
          422)
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 393
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this page apply to Commander interface only.'
          ShowAccelChar = False
          WordWrap = True
        end
        object PanelsGroup: TGroupBox
          Left = 8
          Top = 38
          Width = 389
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Panels'
          TabOrder = 0
          DesignSize = (
            389
            99)
          object Label8: TLabel
            Left = 16
            Top = 21
            Width = 116
            Height = 13
            Caption = '&Explorer-style selection:'
            FocusControl = NortonLikeModeCombo
          end
          object SwappedPanelsCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'S&wap panels (local on right, remote on left)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object NortonLikeModeCombo: TComboBox
            Left = 208
            Top = 17
            Width = 164
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
            Top = 69
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show &directory tree left of file list'
            TabOrder = 2
            OnClick = ControlChange
          end
        end
        object CommanderMiscGroup: TGroupBox
          Left = 8
          Top = 146
          Width = 389
          Height = 77
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 1
          DesignSize = (
            389
            77)
          object Label10: TLabel
            Left = 16
            Top = 21
            Width = 94
            Height = 13
            Caption = '&Keyboard shortcuts'
            FocusControl = ExplorerKeyboardShortcutsCombo
          end
          object UseLocationProfilesCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use Location Profiles instead of Directory Bookmarks'
            TabOrder = 1
            OnClick = ControlChange
          end
          object ExplorerKeyboardShortcutsCombo: TComboBox
            Left = 208
            Top = 17
            Width = 164
            Height = 21
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
          Top = 233
          Width = 389
          Height = 74
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Compare directory criteria'
          TabOrder = 2
          DesignSize = (
            389
            74)
          object CompareByTimeCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Compare by &time'
            TabOrder = 0
            OnClick = ControlChange
          end
          object CompareBySizeCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 357
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
          405
          422)
        object Label4: TLabel
          Left = 8
          Top = 8
          Width = 393
          Height = 29
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Preferences on this page apply to Explorer interface only.'
          ShowAccelChar = False
          WordWrap = True
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 38
          Width = 389
          Height = 54
          Anchors = [akLeft, akTop, akRight]
          Caption = 'View'
          TabOrder = 0
          DesignSize = (
            389
            54)
          object ShowFullAddressCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 357
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
          405
          422)
        object EditorPreferenceGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 404
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Editor preference'
          TabOrder = 0
          DesignSize = (
            389
            404)
          object EditorListView3: TListView
            Left = 16
            Top = 24
            Width = 356
            Height = 307
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
            Left = 16
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = AddEditEditorButtonClick
          end
          object EditEditorButton: TButton
            Left = 112
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = AddEditEditorButtonClick
          end
          object UpEditorButton: TButton
            Left = 290
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 3
            OnClick = UpDownEditorButtonClick
          end
          object DownEditorButton: TButton
            Left = 290
            Top = 368
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 4
            OnClick = UpDownEditorButtonClick
          end
          object RemoveEditorButton: TButton
            Left = 16
            Top = 368
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 5
            OnClick = RemoveEditorButtonClick
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
          405
          422)
        object ShellIconsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 174
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Windows Shell'
          TabOrder = 0
          DesignSize = (
            389
            174)
          object DesktopIconButton: TButton
            Left = 16
            Top = 24
            Width = 357
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Create a &desktop icon'
            TabOrder = 0
            OnClick = IconButtonClick
          end
          object SendToHookButton: TButton
            Left = 16
            Top = 56
            Width = 357
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add upload shortcut to Explorer'#39's '#39'&Send to'#39' context menu'
            TabOrder = 1
            OnClick = IconButtonClick
          end
          object RegisterAsUrlHandlersButton: TButton
            Left = 16
            Top = 103
            Width = 357
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Register to handle &URL addresses'
            TabOrder = 3
            OnClick = RegisterAsUrlHandlersButtonClick
          end
          object AddSearchPathButton: TButton
            Left = 16
            Top = 135
            Width = 357
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Add WinSCP to &search path'
            TabOrder = 4
            OnClick = AddSearchPathButtonClick
          end
          object ShellIconsText2: TStaticText
            Left = 43
            Top = 84
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
          405
          422)
        object CustomCommandsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 404
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom commands'
          TabOrder = 0
          DesignSize = (
            389
            404)
          object CustomCommandsView: TListView
            Left = 16
            Top = 24
            Width = 356
            Height = 307
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
            Left = 16
            Top = 337
            Width = 98
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            Style = bsSplitButton
            TabOrder = 1
            OnClick = AddCommandButtonClick
            OnDropDownClick = AddCommandButtonDropDownClick
          end
          object RemoveCommandButton: TButton
            Left = 16
            Top = 368
            Width = 98
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 4
            OnClick = RemoveCommandButtonClick
          end
          object UpCommandButton: TButton
            Left = 290
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 5
            OnClick = UpDownCommandButtonClick
          end
          object DownCommandButton: TButton
            Left = 290
            Top = 368
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 6
            OnClick = UpDownCommandButtonClick
          end
          object EditCommandButton: TButton
            Left = 127
            Top = 337
            Width = 98
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = EditCommandButtonClick
          end
          object ConfigureCommandButton: TButton
            Left = 127
            Top = 337
            Width = 98
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
          405
          422)
        object DragDropDownloadsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 310
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Drag && Drop downloads'
          TabOrder = 0
          DesignSize = (
            389
            310)
          object DDFakeFileEnabledLabel: TLabel
            Left = 35
            Top = 44
            Width = 345
            Height = 53
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'Allows direct downloads to regular local folders (e.g. Windows F' +
              'ile Explorer). Does not allow downloads to other destinations (Z' +
              'IP archives,  FTP, etc.). Uses drag&&drop shell extension, when ' +
              'available.'
            WordWrap = True
            OnClick = DDLabelClick
          end
          object DDFakeFileDisabledLabel: TLabel
            Left = 35
            Top = 221
            Width = 346
            Height = 54
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'Allows downloads to any destinations (regular folders, ZIP archi' +
              'ves,  FTP, etc.). Files are downloaded first to temporary folder' +
              ', from where they are delivered to the destination.'
            WordWrap = True
            OnClick = DDLabelClick
          end
          object DragExtStatusLabel: TLabel
            Left = 35
            Top = 96
            Width = 95
            Height = 13
            Anchors = [akLeft, akTop, akRight]
            Caption = 'DragExtStatusLabel'
            ShowAccelChar = False
            OnClick = DDLabelClick
          end
          object DDDrivesLabel: TLabel
            Left = 35
            Top = 116
            Width = 246
            Height = 13
            AutoSize = False
            Caption = 'Allow dropping files to these &network drives:'
            FocusControl = DDDrivesMemo
          end
          object DDFakeFileEnabledButton: TRadioButton
            Left = 16
            Top = 24
            Width = 364
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Determine drop target by dragging a &fake file'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDFakeFileDisabledButton: TRadioButton
            Left = 16
            Top = 202
            Width = 356
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Download files via &temporary folder'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DDFakeFileDisabledPanel: TPanel
            Left = 34
            Top = 272
            Width = 315
            Height = 28
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              315
              28)
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
          end
          object DDDrivesMemo: TMemo
            Left = 34
            Top = 132
            Width = 337
            Height = 61
            Lines.Strings = (
              'DDDrivesMemo')
            ScrollBars = ssVertical
            TabOrder = 3
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
          405
          422)
        object QueueGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 200
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Background transfers'
          TabOrder = 0
          DesignSize = (
            389
            200)
          object Label5: TLabel
            Left = 16
            Top = 25
            Width = 224
            Height = 13
            Caption = '&Maximal number of transfers at the same time:'
            FocusControl = QueueTransferLimitEdit
          end
          object QueueKeepDoneItemsCheck: TLabel
            Left = 16
            Top = 172
            Width = 198
            Height = 13
            Caption = 'Display &completed transfers in queue for:'
            FocusControl = QueueKeepDoneItemsForCombo
            OnClick = ControlChange
          end
          object QueueTransferLimitEdit: TUpDownEdit
            Left = 304
            Top = 22
            Width = 73
            Height = 21
            Alignment = taRightJustify
            MaxValue = 9.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            MaxLength = 1
            TabOrder = 0
          end
          object QueueAutoPopupCheck: TCheckBox
            Left = 16
            Top = 146
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically popup prompts of background transfers when idle'
            TabOrder = 5
          end
          object QueueCheck: TCheckBox
            Left = 16
            Top = 74
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Transfer on &background by default'
            TabOrder = 2
          end
          object QueueNoConfirmationCheck: TCheckBox
            Left = 16
            Top = 122
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&No confirmations for background transfers'
            TabOrder = 4
          end
          object QueueParallelCheck: TCheckBox
            Left = 16
            Top = 98
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use multiple connections for single transfer'
            TabOrder = 3
          end
          object EnableQueueByDefaultCheck: TCheckBox
            Left = 16
            Top = 50
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Enable queue processing by default'
            TabOrder = 1
          end
          object QueueKeepDoneItemsForCombo: TComboBox
            Left = 280
            Top = 169
            Width = 97
            Height = 21
            Style = csDropDownList
            Anchors = [akTop, akRight]
            MaxLength = 1
            TabOrder = 6
            OnChange = ControlChange
            Items.Strings = (
              'Never'
              '15 seconds'
              '1 minute'
              '15 minutes'
              '1 hour'
              'Forever')
          end
        end
        object QueueViewGroup: TGroupBox
          Left = 8
          Top = 214
          Width = 389
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Queue list'
          TabOrder = 1
          DesignSize = (
            389
            99)
          object QueueViewShowButton: TRadioButton
            Left = 16
            Top = 21
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Show'
            TabOrder = 0
          end
          object QueueViewHideWhenEmptyButton: TRadioButton
            Left = 16
            Top = 45
            Width = 369
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Hide &when empty'
            TabOrder = 1
          end
          object QueueViewHideButton: TRadioButton
            Left = 16
            Top = 69
            Width = 369
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
          405
          422)
        object StorageGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 96
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Configuration storage'
          TabOrder = 0
          DesignSize = (
            389
            96)
          object AutomaticIniFileStorageLabel: TPathLabel
            Left = 167
            Top = 46
            Width = 209
            Height = 13
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
            Left = 16
            Top = 21
            Width = 360
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Windows re&gistry'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutomaticIniFileStorageButton: TRadioButton
            Left = 16
            Top = 45
            Width = 148
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatic INI file'
            TabOrder = 1
            OnClick = ControlChange
          end
          object CustomIniFileStorageButton: TRadioButton
            Left = 16
            Top = 68
            Width = 148
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Custo&m INI file:'
            TabOrder = 2
            OnClick = CustomIniFileStorageButtonClick
          end
          object CustomIniFileStorageEdit: TFilenameEdit
            Left = 168
            Top = 66
            Width = 208
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
          Top = 112
          Width = 389
          Height = 223
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Temporary directory'
          TabOrder = 1
          DesignSize = (
            389
            223)
          object Label6: TLabel
            Left = 16
            Top = 22
            Width = 360
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Specify where to temporarily store edited and downloaded files.'
            ShowAccelChar = False
            WordWrap = True
          end
          object DDSystemTemporaryDirectoryButton: TRadioButton
            Left = 16
            Top = 45
            Width = 360
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use temporary directory of system'
            TabOrder = 0
            OnClick = ControlChange
          end
          object DDCustomTemporaryDirectoryButton: TRadioButton
            Left = 16
            Top = 69
            Width = 148
            Height = 17
            Caption = 'Use this &directory:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object DDTemporaryDirectoryEdit: TDirectoryEdit
            Left = 168
            Top = 65
            Width = 208
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
            Left = 16
            Top = 169
            Width = 360
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Cleanup obsolete temporary directories on startup'
            TabOrder = 6
            OnClick = ControlChange
          end
          object ConfirmTemporaryDirectoryCleanupCheck: TCheckBox
            Left = 32
            Top = 194
            Width = 344
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Ask before cleanup'
            TabOrder = 7
            OnClick = ControlChange
          end
          object TemporaryDirectoryAppendSessionCheck: TCheckBox
            Left = 16
            Top = 94
            Width = 360
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Append &session name to temporary path'
            TabOrder = 3
            OnClick = ControlChange
          end
          object TemporaryDirectoryAppendPathCheck: TCheckBox
            Left = 16
            Top = 119
            Width = 360
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Append remote &path to temporary path'
            TabOrder = 4
            OnClick = ControlChange
          end
          object TemporaryDirectoryDeterministicCheck: TCheckBox
            Left = 16
            Top = 144
            Width = 360
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Keep temporary copies of remote files in &deterministic paths'
            TabOrder = 5
            OnClick = ControlChange
          end
        end
        object OtherStorageGroup: TGroupBox
          Left = 8
          Top = 342
          Width = 389
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Other'
          TabOrder = 2
          DesignSize = (
            389
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
            Left = 144
            Top = 19
            Width = 232
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
          405
          422)
        object ResumeBox: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 123
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable transfer resume/transfer to temporary filename for'
          TabOrder = 0
          object ResumeThresholdUnitLabel2: TLabel
            Left = 136
            Top = 71
            Width = 12
            Height = 13
            Caption = 'KB'
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
          Width = 389
          Height = 203
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatic reconnect'
          TabOrder = 1
          object SessionReopenAutoLabel: TLabel
            Left = 34
            Top = 48
            Width = 82
            Height = 13
            Caption = '&Reconnect after:'
            FocusControl = SessionReopenAutoEdit
          end
          object SessionReopenAutoSecLabel: TLabel
            Left = 271
            Top = 48
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenAutoEdit
          end
          object SessionReopenTimeoutLabel: TLabel
            Left = 16
            Top = 176
            Width = 110
            Height = 13
            Caption = '&Keep reconnecting for:'
            FocusControl = SessionReopenTimeoutEdit
          end
          object SessionReopenTimeoutSecLabel: TLabel
            Left = 271
            Top = 176
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenTimeoutEdit
          end
          object SessionReopenAutoStallLabel: TLabel
            Left = 34
            Top = 149
            Width = 82
            Height = 13
            Caption = 'Re&connect after:'
            FocusControl = SessionReopenAutoStallEdit
          end
          object SessionReopenAutoStallSecLabel: TLabel
            Left = 271
            Top = 149
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenAutoStallEdit
          end
          object SessionReopenAutoIdleLabel: TLabel
            Left = 34
            Top = 99
            Width = 82
            Height = 13
            Caption = 'Reco&nnect after:'
            FocusControl = SessionReopenAutoIdleEdit
          end
          object SessionReopenAutoIdleSecLabel: TLabel
            Left = 271
            Top = 99
            Width = 39
            Height = 13
            Caption = 'seconds'
            FocusControl = SessionReopenAutoIdleEdit
          end
          object SessionReopenAutoCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 363
            Height = 17
            Caption = '&Automatically reconnect session, if it breaks during transfer'
            TabOrder = 0
            OnClick = ControlChange
          end
          object SessionReopenAutoEdit: TUpDownEdit
            Left = 168
            Top = 43
            Width = 97
            Height = 21
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 1
          end
          object SessionReopenAutoIdleCheck: TCheckBox
            Left = 16
            Top = 72
            Width = 363
            Height = 17
            Caption = 'Automatically reconnect session, if it breaks &while idle'
            TabOrder = 2
            OnClick = ControlChange
          end
          object SessionReopenTimeoutEdit: TUpDownEdit
            Left = 168
            Top = 171
            Width = 97
            Height = 21
            Alignment = taRightJustify
            Increment = 30.000000000000000000
            MaxValue = 86400.000000000000000000
            MaxLength = 5
            TabOrder = 6
            OnGetValue = SessionReopenTimeoutEditGetValue
            OnSetValue = SessionReopenTimeoutEditSetValue
          end
          object SessionReopenAutoStallCheck: TCheckBox
            Left = 16
            Top = 122
            Width = 363
            Height = 17
            Caption = 'Automatically reconnect session, if it &stalls'
            TabOrder = 4
            OnClick = ControlChange
          end
          object SessionReopenAutoStallEdit: TUpDownEdit
            Left = 168
            Top = 144
            Width = 97
            Height = 21
            Alignment = taRightJustify
            Increment = 5.000000000000000000
            MaxValue = 300.000000000000000000
            MinValue = 1.000000000000000000
            Value = 5.000000000000000000
            MaxLength = 3
            TabOrder = 5
          end
          object SessionReopenAutoIdleEdit: TUpDownEdit
            Left = 168
            Top = 94
            Width = 97
            Height = 21
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
          405
          422)
        object UpdatesGroup2: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 125
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Automatic updates'
          TabOrder = 0
          DesignSize = (
            389
            125)
          object Label12: TLabel
            Left = 16
            Top = 70
            Width = 115
            Height = 13
            Caption = 'Automatic check &period:'
            FocusControl = UpdatesPeriodCombo
          end
          object UpdatesAuthenticationEmailLabel: TLabel
            Left = 16
            Top = 21
            Width = 228
            Height = 13
            Caption = '&Email address authorized to automatic updates:'
            FocusControl = UpdatesAuthenticationEmailEdit
          end
          object UpdatesPeriodCombo: TComboBox
            Left = 278
            Top = 67
            Width = 98
            Height = 21
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
            Left = 16
            Top = 98
            Width = 262
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Display information about update on startup'
            TabOrder = 3
            OnClick = ControlChange
          end
          object UpdatesAuthenticationEmailEdit: TEdit
            Left = 16
            Top = 38
            Width = 256
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = ControlChange
            OnExit = UpdatesAuthenticationEmailEditExit
          end
          object UpdatesLink: TStaticText
            Left = 280
            Top = 42
            Width = 58
            Height = 17
            Caption = 'Learn more'
            TabOrder = 1
            TabStop = True
            OnClick = UpdatesLinkClick
          end
        end
        object UpdatesProxyGroup: TGroupBox
          Left = 8
          Top = 226
          Width = 389
          Height = 142
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connection'
          TabOrder = 2
          DesignSize = (
            389
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
            Left = 280
            Top = 91
            Width = 63
            Height = 13
            Anchors = [akTop, akRight]
            Caption = 'Po&rt number:'
            FocusControl = UpdatesProxyPortEdit
          end
          object UpdatesProxyPortEdit: TUpDownEdit
            Left = 278
            Top = 108
            Width = 98
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
          end
          object UpdatesProxyHostEdit: TEdit
            Left = 34
            Top = 108
            Width = 238
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 50
            TabOrder = 3
            Text = 'UpdatesProxyHostEdit'
          end
          object UpdatesProxyCheck: TRadioButton
            Left = 16
            Top = 69
            Width = 365
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use proxy server'
            TabOrder = 2
            OnClick = ControlChange
          end
          object UpdatesDirectCheck: TRadioButton
            Left = 16
            Top = 21
            Width = 365
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'No &proxy'
            TabOrder = 0
            OnClick = ControlChange
          end
          object UpdatesAutoCheck: TRadioButton
            Left = 16
            Top = 45
            Width = 365
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically detect proxy settings'
            TabOrder = 1
            OnClick = ControlChange
          end
        end
        object UpdatesOptionsGroup: TGroupBox
          Left = 8
          Top = 139
          Width = 389
          Height = 81
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Options'
          TabOrder = 1
          DesignSize = (
            389
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
            Left = 278
            Top = 18
            Width = 98
            Height = 21
            Style = csDropDownList
            Anchors = [akTop, akRight]
            TabOrder = 0
          end
          object CollectUsageCheck: TCheckBox
            Left = 16
            Top = 49
            Width = 262
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Allow &anonymous usage statistics'
            TabOrder = 1
            OnClick = ControlChange
          end
          object UsageViewButton: TButton
            Left = 278
            Top = 45
            Width = 98
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
          405
          422)
        object CopyParamListGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 404
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Transfer settings presets'
          TabOrder = 0
          DesignSize = (
            389
            404)
          object CopyParamLabel: TLabel
            Left = 18
            Top = 252
            Width = 354
            Height = 53
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Caption = 'CopyParamLabel'
            ShowAccelChar = False
            WordWrap = True
            OnClick = CopyParamLabelClick
          end
          object CopyParamListView: TListView
            Left = 16
            Top = 24
            Width = 356
            Height = 222
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
            Left = 16
            Top = 311
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = AddCopyParamButtonClick
          end
          object RemoveCopyParamButton: TButton
            Left = 16
            Top = 343
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveCopyParamButtonClick
          end
          object UpCopyParamButton: TButton
            Left = 289
            Top = 311
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 5
            OnClick = UpDownCopyParamButtonClick
          end
          object DownCopyParamButton: TButton
            Left = 289
            Top = 343
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 6
            OnClick = UpDownCopyParamButtonClick
          end
          object EditCopyParamButton: TButton
            Left = 112
            Top = 311
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = EditCopyParamButtonClick
          end
          object DuplicateCopyParamButton: TButton
            Left = 112
            Top = 343
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Du&plicate...'
            TabOrder = 4
            OnClick = DuplicateCopyParamButtonClick
          end
          object CopyParamAutoSelectNoticeCheck: TCheckBox
            Left = 18
            Top = 374
            Width = 354
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
          405
          422)
        object PathInCaptionGroup: TGroupBox
          Left = 8
          Top = 131
          Width = 389
          Height = 94
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Path in window title'
          TabOrder = 1
          DesignSize = (
            389
            94)
          object PathInCaptionFullButton: TRadioButton
            Left = 16
            Top = 21
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show &full path'
            TabOrder = 0
          end
          object PathInCaptionShortButton: TRadioButton
            Left = 16
            Top = 44
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Sho&w short path'
            TabOrder = 1
          end
          object PathInCaptionNoneButton: TRadioButton
            Left = 16
            Top = 67
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do &not show'
            TabOrder = 2
          end
        end
        object WindowMiscellaneousGroup: TGroupBox
          Left = 8
          Top = 231
          Width = 389
          Height = 150
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Miscellaneous'
          TabOrder = 2
          DesignSize = (
            389
            150)
          object MinimizeToTrayCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Minimize main window to taskbar status area (system tray)'
            TabOrder = 0
            OnClick = ControlChange
          end
          object ExternalSessionInExistingInstanceCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Open new externally initiated sessions in &existing window'
            TabOrder = 1
            OnClick = ControlChange
          end
          object KeepOpenWhenNoSessionCheck: TCheckBox
            Left = 16
            Top = 93
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Keep main window open when the last session is closed'
            TabOrder = 3
            OnClick = ControlChange
          end
          object ShowTipsCheck: TCheckBox
            Left = 16
            Top = 117
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Display tips on startup'
            TabOrder = 4
            OnClick = ControlChange
          end
          object ShowLoginWhenNoSessionCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 
              '&Show Login dialog on startup and when the last session is close' +
              'd'
            TabOrder = 2
            OnClick = ControlChange
          end
        end
        object WorkspacesGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 117
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Workspaces'
          TabOrder = 0
          DesignSize = (
            389
            117)
          object AutoWorkspaceLabel: TLabel
            Left = 45
            Top = 45
            Width = 122
            Height = 13
            Caption = '&Default workspace name:'
            FocusControl = AutoWorkspaceCombo
          end
          object AutoSaveWorkspaceCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Automatically save workspace on exit'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutoWorkspaceCombo: TComboBox
            Left = 45
            Top = 61
            Width = 332
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            DropDownCount = 16
            TabOrder = 1
            OnClick = ControlChange
          end
          object AutoSaveWorkspacePasswordsCheck: TCheckBox
            Left = 45
            Top = 87
            Width = 332
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
          405
          422)
        object MasterPasswordGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 92
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Master password'
          TabOrder = 0
          DesignSize = (
            389
            92)
          object SetMasterPasswordButton: TButton
            Left = 16
            Top = 51
            Width = 357
            Height = 25
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Change master password...'
            TabOrder = 1
            OnClick = SetMasterPasswordButtonClick
          end
          object UseMasterPasswordCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 356
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Use master password'
            TabOrder = 0
            OnClick = UseMasterPasswordCheckClick
          end
        end
        object PasswordGroupBox: TGroupBox
          Left = 8
          Top = 106
          Width = 389
          Height = 52
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Session password'
          TabOrder = 1
          DesignSize = (
            389
            52)
          object SessionRememberPasswordCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 356
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Remember &password for duration of session'
            TabOrder = 0
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
          405
          422)
        object ExternalAppsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 208
          Anchors = [akLeft, akTop, akRight]
          Caption = 'External applications'
          TabOrder = 0
          DesignSize = (
            389
            208)
          object PuttyPathLabel: TLabel
            Left = 16
            Top = 21
            Width = 131
            Height = 13
            Caption = 'PuTTY/Terminal &client path:'
            FocusControl = PuttyPathEdit
          end
          object PuttyRegistryStorageKeyLabel: TLabel
            Left = 16
            Top = 158
            Width = 94
            Height = 13
            Caption = 'PuTTY registry &key:'
            FocusControl = PuttyRegistryStorageKeyEdit
          end
          object PuttyPathEdit: THistoryComboBox
            Left = 16
            Top = 38
            Width = 281
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = PuttyPathEditChange
            OnExit = PuttyPathEditExit
          end
          object PuttyPasswordCheck2: TCheckBox
            Left = 24
            Top = 83
            Width = 353
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Remember session password and pass it to PuTTY (SSH)'
            TabOrder = 3
          end
          object AutoOpenInPuttyCheck: TCheckBox
            Left = 24
            Top = 133
            Width = 353
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Automatically &open new sessions in PuTTY'
            TabOrder = 5
          end
          object PuttyPathBrowseButton: TButton
            Left = 303
            Top = 36
            Width = 75
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'B&rowse...'
            TabOrder = 1
            OnClick = PuttyPathBrowseButtonClick
          end
          object TelnetForFtpInPuttyCheck: TCheckBox
            Left = 24
            Top = 108
            Width = 353
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Open &Telnet sessions in PuTTY for FTP sessions'
            TabOrder = 4
          end
          object PuttyPathHintText: TStaticText
            Left = 184
            Top = 61
            Width = 113
            Height = 16
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = '&patterns'
            TabOrder = 2
            TabStop = True
          end
          object PuttyRegistryStorageKeyEdit: THistoryComboBox
            Left = 16
            Top = 174
            Width = 362
            Height = 21
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
          405
          422)
        object ExternalIpAddressGroupBox2: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 152
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Incoming FTP connections (active mode)'
          TabOrder = 0
          DesignSize = (
            389
            152)
          object LocalPortNumberRangeLabel: TLabel
            Left = 133
            Top = 120
            Width = 6
            Height = 13
            Caption = #8211
          end
          object RetrieveExternalIpAddressButton: TRadioButton
            Left = 16
            Top = 21
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Retrieve the external IP address from &operating system'
            TabOrder = 0
            OnClick = ControlChange
          end
          object CustomExternalIpAddressButton: TRadioButton
            Left = 16
            Top = 45
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use the &following external IP address:'
            TabOrder = 1
            OnClick = ControlChange
          end
          object CustomExternalIpAddressEdit: TEdit
            Left = 45
            Top = 67
            Width = 182
            Height = 21
            TabOrder = 2
            OnClick = ControlChange
          end
          object LocalPortNumberCheck: TCheckBox
            Left = 16
            Top = 94
            Width = 361
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Limit listening &ports to:'
            TabOrder = 3
            OnClick = ControlChange
          end
          object LocalPortNumberMinEdit: TUpDownEdit
            Left = 45
            Top = 117
            Width = 82
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1024.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 4
            OnChange = ControlChange
            OnExit = LocalPortNumberMinEditExit
          end
          object LocalPortNumberMaxEdit: TUpDownEdit
            Left = 145
            Top = 117
            Width = 82
            Height = 21
            Alignment = taRightJustify
            MaxValue = 65535.000000000000000000
            MinValue = 1024.000000000000000000
            Anchors = [akTop, akRight]
            TabOrder = 5
            OnChange = ControlChange
            OnExit = LocalPortNumberMaxEditExit
          end
        end
        object ConnectionsGroup: TGroupBox
          Left = 8
          Top = 166
          Width = 389
          Height = 53
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Connections'
          TabOrder = 1
          DesignSize = (
            389
            53)
          object TryFtpWhenSshFailsCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 361
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
          405
          422)
        object PanelsRemoteDirectoryGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Remote panel'
          TabOrder = 0
          DesignSize = (
            389
            99)
          object RefreshRemoteDirectoryUnitLabel: TLabel
            Left = 336
            Top = 69
            Width = 5
            Height = 13
            Caption = 's'
            ShowAccelChar = False
          end
          object ShowInaccesibleDirectoriesCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Show in&accessible directories'
            TabOrder = 0
            OnClick = ControlChange
          end
          object AutoReadDirectoryAfterOpCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Auto&matically refresh directory after operation (Ctrl+Alt+R)'
            TabOrder = 1
            OnClick = ControlChange
          end
          object RefreshRemotePanelCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 266
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
            MinValue = 10.000000000000000000
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
          405
          422)
        object LocalPanelGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 99
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Local panel'
          TabOrder = 0
          DesignSize = (
            389
            99)
          object PreserveLocalDirectoryCheck: TCheckBox
            Left = 16
            Top = 45
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Do not &change state when switching sessions'
            TabOrder = 1
            OnClick = ControlChange
          end
          object SystemContextMenuCheck: TCheckBox
            Left = 16
            Top = 69
            Width = 357
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Use &system file context menu'
            TabOrder = 2
            OnClick = ControlChange
          end
          object DeleteToRecycleBinCheck: TCheckBox
            Left = 16
            Top = 21
            Width = 357
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
          405
          422)
        object LanguagesGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 404
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Languages'
          TabOrder = 0
          DesignSize = (
            389
            404)
          object LanguageChangeLabel: TLabel
            Left = 16
            Top = 373
            Width = 177
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Changes will apply on the next start.'
            ShowAccelChar = False
          end
          object LanguagesView: TListView
            Left = 16
            Top = 24
            Width = 356
            Height = 338
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
            Left = 273
            Top = 368
            Width = 100
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
          405
          422)
        object InternalEditorGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 146
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Display'
          TabOrder = 0
          DesignSize = (
            389
            146)
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
            Caption = 'Default en&coding:'
            FocusControl = EditorEncodingCombo
          end
          object EditorWordWrapCheck: TCheckBox
            Left = 16
            Top = 20
            Width = 158
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = '&Wrap long lines'
            TabOrder = 0
            OnClick = ControlChange
          end
          object EditorTabSizeEdit: TUpDownEdit
            Left = 16
            Top = 64
            Width = 145
            Height = 21
            Alignment = taRightJustify
            MaxValue = 99.000000000000000000
            MinValue = 1.000000000000000000
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 2
            TabOrder = 1
            OnChange = ControlChange
          end
          object EditorEncodingCombo: TComboBox
            Left = 16
            Top = 112
            Width = 145
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 2
            TabOrder = 2
            OnChange = ControlChange
          end
        end
        object FontGroup: TGroupBox
          Left = 8
          Top = 160
          Width = 389
          Height = 118
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Font'
          TabOrder = 1
          DesignSize = (
            389
            118)
          object EditorFontLabel: TLabel
            Left = 172
            Top = 18
            Width = 204
            Height = 87
            Anchors = [akTop, akRight]
            AutoSize = False
            Caption = 'EditorFontLabel'
            Color = clWhite
            ParentColor = False
            ShowAccelChar = False
            Transparent = False
            OnDblClick = EditorFontLabelDblClick
          end
          object EditorFontButton: TButton
            Left = 16
            Top = 18
            Width = 145
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Select font...'
            TabOrder = 0
            OnClick = EditorFontButtonClick
          end
          object EditorFontColorButton: TButton
            Left = 16
            Top = 49
            Width = 145
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Text color'
            TabOrder = 1
            OnClick = EditorFontColorButtonClick
          end
          object EditorBackgroundColorButton: TButton
            Left = 16
            Top = 80
            Width = 145
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Default &background'
            TabOrder = 2
            OnClick = EditorBackgroundColorButtonClick
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
          405
          422)
        object FileColorsGroup: TGroupBox
          Left = 8
          Top = 8
          Width = 389
          Height = 404
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'File colors'
          TabOrder = 0
          DesignSize = (
            389
            404)
          object FileColorsView: TListView
            Left = 16
            Top = 24
            Width = 356
            Height = 307
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
            Left = 16
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = AddEditFileColorButtonClick
          end
          object RemoveFileColorButton: TButton
            Left = 16
            Top = 368
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Remove'
            TabOrder = 3
            OnClick = RemoveFileColorButtonClick
          end
          object UpFileColorButton: TButton
            Left = 290
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Up'
            TabOrder = 4
            OnClick = UpDownFileColorButtonClick
          end
          object DownFileColorButton: TButton
            Left = 290
            Top = 368
            Width = 83
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Down'
            TabOrder = 5
            OnClick = UpDownFileColorButtonClick
          end
          object EditFileColorButton: TButton
            Left = 112
            Top = 337
            Width = 83
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Edit...'
            TabOrder = 2
            OnClick = AddEditFileColorButtonClick
          end
        end
      end
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 132
      Height = 432
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        132
        432)
      object NavigationTree: TTreeView
        Left = 8
        Top = 9
        Width = 116
        Height = 422
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
        OnChanging = NavigationTreeChanging
        OnCollapsing = NavigationTreeCollapsing
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
  end
  object HelpButton: TButton
    Left = 458
    Top = 438
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object ComponentsPanel: TPanel
    Left = 0
    Top = 469
    Width = 545
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
