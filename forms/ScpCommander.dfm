inherited ScpCommanderForm: TScpCommanderForm
  Left = 319
  Top = 33
  Width = 661
  Height = 622
  Caption = 'ScpCommanderForm'
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter [0]
    Left = 313
    Top = 170
    Width = 5
    Height = 213
    Cursor = crHSplit
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
    OnMoved = SplitterMoved
  end
  inherited QueueSplitter: TSplitter
    Top = 404
    Width = 653
  end
  inherited TopCoolBar: TCoolBar
    Width = 653
    Height = 170
    Bands = <
      item
        Control = MenuToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end
      item
        Control = SessionToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end
      item
        Control = PreferencesToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end
      item
        Control = SelectionToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end
      item
        Control = CommandToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end
      item
        Control = SortToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end
      item
        Control = CommandsToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 649
      end>
    FixedSize = False
    object MenuToolBar: TToolBar
      Left = 9
      Top = 0
      Width = 343
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 65
      Caption = 'MenuToolBar'
      EdgeBorders = []
      Flat = True
      ShowCaptions = True
      TabOrder = 0
      Wrapable = False
      object ToolButton2: TToolButton
        Left = 0
        Top = 0
        Hint = 'Change local panel layout or change displayed directory/drive'
        AutoSize = True
        Caption = '&Local'
        Grouped = True
        MenuItem = NonVisualDataModule.CommanderLocalMenu
      end
      object ToolButton1: TToolButton
        Left = 37
        Top = 0
        Hint = 'Mark commands'
        AutoSize = True
        Caption = '&Mark'
        Grouped = True
        MenuItem = NonVisualDataModule.CommonMarkMenu
      end
      object ToolButton4: TToolButton
        Left = 72
        Top = 0
        Hint = 'File operation commands'
        AutoSize = True
        Caption = '&Files'
        Grouped = True
        MenuItem = NonVisualDataModule.CommanderFilesMenu
      end
      object ToolButton49: TToolButton
        Left = 104
        Top = 0
        Hint = 'Other commands'
        AutoSize = True
        Caption = '&Commands'
        Grouped = True
        MenuItem = NonVisualDataModule.CommanderCommandsMenu
      end
      object ToolButton19: TToolButton
        Left = 167
        Top = 0
        Hint = 'Session commands'
        AutoSize = True
        Caption = '&Session'
        Grouped = True
        MenuItem = NonVisualDataModule.CommonSessionMenu
      end
      object ToolButton7: TToolButton
        Left = 215
        Top = 0
        Hint = 'Change program layout/preferences'
        AutoSize = True
        Caption = '&Options'
        Grouped = True
        MenuItem = NonVisualDataModule.CommanderOptionsMenu
      end
      object ToolButton3: TToolButton
        Left = 262
        Top = 0
        Hint = 'Change remote panel layout or change displayed directory'
        AutoSize = True
        Caption = '&Remote'
        Grouped = True
        MenuItem = NonVisualDataModule.CommanderRemoteMenu
      end
      object ToolButton20: TToolButton
        Left = 310
        Top = 0
        Hint = 'Help'
        AutoSize = True
        Caption = '&Help'
        Grouped = True
        MenuItem = NonVisualDataModule.CommonHelpMenu
      end
    end
    object SelectionToolbar: TToolBar
      Left = 9
      Top = 72
      Width = 123
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'SelectionToolbar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Transparent = True
      Wrapable = False
      object ToolButton23: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.SelectAction
      end
      object ToolButton24: TToolButton
        Left = 23
        Top = 0
        Action = NonVisualDataModule.UnselectAction
      end
      object ToolButton25: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton25'
        ImageIndex = 24
        Style = tbsSeparator
      end
      object ToolButton26: TToolButton
        Left = 54
        Top = 0
        Action = NonVisualDataModule.SelectAllAction
      end
      object ToolButton28: TToolButton
        Left = 77
        Top = 0
        Action = NonVisualDataModule.InvertSelectionAction
      end
      object ToolButton29: TToolButton
        Left = 100
        Top = 0
        Action = NonVisualDataModule.ClearSelectionAction
      end
    end
    object PreferencesToolbar: TToolBar
      Left = 9
      Top = 48
      Width = 90
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'PreferencesToolbar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Transparent = True
      Wrapable = False
      object ToolButton33: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.PreferencesAction
      end
      object ToolButton35: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton35'
        ImageIndex = 28
        Style = tbsSeparator
      end
      object ToolButton36: TToolButton
        Left = 31
        Top = 0
        Action = NonVisualDataModule.ViewLogAction
        Style = tbsCheck
      end
      object ToolButton50: TToolButton
        Left = 54
        Top = 0
        Action = NonVisualDataModule.QueueToggleShowAction
        DropdownMenu = NonVisualDataModule.QueueShowPopup
        Style = tbsDropDown
      end
    end
    object SessionToolbar: TToolBar
      Left = 9
      Top = 24
      Width = 235
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'SessionToolbar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Transparent = True
      Wrapable = False
      DesignSize = (
        235
        22)
      object ToolButton30: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.NewSessionAction
      end
      object ToolButton48: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton48'
        ImageIndex = 44
        Style = tbsSeparator
      end
      object SessionCombo: TComboBox
        Left = 31
        Top = 1
        Width = 114
        Height = 19
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        DropDownCount = 15
        ItemHeight = 13
        TabOrder = 0
        TabStop = False
        OnCloseUp = SessionComboCloseUp
      end
      object ToolButton31: TToolButton
        Left = 145
        Top = 0
        Action = NonVisualDataModule.CloseSessionAction
      end
      object ToolButton32: TToolButton
        Left = 168
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton32'
        ImageIndex = 27
        Style = tbsSeparator
      end
      object SavedSessionsButton: TToolButton
        Left = 176
        Top = 0
        Action = NonVisualDataModule.SavedSessionsAction
        MenuItem = NonVisualDataModule.SavedSessionsMenu
        Style = tbsDropDown
      end
      object ToolButton21: TToolButton
        Left = 212
        Top = 0
        Action = NonVisualDataModule.SaveCurrentSessionAction
      end
    end
    object CommandToolBar: TToolBar
      Left = 9
      Top = 96
      Width = 223
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'CommandToolBar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Transparent = True
      Wrapable = False
      object ToolButton6: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.CurrentCopyAction
      end
      object ToolButton17: TToolButton
        Left = 23
        Top = 0
        Action = NonVisualDataModule.CurrentMoveAction
      end
      object ToolButton38: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton38'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object ToolButton43: TToolButton
        Left = 54
        Top = 0
        Action = NonVisualDataModule.CurrentEditAction
      end
      object ToolButton44: TToolButton
        Left = 77
        Top = 0
        Action = NonVisualDataModule.CurrentOpenAction
      end
      object ToolButton5: TToolButton
        Left = 100
        Top = 0
        Action = NonVisualDataModule.CurrentRenameAction
      end
      object ToolButton27: TToolButton
        Left = 123
        Top = 0
        Action = NonVisualDataModule.CurrentDeleteAction
      end
      object ToolButton16: TToolButton
        Left = 146
        Top = 0
        Action = NonVisualDataModule.CurrentPropertiesAction
      end
      object ToolButton37: TToolButton
        Left = 169
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton37'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object ToolButton34: TToolButton
        Left = 177
        Top = 0
        Action = NonVisualDataModule.CurrentCreateDirAction
      end
      object ToolButton47: TToolButton
        Left = 200
        Top = 0
        Action = NonVisualDataModule.AddEditLinkAction
      end
    end
    object SortToolbar: TToolBar
      Left = 9
      Top = 120
      Width = 215
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'SortToolbar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Transparent = True
      Wrapable = False
      object ToolButton8: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.CurrentSortAscendingAction
        Style = tbsCheck
      end
      object ToolButton10: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton10'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object ToolButton9: TToolButton
        Left = 31
        Top = 0
        Action = NonVisualDataModule.CurrentSortByNameAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton22: TToolButton
        Left = 54
        Top = 0
        Action = NonVisualDataModule.CurrentSortByExtAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton11: TToolButton
        Left = 77
        Top = 0
        Action = NonVisualDataModule.CurrentSortByTypeAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton13: TToolButton
        Left = 100
        Top = 0
        Action = NonVisualDataModule.CurrentSortByChangedAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton15: TToolButton
        Left = 123
        Top = 0
        Action = NonVisualDataModule.CurrentSortBySizeAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton12: TToolButton
        Left = 146
        Top = 0
        Action = NonVisualDataModule.CurrentSortByRightsAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton14: TToolButton
        Left = 169
        Top = 0
        Action = NonVisualDataModule.CurrentSortByOwnerAction
        Style = tbsCheck
      end
      object ToolButton18: TToolButton
        Left = 192
        Top = 0
        Action = NonVisualDataModule.CurrentSortByGroupAction
        Style = tbsCheck
      end
    end
    object CommandsToolbar: TToolBar
      Left = 9
      Top = 144
      Width = 131
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'CommandsToolbar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Transparent = True
      Wrapable = False
      object ToolButton39: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.CompareDirectoriesAction
      end
      object ToolButton40: TToolButton
        Left = 23
        Top = 0
        Action = NonVisualDataModule.SynchronizeAction
      end
      object ToolButton51: TToolButton
        Left = 46
        Top = 0
        Action = NonVisualDataModule.FullSynchronizeAction
      end
      object ToolButton41: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton41'
        ImageIndex = 54
        Style = tbsSeparator
      end
      object ToolButton42: TToolButton
        Left = 77
        Top = 0
        Action = NonVisualDataModule.ConsoleAction
      end
      object ToolButton45: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton45'
        ImageIndex = 56
        Style = tbsSeparator
      end
      object ToolButton46: TToolButton
        Left = 108
        Top = 0
        Action = NonVisualDataModule.SynchronizeBrowsingAction
        Style = tbsCheck
      end
    end
  end
  inherited RemotePanel: TPanel
    Left = 318
    Top = 170
    Width = 335
    Height = 213
    Constraints.MinHeight = 200
    Constraints.MinWidth = 185
    TabOrder = 1
    object RemotePathLabel: TPathLabel [0]
      Left = 0
      Top = 72
      Width = 335
      Height = 15
      UnixPath = True
      AutoSize = False
      PopupMenu = NonVisualDataModule.RemotePanelPopup
    end
    inherited RemoteStatusBar: TAssociatedStatusBar
      Top = 194
      Width = 335
      Hint = ''
    end
    inherited RemoteDirView: TUnixDirView
      Top = 87
      Width = 335
      Height = 107
      Constraints.MinHeight = 100
      RowSelect = True
      NortonLike = True
      PathComboBox = RemotePathComboBox
      PathLabel = RemotePathLabel
      AddParentDir = True
      OnLoaded = DirViewLoaded
      OnDDFileOperationExecuted = RemoteDirViewDDFileOperationExecuted
      OnWarnLackOfTempSpace = nil
    end
    object RemoteCoolBar: TCoolBar
      Left = 0
      Top = 0
      Width = 335
      Height = 72
      AutoSize = True
      BandMaximize = bmDblClick
      Bands = <
        item
          Control = ToolBar6
          ImageIndex = -1
          MinHeight = 22
          MinWidth = 50
          Width = 331
        end
        item
          Control = ToolBar3
          ImageIndex = -1
          MinHeight = 22
          Width = 331
        end
        item
          Control = ToolBar4
          ImageIndex = -1
          MinHeight = 22
          Width = 331
        end>
      EdgeBorders = [ebLeft, ebRight, ebBottom]
      FixedSize = True
      PopupMenu = NonVisualDataModule.RemotePanelPopup
      object ToolBar3: TToolBar
        Left = 9
        Top = 24
        Width = 72
        Height = 22
        Hint = '|E'
        Align = alLeft
        AutoSize = True
        Caption = 'ToolBar3'
        DisabledImages = NonVisualDataModule.ExplorerDisabledImages
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Transparent = True
        Wrapable = False
        object RemoteBackButton: TToolButton
          Left = 0
          Top = 0
          Action = NonVisualDataModule.RemoteBackAction
          Style = tbsDropDown
        end
        object RemoteForwardButton: TToolButton
          Left = 36
          Top = 0
          Action = NonVisualDataModule.RemoteForwardAction
          Style = tbsDropDown
        end
      end
      object ToolBar4: TToolBar
        Left = 9
        Top = 48
        Width = 123
        Height = 22
        Hint = '|E'
        Align = alLeft
        AutoSize = True
        Caption = 'ToolBar4'
        DisabledImages = NonVisualDataModule.ExplorerDisabledImages
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Transparent = True
        Wrapable = False
        object ToolButton87: TToolButton
          Left = 0
          Top = 0
          Action = NonVisualDataModule.RemoteParentDirAction
        end
        object ToolButton88: TToolButton
          Left = 23
          Top = 0
          Action = NonVisualDataModule.RemoteRootDirAction
        end
        object ToolButton89: TToolButton
          Left = 46
          Top = 0
          Action = NonVisualDataModule.RemoteHomeDirAction
        end
        object ToolButton90: TToolButton
          Left = 69
          Top = 0
          Action = NonVisualDataModule.RemoteRefreshAction
        end
        object ToolButton91: TToolButton
          Left = 92
          Top = 0
          Width = 8
          Hint = 'E'
          Caption = 'ToolButton91'
          ImageIndex = 9
          Style = tbsSeparator
        end
        object ToolButton92: TToolButton
          Left = 100
          Top = 0
          Action = NonVisualDataModule.RemoteOpenDirAction
        end
      end
      object ToolBar6: TToolBar
        Tag = 1
        Left = 9
        Top = 0
        Width = 318
        Height = 22
        Hint = '|E'
        Align = alClient
        Caption = 'ToolBar6'
        DisabledImages = NonVisualDataModule.ExplorerDisabledImages
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Transparent = True
        Wrapable = False
        OnResize = ToolBarResize
        object RemotePathComboBox: TUnixPathComboBox
          Left = 0
          Top = 0
          Width = 316
          Height = 21
          OnCloseUp = PathComboBoxCloseUp
          Align = alClient
          DropDownCount = 20
          TabOrder = 0
        end
      end
    end
  end
  inherited QueuePanel: TPanel
    Top = 407
    Width = 653
    TabOrder = 5
    inherited QueueView: TListView
      Width = 653
      TabStop = False
    end
    inherited QueueCoolBar: TCoolBar
      Width = 653
      Bands = <
        item
          Control = QueueToolBar
          ImageIndex = -1
          MinHeight = 22
          Width = 649
        end>
      inherited QueueToolBar: TToolBar
        Width = 636
        PopupMenu = NonVisualDataModule.CommanderBarPopup
      end
    end
  end
  object LocalPanel: TPanel
    Left = 0
    Top = 170
    Width = 313
    Height = 213
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinHeight = 200
    Constraints.MinWidth = 185
    TabOrder = 0
    object LocalPathLabel: TPathLabel
      Left = 0
      Top = 72
      Width = 313
      Height = 15
      AutoSize = False
      PopupMenu = NonVisualDataModule.LocalPanelPopup
    end
    object LocalStatusBar: TAssociatedStatusBar
      Left = 0
      Top = 194
      Width = 313
      Height = 19
      Panels = <
        item
          Text = '0 b of 0 b in 0 of 0'
          Width = 50
        end>
      ParentFont = True
      ParentShowHint = False
      PopupMenu = NonVisualDataModule.LocalPanelPopup
      ShowHint = True
      SimplePanel = False
      UseSystemFont = False
      OnResize = StatusBarResize
      FocusControl = LocalDirView
    end
    object LocalDirView: TDirView
      Left = 0
      Top = 87
      Width = 313
      Height = 107
      Align = alClient
      Constraints.MinHeight = 100
      FullDrag = True
      HideSelection = False
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnRightClick = DirViewColumnRightClick
      OnEnter = DirViewEnter
      DirColProperties.ExtVisible = False
      PathComboBox = LocalPathComboBox
      PathLabel = LocalPathLabel
      StatusBar = LocalStatusBar
      OnGetSelectFilter = RemoteDirViewGetSelectFilter
      HeaderImages = NonVisualDataModule.ArrowImages
      AddParentDir = True
      OnLoaded = DirViewLoaded
      OnDDDragEnter = LocalDirViewDDDragEnter
      OnDDDragLeave = DirViewDDDragLeave
      OnDDDragOver = LocalDirViewDDDragOver
      OnDDTargetHasDropHandler = LocalDirViewDDTargetHasDropHandler
      OnDDFileOperation = LocalDirViewDDFileOperation
      OnExecFile = LocalDirViewExecFile
      ConfirmDelete = False
      ConfirmOverwrite = False
      WatchForChanges = True
      OnChangeDetected = LocalDirViewChangeDetected
    end
    object LocalCoolBar: TCoolBar
      Left = 0
      Top = 0
      Width = 313
      Height = 72
      AutoSize = True
      BandMaximize = bmDblClick
      Bands = <
        item
          Control = ToolBar5
          ImageIndex = -1
          MinHeight = 22
          MinWidth = 50
          Width = 309
        end
        item
          Control = ToolBar1
          ImageIndex = -1
          MinHeight = 22
          Width = 309
        end
        item
          Control = ToolBar2
          ImageIndex = -1
          MinHeight = 22
          Width = 309
        end>
      EdgeBorders = [ebLeft, ebRight, ebBottom]
      FixedSize = True
      PopupMenu = NonVisualDataModule.LocalPanelPopup
      object ToolBar1: TToolBar
        Left = 9
        Top = 24
        Width = 72
        Height = 22
        Hint = '|E'
        Align = alLeft
        AutoSize = True
        Caption = 'ToolBar1'
        DisabledImages = NonVisualDataModule.ExplorerDisabledImages
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Transparent = True
        Wrapable = False
        object LocalBackButton: TToolButton
          Left = 0
          Top = 0
          Action = NonVisualDataModule.LocalBackAction
          Style = tbsDropDown
        end
        object LocalForwardButton: TToolButton
          Left = 36
          Top = 0
          Action = NonVisualDataModule.LocalForwardAction
          Style = tbsDropDown
        end
      end
      object ToolBar2: TToolBar
        Left = 9
        Top = 48
        Width = 123
        Height = 22
        Hint = '|E'
        Align = alLeft
        AutoSize = True
        Caption = 'ToolBar2'
        DisabledImages = NonVisualDataModule.ExplorerDisabledImages
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Transparent = True
        Wrapable = False
        object ToolButton57: TToolButton
          Left = 0
          Top = 0
          Action = NonVisualDataModule.LocalParentDirAction
        end
        object ToolButton58: TToolButton
          Left = 23
          Top = 0
          Action = NonVisualDataModule.LocalRootDirAction
        end
        object ToolButton59: TToolButton
          Left = 46
          Top = 0
          Action = NonVisualDataModule.LocalHomeDirAction
        end
        object ToolButton60: TToolButton
          Left = 69
          Top = 0
          Action = NonVisualDataModule.LocalRefreshAction
        end
        object ToolButton61: TToolButton
          Left = 92
          Top = 0
          Width = 8
          Hint = 'E'
          Caption = 'ToolButton61'
          ImageIndex = 9
          Style = tbsSeparator
        end
        object ToolButton62: TToolButton
          Left = 100
          Top = 0
          Action = NonVisualDataModule.LocalOpenDirAction
        end
      end
      object ToolBar5: TToolBar
        Tag = 1
        Left = 9
        Top = 0
        Width = 296
        Height = 22
        Hint = '|E'
        Align = alClient
        Caption = 'ToolBar5'
        DisabledImages = NonVisualDataModule.ExplorerDisabledImages
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Transparent = True
        Wrapable = False
        OnResize = ToolBarResize
        object LocalPathComboBox: TIEPathComboBox
          Left = 0
          Top = 0
          Width = 296
          Height = 21
          OnCloseUp = PathComboBoxCloseUp
          Align = alClient
          DropDownCount = 30
          TabOrder = 0
          TabStop = False
        end
      end
    end
  end
  object ToolbarPanel: TToolbarPanel
    Left = 0
    Top = 547
    Width = 653
    Height = 22
    Category = 'Toolbar Operation (selected + rename + mkdir + close)'
    ActionList = NonVisualDataModule.ExplorerActions
    Stretch = True
    DisabledImages = NonVisualDataModule.ExplorerDisabledImages
    PopupMenu = NonVisualDataModule.CommanderBarPopup
    TabOrder = 4
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 569
    Width = 653
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Alignment = taCenter
        Style = psOwnerDraw
        Width = 35
      end
      item
        Alignment = taCenter
        Style = psOwnerDraw
        Width = 35
      end
      item
        Style = psOwnerDraw
        Width = 100
      end
      item
        Alignment = taCenter
        Width = 80
      end
      item
        Alignment = taCenter
        Width = 80
      end
      item
        Alignment = taCenter
        Width = 50
      end>
    ParentFont = True
    ParentShowHint = False
    PopupMenu = NonVisualDataModule.CommanderBarPopup
    ShowHint = True
    SimplePanel = False
    UseSystemFont = False
    OnDblClick = StatusBarDblClick
    OnMouseMove = SessionStatusBarMouseMove
    OnDrawPanel = SessionStatusBarDrawPanel
    OnResize = StatusBarResize
  end
  object CommandLinePanel: TPanel
    Left = 0
    Top = 383
    Width = 653
    Height = 21
    Align = alBottom
    BevelOuter = bvNone
    PopupMenu = NonVisualDataModule.CommanderBarPopup
    TabOrder = 2
    DesignSize = (
      653
      21)
    object CommandLineLabel: TPathLabel
      Left = 4
      Top = 4
      Width = 159
      Height = 15
      IndentHorizontal = 0
      IndentVertical = 0
      Align = alNone
      Alignment = taRightJustify
      AutoSize = False
    end
    object CommandLinePromptLabel: TLabel
      Left = 164
      Top = 4
      Width = 6
      Height = 13
      Caption = '>'
    end
    object CommandLineCombo: THistoryComboBox
      Left = 173
      Top = 0
      Width = 477
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MaxLength = 250
      TabOrder = 0
      TabStop = False
      Text = 'CommandLineCombo'
      OnDropDown = CommandLineComboDropDown
      OnEnter = CommandLineComboEnter
      OnExit = CommandLineComboExit
      OnKeyDown = CommandLineComboKeyDown
      SaveOn = []
    end
  end
end
