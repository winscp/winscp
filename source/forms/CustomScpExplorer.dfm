object CustomScpExplorerForm: TCustomScpExplorerForm
  Left = 251
  Top = 166
  Caption = 'CustomScpExplorerForm'
  ClientHeight = 432
  ClientWidth = 620
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnConstrainedResize = FormConstrainedResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object QueueSplitter: TSplitter
    Left = 0
    Top = 289
    Width = 620
    Height = 3
    Cursor = crSizeNS
    Hint = 'Drag to resize queue list. Double click to hide queue list.'
    Align = alBottom
    AutoSnap = False
    MinSize = 70
    ResizeStyle = rsUpdate
    OnCanResize = QueueSplitterCanResize
  end
  object TopDock: TTBXDock
    Left = 0
    Top = 0
    Width = 620
    Height = 9
    FixAlign = True
  end
  object RemotePanel: TPanel
    Left = 0
    Top = 30
    Width = 620
    Height = 259
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object RemotePanelSplitter: TSplitter
      Left = 169
      Top = 0
      Height = 240
      Cursor = crSizeWE
      AutoSnap = False
      MinSize = 70
      ResizeStyle = rsUpdate
    end
    object RemoteStatusBar: TTBXStatusBar
      Left = 0
      Top = 240
      Width = 620
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
      UseSystemFont = False
      OnClick = RemoteStatusBarClick
      OnMouseDown = RemoteStatusBarMouseDown
    end
    object RemoteDirPanel: TPanel
      Left = 172
      Top = 0
      Width = 448
      Height = 240
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object RemoteDirView: TUnixDirView
        Left = 0
        Top = 0
        Width = 448
        Height = 240
        Align = alClient
        DoubleBuffered = True
        FullDrag = True
        HideSelection = False
        ParentDoubleBuffered = False
        PopupMenu = NonVisualDataModule.RemoteDirViewPopup
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnRightClick = DirViewColumnRightClick
        OnEditing = DirViewEditing
        OnEnter = RemoteDirViewEnter
        OnExit = DirViewExit
        OnKeyDown = DirViewKeyDown
        OnKeyPress = DirViewKeyPress
        NortonLike = nlOff
        UnixColProperties.ExtWidth = 20
        UnixColProperties.TypeVisible = False
        OnDDDragFileName = RemoteFileControlDDDragFileName
        OnBusy = DirViewBusy
        OnChangeFocus = DirViewChangeFocus
        OnSelectItem = DirViewSelectItem
        OnLoaded = DirViewLoaded
        OnExecFile = DirViewExecFile
        OnMatchMask = DirViewMatchMask
        OnGetOverlay = DirViewGetOverlay
        OnDDDragEnter = FileControlDDDragEnter
        OnDDDragLeave = FileControlDDDragLeave
        OnDDQueryContinueDrag = RemoteFileControlDDQueryContinueDrag
        OnDDGiveFeedback = RemoteFileControlDDGiveFeedback
        OnDDChooseEffect = RemoteFileContolDDChooseEffect
        OnDDDragDetect = RemoteFileControlDDDragDetect
        OnDDEnd = RemoteFileControlDDEnd
        OnDDCreateDragFileList = RemoteFileControlDDCreateDragFileList
        OnDDFileOperation = RemoteFileControlDDFileOperation
        OnDDCreateDataObject = RemoteFileControlDDCreateDataObject
        OnContextPopup = RemoteDirViewContextPopup
        OnHistoryChange = DirViewHistoryChange
        OnDisplayProperties = RemoteDirViewDisplayProperties
        OnRead = RemoteDirViewRead
      end
    end
    object RemoteDrivePanel: TPanel
      Left = 0
      Top = 0
      Width = 169
      Height = 240
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object RemoteDriveView: TUnixDriveView
        Left = 0
        Top = 0
        Width = 169
        Height = 240
        DirView = RemoteDirView
        OnDDDragFileName = RemoteFileControlDDDragFileName
        OnDDEnd = RemoteFileControlDDEnd
        UseSystemContextMenu = False
        OnDDDragEnter = FileControlDDDragEnter
        OnDDDragLeave = FileControlDDDragLeave
        OnDDQueryContinueDrag = RemoteFileControlDDQueryContinueDrag
        OnDDChooseEffect = RemoteFileContolDDChooseEffect
        OnDDGiveFeedback = RemoteFileControlDDGiveFeedback
        OnDDDragDetect = RemoteFileControlDDDragDetect
        OnDDFileOperation = RemoteFileControlDDFileOperation
        OnDDCreateDragFileList = RemoteFileControlDDCreateDragFileList
        OnDDCreateDataObject = RemoteFileControlDDCreateDataObject
        Align = alClient
        DoubleBuffered = True
        HideSelection = False
        Indent = 19
        ParentColor = False
        ParentDoubleBuffered = False
        ReadOnly = True
        TabOrder = 0
        OnEnter = RemoteDriveViewEnter
        OnBusy = DirViewBusy
      end
    end
  end
  object QueuePanel: TPanel
    Left = 0
    Top = 292
    Width = 620
    Height = 140
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object QueueLabel: TPathLabel
      Left = 0
      Top = 0
      Width = 620
      Height = 19
      IndentVertical = 3
      AutoSizeVertical = True
      AutoSize = False
      Transparent = False
    end
    object QueueView3: TListView
      Left = 0
      Top = 45
      Width = 620
      Height = 95
      Align = alClient
      Columns = <
        item
          Caption = 'Operation'
          Width = 70
        end
        item
          Caption = 'Source'
          Width = 170
        end
        item
          Caption = 'Destination'
          Width = 170
        end
        item
          Alignment = taRightJustify
          Caption = 'Transferred'
          Width = 80
        end
        item
          Alignment = taRightJustify
          Caption = 'Time'
          Width = 80
        end
        item
          Alignment = taRightJustify
          Caption = 'Speed'
          Width = 80
        end
        item
          Alignment = taCenter
          Caption = 'Progress'
          Width = 80
        end>
      ColumnClick = False
      DoubleBuffered = True
      DragMode = dmAutomatic
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      PopupMenu = NonVisualDataModule.QueuePopup
      SmallImages = GlyphsModule.QueueImages
      StateImages = GlyphsModule.QueueImages
      TabOrder = 0
      ViewStyle = vsReport
      OnContextPopup = QueueView3ContextPopup
      OnDeletion = QueueView3Deletion
      OnEnter = QueueView3Enter
      OnExit = QueueView3Exit
      OnDragDrop = QueueView3DragDrop
      OnDragOver = QueueView3DragOver
      OnSelectItem = QueueView3SelectItem
      OnStartDrag = QueueView3StartDrag
    end
    object QueueDock: TTBXDock
      Tag = 1
      Left = 0
      Top = 19
      Width = 620
      Height = 26
      AllowDrag = False
      object QueueToolbar: TTBXToolbar
        Left = 0
        Top = 0
        Caption = 'QueueToolbar'
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object QueueEnableItem: TTBXItem
          Action = NonVisualDataModule.QueueEnableAction
        end
        object TBXSeparatorItem203: TTBXSeparatorItem
        end
        object TBXItem201: TTBXItem
          Action = NonVisualDataModule.QueueItemQueryAction
        end
        object TBXItem202: TTBXItem
          Action = NonVisualDataModule.QueueItemErrorAction
        end
        object TBXItem203: TTBXItem
          Action = NonVisualDataModule.QueueItemPromptAction
        end
        object TBXItem204: TTBXItem
          Action = NonVisualDataModule.QueueItemExecuteAction
        end
        object TBXItem195: TTBXItem
          Action = NonVisualDataModule.QueueItemPauseAction
        end
        object TBXItem194: TTBXItem
          Action = NonVisualDataModule.QueueItemResumeAction
        end
        object TBXItem205: TTBXItem
          Action = NonVisualDataModule.QueueItemDeleteAction
        end
        object TBXSeparatorItem201: TTBXSeparatorItem
        end
        object TBXItem206: TTBXItem
          Action = NonVisualDataModule.QueueItemUpAction
        end
        object TBXItem207: TTBXItem
          Action = NonVisualDataModule.QueueItemDownAction
        end
        object TBXSeparatorItem57: TTBXSeparatorItem
        end
        object QueueDeleteAllDoneQueueToolbarItem: TTBXItem
          Action = NonVisualDataModule.QueueDeleteAllDoneAction
        end
        object TBXSeparatorItem202: TTBXSeparatorItem
        end
        object TBXSubmenuItem27: TTBXSubmenuItem
          Action = NonVisualDataModule.QueueCycleOnceEmptyAction
          DropdownCombo = True
          object TBXItem211: TTBXItem
            Action = NonVisualDataModule.QueueIdleOnceEmptyAction
            RadioItem = True
          end
          object TBXItem225: TTBXItem
            Action = NonVisualDataModule.QueueDisconnectOnceEmptyAction2
            RadioItem = True
          end
          object TBXItem173: TTBXItem
            Action = NonVisualDataModule.QueueSuspendOnceEmptyAction2
            RadioItem = True
          end
          object TBXItem226: TTBXItem
            Action = NonVisualDataModule.QueueShutDownOnceEmptyAction2
            RadioItem = True
          end
        end
        object TBXItem208: TTBXItem
          Action = NonVisualDataModule.QueuePreferencesAction
        end
      end
    end
  end
  object SessionsPageControl: TThemePageControl
    Left = 0
    Top = 9
    Width = 620
    Height = 21
    ActivePage = TabSheet1
    Align = alTop
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 3
    TabStop = False
    OnChange = SessionsPageControlChange
    OnContextPopup = SessionsPageControlContextPopup
    OnDragDrop = SessionsPageControlDragDrop
    OnDragOver = SessionsPageControlDragOver
    OnMouseDown = SessionsPageControlMouseDown
    OnCloseButtonClick = SessionsPageControlCloseButtonClick
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnDeactivate = ApplicationEventsDeactivate
    OnMinimize = ApplicationMinimize
    OnModalBegin = ApplicationEventsModalBegin
    OnRestore = ApplicationRestore
    Left = 88
    Top = 200
  end
end
