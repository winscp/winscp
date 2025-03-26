object CustomScpExplorerForm: TCustomScpExplorerForm
  Left = 251
  Top = 166
  Caption = 'CustomScpExplorerForm'
  ClientHeight = 429
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnConstrainedResize = FormConstrainedResize
  OnShow = FormShow
  TextHeight = 15
  object QueueSplitter: TSplitter
    Left = 0
    Top = 286
    Width = 608
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
    Width = 608
    Height = 9
    FixAlign = True
  end
  object RemotePanel: TPanel
    Left = 0
    Top = 39
    Width = 608
    Height = 247
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    object RemotePanelSplitter: TSplitter
      Left = 169
      Top = 0
      Height = 228
      Cursor = crSizeWE
      AutoSnap = False
      MinSize = 70
      ResizeStyle = rsUpdate
    end
    object RemoteStatusBar: TTBXStatusBar
      Left = 0
      Top = 228
      Width = 608
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
      Width = 436
      Height = 228
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object RemoteDirView: TUnixDirView
        Left = 0
        Top = 0
        Width = 448
        Height = 231
        Align = alClient
        DoubleBuffered = True
        FullDrag = True
        HideSelection = False
        IconOptions.AutoArrange = True
        ParentDoubleBuffered = False
        PopupMenu = NonVisualDataModule.RemoteDirViewPopup
        TabOrder = 0
        OnColumnRightClick = DirViewColumnRightClick
        OnEditing = DirViewEditing
        OnEnter = RemoteDirViewEnter
        OnExit = DirViewExit
        OnKeyDown = DirViewKeyDown
        OnKeyPress = DirViewKeyPress
        OnResize = RemoteDirViewResize
        NortonLike = nlOff
        UnixColProperties.ExtWidth = 20
        UnixColProperties.TypeVisible = False
        OnDDDragFileName = RemoteFileControlDDDragFileName
        OnBusy = DirViewBusy
        OnChangeFocus = DirViewChangeFocus
        OnSelectItem = DirViewSelectItem
        OnStartLoading = RemoteDirViewStartLoading
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
        DirViewStyle = dvsReport
        OnRead = RemoteDirViewRead
        OnStartReading = RemoteDirViewStartReading
        OnThumbnailNeeded = RemoteDirViewThumbnailNeeded
      end
      object ReconnectToolbar: TTBXToolbar
        Left = 136
        Top = 112
        Width = 107
        Height = 21
        Caption = 'ReconnectToolbar'
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object TBXItem254: TTBXItem
          Action = NonVisualDataModule.ReconnectSessionAction
        end
      end
    end
    object RemoteDrivePanel: TPanel
      Left = 0
      Top = 0
      Width = 169
      Height = 228
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object RemoteDriveView: TUnixDriveView
        Left = 0
        Top = 0
        Width = 169
        Height = 228
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
    Top = 289
    Width = 608
    Height = 140
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object QueueLabel: TPathLabel
      Left = 0
      Top = 0
      Width = 620
      Height = 21
      IndentVertical = 3
      AutoSizeVertical = True
      OnGetStatus = QueueLabelGetStatus
      AutoSize = False
      Transparent = False
    end
    object QueueFileListSplitter: TSplitter
      Left = 0
      Top = 117
      Width = 620
      Height = 3
      Cursor = crSizeNS
      Hint = 
        'Drag to resize queue file list. Double click to hide queue file ' +
        'list.'
      Align = alBottom
      AutoSnap = False
      MinSize = 10
      ResizeStyle = rsUpdate
      OnCanResize = QueueFileListSplitterCanResize
    end
    object QueueView3: TListView
      Left = 0
      Top = 47
      Width = 620
      Height = 70
      Align = alClient
      Columns = <
        item
          Caption = 'Operation'
          MinWidth = 20
          Width = 70
        end
        item
          Caption = 'Source'
          MinWidth = 20
          Width = 170
        end
        item
          Caption = 'Destination'
          MinWidth = 20
          Width = 170
        end
        item
          Alignment = taRightJustify
          Caption = 'Transferred'
          MinWidth = 20
          Width = 80
        end
        item
          Alignment = taRightJustify
          Caption = 'Time'
          MinWidth = 20
          Width = 80
        end
        item
          Alignment = taRightJustify
          Caption = 'Speed'
          MinWidth = 20
          Width = 80
        end
        item
          Alignment = taCenter
          Caption = 'Progress'
          MinWidth = 20
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
      OnChange = QueueView3Change
      OnContextPopup = QueueView3ContextPopup
      OnDeletion = QueueView3Deletion
      OnEndDrag = QueueView3EndDrag
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
      Top = 21
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
    object QueueFileList: TListView
      Left = 0
      Top = 120
      Width = 620
      Height = 20
      Align = alBottom
      Columns = <
        item
        end>
      DoubleBuffered = True
      OwnerData = True
      ReadOnly = True
      ParentDoubleBuffered = False
      ShowColumnHeaders = False
      TabOrder = 2
      TabStop = False
      ViewStyle = vsReport
      OnCustomDrawItem = QueueFileListCustomDrawItem
      OnData = QueueFileListData
      OnEnter = QueueFileListEnterExit
      OnExit = QueueFileListEnterExit
      OnResize = QueueFileListResize
    end
  end
  object SessionsPageControl: TThemePageControl
    Left = 0
    Top = 18
    Width = 608
    Height = 21
    ActivePage = TabSheet1
    Align = alTop
    DoubleBuffered = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    TabStop = False
    OnChange = SessionsPageControlChange
    OnContextPopup = SessionsPageControlContextPopup
    OnDragDrop = SessionsPageControlDragDrop
    OnDragOver = SessionsPageControlDragOver
    OnMouseDown = SessionsPageControlMouseDown
    OnResize = SessionsPageControlResize
    OnTabButtonClick = SessionsPageControlTabButtonClick
    OnTabHint = SessionsPageControlTabHint
    object TabSheet1: TThemeTabSheet
      Caption = 'TabSheet1'
    end
  end
  object MessageDock: TTBXDock
    Tag = 1
    Left = 0
    Top = 9
    Width = 608
    Height = 9
    AllowDrag = False
    FixAlign = True
    Visible = False
    OnRequestDock = MessageDockRequestDock
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
