object CustomScpExplorerForm: TCustomScpExplorerForm
  Left = 304
  Top = 166
  Width = 636
  Height = 470
  Caption = 'CustomScpExplorerForm'
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object QueueSplitter: TSplitter
    Left = 0
    Top = 293
    Width = 628
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    MinSize = 70
    ResizeStyle = rsUpdate
    OnCanResize = QueueSplitterCanResize
  end
  object TopDock: TTBXDock
    Left = 0
    Top = 0
    Width = 628
    Height = 9
    FixAlign = True
  end
  object RemotePanel: TPanel
    Left = 0
    Top = 9
    Width = 628
    Height = 284
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object RemotePanelSplitter: TSplitter
      Left = 169
      Top = 0
      Width = 3
      Height = 265
      Cursor = crHSplit
      AutoSnap = False
      MinSize = 70
      ResizeStyle = rsUpdate
    end
    object RemoteStatusBar: TTBXStatusBar
      Left = 0
      Top = 265
      Width = 628
      Height = 19
      Panels = <>
      ParentShowHint = False
      ShowHint = True
      UseSystemFont = False
      OnClick = RemoteStatusBarClick
    end
    object RemoteDirView: TUnixDirView
      Left = 172
      Top = 0
      Width = 456
      Height = 265
      Align = alClient
      FullDrag = True
      HideSelection = False
      PopupMenu = NonVisualDataModule.RemoteDirViewPopup
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnRightClick = DirViewColumnRightClick
      OnEnter = RemoteDirViewEnter
      NortonLike = False
      UnixColProperties.ExtWidth = 20
      OnDDDragFileName = RemoteFileControlDDDragFileName
      OnGetSelectFilter = RemoteDirViewGetSelectFilter
      OnLoaded = DirViewLoaded
      OnExecFile = DirViewExecFile
      OnMatchMask = DirViewMatchMask
      OnGetOverlay = RemoteDirViewGetOverlay
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
    end
    object RemoteDriveView: TUnixDriveView
      Left = 0
      Top = 0
      Width = 169
      Height = 265
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
      Align = alLeft
      Indent = 19
      ParentColor = False
      ReadOnly = True
      TabOrder = 2
      OnEnter = RemoteDriveViewEnter
    end
  end
  object QueuePanel: TPanel
    Left = 0
    Top = 296
    Width = 628
    Height = 140
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object QueueView: TListView
      Left = 0
      Top = 26
      Width = 628
      Height = 114
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
          Caption = 'Transfered'
          Width = 80
        end
        item
          Alignment = taRightJustify
          Caption = 'Elap./Speed'
          Width = 80
        end
        item
          Alignment = taCenter
          Caption = 'Progress'
          Width = 80
        end>
      ColumnClick = False
      DragMode = dmAutomatic
      ReadOnly = True
      RowSelect = True
      PopupMenu = NonVisualDataModule.QueuePopup
      SmallImages = GlyphsModule.QueueImages
      StateImages = GlyphsModule.QueueImages
      TabOrder = 0
      ViewStyle = vsReport
      OnContextPopup = QueueViewContextPopup
      OnDeletion = QueueViewDeletion
      OnEnter = QueueViewEnter
      OnDragDrop = QueueViewDragDrop
      OnDragOver = QueueViewDragOver
      OnSelectItem = QueueViewSelectItem
      OnStartDrag = QueueViewStartDrag
    end
    object QueueDock: TTBXDock
      Left = 0
      Top = 0
      Width = 628
      Height = 26
      AllowDrag = False
      object QueueToolbar: TTBXToolbar
        Left = 0
        Top = 0
        Caption = 'QueueToolbar'
        Images = GlyphsModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
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
        object TBXSeparatorItem202: TTBXSeparatorItem
        end
        object TBXItem208: TTBXItem
          Action = NonVisualDataModule.QueuePreferencesAction
        end
      end
    end
  end
end
