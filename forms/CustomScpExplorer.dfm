object CustomScpExplorerForm: TCustomScpExplorerForm
  Left = 328
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
  OnResize = FormResize
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
  object TopCoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 628
    Height = 41
    AutoSize = True
    BandMaximize = bmDblClick
    Bands = <>
    FixedSize = True
  end
  object RemotePanel: TPanel
    Left = 0
    Top = 41
    Width = 628
    Height = 252
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object RemoteStatusBar: TAssociatedStatusBar
      Left = 0
      Top = 233
      Width = 628
      Height = 19
      Hint = '1'
      Panels = <
        item
          Text = '0 b of 0 b in 0 of 0'
          Width = 50
        end>
      ParentFont = True
      ParentShowHint = False
      ShowHint = True
      SimplePanel = False
      UseSystemFont = False
      OnResize = StatusBarResize
      FocusControl = RemoteDirView
    end
    object RemoteDirView: TUnixDirView
      Left = 0
      Top = 0
      Width = 628
      Height = 233
      Align = alClient
      FullDrag = True
      HideSelection = False
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnRightClick = DirViewColumnRightClick
      OnEnter = DirViewEnter
      NortonLike = False
      UnixColProperties.ExtWidth = 20
      UnixColProperties.ExtVisible = False
      OnGetCopyParam = RemoteDirViewGetCopyParam
      OnDDTargetDrop = RemoteDirViewDDTargetDrop
      StatusBar = RemoteStatusBar
      OnGetSelectFilter = RemoteDirViewGetSelectFilter
      OnExecFile = DirViewExecFile
      OnDDDragEnter = DirViewDDDragEnter
      OnDDDragLeave = DirViewDDDragLeave
      OnDDGiveFeedback = RemoteDirViewDDGiveFeedback
      OnDDEnd = RemoteDirViewDDEnd
      OnDDCreateDragFileList = RemoteDirViewDDCreateDragFileList
      OnDDCreateDataObject = RemoteDirViewDDCreateDataObject
      OnContextPopup = RemoteDirViewContextPopup
      OnDisplayProperties = RemoteDirViewDisplayProperties
      OnWarnLackOfTempSpace = RemoteDirViewWarnLackOfTempSpace
    end
  end
  object QueuePanel: TPanel
    Left = 0
    Top = 296
    Width = 628
    Height = 140
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
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
          Alignment = taCenter
          Caption = 'Progress'
          Width = 80
        end>
      ColumnClick = False
      DragMode = dmAutomatic
      ReadOnly = True
      RowSelect = True
      PopupMenu = NonVisualDataModule.QueuePopup
      SmallImages = NonVisualDataModule.QueueImages
      StateImages = NonVisualDataModule.QueueImages
      TabOrder = 0
      ViewStyle = vsReport
      OnContextPopup = QueueViewContextPopup
      OnDblClick = QueueViewDblClick
      OnDeletion = QueueViewDeletion
      OnEnter = QueueViewEnter
      OnDragDrop = QueueViewDragDrop
      OnDragOver = QueueViewDragOver
      OnKeyDown = QueueViewKeyDown
      OnSelectItem = QueueViewSelectItem
      OnStartDrag = QueueViewStartDrag
    end
    object QueueCoolBar: TCoolBar
      Left = 0
      Top = 0
      Width = 628
      Height = 26
      AutoSize = True
      BandMaximize = bmDblClick
      Bands = <
        item
          Control = QueueToolBar
          ImageIndex = -1
          MinHeight = 22
          Width = 624
        end>
      FixedSize = True
      object QueueToolBar: TToolBar
        Left = 9
        Top = 0
        Width = 611
        Height = 22
        Hint = '|E'
        Caption = 'QueueToolBar'
        EdgeBorders = []
        Flat = True
        Images = NonVisualDataModule.ExplorerImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Transparent = True
        object ToolButton52: TToolButton
          Left = 0
          Top = 0
          Action = NonVisualDataModule.QueueItemQueryAction
        end
        object ToolButton54: TToolButton
          Left = 23
          Top = 0
          Action = NonVisualDataModule.QueueItemErrorAction
        end
        object ToolButton53: TToolButton
          Left = 46
          Top = 0
          Action = NonVisualDataModule.QueueItemPromptAction
        end
        object ToolButton55: TToolButton
          Left = 69
          Top = 0
          Action = NonVisualDataModule.QueueItemExecuteAction
        end
        object ToolButton63: TToolButton
          Left = 92
          Top = 0
          Action = NonVisualDataModule.QueueItemDeleteAction
        end
        object ToolButton56: TToolButton
          Left = 115
          Top = 0
          Width = 8
          Hint = 'E'
          Caption = 'ToolButton56'
          ImageIndex = 71
          Style = tbsSeparator
        end
        object ToolButton64: TToolButton
          Left = 123
          Top = 0
          Action = NonVisualDataModule.QueueItemUpAction
        end
        object ToolButton65: TToolButton
          Left = 146
          Top = 0
          Action = NonVisualDataModule.QueueItemDownAction
        end
        object ToolButton66: TToolButton
          Left = 169
          Top = 0
          Width = 8
          Hint = 'E'
          Caption = 'ToolButton66'
          ImageIndex = 74
          Style = tbsSeparator
        end
        object ToolButton67: TToolButton
          Left = 177
          Top = 0
          Action = NonVisualDataModule.QueuePreferencesAction
        end
      end
    end
  end
end
