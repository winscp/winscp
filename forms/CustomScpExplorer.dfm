object CustomScpExplorerForm: TCustomScpExplorerForm
  Left = 333
  Top = 184
  Width = 654
  Height = 485
  Caption = 'CustomScpExplorerForm'
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object TopCoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 646
    Height = 41
    AutoSize = True
    BandMaximize = bmDblClick
    Bands = <>
    FixedSize = True
  end
  object RemotePanel: TPanel
    Left = 0
    Top = 41
    Width = 646
    Height = 410
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object RemoteStatusBar: TAssociatedStatusBar
      Left = 0
      Top = 391
      Width = 646
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
      FocusControl = RemoteDirView
    end
    object RemoteDirView: TUnixDirView
      Left = 0
      Top = 0
      Width = 646
      Height = 391
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
      StatusBar = RemoteStatusBar
      OnGetSelectFilter = RemoteDirViewGetSelectFilter
      TargetPopupMenu = False
      OnExecFile = DirViewExecFile
      OnDDDragEnter = DirViewDDDragEnter
      OnDDDragLeave = DirViewDDDragLeave
      OnContextPopup = RemoteDirViewContextPopup
      OnDisplayProperties = RemoteDirViewDisplayProperties
      OnWarnLackOfTempSpace = RemoteDirViewWarnLackOfTempSpace
    end
  end
end
