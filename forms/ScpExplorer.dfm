inherited ScpExplorerForm: TScpExplorerForm
  Left = 340
  Top = 148
  Width = 648
  HelpType = htKeyword
  HelpKeyword = 'ui_explorer'
  ActiveControl = RemoteDirView
  Caption = 'ScpExplorerForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited QueueSplitter: TSplitter
    Width = 640
  end
  inherited TopCoolBar: TCoolBar
    Width = 640
    Height = 170
    Bands = <
      item
        Control = MenuToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 636
      end
      item
        Control = ToolBar5
        ImageIndex = -1
        MinHeight = 21
        MinWidth = 150
        Text = 'Address'
        Width = 636
      end
      item
        Control = ButtonsToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 636
      end
      item
        Control = SelectionToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 636
      end
      item
        Control = SessionToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 636
      end
      item
        Control = PreferencesToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 636
      end
      item
        Control = SortToolbar
        ImageIndex = -1
        MinHeight = 22
        Width = 636
      end>
    FixedSize = False
    object MenuToolBar: TToolBar
      Left = 9
      Top = 0
      Width = 240
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
        Hint = 'File operations'
        AutoSize = True
        Caption = '&File'
        Grouped = True
        MenuItem = NonVisualDataModule.ExplorerFileMenu
      end
      object ToolButton42: TToolButton
        Left = 27
        Top = 0
        Hint = 'Other commands'
        AutoSize = True
        Caption = '&Commands'
        Grouped = True
        MenuItem = NonVisualDataModule.ExporerCommandsMenu
      end
      object ToolButton19: TToolButton
        Left = 90
        Top = 0
        Hint = 'Mark commands'
        AutoSize = True
        Caption = '&Mark'
        Grouped = True
        MenuItem = NonVisualDataModule.CommonMarkMenu
      end
      object ToolButton27: TToolButton
        Left = 125
        Top = 0
        Hint = 'Session commands'
        AutoSize = True
        Caption = '&Session'
        Grouped = True
        MenuItem = NonVisualDataModule.CommonSessionMenu
      end
      object ToolButton16: TToolButton
        Left = 173
        Top = 0
        Hint = 'Change program layout'
        AutoSize = True
        Caption = '&View'
        Grouped = True
        MenuItem = NonVisualDataModule.ExplorerViewMenu
      end
      object ToolButton17: TToolButton
        Left = 207
        Top = 0
        Hint = 'Help'
        AutoSize = True
        Caption = '&Help'
        Grouped = True
        MenuItem = NonVisualDataModule.CommonHelpMenu
      end
    end
    object ButtonsToolBar: TToolBar
      Left = 9
      Top = 47
      Width = 426
      Height = 22
      Hint = '|E'
      Align = alLeft
      AutoSize = True
      Caption = 'ButtonsToolBar'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Transparent = True
      Wrapable = False
      object BackButton: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.RemoteBackAction
        Style = tbsDropDown
      end
      object ForwardButton: TToolButton
        Left = 36
        Top = 0
        Action = NonVisualDataModule.RemoteForwardAction
        Style = tbsDropDown
      end
      object ToolButton12: TToolButton
        Left = 72
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton12'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 80
        Top = 0
        Action = NonVisualDataModule.RemoteParentDirAction
      end
      object ToolButton9: TToolButton
        Left = 103
        Top = 0
        Action = NonVisualDataModule.RemoteRootDirAction
      end
      object ToolButton10: TToolButton
        Left = 126
        Top = 0
        Action = NonVisualDataModule.RemoteHomeDirAction
      end
      object ToolButton11: TToolButton
        Left = 149
        Top = 0
        Action = NonVisualDataModule.RemoteRefreshAction
      end
      object ToolButton18: TToolButton
        Left = 172
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton18'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton44: TToolButton
        Left = 180
        Top = 0
        Action = NonVisualDataModule.CurrentEditAction
      end
      object ToolButton45: TToolButton
        Left = 203
        Top = 0
        Action = NonVisualDataModule.CurrentOpenAction
      end
      object ToolButton6: TToolButton
        Left = 226
        Top = 0
        Action = NonVisualDataModule.CurrentDeleteAction
      end
      object ToolButton7: TToolButton
        Left = 249
        Top = 0
        Action = NonVisualDataModule.CurrentPropertiesAction
      end
      object ToolButton14: TToolButton
        Left = 272
        Top = 0
        Action = NonVisualDataModule.CurrentRenameAction
      end
      object ToolButton15: TToolButton
        Left = 295
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton15'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton13: TToolButton
        Left = 303
        Top = 0
        Action = NonVisualDataModule.CurrentCreateDirAction
      end
      object ToolButton46: TToolButton
        Left = 326
        Top = 0
        Action = NonVisualDataModule.AddEditLinkAction
      end
      object ToolButton43: TToolButton
        Left = 349
        Top = 0
        Action = NonVisualDataModule.ConsoleAction
      end
      object ToolButton48: TToolButton
        Left = 372
        Top = 0
        Width = 8
        Caption = 'ToolButton48'
        ImageIndex = 56
        Style = tbsSeparator
      end
      object ToolButton51: TToolButton
        Left = 380
        Top = 0
        Action = NonVisualDataModule.SynchronizeAction
      end
      object ToolButton49: TToolButton
        Left = 403
        Top = 0
        Action = NonVisualDataModule.FullSynchronizeAction
      end
    end
    object SelectionToolbar: TToolBar
      Left = 9
      Top = 71
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
      TabOrder = 2
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
      object ToolButton21: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton21'
        ImageIndex = 24
        Style = tbsSeparator
      end
      object ToolButton25: TToolButton
        Left = 54
        Top = 0
        Action = NonVisualDataModule.SelectAllAction
      end
      object ToolButton26: TToolButton
        Left = 77
        Top = 0
        Action = NonVisualDataModule.InvertSelectionAction
      end
      object ToolButton20: TToolButton
        Left = 100
        Top = 0
        Action = NonVisualDataModule.ClearSelectionAction
      end
    end
    object SessionToolbar: TToolBar
      Left = 9
      Top = 95
      Width = 242
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
      object ToolButton28: TToolButton
        Left = 0
        Top = 0
        Action = NonVisualDataModule.NewSessionAction
      end
      object ToolButton47: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton47'
        ImageIndex = 44
        Style = tbsSeparator
      end
      object SessionCombo: TComboBox
        Left = 31
        Top = 0
        Width = 114
        Height = 21
        Style = csOwnerDrawFixed
        DropDownCount = 15
        ItemHeight = 15
        TabOrder = 0
        TabStop = False
      end
      object SessionComboResizer: TSplitter
        Left = 145
        Top = 0
        Width = 4
        Height = 22
        Cursor = crHSplit
        Hint = 'Drag to resize session drop down menu'
        AutoSnap = False
        ResizeStyle = rsUpdate
        OnMoved = SessionComboResizerMoved
      end
      object ToolButton5: TToolButton
        Left = 149
        Top = 0
        Width = 3
        Caption = 'ToolButton5'
        ImageIndex = 44
        Style = tbsSeparator
      end
      object ToolButton29: TToolButton
        Left = 152
        Top = 0
        Action = NonVisualDataModule.CloseSessionAction
      end
      object ToolButton30: TToolButton
        Left = 175
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton30'
        ImageIndex = 27
        Style = tbsSeparator
      end
      object SavedSessionsButton: TToolButton
        Left = 183
        Top = 0
        Action = NonVisualDataModule.SavedSessionsAction
        MenuItem = NonVisualDataModule.SavedSessionsMenu
        Style = tbsDropDown
      end
      object ToolButton34: TToolButton
        Left = 219
        Top = 0
        Action = NonVisualDataModule.SaveCurrentSessionAction
      end
    end
    object PreferencesToolbar: TToolBar
      Left = 9
      Top = 119
      Width = 149
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
      TabOrder = 4
      Transparent = True
      Wrapable = False
      object ToolButton31: TToolButton
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
      object ToolButton32: TToolButton
        Left = 31
        Top = 0
        Action = NonVisualDataModule.CurrentCycleStyleAction
        DropdownMenu = NonVisualDataModule.ExplorerStylePopup
        Style = tbsDropDown
      end
      object ToolButton33: TToolButton
        Left = 67
        Top = 0
        Action = NonVisualDataModule.ViewLogAction
        Style = tbsCheck
      end
      object ToolButton50: TToolButton
        Left = 90
        Top = 0
        Action = NonVisualDataModule.QueueToggleShowAction
        DropdownMenu = NonVisualDataModule.QueueShowPopup
        Style = tbsDropDown
      end
      object ToolButton3: TToolButton
        Left = 126
        Top = 0
        Action = NonVisualDataModule.RemoteTreeAction
      end
    end
    object SortToolbar: TToolBar
      Left = 9
      Top = 143
      Width = 192
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
      object ToolButton1: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Hint = 'E'
        Caption = 'ToolButton1'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object ToolButton22: TToolButton
        Left = 31
        Top = 0
        Action = NonVisualDataModule.CurrentSortByNameAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton41: TToolButton
        Left = 54
        Top = 0
        Action = NonVisualDataModule.CurrentSortByExtAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton36: TToolButton
        Left = 77
        Top = 0
        Action = NonVisualDataModule.CurrentSortByChangedAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton37: TToolButton
        Left = 100
        Top = 0
        Action = NonVisualDataModule.CurrentSortBySizeAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton38: TToolButton
        Left = 123
        Top = 0
        Action = NonVisualDataModule.CurrentSortByRightsAction
        Grouped = True
        Style = tbsCheck
      end
      object ToolButton39: TToolButton
        Left = 146
        Top = 0
        Action = NonVisualDataModule.CurrentSortByOwnerAction
        Style = tbsCheck
      end
      object ToolButton40: TToolButton
        Left = 169
        Top = 0
        Action = NonVisualDataModule.CurrentSortByGroupAction
        Style = tbsCheck
      end
    end
    object ToolBar5: TToolBar
      Tag = 1
      Left = 51
      Top = 24
      Width = 581
      Height = 21
      Hint = '|E'
      Align = alClient
      Caption = 'ToolBar5'
      DisabledImages = NonVisualDataModule.ExplorerDisabledImages
      EdgeBorders = []
      Flat = True
      Images = NonVisualDataModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Transparent = True
      Wrapable = False
      OnResize = ToolBarResize
      object UnixPathComboBox: TUnixPathComboBox
        Tag = 1
        Left = 0
        Top = 0
        Width = 555
        Height = 21
        Align = alClient
        TabOrder = 0
      end
      object ToolButton57: TToolButton
        Left = 555
        Top = 0
        Action = NonVisualDataModule.RemoteOpenDirAction
      end
    end
  end
  inherited RemotePanel: TPanel
    Top = 170
    Width = 640
    Height = 123
    Constraints.MinHeight = 100
    Constraints.MinWidth = 210
    inherited RemotePanelSplitter: TSplitter
      Height = 104
    end
    inherited RemoteStatusBar: TAssociatedStatusBar
      Tag = 1
      Top = 104
      Width = 640
      Hint = ''
      Panels = <
        item
          Text = '0 b of 0 b in 0 of 0'
          Width = 190
        end
        item
          Alignment = taCenter
          Width = 70
        end
        item
          Alignment = taCenter
          Width = 70
        end
        item
          Alignment = taCenter
          Style = psOwnerDraw
          Width = 25
        end
        item
          Alignment = taCenter
          Style = psOwnerDraw
          Width = 25
        end
        item
          Style = psOwnerDraw
          Width = 70
        end
        item
          Alignment = taCenter
          Width = 90
        end
        item
          Alignment = taCenter
          Width = 80
        end
        item
          Alignment = taCenter
          Width = 80
        end>
      OnDblClick = RemoteStatusBarDblClick
      OnMouseMove = SessionStatusBarMouseMove
      OnDrawPanel = SessionStatusBarDrawPanel
    end
    inherited RemoteDirView: TUnixDirView
      Width = 468
      Height = 104
      PathComboBox = UnixPathComboBox
    end
    inherited RemoteDriveView: TUnixDriveView
      Height = 104
      Constraints.MinWidth = 40
    end
  end
  inherited QueuePanel: TPanel
    Width = 640
    inherited QueueView: TListView
      Width = 640
    end
    inherited QueueCoolBar: TCoolBar
      Width = 640
      Bands = <
        item
          Control = QueueToolBar
          ImageIndex = -1
          MinHeight = 22
          Width = 636
        end>
      PopupMenu = NonVisualDataModule.ExplorerBarPopup
      inherited QueueToolBar: TToolBar
        Width = 623
      end
    end
  end
end
