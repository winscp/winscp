inherited ScpCommanderForm: TScpCommanderForm
  Left = 162
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'ui_commander'
  Caption = 'ScpCommanderForm'
  ClientHeight = 670
  ClientWidth = 898
  TextHeight = 15
  object Splitter: TSplitter [0]
    Left = 435
    Top = 186
    Width = 5
    Height = 289
    Cursor = crSizeWE
    Hint = 
      '|Drag to change ratio of file panels. Double click to make width' +
      ' of file panels equal.'
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
    OnMoved = SplitterMoved
  end
  inherited QueueSplitter: TSplitter
    Top = 529
    Width = 898
  end
  inherited TopDock: TTBXDock
    Width = 898
    Height = 156
    OnContextPopup = DockContextPopup
    object MenuToolbar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Menu'
      CloseButton = False
      Images = GlyphsModule.ExplorerImages
      MenuBar = True
      Options = [tboNoAutoHint, tboShowHint]
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 4
      object LocalMenuButton: TTBXSubmenuItem
        Caption = '&LocalX'
        HelpKeyword = 'ui_commander_menu#local'
        object TBXItem1: TTBXItem
          Action = NonVisualDataModule.LocalChangePathAction2
        end
        object TBXSeparatorItem1: TTBXSeparatorItem
        end
        object TBXSubmenuItem2: TTBXSubmenuItem
          Caption = '&Go To'
          HelpKeyword = 'task_navigate'
          Hint = 'Go to directory'
          object TBXItem2: TTBXItem
            Action = NonVisualDataModule.LocalOpenDirAction
          end
          object TBXItem3: TTBXItem
            Action = NonVisualDataModule.LocalExploreDirectoryAction
          end
          object TBXSeparatorItem2: TTBXSeparatorItem
          end
          object TBXItem4: TTBXItem
            Action = NonVisualDataModule.LocalParentDirAction
          end
          object TBXItem5: TTBXItem
            Action = NonVisualDataModule.LocalRootDirAction
          end
          object TBXItem6: TTBXItem
            Action = NonVisualDataModule.LocalHomeDirAction
          end
          object TBXItem262: TTBXItem
            Action = NonVisualDataModule.LocalOtherDirAction
          end
          object TBXSeparatorItem3: TTBXSeparatorItem
          end
          object TBXItem7: TTBXItem
            Action = NonVisualDataModule.LocalBackAction
          end
          object TBXItem8: TTBXItem
            Action = NonVisualDataModule.LocalForwardAction
          end
        end
        object TBXItem9: TTBXItem
          Action = NonVisualDataModule.LocalRefreshAction
        end
        object TBXItem10: TTBXItem
          Action = NonVisualDataModule.LocalAddBookmarkAction2
        end
        object TBXItem11: TTBXItem
          Action = NonVisualDataModule.LocalPathToClipboardAction2
        end
        object TBXSeparatorItem4: TTBXSeparatorItem
        end
        object TBXSubmenuItem17: TTBXSubmenuItem
          Caption = '&View'
          HelpKeyword = 'ui_file_panel#view_style'
          Hint = 'Change directory view style'
          object TBXItem270: TTBXItem
            Action = NonVisualDataModule.LocalReportAction
          end
          object TBXSeparatorItem76: TTBXSeparatorItem
          end
          object TBXItem271: TTBXItem
            Action = NonVisualDataModule.LocalThumbnailAction
          end
        end
        object TBXSubmenuItem3: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in local panel'
          object TBXItem12: TTBXItem
            Action = NonVisualDataModule.LocalSortAscendingAction2
          end
          object TBXSeparatorItem5: TTBXSeparatorItem
          end
          object TBXItem13: TTBXItem
            Action = NonVisualDataModule.LocalSortByNameAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem14: TTBXItem
            Action = NonVisualDataModule.LocalSortByExtAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem17: TTBXItem
            Action = NonVisualDataModule.LocalSortBySizeAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem15: TTBXItem
            Action = NonVisualDataModule.LocalSortByTypeAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem16: TTBXItem
            Action = NonVisualDataModule.LocalSortByChangedAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem18: TTBXItem
            Action = NonVisualDataModule.LocalSortByAttrAction2
            GroupIndex = 1
            RadioItem = True
          end
        end
        object LocalColumnsSubmenuItem: TTBXSubmenuItem
          Caption = '&Columns'
          HelpKeyword = 'ui_file_panel#selecting_columns'
          object TBXItem19: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalNameColumnAction2
          end
          object TBXItem20: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalSizeColumnAction2
          end
          object TBXItem21: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalTypeColumnAction2
          end
          object TBXItem22: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalChangedColumnAction2
          end
          object TBXItem23: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalAttrColumnAction2
          end
          object TBXSeparatorItem72: TTBXSeparatorItem
          end
          object TBXItem263: TTBXItem
            Action = NonVisualDataModule.AutoSizeLocalColumnsAction
          end
          object TBXItem265: TTBXItem
            Action = NonVisualDataModule.ResetLayoutLocalColumnsAction
          end
        end
        object TBXItem221: TTBXItem
          Action = NonVisualDataModule.LocalFilterAction
        end
      end
      object TBXSubmenuItem18: TTBXSubmenuItem
        Caption = '&Mark'
        HelpKeyword = 'ui_commander_menu#mark'
        Hint = 'Mark commands'
        object TBXItem107: TTBXItem
          Action = NonVisualDataModule.SelectOneAction
        end
        object TBXItem108: TTBXItem
          Action = NonVisualDataModule.SelectAction
        end
        object TBXItem109: TTBXItem
          Action = NonVisualDataModule.UnselectAction
        end
        object TBXItem110: TTBXItem
          Action = NonVisualDataModule.SelectAllAction
        end
        object TBXSeparatorItem60: TTBXSeparatorItem
        end
        object TBXItem111: TTBXItem
          Action = NonVisualDataModule.InvertSelectionAction
        end
        object TBXItem112: TTBXItem
          Action = NonVisualDataModule.ClearSelectionAction
        end
        object TBXItem27: TTBXItem
          Action = NonVisualDataModule.RestoreSelectionAction
        end
        object TBXSeparatorItem61: TTBXSeparatorItem
        end
        object TBXItem212: TTBXItem
          Action = NonVisualDataModule.SelectSameExtAction
        end
        object TBXItem213: TTBXItem
          Action = NonVisualDataModule.UnselectSameExtAction
        end
      end
      object TBXSubmenuItem5: TTBXSubmenuItem
        Caption = '&Files'
        HelpKeyword = 'ui_commander_menu#files'
        Hint = 'File operation commands'
        object TBXSubmenuItem26: TTBXSubmenuItem
          Caption = '&New'
          HelpKeyword = 'task_index'
          Hint = 'Create object|Create new object'
          object TBXItem28: TTBXItem
            Action = NonVisualDataModule.NewFileAction
          end
          object TBXItem24: TTBXItem
            Action = NonVisualDataModule.NewDirAction
          end
          object TBXItem209: TTBXItem
            Action = NonVisualDataModule.NewLinkAction
          end
        end
        object TBXSeparatorItem6: TTBXSeparatorItem
        end
        object TBXItem25: TTBXItem
          Action = NonVisualDataModule.CurrentOpenAction
        end
        object TBXItem26: TTBXSubmenuItem
          Action = NonVisualDataModule.CurrentEditAction
          DropdownCombo = True
          OnPopup = EditMenuItemPopup
        end
        object TBXItem29: TTBXItem
          Action = NonVisualDataModule.CurrentAddEditLinkAction
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
        end
        object CurrentCopyItem: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteCopyAction
          DropdownCombo = True
          object CurrentCopyNonQueueItem: TTBXItem
            Action = NonVisualDataModule.RemoteCopyNonQueueAction
          end
          object CurrentCopyQueueItem: TTBXItem
            Action = NonVisualDataModule.RemoteCopyQueueAction
          end
          object TBXSeparatorItem51: TTBXSeparatorItem
          end
          object CurrentMoveItem: TTBXItem
            Action = NonVisualDataModule.RemoteMoveAction
          end
        end
        object CurrentCopyToItem: TTBXItem
          Action = NonVisualDataModule.RemoteCopyToAction
        end
        object CurrentMoveToItem: TTBXItem
          Action = NonVisualDataModule.RemoteMoveToAction
        end
        object TBXItem34: TTBXItem
          Action = NonVisualDataModule.CurrentDeleteAction
        end
        object TBXItem35: TTBXItem
          Action = NonVisualDataModule.CurrentRenameAction
        end
        object TBXSeparatorItem62: TTBXSeparatorItem
        end
        object TBXItem163: TTBXItem
          Action = NonVisualDataModule.CurrentCopyToClipboardAction2
        end
        object TBXItem36: TTBXItem
          Action = NonVisualDataModule.PasteAction3
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
        end
        object CustomCommandsMenu: TTBXSubmenuItem
          Action = NonVisualDataModule.CustomCommandsFileAction
        end
        object TBXSubmenuItem6: TTBXSubmenuItem
          Caption = '&File Names'
          HelpKeyword = 'filenames'
          Hint = 'Operations with name(s) of selected file(s)'
          object TBXItem37: TTBXItem
            Action = NonVisualDataModule.FileListToCommandLineAction
          end
          object TBXItem38: TTBXItem
            Action = NonVisualDataModule.FileListToClipboardAction
          end
          object TBXItem39: TTBXItem
            Action = NonVisualDataModule.FullFileListToClipboardAction
          end
          object TBXItem40: TTBXItem
            Action = NonVisualDataModule.FileGenerateUrlAction2
          end
        end
        object TBXSubmenuItem25: TTBXSubmenuItem
          Caption = '&Locking'
          Hint = 'Manage file locks'
          object TBXItem214: TTBXItem
            Action = NonVisualDataModule.LockAction
          end
          object TBXItem216: TTBXItem
            Action = NonVisualDataModule.UnlockAction
          end
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
        end
        object TBXItem41: TTBXItem
          Action = NonVisualDataModule.CurrentPropertiesAction
        end
        object TBXItem239: TTBXItem
          Action = NonVisualDataModule.CalculateDirectorySizesAction
        end
      end
      object TBXSubmenuItem7: TTBXSubmenuItem
        Caption = '&Commands'
        HelpKeyword = 'ui_commander_menu#commands'
        Hint = 'Other commands'
        object TBXItem42: TTBXItem
          Action = NonVisualDataModule.CompareDirectoriesAction2
        end
        object TBXItem43: TTBXItem
          Action = NonVisualDataModule.SynchronizeAction
        end
        object TBXItem44: TTBXItem
          Action = NonVisualDataModule.FullSynchronizeAction
        end
        object TBXItem45: TTBXItem
          Action = NonVisualDataModule.SynchronizeBrowsingAction2
        end
        object TBXItem210: TTBXItem
          Action = NonVisualDataModule.RemoteFindFilesAction2
        end
        object QueueSubmenuItem: TTBXSubmenuItem
          Caption = 'Q&ueue'
          HelpKeyword = 'ui_queue#manage'
          Hint = 'Queue list commands'
          OnPopup = QueueSubmenuItemPopup
          object QueueEnableItem2: TTBXItem
            Action = NonVisualDataModule.QueueEnableAction
          end
          object TBXItem46: TTBXItem
            Action = NonVisualDataModule.QueueGoToAction
          end
          object TBXSeparatorItem10: TTBXSeparatorItem
          end
          object TBXItem47: TTBXItem
            Action = NonVisualDataModule.QueueItemQueryAction
          end
          object TBXItem48: TTBXItem
            Action = NonVisualDataModule.QueueItemErrorAction
          end
          object TBXItem49: TTBXItem
            Action = NonVisualDataModule.QueueItemPromptAction
          end
          object TBXSeparatorItem11: TTBXSeparatorItem
          end
          object TBXItem50: TTBXItem
            Action = NonVisualDataModule.QueueItemExecuteAction
          end
          object TBXItem196: TTBXItem
            Action = NonVisualDataModule.QueueItemPauseAction
          end
          object TBXItem197: TTBXItem
            Action = NonVisualDataModule.QueueItemResumeAction
          end
          object TBXItem51: TTBXItem
            Action = NonVisualDataModule.QueueItemDeleteAction
          end
          object QueueSpeedComboBoxItem: TTBXComboBoxItem
            Action = NonVisualDataModule.QueueItemSpeedAction
          end
          object TBXSeparatorItem12: TTBXSeparatorItem
          end
          object TBXItem52: TTBXItem
            Action = NonVisualDataModule.QueueItemUpAction
          end
          object TBXItem53: TTBXItem
            Action = NonVisualDataModule.QueueItemDownAction
          end
          object TBXSeparatorItem48: TTBXSeparatorItem
          end
          object TBXSubmenuItem13: TTBXSubmenuItem
            Caption = '&All'
            HelpKeyword = 'ui_queue#manage'
            Hint = 'Mass queue management commands'
            object TBXItem198: TTBXItem
              Action = NonVisualDataModule.QueuePauseAllAction
            end
            object TBXItem199: TTBXItem
              Action = NonVisualDataModule.QueueResumeAllAction
            end
            object TBXItem142: TTBXItem
              Action = NonVisualDataModule.QueueDeleteAllAction
            end
            object TBXSeparatorItem39: TTBXSeparatorItem
            end
            object TBXItem134: TTBXItem
              Action = NonVisualDataModule.QueueDeleteAllDoneAction
            end
          end
        end
        object TBXSubmenuItem28: TTBXSubmenuItem
          Action = NonVisualDataModule.CustomCommandsNonFileAction
        end
        object TBXSeparatorItem13: TTBXSeparatorItem
        end
        object TBXItem54: TTBXItem
          Action = NonVisualDataModule.ConsoleAction
        end
        object TBXItem55: TTBXItem
          Action = NonVisualDataModule.PuttyAction
        end
        object TBXSeparatorItem14: TTBXSeparatorItem
        end
        object TBXItem57: TTBXItem
          Action = NonVisualDataModule.ClearCachesAction
        end
        object TBXSeparatorItem15: TTBXSeparatorItem
        end
        object TBXItem58: TTBXItem
          Action = NonVisualDataModule.CloseApplicationAction2
        end
      end
      object TBXSubmenuItem29: TTBXSubmenuItem
        Caption = '&Tabs'
        HelpKeyword = 'ui_commander_menu#tabs'
        Hint = 'Tab commands'
        object TBXSubmenuItem30: TTBXSubmenuItem
          Action = NonVisualDataModule.NewTabAction
          DropdownCombo = True
          object TBXItem33: TTBXItem
            Action = NonVisualDataModule.NewRemoteTabAction
          end
          object TBXItem31: TTBXItem
            Action = NonVisualDataModule.NewLocalTabAction
          end
          object TBXSeparatorItem67: TTBXSeparatorItem
          end
          object TBXItem232: TTBXItem
            Action = NonVisualDataModule.DefaultToNewRemoteTabAction
          end
        end
        object TBXItem115: TTBXItem
          Action = NonVisualDataModule.CloseTabAction
        end
        object TBXItem218: TTBXItem
          Action = NonVisualDataModule.DuplicateTabAction
        end
        object TBXItem127: TTBXItem
          Action = NonVisualDataModule.RenameTabAction
        end
        object TBXSeparatorItem53: TTBXSeparatorItem
        end
        object ColorMenuItem: TTBXColorItem
          Action = NonVisualDataModule.ColorMenuAction2
          Color = clNone
        end
        object TBXSeparatorItem69: TTBXSeparatorItem
        end
        object TBXItem252: TTBXItem
          Action = NonVisualDataModule.DisconnectSessionAction
        end
        object TBXItem253: TTBXItem
          Action = NonVisualDataModule.ReconnectSessionAction
        end
        object TBXItem114: TTBXItem
          Action = NonVisualDataModule.SaveCurrentSessionAction2
        end
        object TBXSeparatorItem50: TTBXSeparatorItem
        end
        object TBXItem56: TTBXItem
          Action = NonVisualDataModule.FileSystemInfoAction
        end
        object TBXItem135: TTBXItem
          Action = NonVisualDataModule.SessionGenerateUrlAction2
        end
        object TBXItem227: TTBXItem
          Action = NonVisualDataModule.ChangePasswordAction
        end
        object TBXItem76: TTBXItem
          Action = NonVisualDataModule.PrivateKeyUploadAction
        end
        object TBXSeparatorItem29: TTBXSeparatorItem
        end
        object TBXSubmenuItem21: TTBXSubmenuItem
          Action = NonVisualDataModule.OpenedTabsAction
        end
        object TBXSubmenuItem231: TTBXSubmenuItem
          Action = NonVisualDataModule.WorkspacesAction
        end
        object TBXItem230: TTBXItem
          Action = NonVisualDataModule.SaveWorkspaceAction
        end
        object TBXSeparatorItem23: TTBXSeparatorItem
        end
        object TBXSubmenuItem20: TTBXSubmenuItem
          Action = NonVisualDataModule.SavedSessionsAction2
        end
      end
      object TBXSubmenuItem9: TTBXSubmenuItem
        Caption = '&Options'
        HelpKeyword = 'ui_commander_menu#options'
        Hint = 'Change program layout/preferences'
        object TBXSubmenuItem10: TTBXSubmenuItem
          Caption = '&Toolbars'
          HelpKeyword = 'ui_toolbars'
          Hint = 'Show/hide toolbars'
          object TBXItem64: TTBXItem
            Action = NonVisualDataModule.CommanderCommandsBandAction
          end
          object TBXItem60: TTBXItem
            Action = NonVisualDataModule.CommanderSessionBandAction2
          end
          object TBXItem62: TTBXItem
            Action = NonVisualDataModule.CommanderPreferencesBandAction
          end
          object TBXItem63: TTBXItem
            Action = NonVisualDataModule.CommanderSortBandAction
          end
          object TBXItem186: TTBXItem
            Action = NonVisualDataModule.CommanderUpdatesBandAction
          end
          object TBXItem188: TTBXItem
            Action = NonVisualDataModule.CommanderTransferBandAction
          end
          object TBXItem215: TTBXItem
            Action = NonVisualDataModule.CommanderCustomCommandsBandAction
          end
          object TBXSeparatorItem38: TTBXSeparatorItem
          end
          object TBXItem74: TTBXItem
            Action = NonVisualDataModule.ToolBar2Action
          end
          object TBXSeparatorItem47: TTBXSeparatorItem
          end
          object TBXItem191: TTBXItem
            Action = NonVisualDataModule.LockToolbarsAction
          end
          object TBXItem133: TTBXItem
            Action = NonVisualDataModule.SelectiveToolbarTextAction
          end
          object TBXItem272: TTBXSubmenuItem
            Action = NonVisualDataModule.ToolbarIconSizeAction
            object TBXItem273: TTBXItem
              Action = NonVisualDataModule.ToolbarIconSizeNormalAction
              RadioItem = True
            end
            object TBXItem274: TTBXItem
              Action = NonVisualDataModule.ToolbarIconSizeLargeAction
              RadioItem = True
            end
            object TBXItem275: TTBXItem
              Action = NonVisualDataModule.ToolbarIconSizeVeryLargeAction
              RadioItem = True
            end
          end
        end
        object TBXSubmenuItem11: TTBXSubmenuItem
          Action = NonVisualDataModule.CommanderLocalPanelAction
          object TBXItem65: TTBXItem
            Action = NonVisualDataModule.CommanderLocalHistoryBandAction2
          end
          object TBXItem66: TTBXItem
            Action = NonVisualDataModule.CommanderLocalNavigationBandAction2
          end
          object TBXItem59: TTBXItem
            Action = NonVisualDataModule.CommanderLocalFileBandAction2
          end
          object TBXItem61: TTBXItem
            Action = NonVisualDataModule.CommanderLocalSelectionBandAction2
          end
          object TBXSeparatorItem16: TTBXSeparatorItem
          end
          object TBXItem67: TTBXItem
            Action = NonVisualDataModule.LocalTreeAction
          end
          object TBXSeparatorItem17: TTBXSeparatorItem
          end
          object TBXItem68: TTBXItem
            Action = NonVisualDataModule.LocalStatusBarAction2
          end
        end
        object TBXSubmenuItem12: TTBXSubmenuItem
          Action = NonVisualDataModule.CommanderRemotePanelAction
          object TBXItem69: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteHistoryBandAction2
          end
          object TBXItem70: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteNavigationBandAction2
          end
          object TBXItem136: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteFileBandAction2
          end
          object TBXItem131: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteSelectionBandAction2
          end
          object TBXSeparatorItem18: TTBXSeparatorItem
          end
          object TBXItem71: TTBXItem
            Action = NonVisualDataModule.RemoteTreeAction
          end
          object TBXSeparatorItem19: TTBXSeparatorItem
          end
          object TBXItem72: TTBXItem
            Action = NonVisualDataModule.RemoteStatusBarAction2
          end
        end
        object TBXSeparatorItem20: TTBXSeparatorItem
        end
        object SessionsTabs3: TTBXItem
          Action = NonVisualDataModule.SessionsTabsAction2
        end
        object TBXItem73: TTBXItem
          Action = NonVisualDataModule.CommandLinePanelAction
        end
        object TBXItem75: TTBXItem
          Action = NonVisualDataModule.StatusBarAction
        end
        object TBXSubmenuItem14: TTBXSubmenuItem
          Caption = '&Queue'
          HelpKeyword = 'ui_queue'
          Hint = 'Configure queue list'
          object TBXItem77: TTBXItem
            Action = NonVisualDataModule.QueueShowAction
            RadioItem = True
          end
          object TBXItem78: TTBXItem
            Action = NonVisualDataModule.QueueHideWhenEmptyAction
            RadioItem = True
          end
          object TBXItem79: TTBXItem
            Action = NonVisualDataModule.QueueHideAction
            RadioItem = True
          end
          object TBXSeparatorItem21: TTBXSeparatorItem
          end
          object TBXItem80: TTBXItem
            Action = NonVisualDataModule.QueueToolbarAction
          end
          object TBXItem255: TTBXItem
            Action = NonVisualDataModule.QueueFileListAction
          end
          object TBXSeparatorItem74: TTBXSeparatorItem
          end
          object TBXItem267: TTBXItem
            Action = NonVisualDataModule.QueueResetLayoutColumnsAction
          end
          object TBXSeparatorItem22: TTBXSeparatorItem
          end
          object TBXSubmenuItem8: TTBXSubmenuItem
            Action = NonVisualDataModule.QueueCycleOnceEmptyAction
            DropdownCombo = True
            object TBXItem222: TTBXItem
              Action = NonVisualDataModule.QueueIdleOnceEmptyAction
              RadioItem = True
            end
            object TBXItem223: TTBXItem
              Action = NonVisualDataModule.QueueDisconnectOnceEmptyAction2
              RadioItem = True
            end
            object TBXItem141: TTBXItem
              Action = NonVisualDataModule.QueueSuspendOnceEmptyAction2
            end
            object TBXItem224: TTBXItem
              Action = NonVisualDataModule.QueueShutDownOnceEmptyAction2
              RadioItem = True
            end
          end
          object TBXItem81: TTBXItem
            Action = NonVisualDataModule.QueuePreferencesAction
          end
        end
        object TBXSeparatorItem49: TTBXSeparatorItem
        end
        object TBXItem82: TTBXItem
          Action = NonVisualDataModule.PreferencesAction
        end
      end
      object RemoteMenuButton: TTBXSubmenuItem
        Caption = '&RemoteX'
        HelpKeyword = 'ui_commander_menu#remote'
        object TBXItem83: TTBXItem
          Action = NonVisualDataModule.RemoteChangePathAction2
        end
        object TBXSeparatorItem24: TTBXSeparatorItem
        end
        object TBXSubmenuItem15: TTBXSubmenuItem
          Caption = '&Go To'
          HelpKeyword = 'task_navigate'
          Hint = 'Go to directory'
          object TBXItem84: TTBXItem
            Action = NonVisualDataModule.RemoteOpenDirAction
          end
          object TBXItem257: TTBXItem
            Action = NonVisualDataModule.RemoteExploreDirectoryAction
          end
          object TBXSeparatorItem25: TTBXSeparatorItem
          end
          object TBXItem85: TTBXItem
            Action = NonVisualDataModule.RemoteParentDirAction
          end
          object TBXItem86: TTBXItem
            Action = NonVisualDataModule.RemoteRootDirAction
          end
          object TBXItem87: TTBXItem
            Action = NonVisualDataModule.RemoteHomeDirAction
          end
          object TBXItem261: TTBXItem
            Action = NonVisualDataModule.RemoteOtherDirAction
          end
          object TBXSeparatorItem26: TTBXSeparatorItem
          end
          object TBXItem88: TTBXItem
            Action = NonVisualDataModule.RemoteBackAction
          end
          object TBXItem89: TTBXItem
            Action = NonVisualDataModule.RemoteForwardAction
          end
        end
        object TBXItem90: TTBXItem
          Action = NonVisualDataModule.RemoteRefreshAction
        end
        object TBXItem91: TTBXItem
          Action = NonVisualDataModule.RemoteAddBookmarkAction2
        end
        object TBXItem92: TTBXItem
          Action = NonVisualDataModule.RemotePathToClipboardAction2
        end
        object TBXSeparatorItem27: TTBXSeparatorItem
        end
        object TBXSubmenuItem19: TTBXSubmenuItem
          Caption = '&View'
          HelpKeyword = 'ui_file_panel#view_style'
          Hint = 'Change directory view style'
          object TBXItem276: TTBXItem
            Action = NonVisualDataModule.RemoteReportAction
          end
          object TBXSeparatorItem77: TTBXSeparatorItem
          end
          object TBXItem277: TTBXItem
            Action = NonVisualDataModule.RemoteThumbnailAction
          end
        end
        object TBXSubmenuItem16: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in remote panel'
          object TBXItem93: TTBXItem
            Action = NonVisualDataModule.RemoteSortAscendingAction2
          end
          object TBXSeparatorItem28: TTBXSeparatorItem
          end
          object TBXItem94: TTBXItem
            Action = NonVisualDataModule.RemoteSortByNameAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem95: TTBXItem
            Action = NonVisualDataModule.RemoteSortByExtAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem97: TTBXItem
            Action = NonVisualDataModule.RemoteSortBySizeAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem193: TTBXItem
            Action = NonVisualDataModule.RemoteSortByTypeAction2
            RadioItem = True
          end
          object TBXItem96: TTBXItem
            Action = NonVisualDataModule.RemoteSortByChangedAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem98: TTBXItem
            Action = NonVisualDataModule.RemoteSortByRightsAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem99: TTBXItem
            Action = NonVisualDataModule.RemoteSortByOwnerAction2
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem100: TTBXItem
            Action = NonVisualDataModule.RemoteSortByGroupAction2
            GroupIndex = 1
            RadioItem = True
          end
        end
        object RemoteColumnsSubmenuItem: TTBXSubmenuItem
          Caption = '&Columns'
          HelpKeyword = 'ui_file_panel#selecting_columns'
          object TBXItem101: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteNameColumnAction2
          end
          object TBXItem102: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteSizeColumnAction2
          end
          object TBXItem192: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteTypeColumnAction2
          end
          object TBXItem103: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteChangedColumnAction2
          end
          object TBXItem104: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteRightsColumnAction2
          end
          object TBXItem105: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteOwnerColumnAction2
          end
          object TBXItem106: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteGroupColumnAction2
          end
          object TBXItem179: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteLinkTargetColumnAction2
          end
          object TBXSeparatorItem73: TTBXSeparatorItem
          end
          object TBXItem264: TTBXItem
            Action = NonVisualDataModule.AutoSizeRemoteColumnsAction
          end
          object TBXItem266: TTBXItem
            Action = NonVisualDataModule.ResetLayoutRemoteColumnsAction
          end
        end
        object TBXItem220: TTBXItem
          Action = NonVisualDataModule.RemoteFilterAction
        end
      end
      object TBXSubmenuItem22: TTBXSubmenuItem
        Caption = '&Help'
        HelpKeyword = 'ui_commander_menu#help'
        Hint = 'Help'
        object TBXItem116: TTBXItem
          Action = NonVisualDataModule.TableOfContentsAction
        end
        object TBXItem217: TTBXItem
          Action = NonVisualDataModule.TipsAction
        end
        object TBXSeparatorItem30: TTBXSeparatorItem
        end
        object TBXItem117: TTBXItem
          Action = NonVisualDataModule.HomepageAction
        end
        object TBXItem118: TTBXItem
          Action = NonVisualDataModule.ForumPageAction
        end
        object TBXItem119: TTBXItem
          Action = NonVisualDataModule.HistoryPageAction
        end
        object TBXSeparatorItem31: TTBXSeparatorItem
        end
        object TBXItem120: TTBXItem
          Action = NonVisualDataModule.CheckForUpdatesAction
        end
        object TBXSeparatorItem32: TTBXSeparatorItem
        end
        object TBXItem121: TTBXItem
          Action = NonVisualDataModule.DonatePageAction
        end
        object TBXSeparatorItem33: TTBXSeparatorItem
        end
        object TBXItem122: TTBXItem
          Action = NonVisualDataModule.AboutAction
        end
      end
    end
    object PreferencesToolbar: TTBXToolbar
      Left = 0
      Top = 51
      Caption = 'Preferences'
      DockPos = 0
      DockRow = 2
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object TBXItem126: TTBXItem
        Action = NonVisualDataModule.PreferencesAction
      end
      object TBXSeparatorItem36: TTBXSeparatorItem
      end
      object TBXSubmenuItem24: TTBXSubmenuItem
        Action = NonVisualDataModule.QueueToggleShowAction
        DisplayMode = nbdmImageAndText
        DropdownCombo = True
        object TBXItem128: TTBXItem
          Action = NonVisualDataModule.QueueShowAction
          RadioItem = True
        end
        object TBXItem129: TTBXItem
          Action = NonVisualDataModule.QueueHideWhenEmptyAction
          RadioItem = True
        end
        object TBXItem130: TTBXItem
          Action = NonVisualDataModule.QueueHideAction
          RadioItem = True
        end
        object TBXSeparatorItem65: TTBXSeparatorItem
        end
        object TBXItem256: TTBXItem
          Action = NonVisualDataModule.QueueFileListAction
        end
      end
    end
    object SessionToolbar2: TTBXToolbar
      Left = 0
      Top = 25
      Caption = 'Sessions and Tabs'
      DockPos = 0
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object TBXSubmenuItem31: TTBXSubmenuItem
        Action = NonVisualDataModule.NewTabAction
        DisplayMode = nbdmImageAndText
        DropdownCombo = True
        object TBXItem231: TTBXItem
          Action = NonVisualDataModule.NewRemoteTabAction
        end
        object TBXItem123: TTBXItem
          Action = NonVisualDataModule.NewLocalTabAction
        end
        object TBXSeparatorItem68: TTBXSeparatorItem
        end
        object TBXItem238: TTBXItem
          Action = NonVisualDataModule.DefaultToNewRemoteTabAction
        end
      end
      object TBXItem125: TTBXItem
        Action = NonVisualDataModule.SaveCurrentSessionAction2
      end
      object TBXSeparatorItem66: TTBXSeparatorItem
      end
      object TBXItem219: TTBXItem
        Action = NonVisualDataModule.DuplicateTabAction
      end
      object TBXItem124: TTBXItem
        Action = NonVisualDataModule.CloseTabAction
      end
      object TBXSeparatorItem34: TTBXSeparatorItem
      end
      object TBXSubmenuItem23: TTBXSubmenuItem
        Action = NonVisualDataModule.SavedSessionsAction2
        DisplayMode = nbdmImageAndText
        Options = [tboDropdownArrow]
      end
    end
    object SortToolbar: TTBXToolbar
      Left = 0
      Top = 77
      Caption = 'Sort'
      DockPos = -8
      DockRow = 5
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      object TBXItem145: TTBXItem
        Action = NonVisualDataModule.CurrentSortAscendingAction
      end
      object TBXSeparatorItem40: TTBXSeparatorItem
      end
      object TBXItem146: TTBXItem
        Action = NonVisualDataModule.CurrentSortByNameAction
      end
      object TBXItem147: TTBXItem
        Action = NonVisualDataModule.CurrentSortByExtAction
      end
      object TBXItem150: TTBXItem
        Action = NonVisualDataModule.CurrentSortBySizeAction
      end
      object TBXItem148: TTBXItem
        Action = NonVisualDataModule.CurrentSortByTypeAction2
      end
      object TBXItem149: TTBXItem
        Action = NonVisualDataModule.CurrentSortByChangedAction
      end
      object TBXItem151: TTBXItem
        Action = NonVisualDataModule.CurrentSortByRightsAction
      end
      object TBXItem152: TTBXItem
        Action = NonVisualDataModule.CurrentSortByOwnerAction
      end
      object TBXItem153: TTBXItem
        Action = NonVisualDataModule.CurrentSortByGroupAction
      end
    end
    object CommandsToolbar: TTBXToolbar
      Left = 0
      Top = 103
      Caption = 'Commands'
      DockPos = 0
      DockRow = 6
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      object TBXItem154: TTBXItem
        Action = NonVisualDataModule.CompareDirectoriesAction2
      end
      object TBXItem155: TTBXItem
        Action = NonVisualDataModule.SynchronizeAction
      end
      object TBXItem156: TTBXItem
        Action = NonVisualDataModule.FullSynchronizeAction
        DisplayMode = nbdmImageAndText
      end
      object TBXSeparatorItem41: TTBXSeparatorItem
      end
      object TBXItem157: TTBXItem
        Action = NonVisualDataModule.ConsoleAction
      end
      object TBXItem190: TTBXItem
        Action = NonVisualDataModule.PuttyAction
      end
      object TBXSeparatorItem42: TTBXSeparatorItem
      end
      object TBXItem158: TTBXItem
        Action = NonVisualDataModule.SynchronizeBrowsingAction2
      end
    end
    object UpdatesToolbar: TTBXToolbar
      Left = 0
      Top = 129
      Caption = 'Updates'
      DockPos = -7
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      object TBXSubmenuItem1: TTBXSubmenuItem
        Action = NonVisualDataModule.CheckForUpdatesAction
        DropdownCombo = True
        object TBXItem184: TTBXItem
          Action = NonVisualDataModule.CheckForUpdatesAction
          Options = [tboDefault]
        end
        object TBXSeparatorItem46: TTBXSeparatorItem
        end
        object TBXItem180: TTBXItem
          Action = NonVisualDataModule.HomepageAction
        end
        object TBXItem187: TTBXItem
          Action = NonVisualDataModule.DownloadPageAction
        end
        object TBXItem181: TTBXItem
          Action = NonVisualDataModule.ForumPageAction
        end
        object TBXItem182: TTBXItem
          Action = NonVisualDataModule.HistoryPageAction
        end
        object TBXItem185: TTBXItem
          Action = NonVisualDataModule.DonatePageAction
        end
        object TBXSeparatorItem45: TTBXSeparatorItem
        end
        object TBXItem183: TTBXItem
          Action = NonVisualDataModule.UpdatesPreferencesAction
        end
      end
    end
    object TransferToolbar: TTBXToolbar
      Left = 45
      Top = 129
      Caption = 'Transfer Settings'
      DockPos = 44
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      object TransferSettingsLabelItem: TTBXLabelItem
        Caption = 'Transfer Settings'
        Margin = 2
      end
      object TransferDropDown: TTBXDropDownItem
        EditWidth = 150
        Hint = 'Select transfer settings preset'
        DropDownList = True
        object TransferList: TTBXStringList
          MaxVisibleItems = 15
          MinWidth = 350
        end
        object TransferLabel: TTBXLabelItem
          Caption = ''
          Margin = 4
          ShowAccelChar = False
        end
        object TBXSeparatorItem52: TTBXSeparatorItem
        end
        object TBXItem189: TTBXItem
          Action = NonVisualDataModule.PresetsPreferencesAction
          DisplayMode = nbdmImageAndText
        end
      end
    end
    object CustomCommandsToolbar: TTBXToolbar
      Left = 300
      Top = 129
      Caption = 'Custom Commands'
      ChevronMenu = True
      ChevronPriorityForNewItems = tbcpLowest
      DockPos = 168
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Visible = False
    end
  end
  inherited RemotePanel: TPanel
    Left = 440
    Top = 186
    Width = 458
    Height = 289
    Constraints.MinHeight = 220
    Constraints.MinWidth = 185
    ParentColor = True
    TabOrder = 1
    object RemotePathLabel: TPathLabel [0]
      Left = 0
      Top = 79
      Width = 458
      Height = 21
      UnixPath = True
      IndentVertical = 3
      AutoSizeVertical = True
      HotTrack = True
      OnGetStatus = RemotePathLabelGetStatus
      OnPathClick = RemotePathLabelPathClick
      OnMaskClick = RemotePathLabelMaskClick
      AutoSize = False
      Transparent = False
      OnDblClick = PathLabelDblClick
    end
    inherited RemotePanelSplitter: TSplitter
      Left = 0
      Top = 145
      Width = 458
      Height = 3
      Cursor = crSizeNS
      Hint = 
        'Drag to resize directory tree. Double click to make size of dire' +
        'ctory trees equal.'
      Align = alTop
    end
    inherited RemoteStatusBar: TTBXStatusBar
      Top = 270
      Width = 458
      Panels = <
        item
          Framed = False
          Size = 170
          StretchPriority = 1
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taRightJustify
          Framed = False
          Hint = 'Click to show hidden files'
          MaxSize = 120
          Size = 80
          StretchPriority = 2
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taRightJustify
          Framed = False
          Hint = 'Click to modify or clear the filter'
          MaxSize = 120
          Size = 80
          StretchPriority = 2
          Tag = 0
          TextTruncation = twEndEllipsis
        end>
      OnPanelClick = RemoteStatusBarPanelClick
    end
    inherited RemoteDirPanel: TPanel
      Left = 0
      Top = 148
      Width = 458
      Height = 113
      Constraints.MinHeight = 70
      inherited RemoteDirView: TUnixDirView
        Width = 280
        Height = 113
        NortonLike = nlOn
        OnUpdateStatusBar = RemoteDirViewUpdateStatusBar
        PathLabel = RemotePathLabel
        AddParentDir = True
        OnDDFileOperationExecuted = RemoteFileControlDDFileOperationExecuted
        OnHistoryGo = DirViewHistoryGo
        OnPathChange = RemoteDirViewPathChange
      end
      inherited ReconnectToolbar: TTBXToolbar
        TabOrder = 2
      end
      object OtherLocalDirView: TDirView
        Left = 280
        Top = 0
        Width = 178
        Height = 113
        Align = alRight
        Constraints.MinHeight = 70
        DoubleBuffered = True
        FullDrag = True
        HideSelection = False
        IconOptions.AutoArrange = True
        ParentDoubleBuffered = False
        PopupMenu = NonVisualDataModule.RemoteDirViewPopup
        TabOrder = 1
        OnColumnRightClick = DirViewColumnRightClick
        OnEditing = DirViewEditing
        OnEnter = OtherLocalDirViewEnter
        OnExit = DirViewExit
        OnKeyDown = DirViewKeyDown
        OnKeyPress = DirViewKeyPress
        DirColProperties.ExtVisible = False
        OnUpdateStatusBar = OtherLocalDirViewUpdateStatusBar
        AddParentDir = True
        OnSelectItem = DirViewSelectItem
        OnLoaded = DirViewLoaded
        OnDDDragEnter = LocalFileControlDDDragEnter
        OnDDDragLeave = FileControlDDDragLeave
        OnDDTargetHasDropHandler = LocalDirViewDDTargetHasDropHandler
        OnDDFileOperation = LocalFileControlDDFileOperation
        OnExecFile = LocalDirViewExecFile
        OnMatchMask = DirViewMatchMask
        OnGetOverlay = DirViewGetOverlay
        ConfirmDelete = False
        UseIconUpdateThread = True
        WatchForChanges = True
        OnFileIconForName = LocalDirViewFileIconForName
        OnContextPopup = OtherLocalDirViewContextPopup
        OnHistoryChange = DirViewHistoryChange
        OnHistoryGo = DirViewHistoryGo
        OnPathChange = OtherLocalDirViewPathChange
        OnBusy = DirViewBusy
        OnChangeFocus = DirViewChangeFocus
        DirViewStyle = dvsReport
      end
    end
    inherited RemoteDrivePanel: TPanel
      Top = 100
      Width = 458
      Height = 45
      Align = alTop
      Constraints.MinHeight = 30
      object OtherLocalDriveView: TDriveView [0]
        Left = 280
        Top = 0
        Width = 178
        Height = 45
        WatchDirectory = True
        DirView = OtherLocalDirView
        OnRefreshDrives = LocalDriveViewRefreshDrives
        OnBusy = DirViewBusy
        OnDDDragEnter = LocalFileControlDDDragEnter
        OnDDDragLeave = FileControlDDDragLeave
        OnDDFileOperation = LocalFileControlDDFileOperation
        Align = alRight
        Constraints.MinHeight = 30
        DoubleBuffered = True
        HideSelection = False
        Indent = 19
        ParentColor = False
        ParentDoubleBuffered = False
        TabOrder = 1
        TabStop = False
        OnEnter = OtherLocalDriveViewEnter
        OnNeedHiddenDirectories = LocalDriveViewNeedHiddenDirectories
      end
      inherited RemoteDriveView: TUnixDriveView
        Width = 280
        Height = 45
        TabStop = False
      end
    end
    object RemoteTopDock: TTBXDock
      Left = 0
      Top = 0
      Width = 458
      Height = 79
      FixAlign = True
      OnContextPopup = DockContextPopup
      object RemoteHistoryToolbar: TTBXToolbar
        Left = 0
        Top = 27
        Caption = 'Remote History'
        DockPos = -6
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object RemoteBackButton: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteBackAction
          DropdownCombo = True
        end
        object RemoteForwardButton: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteForwardAction
          DropdownCombo = True
        end
      end
      object RemoteNavigationToolbar: TTBXToolbar
        Left = 80
        Top = 27
        Caption = 'Remote Navigation'
        DockPos = 72
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object TBXItem165: TTBXItem
          Action = NonVisualDataModule.RemoteParentDirAction
        end
        object TBXItem166: TTBXItem
          Action = NonVisualDataModule.RemoteRootDirAction
        end
        object TBXItem167: TTBXItem
          Action = NonVisualDataModule.RemoteHomeDirAction
        end
        object TBXItem168: TTBXItem
          Action = NonVisualDataModule.RemoteRefreshAction
        end
        object TBXSeparatorItem37: TTBXSeparatorItem
        end
        object TBXItem132: TTBXItem
          Action = NonVisualDataModule.RemoteFindFilesAction2
          DisplayMode = nbdmImageAndText
        end
        object TBXSeparatorItem44: TTBXSeparatorItem
        end
        object TBXItem170: TTBXItem
          Action = NonVisualDataModule.RemoteTreeAction
        end
        object TBXSubmenuItem32: TTBXSubmenuItem
          Caption = 'Change directory view style'
          ImageIndex = 11
          Options = [tboDropdownArrow]
          object TBXItem278: TTBXItem
            Action = NonVisualDataModule.RemoteReportAction
          end
          object TBXSeparatorItem78: TTBXSeparatorItem
          end
          object TBXItem279: TTBXItem
            Action = NonVisualDataModule.RemoteThumbnailAction
          end
        end
      end
      object RemotePathToolbar: TTBXToolbar
        Left = 0
        Top = 0
        Caption = 'Remote Path'
        DockableTo = [dpTop, dpBottom]
        DockMode = dmCannotFloat
        DockPos = 0
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        Stretch = True
        TabOrder = 0
        OnResize = ToolBarResize
        OnGetBaseSize = ToolbarGetBaseSize
        object RemotePathComboBox: TTBXComboBoxItem
          EditWidth = 200
          ShowImage = True
          DropDownList = True
          MaxVisibleItems = 20
          ShowListImages = True
          OnAdjustImageIndex = RemotePathComboBoxAdjustImageIndex
          OnDrawItem = RemotePathComboBoxDrawItem
          OnItemClick = RemotePathComboBoxItemClick
          OnMeasureWidth = RemotePathComboBoxMeasureWidth
          OnCancel = RemotePathComboBoxCancel
        end
        object RemoteOpenDirButton: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteOpenDirAction
          DropdownCombo = True
          OnPopup = RemoteOpenDirButtonPopup
        end
        object TBXItem229: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteFilterAction
          DropdownCombo = True
          object TBXItem169: TTBXItem
            Action = NonVisualDataModule.RemoteFilterAction
            Options = [tboDefault]
          end
          object TBXSeparatorItem63: TTBXSeparatorItem
          end
          object TBXItem237: TTBXItem
            Action = NonVisualDataModule.FileColorsPreferencesAction
          end
        end
      end
      object RemoteFileToolbar: TTBXToolbar
        Left = 1
        Top = 53
        Caption = 'Remote Files'
        DockPos = 1
        DockRow = 2
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        object RemoteCopyItem: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteCopyAction
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          object TBXItem143: TTBXItem
            Action = NonVisualDataModule.RemoteCopyNonQueueAction
          end
          object TBXItem200: TTBXItem
            Action = NonVisualDataModule.RemoteCopyQueueAction
          end
          object TBXSeparatorItem59: TTBXSeparatorItem
          end
          object RemoteMoveItem: TTBXItem
            Action = NonVisualDataModule.RemoteMoveAction
          end
        end
        object TBXSeparatorItem55: TTBXSeparatorItem
        end
        object TBXItem242: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteEditAction2
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          OnPopup = EditMenuItemPopup
        end
        object TBXItem241: TTBXItem
          Action = NonVisualDataModule.RemoteDeleteAction2
        end
        object TBXItem240: TTBXItem
          Action = NonVisualDataModule.RemoteRenameAction2
        end
        object TBXItem243: TTBXSubmenuItem
          Action = NonVisualDataModule.RemotePropertiesAction2
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          object TBXItem259: TTBXItem
            Action = NonVisualDataModule.RemotePropertiesAction2
            Options = [tboDefault]
          end
          object TBXSeparatorItem71: TTBXSeparatorItem
          end
          object TBXItem260: TTBXItem
            Action = NonVisualDataModule.RemoteCalculateDirectorySizesAction
          end
        end
        object TBXSeparatorItem56: TTBXSeparatorItem
        end
        object RemoteNewSubmenuItem: TTBXSubmenuItem
          Caption = '&New'
          DisplayMode = nbdmImageAndText
          Hint = 'Create object|Create new object'
          ImageIndex = 5
          Options = [tboDropdownArrow]
          object TBXItem247: TTBXItem
            Action = NonVisualDataModule.RemoteNewFileAction
          end
          object TBXItem244: TTBXItem
            Action = NonVisualDataModule.RemoteCreateDirAction3
          end
          object TBXItem246: TTBXItem
            Action = NonVisualDataModule.RemoteAddEditLinkAction3
          end
        end
      end
      object RemoteSelectionToolbar: TTBXToolbar
        Left = 328
        Top = 27
        Caption = 'Remote Selection'
        DockPos = 282
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        object TBXItem138: TTBXItem
          Action = NonVisualDataModule.RemoteSelectAction2
        end
        object TBXItem139: TTBXItem
          Action = NonVisualDataModule.RemoteUnselectAction2
        end
        object TBXItem140: TTBXItem
          Action = NonVisualDataModule.RemoteSelectAllAction2
        end
      end
    end
    object RemoteBottomDock: TTBXDock
      Left = 0
      Top = 261
      Width = 458
      Height = 9
      FixAlign = True
      Position = dpBottom
    end
  end
  inherited QueuePanel: TPanel
    Top = 532
    Width = 898
    Height = 116
    ParentColor = True
    TabOrder = 2
    inherited QueueLabel: TPathLabel
      Width = 898
    end
    inherited QueueFileListSplitter: TSplitter
      Top = 93
      Width = 898
    end
    inherited QueueView3: TListView
      Width = 898
      Height = 46
      TabStop = False
    end
    inherited QueueDock: TTBXDock
      Width = 898
    end
    inherited QueueFileList: TListView
      Top = 96
      Width = 898
    end
  end
  inherited SessionsPageControl: TThemePageControl
    Top = 165
    Width = 898
  end
  object LocalPanel: TPanel [6]
    Left = 0
    Top = 186
    Width = 435
    Height = 289
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinHeight = 220
    Constraints.MinWidth = 185
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object LocalPathLabel: TPathLabel
      Left = 0
      Top = 79
      Width = 435
      Height = 21
      IndentVertical = 3
      AutoSizeVertical = True
      HotTrack = True
      OnGetStatus = LocalPathLabelGetStatus
      OnPathClick = LocalPathLabelPathClick
      OnMaskClick = LocalPathLabelMaskClick
      AutoSize = False
      PopupMenu = NonVisualDataModule.LocalPanelPopup
      Transparent = False
      OnDblClick = PathLabelDblClick
    end
    object LocalPanelSplitter: TSplitter
      Left = 0
      Top = 145
      Width = 435
      Height = 3
      Cursor = crSizeNS
      Hint = 
        'Drag to resize directory tree. Double click to make size of dire' +
        'ctory trees equal.'
      Align = alTop
      AutoSnap = False
      MinSize = 70
      ResizeStyle = rsUpdate
    end
    object LocalStatusBar: TTBXStatusBar
      Left = 0
      Top = 270
      Width = 435
      Height = 19
      Panels = <
        item
          Framed = False
          Size = 170
          StretchPriority = 1
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taRightJustify
          Framed = False
          Hint = 'Click to show hidden files'
          MaxSize = 120
          Size = 80
          StretchPriority = 2
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taRightJustify
          Framed = False
          Hint = 'Click to modify or clear the filter'
          MaxSize = 120
          Size = 80
          StretchPriority = 2
          Tag = 0
          TextTruncation = twEndEllipsis
        end>
      ParentShowHint = False
      ShowHint = True
      UseSystemFont = False
      OnClick = LocalStatusBarClick
      OnPanelClick = LocalStatusBarPanelClick
    end
    object LocalDirView: TDirView
      Left = 0
      Top = 148
      Width = 435
      Height = 113
      Align = alClient
      Constraints.MinHeight = 70
      DoubleBuffered = True
      FullDrag = True
      HideSelection = False
      IconOptions.AutoArrange = True
      ParentDoubleBuffered = False
      PopupMenu = NonVisualDataModule.LocalDirViewPopup
      TabOrder = 1
      OnColumnRightClick = DirViewColumnRightClick
      OnEditing = DirViewEditing
      OnEnter = LocalDirViewEnter
      OnExit = DirViewExit
      OnKeyDown = DirViewKeyDown
      OnKeyPress = DirViewKeyPress
      DirColProperties.ExtVisible = False
      PathLabel = LocalPathLabel
      OnUpdateStatusBar = LocalDirViewUpdateStatusBar
      AddParentDir = True
      OnSelectItem = DirViewSelectItem
      OnLoaded = DirViewLoaded
      OnDDDragEnter = LocalFileControlDDDragEnter
      OnDDDragLeave = FileControlDDDragLeave
      OnDDTargetHasDropHandler = LocalDirViewDDTargetHasDropHandler
      OnDDFileOperation = LocalFileControlDDFileOperation
      OnExecFile = LocalDirViewExecFile
      OnMatchMask = DirViewMatchMask
      OnGetOverlay = DirViewGetOverlay
      ConfirmDelete = False
      UseIconUpdateThread = True
      WatchForChanges = True
      OnFileIconForName = LocalDirViewFileIconForName
      OnContextPopup = LocalDirViewContextPopup
      OnHistoryChange = DirViewHistoryChange
      OnHistoryGo = DirViewHistoryGo
      OnPathChange = LocalDirViewPathChange
      OnBusy = DirViewBusy
      OnChangeFocus = DirViewChangeFocus
      DirViewStyle = dvsReport
    end
    object LocalTopDock: TTBXDock
      Left = 0
      Top = 0
      Width = 435
      Height = 79
      FixAlign = True
      OnContextPopup = DockContextPopup
      object LocalHistoryToolbar: TTBXToolbar
        Left = 0
        Top = 27
        Caption = 'Local History'
        DockPos = -6
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object LocalBackButton: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalBackAction
          DropdownCombo = True
        end
        object LocalForwardButton: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalForwardAction
          DropdownCombo = True
        end
      end
      object LocalNavigationToolbar: TTBXToolbar
        Left = 80
        Top = 27
        Caption = 'Local Navigation'
        DockPos = 68
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object TBXItem159: TTBXItem
          Action = NonVisualDataModule.LocalParentDirAction
        end
        object TBXItem160: TTBXItem
          Action = NonVisualDataModule.LocalRootDirAction
        end
        object TBXItem161: TTBXItem
          Action = NonVisualDataModule.LocalHomeDirAction
        end
        object TBXItem162: TTBXItem
          Action = NonVisualDataModule.LocalRefreshAction
        end
        object TBXSeparatorItem43: TTBXSeparatorItem
        end
        object TBXItem164: TTBXItem
          Action = NonVisualDataModule.LocalTreeAction
        end
        object TBXSubmenuItem4: TTBXSubmenuItem
          Caption = 'Change directory view style'
          ImageIndex = 11
          Options = [tboDropdownArrow]
          object TBXItem269: TTBXItem
            Action = NonVisualDataModule.LocalReportAction
          end
          object TBXSeparatorItem75: TTBXSeparatorItem
          end
          object TBXItem268: TTBXItem
            Action = NonVisualDataModule.LocalThumbnailAction
          end
        end
      end
      object LocalPathToolbar: TTBXToolbar
        Left = 0
        Top = 0
        Caption = 'Local Path'
        DockableTo = [dpTop, dpBottom]
        DockMode = dmCannotFloat
        DockPos = 0
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        Stretch = True
        TabOrder = 0
        OnResize = ToolBarResize
        OnGetBaseSize = ToolbarGetBaseSize
        object LocalPathComboBox: TTBXComboBoxItem
          EditWidth = 200
          ShowImage = True
          DropDownList = True
          MaxVisibleItems = 30
          ShowListImages = True
          OnAdjustImageIndex = LocalPathComboBoxAdjustImageIndex
          OnItemClick = LocalPathComboBoxItemClick
          OnCancel = LocalPathComboBoxCancel
        end
        object LocalOpenDirButton: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalOpenDirAction
          DropdownCombo = True
          OnPopup = LocalOpenDirButtonPopup
        end
        object TBXItem228: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalFilterAction
          DropdownCombo = True
          object TBXItem245: TTBXItem
            Action = NonVisualDataModule.LocalFilterAction
          end
          object TBXSeparatorItem64: TTBXSeparatorItem
          end
          object TBXItem251: TTBXItem
            Action = NonVisualDataModule.FileColorsPreferencesAction
          end
        end
      end
      object LocalFileToolbar: TTBXToolbar
        Left = 0
        Top = 53
        Caption = 'Local Files'
        DockPos = 0
        DockRow = 2
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        object LocalCopyItem: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalCopyAction
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          object TBXItem144: TTBXItem
            Action = NonVisualDataModule.LocalCopyNonQueueAction
          end
          object TBXItem174: TTBXItem
            Action = NonVisualDataModule.LocalCopyQueueAction
          end
          object TBXSeparatorItem58: TTBXSeparatorItem
          end
          object LocalMoveItem: TTBXItem
            Action = NonVisualDataModule.LocalMoveAction
          end
        end
        object TBXSeparatorItem54: TTBXSeparatorItem
        end
        object TBXItem235: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalEditAction2
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          OnPopup = EditMenuItemPopup
        end
        object TBXItem234: TTBXItem
          Action = NonVisualDataModule.LocalDeleteAction2
        end
        object TBXItem233: TTBXItem
          Action = NonVisualDataModule.LocalRenameAction2
        end
        object TBXItem236: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalPropertiesAction2
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          object TBXItem258: TTBXItem
            Action = NonVisualDataModule.LocalPropertiesAction2
            Options = [tboDefault]
          end
          object TBXSeparatorItem70: TTBXSeparatorItem
          end
          object TBXItem113: TTBXItem
            Action = NonVisualDataModule.LocalCalculateDirectorySizesAction
          end
        end
        object TBXSeparatorItem35: TTBXSeparatorItem
        end
        object LocalNewSubmenuItem: TTBXSubmenuItem
          Caption = '&New'
          DisplayMode = nbdmImageAndText
          Hint = 'Create object|Create new object'
          ImageIndex = 5
          Options = [tboDropdownArrow]
          object TBXItem248: TTBXItem
            Action = NonVisualDataModule.LocalNewFileAction
          end
          object TBXItem249: TTBXItem
            Action = NonVisualDataModule.LocalCreateDirAction3
          end
          object TBXItem250: TTBXItem
            Action = NonVisualDataModule.LocalAddEditLinkAction3
          end
        end
      end
      object LocalSelectionToolbar: TTBXToolbar
        Left = 244
        Top = 27
        Caption = 'Local Selection'
        DockPos = 217
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        object TBXItem137: TTBXItem
          Action = NonVisualDataModule.LocalSelectAction2
        end
        object TBXItem32: TTBXItem
          Action = NonVisualDataModule.LocalUnselectAction2
        end
        object TBXItem30: TTBXItem
          Action = NonVisualDataModule.LocalSelectAllAction2
        end
      end
    end
    object LocalDriveView: TDriveView
      Left = 0
      Top = 100
      Width = 435
      Height = 45
      WatchDirectory = True
      DirView = LocalDirView
      OnRefreshDrives = LocalDriveViewRefreshDrives
      OnBusy = DirViewBusy
      OnDDDragEnter = LocalFileControlDDDragEnter
      OnDDDragLeave = FileControlDDDragLeave
      OnDDFileOperation = LocalFileControlDDFileOperation
      Align = alTop
      Constraints.MinHeight = 30
      DoubleBuffered = True
      HideSelection = False
      Indent = 19
      ParentColor = False
      ParentDoubleBuffered = False
      TabOrder = 2
      TabStop = False
      OnEnter = LocalDriveViewEnter
      OnNeedHiddenDirectories = LocalDriveViewNeedHiddenDirectories
    end
    object LocalBottomDock: TTBXDock
      Left = 0
      Top = 261
      Width = 435
      Height = 9
      FixAlign = True
      Position = dpBottom
    end
  end
  object BottomDock: TTBXDock [7]
    Left = 0
    Top = 475
    Width = 898
    Height = 53
    FixAlign = True
    Position = dpBottom
    OnContextPopup = DockContextPopup
    object Toolbar2Toolbar: TTBXToolbar
      Left = 0
      Top = 27
      Caption = 'Hot Keys'
      DockPos = 0
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = False
      Stretch = True
      TabOrder = 0
      Visible = False
      object TBXItem171: TTBXItem
        Action = NonVisualDataModule.CurrentRenameAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object TBXItem172: TTBXItem
        Action = NonVisualDataModule.CurrentEditAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object CurrentCopyToolbar2Item: TTBXItem
        Action = NonVisualDataModule.RemoteCopyAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object CurrentMoveToolbar2Item: TTBXItem
        Action = NonVisualDataModule.RemoteMoveAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object TBXItem175: TTBXItem
        Action = NonVisualDataModule.CurrentCreateDirAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object TBXItem176: TTBXItem
        Action = NonVisualDataModule.CurrentDeleteAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object TBXItem177: TTBXItem
        Action = NonVisualDataModule.CurrentPropertiesAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object TBXItem178: TTBXItem
        Action = NonVisualDataModule.CloseApplicationAction2
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
    end
    object CommandLineToolbar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'CommandLineToolbar'
      DockMode = dmCannotFloat
      Stretch = True
      TabOrder = 1
      Visible = False
      OnResize = ToolBarResize
      OnGetBaseSize = ToolbarGetBaseSize
      object CommandLinePromptLabel: TTBXLabelItem
        Caption = 'CommandX >'
        Margin = 2
      end
      object CommandLineCombo: TTBXComboBoxItem
        OnBeginEdit = CommandLineComboBeginEdit
        ExtendedAccept = True
        OnPopup = CommandLineComboPopup
      end
    end
  end
  object StatusBar: TTBXStatusBar [8]
    Left = 0
    Top = 648
    Width = 898
    Images = GlyphsModule.SessionImages
    Panels = <
      item
        Size = 100
        StretchPriority = 1
        Tag = 0
        TextTruncation = twEndEllipsis
      end
      item
        Alignment = taCenter
        MaxSize = 70
        ViewPriority = 98
        Size = 70
        Tag = 0
        TextTruncation = twEndEllipsis
      end
      item
        Alignment = taCenter
        ImageIndex = 1
        MaxSize = 35
        Size = 35
        Tag = 0
      end
      item
        Alignment = taCenter
        MaxSize = 80
        ViewPriority = 99
        Size = 80
        Tag = 0
        TextTruncation = twEndEllipsis
      end>
    ParentShowHint = False
    PopupMenu = NonVisualDataModule.CommanderBarPopup
    ShowHint = True
    UseSystemFont = False
    OnPanelDblClick = StatusBarPanelDblClick
  end
  object QueueSeparatorPanel: TPanel [9]
    Left = 0
    Top = 528
    Width = 898
    Height = 1
    Align = alBottom
    BevelEdges = [beBottom]
    BevelKind = bkFlat
    TabOrder = 7
  end
  inherited MessageDock: TTBXDock
    Top = 156
    Width = 898
  end
  inherited ApplicationEvents: TApplicationEvents
    Left = 72
    Top = 352
  end
end
