inherited ScpCommanderForm: TScpCommanderForm
  Left = 162
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'ui_commander'
  Caption = 'ScpCommanderForm'
  ClientHeight = 670
  ClientWidth = 898
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter [0]
    Left = 435
    Top = 177
    Width = 5
    Height = 298
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
        Caption = '&Local'
        HelpKeyword = 'ui_commander_menu#local'
        Hint = 'Change local panel layout or change displayed directory/drive'
        object TBXItem1: TTBXItem
          Action = NonVisualDataModule.LocalChangePathAction
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
          Action = NonVisualDataModule.LocalAddBookmarkAction
        end
        object TBXItem11: TTBXItem
          Action = NonVisualDataModule.LocalPathToClipboardAction
        end
        object TBXSeparatorItem4: TTBXSeparatorItem
        end
        object TBXSubmenuItem3: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in local panel'
          object TBXItem12: TTBXItem
            Action = NonVisualDataModule.LocalSortAscendingAction
          end
          object TBXSeparatorItem5: TTBXSeparatorItem
          end
          object TBXItem13: TTBXItem
            Action = NonVisualDataModule.LocalSortByNameAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem14: TTBXItem
            Action = NonVisualDataModule.LocalSortByExtAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem15: TTBXItem
            Action = NonVisualDataModule.LocalSortByTypeAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem16: TTBXItem
            Action = NonVisualDataModule.LocalSortByChangedAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem17: TTBXItem
            Action = NonVisualDataModule.LocalSortBySizeAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem18: TTBXItem
            Action = NonVisualDataModule.LocalSortByAttrAction
            GroupIndex = 1
            RadioItem = True
          end
        end
        object TBXSubmenuItem4: TTBXSubmenuItem
          Caption = 'S&how Columns'
          HelpKeyword = 'ui_file_panel#selecting_columns'
          Hint = 'Select columns to show in panel'
          object TBXItem19: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalNameColumnAction
          end
          object TBXItem20: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalSizeColumnAction
          end
          object TBXItem21: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalTypeColumnAction
          end
          object TBXItem22: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalChangedColumnAction
          end
          object TBXItem23: TTBXItem
            Action = NonVisualDataModule.ShowHideLocalAttrColumnAction
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
        object TBXItem31: TTBXItem
          Action = NonVisualDataModule.RemoteCopyToAction
        end
        object TBXItem33: TTBXItem
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
          Action = NonVisualDataModule.CurrentCopyToClipboardAction
        end
        object TBXItem36: TTBXItem
          Action = NonVisualDataModule.PasteAction2
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
      end
      object TBXSubmenuItem7: TTBXSubmenuItem
        Caption = '&Commands'
        HelpKeyword = 'ui_commander_menu#commands'
        Hint = 'Other commands'
        object TBXItem42: TTBXItem
          Action = NonVisualDataModule.CompareDirectoriesAction
        end
        object TBXItem43: TTBXItem
          Action = NonVisualDataModule.SynchronizeAction
        end
        object TBXItem44: TTBXItem
          Action = NonVisualDataModule.FullSynchronizeAction
        end
        object TBXItem45: TTBXItem
          Action = NonVisualDataModule.SynchronizeBrowsingAction
        end
        object TBXItem210: TTBXItem
          Action = NonVisualDataModule.RemoteFindFilesAction
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
          Action = NonVisualDataModule.CloseApplicationAction
        end
      end
      object TBXSubmenuItem19: TTBXSubmenuItem
        Caption = '&Session'
        HelpKeyword = 'ui_commander_menu#session'
        Hint = 'Session commands'
        object TBXItem113: TTBXItem
          Action = NonVisualDataModule.NewSessionAction
        end
        object TBXItem115: TTBXItem
          Action = NonVisualDataModule.CloseSessionAction2
        end
        object TBXItem252: TTBXItem
          Action = NonVisualDataModule.DisconnectSessionAction
        end
        object TBXItem253: TTBXItem
          Action = NonVisualDataModule.ReconnectSessionAction
        end
        object TBXItem218: TTBXItem
          Action = NonVisualDataModule.DuplicateSessionAction
        end
        object TBXItem127: TTBXItem
          Action = NonVisualDataModule.RenameSessionAction
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
        object TBXSeparatorItem23: TTBXSeparatorItem
        end
        object ColorMenuItem: TTBXColorItem
          Action = NonVisualDataModule.ColorMenuAction
          Color = clNone
        end
        object TBXSeparatorItem29: TTBXSeparatorItem
        end
        object TBXSubmenuItem21: TTBXSubmenuItem
          Action = NonVisualDataModule.OpenedSessionsAction
        end
        object TBXSubmenuItem231: TTBXSubmenuItem
          Action = NonVisualDataModule.WorkspacesAction
        end
        object TBXItem230: TTBXItem
          Action = NonVisualDataModule.SaveWorkspaceAction
        end
        object TBXSeparatorItem53: TTBXSeparatorItem
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
            Action = NonVisualDataModule.CommanderSessionBandAction
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
        end
        object TBXSubmenuItem11: TTBXSubmenuItem
          Caption = '&Local Panel'
          HelpKeyword = 'ui_file_panel'
          Hint = 'Change local panel layout'
          object TBXItem65: TTBXItem
            Action = NonVisualDataModule.CommanderLocalHistoryBandAction
          end
          object TBXItem66: TTBXItem
            Action = NonVisualDataModule.CommanderLocalNavigationBandAction
          end
          object TBXItem59: TTBXItem
            Action = NonVisualDataModule.CommanderLocalFileBandAction
          end
          object TBXItem61: TTBXItem
            Action = NonVisualDataModule.CommanderLocalSelectionBandAction
          end
          object TBXSeparatorItem16: TTBXSeparatorItem
          end
          object TBXItem67: TTBXItem
            Action = NonVisualDataModule.LocalTreeAction
          end
          object TBXSeparatorItem17: TTBXSeparatorItem
          end
          object TBXItem68: TTBXItem
            Action = NonVisualDataModule.LocalStatusBarAction
          end
        end
        object TBXSubmenuItem12: TTBXSubmenuItem
          Caption = '&Remote Panel'
          HelpKeyword = 'ui_file_panel'
          Hint = 'Change remote panel layout'
          object TBXItem69: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteHistoryBandAction
          end
          object TBXItem70: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteNavigationBandAction
          end
          object TBXItem136: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteFileBandAction
          end
          object TBXItem131: TTBXItem
            Action = NonVisualDataModule.CommanderRemoteSelectionBandAction
          end
          object TBXSeparatorItem18: TTBXSeparatorItem
          end
          object TBXItem71: TTBXItem
            Action = NonVisualDataModule.RemoteTreeAction
          end
          object TBXSeparatorItem19: TTBXSeparatorItem
          end
          object TBXItem72: TTBXItem
            Action = NonVisualDataModule.RemoteStatusBarAction
          end
        end
        object TBXSeparatorItem20: TTBXSeparatorItem
        end
        object SessionsTabsAction3: TTBXItem
          Action = NonVisualDataModule.SessionsTabsAction
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
        Caption = '&Remote'
        HelpKeyword = 'ui_commander_menu#remote'
        Hint = 'Change remote panel layout or change displayed directory'
        object TBXItem83: TTBXItem
          Action = NonVisualDataModule.RemoteChangePathAction
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
          Action = NonVisualDataModule.RemoteAddBookmarkAction
        end
        object TBXItem92: TTBXItem
          Action = NonVisualDataModule.RemotePathToClipboardAction
        end
        object TBXSeparatorItem27: TTBXSeparatorItem
        end
        object TBXSubmenuItem16: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in remote panel'
          object TBXItem93: TTBXItem
            Action = NonVisualDataModule.RemoteSortAscendingAction
          end
          object TBXSeparatorItem28: TTBXSeparatorItem
          end
          object TBXItem94: TTBXItem
            Action = NonVisualDataModule.RemoteSortByNameAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem95: TTBXItem
            Action = NonVisualDataModule.RemoteSortByExtAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem193: TTBXItem
            Action = NonVisualDataModule.RemoteSortByTypeAction
            RadioItem = True
          end
          object TBXItem96: TTBXItem
            Action = NonVisualDataModule.RemoteSortByChangedAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem97: TTBXItem
            Action = NonVisualDataModule.RemoteSortBySizeAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem98: TTBXItem
            Action = NonVisualDataModule.RemoteSortByRightsAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem99: TTBXItem
            Action = NonVisualDataModule.RemoteSortByOwnerAction
            GroupIndex = 1
            RadioItem = True
          end
          object TBXItem100: TTBXItem
            Action = NonVisualDataModule.RemoteSortByGroupAction
            GroupIndex = 1
            RadioItem = True
          end
        end
        object TBXSubmenuItem17: TTBXSubmenuItem
          Caption = 'S&how Columns'
          HelpKeyword = 'ui_file_panel#selecting_columns'
          Hint = 'Select columns to show in panel'
          object TBXItem101: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteNameColumnAction
          end
          object TBXItem102: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteSizeColumnAction
          end
          object TBXItem192: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteTypeColumnAction
          end
          object TBXItem103: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteChangedColumnAction
          end
          object TBXItem104: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteRightsColumnAction
          end
          object TBXItem105: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteOwnerColumnAction
          end
          object TBXItem106: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteGroupColumnAction
          end
          object TBXItem179: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteLinkTargetColumnAction
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
    object SessionToolbar: TTBXToolbar
      Left = 0
      Top = 25
      Caption = 'Session'
      DockPos = 0
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object TBXItem123: TTBXItem
        Action = NonVisualDataModule.NewSessionAction
        DisplayMode = nbdmImageAndText
      end
      object TBXItem219: TTBXItem
        Action = NonVisualDataModule.DuplicateSessionAction
      end
      object TBXItem124: TTBXItem
        Action = NonVisualDataModule.CloseSessionAction2
      end
      object TBXItem125: TTBXItem
        Action = NonVisualDataModule.SaveCurrentSessionAction2
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
      object TBXItem148: TTBXItem
        Action = NonVisualDataModule.CurrentSortByTypeAction
      end
      object TBXItem149: TTBXItem
        Action = NonVisualDataModule.CurrentSortByChangedAction
      end
      object TBXItem150: TTBXItem
        Action = NonVisualDataModule.CurrentSortBySizeAction
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
        Action = NonVisualDataModule.CompareDirectoriesAction
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
        Action = NonVisualDataModule.SynchronizeBrowsingAction
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
      Left = 299
      Top = 129
      Caption = 'Custom Commands'
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
    Top = 177
    Width = 458
    Height = 298
    Constraints.MinHeight = 220
    Constraints.MinWidth = 185
    ParentColor = True
    TabOrder = 1
    object RemotePathLabel: TPathLabel [0]
      Left = 0
      Top = 79
      Width = 458
      Height = 19
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
      Top = 143
      Width = 458
      Height = 3
      Cursor = crSizeNS
      Hint = 
        'Drag to resize directory tree. Double click to make size of dire' +
        'ctory trees equal.'
      Align = alTop
    end
    inherited RemoteStatusBar: TTBXStatusBar
      Top = 279
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
      Top = 146
      Width = 458
      Height = 124
      Constraints.MinHeight = 70
      inherited RemoteDirView: TUnixDirView
        Width = 458
        Height = 124
        NortonLike = nlOn
        OnUpdateStatusBar = RemoteDirViewUpdateStatusBar
        PathLabel = RemotePathLabel
        AddParentDir = True
        OnDDFileOperationExecuted = RemoteFileControlDDFileOperationExecuted
        OnHistoryGo = DirViewHistoryGo
        OnPathChange = RemoteDirViewPathChange
      end
    end
    inherited RemoteDrivePanel: TPanel
      Top = 98
      Width = 458
      Height = 45
      Align = alTop
      Constraints.MinHeight = 30
      inherited RemoteDriveView: TUnixDriveView
        Width = 458
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
          Action = NonVisualDataModule.RemoteFindFilesAction
          DisplayMode = nbdmImageAndText
        end
        object TBXSeparatorItem44: TTBXSeparatorItem
        end
        object TBXItem170: TTBXItem
          Action = NonVisualDataModule.RemoteTreeAction
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
        object TBXItem238: TTBXSubmenuItem
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
          object TBXItem239: TTBXItem
            Action = NonVisualDataModule.RemoteMoveAction
          end
        end
        object TBXSeparatorItem55: TTBXSeparatorItem
        end
        object TBXItem242: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteEditAction
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          OnPopup = EditMenuItemPopup
        end
        object TBXItem241: TTBXItem
          Action = NonVisualDataModule.RemoteDeleteAction
        end
        object TBXItem240: TTBXItem
          Action = NonVisualDataModule.RemoteRenameAction
        end
        object TBXItem243: TTBXItem
          Action = NonVisualDataModule.RemotePropertiesAction
          DisplayMode = nbdmImageAndText
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
            Action = NonVisualDataModule.RemoteCreateDirAction2
          end
          object TBXItem246: TTBXItem
            Action = NonVisualDataModule.RemoteAddEditLinkAction2
          end
        end
      end
      object RemoteSelectionToolbar: TTBXToolbar
        Left = 370
        Top = 53
        Caption = 'Remote Selection'
        DockPos = 347
        DockRow = 2
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        object TBXItem138: TTBXItem
          Action = NonVisualDataModule.RemoteSelectAction
        end
        object TBXItem139: TTBXItem
          Action = NonVisualDataModule.RemoteUnselectAction
        end
        object TBXItem140: TTBXItem
          Action = NonVisualDataModule.RemoteSelectAllAction
        end
      end
    end
    object RemoteBottomDock: TTBXDock
      Left = 0
      Top = 270
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
      Height = 48
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
    Top = 156
    Width = 898
  end
  object LocalPanel: TPanel [6]
    Left = 0
    Top = 177
    Width = 435
    Height = 298
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
      Height = 19
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
      Top = 143
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
      Top = 279
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
      Top = 146
      Width = 435
      Height = 124
      Align = alClient
      Constraints.MinHeight = 70
      DoubleBuffered = True
      FullDrag = True
      HideSelection = False
      ParentDoubleBuffered = False
      PopupMenu = NonVisualDataModule.LocalDirViewPopup
      TabOrder = 1
      ViewStyle = vsReport
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
        object TBXItem231: TTBXSubmenuItem
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
          object TBXItem232: TTBXItem
            Action = NonVisualDataModule.LocalMoveAction
          end
        end
        object TBXSeparatorItem54: TTBXSeparatorItem
        end
        object TBXItem235: TTBXSubmenuItem
          Action = NonVisualDataModule.LocalEditAction
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          OnPopup = EditMenuItemPopup
        end
        object TBXItem234: TTBXItem
          Action = NonVisualDataModule.LocalDeleteAction
        end
        object TBXItem233: TTBXItem
          Action = NonVisualDataModule.LocalRenameAction
        end
        object TBXItem236: TTBXItem
          Action = NonVisualDataModule.LocalPropertiesAction
          DisplayMode = nbdmImageAndText
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
            Action = NonVisualDataModule.LocalCreateDirAction2
          end
          object TBXItem250: TTBXItem
            Action = NonVisualDataModule.LocalAddEditLinkAction2
          end
        end
      end
      object LocalSelectionToolbar: TTBXToolbar
        Left = 353
        Top = 53
        Caption = 'Local Selection'
        DockPos = 329
        DockRow = 2
        Images = GlyphsModule.ExplorerImages
        Options = [tboShowHint]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        object TBXItem137: TTBXItem
          Action = NonVisualDataModule.LocalSelectAction
        end
        object TBXItem32: TTBXItem
          Action = NonVisualDataModule.LocalUnselectAction
        end
        object TBXItem30: TTBXItem
          Action = NonVisualDataModule.LocalSelectAllAction
        end
      end
    end
    object LocalDriveView: TDriveView
      Left = 0
      Top = 98
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
    end
    object LocalBottomDock: TTBXDock
      Left = 0
      Top = 270
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
        Action = NonVisualDataModule.CloseApplicationAction
        DisplayMode = nbdmImageAndText
        ImageIndex = 61
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
        ImageIndex = 1
        MaxSize = 35
        Size = 35
        Tag = 0
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
        ImageIndex = 0
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
  inherited ApplicationEvents: TApplicationEvents
    Left = 72
    Top = 352
  end
end
