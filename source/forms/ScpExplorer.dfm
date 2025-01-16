inherited ScpExplorerForm: TScpExplorerForm
  Left = 239
  Top = 148
  HelpType = htKeyword
  HelpKeyword = 'ui_explorer'
  ActiveControl = RemoteDirView
  Caption = 'ScpExplorerForm'
  ClientHeight = 475
  ClientWidth = 632
  TextHeight = 15
  inherited QueueSplitter: TSplitter
    Top = 332
    Width = 632
  end
  inherited TopDock: TTBXDock
    Width = 632
    Height = 209
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
      TabOrder = 0
      object TBXSubmenuItem5: TTBXSubmenuItem
        Caption = '&Files'
        HelpKeyword = 'ui_explorer_menu#files'
        Hint = 'File operations'
        object TBXSubmenuItem26: TTBXSubmenuItem
          Caption = '&New'
          HelpKeyword = 'task_index'
          Hint = 'Create object|Create new object'
          object TBXItem135: TTBXItem
            Action = NonVisualDataModule.NewFileAction
          end
          object TBXItem136: TTBXItem
            Action = NonVisualDataModule.NewDirAction
          end
          object TBXItem209: TTBXItem
            Action = NonVisualDataModule.NewLinkAction
          end
        end
        object TBXSeparatorItem20: TTBXSeparatorItem
        end
        object TBXItem25: TTBXItem
          Action = NonVisualDataModule.CurrentOpenAction
        end
        object TBXItem26: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteEditAction2
          DropdownCombo = True
          OnPopup = EditMenuItemPopup
        end
        object TBXItem4: TTBXItem
          Action = NonVisualDataModule.CurrentAddEditLinkAction
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
        end
        object TBXItem34: TTBXItem
          Action = NonVisualDataModule.RemoteDeleteAction2
        end
        object TBXItem35: TTBXItem
          Action = NonVisualDataModule.RemoteRenameAction2
        end
        object TBXItem41: TTBXItem
          Action = NonVisualDataModule.RemotePropertiesAction2
        end
        object TBXItem61: TTBXItem
          Action = NonVisualDataModule.CalculateDirectorySizesAction
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
        end
        object TBXItem30: TTBXSubmenuItem
          Action = NonVisualDataModule.RemoteCopyAction
          DropdownCombo = True
          object TBXItem156: TTBXItem
            Action = NonVisualDataModule.RemoteCopyNonQueueAction
          end
          object TBXItem158: TTBXItem
            Action = NonVisualDataModule.RemoteCopyQueueAction
          end
          object TBXSeparatorItem39: TTBXSeparatorItem
          end
          object TBXItem32: TTBXItem
            Action = NonVisualDataModule.RemoteMoveAction
          end
        end
        object TBXItem31: TTBXItem
          Action = NonVisualDataModule.RemoteCopyToAction
        end
        object TBXItem33: TTBXItem
          Action = NonVisualDataModule.RemoteMoveToAction
        end
        object TBXSeparatorItem42: TTBXSeparatorItem
        end
        object TBXItem62: TTBXItem
          Action = NonVisualDataModule.CurrentCopyToClipboardAction2
        end
        object TBXItem36: TTBXItem
          Action = NonVisualDataModule.PasteAction3
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
        end
        object CustomCommandsMenu: TTBXSubmenuItem
          Action = NonVisualDataModule.CustomCommandsFileAction
        end
        object TBXSubmenuItem6: TTBXSubmenuItem
          Caption = '&File Names'
          HelpKeyword = 'filenames'
          Hint = 'Operations with name(s) of selected file(s)'
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
        object TBXSeparatorItem1: TTBXSeparatorItem
        end
        object TBXItem1: TTBXItem
          Action = NonVisualDataModule.CloseTabAction
        end
        object TBXItem2: TTBXItem
          Action = NonVisualDataModule.CloseApplicationAction2
        end
      end
      object TBXSubmenuItem7: TTBXSubmenuItem
        Caption = '&Commands'
        HelpKeyword = 'ui_explorer_menu#commands'
        Hint = 'Other commands'
        object TBXItem43: TTBXItem
          Action = NonVisualDataModule.SynchronizeAction
        end
        object TBXItem44: TTBXItem
          Action = NonVisualDataModule.FullSynchronizeAction
        end
        object TBXItem3: TTBXItem
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
            object TBXItem154: TTBXItem
              Action = NonVisualDataModule.QueueDeleteAllAction
            end
            object TBXSeparatorItem35: TTBXSeparatorItem
            end
            object TBXItem143: TTBXItem
              Action = NonVisualDataModule.QueueDeleteAllDoneAction
            end
          end
        end
        object TBXSubmenuItem28: TTBXSubmenuItem
          Action = NonVisualDataModule.CustomCommandsNonFileAction
        end
        object TBXSeparatorItem13: TTBXSeparatorItem
        end
        object TBXItem5: TTBXItem
          Action = NonVisualDataModule.RemoteAddBookmarkAction2
        end
        object TBXItem6: TTBXItem
          Action = NonVisualDataModule.RemotePathToClipboardAction2
        end
        object TBXSeparatorItem2: TTBXSeparatorItem
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
      end
      object TBXSubmenuItem18: TTBXSubmenuItem
        Caption = '&Mark'
        HelpKeyword = 'ui_explorer_menu#mark'
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
        object TBXSeparatorItem41: TTBXSeparatorItem
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
      object TBXSubmenuItem29: TTBXSubmenuItem
        Caption = '&Tabs'
        HelpKeyword = 'ui_explorer_menu#tabs'
        Hint = 'Tab commands'
        object TBXItem113: TTBXItem
          Action = NonVisualDataModule.NewTabAction
        end
        object TBXItem166: TTBXItem
          Action = NonVisualDataModule.CloseTabAction
        end
        object TBXItem218: TTBXItem
          Action = NonVisualDataModule.DuplicateTabAction
        end
        object TBXItem167: TTBXItem
          Action = NonVisualDataModule.RenameTabAction
        end
        object TBXSeparatorItem47: TTBXSeparatorItem
        end
        object ColorMenuItem: TTBXColorItem
          Action = NonVisualDataModule.ColorMenuAction2
          Color = clNone
        end
        object TBXSeparatorItem49: TTBXSeparatorItem
        end
        object TBXItem162: TTBXItem
          Action = NonVisualDataModule.DisconnectSessionAction
        end
        object TBXItem163: TTBXItem
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
        object TBXItem144: TTBXItem
          Action = NonVisualDataModule.SessionGenerateUrlAction2
        end
        object TBXItem160: TTBXItem
          Action = NonVisualDataModule.ChangePasswordAction
        end
        object TBXItem14: TTBXItem
          Action = NonVisualDataModule.PrivateKeyUploadAction
        end
        object TBXSeparatorItem37: TTBXSeparatorItem
        end
        object TBXSubmenuItem9: TTBXSubmenuItem
          Action = NonVisualDataModule.OpenedTabsAction
        end
        object TBXSubmenuItem10: TTBXSubmenuItem
          Action = NonVisualDataModule.WorkspacesAction
        end
        object TBXItem168: TTBXItem
          Action = NonVisualDataModule.SaveWorkspaceAction
        end
        object TBXSeparatorItem53: TTBXSeparatorItem
        end
        object TBXSubmenuItem20: TTBXSubmenuItem
          Action = NonVisualDataModule.SavedSessionsAction2
        end
      end
      object TBXSubmenuItem1: TTBXSubmenuItem
        Caption = '&View'
        HelpKeyword = 'ui_explorer_menu#view'
        Hint = 'Change program layout'
        object TBXSubmenuItem2: TTBXSubmenuItem
          Caption = '&Toolbars'
          HelpKeyword = 'ui_toolbars'
          Hint = 'Show/hide toolbars'
          object TBXItem7: TTBXItem
            Action = NonVisualDataModule.ExplorerAddressBandAction
          end
          object TBXItem8: TTBXItem
            Action = NonVisualDataModule.ExplorerToolbarBandAction
          end
          object TBXItem9: TTBXItem
            Action = NonVisualDataModule.ExplorerSelectionBandAction
          end
          object TBXItem10: TTBXItem
            Action = NonVisualDataModule.ExplorerSessionBandAction2
          end
          object TBXItem11: TTBXItem
            Action = NonVisualDataModule.ExplorerPreferencesBandAction
          end
          object TBXItem12: TTBXItem
            Action = NonVisualDataModule.ExplorerSortBandAction
          end
          object TBXItem82: TTBXItem
            Action = NonVisualDataModule.ExplorerUpdatesBandAction
          end
          object TBXItem83: TTBXItem
            Action = NonVisualDataModule.ExplorerTransferBandAction
          end
          object TBXItem28: TTBXItem
            Action = NonVisualDataModule.ExplorerCustomCommandsBandAction
          end
          object TBXSeparatorItem19: TTBXSeparatorItem
          end
          object TBXItem92: TTBXItem
            Action = NonVisualDataModule.LockToolbarsAction
          end
          object TBXItem140: TTBXItem
            Action = NonVisualDataModule.SelectiveToolbarTextAction
          end
          object TBXItem169: TTBXSubmenuItem
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
        object SessionsTabs3: TTBXItem
          Action = NonVisualDataModule.SessionsTabsAction2
        end
        object TBXItem13: TTBXItem
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
          object TBXItem164: TTBXItem
            Action = NonVisualDataModule.QueueFileListAction
          end
          object TBXSeparatorItem51: TTBXSeparatorItem
          end
          object TBXItem115: TTBXItem
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
            object TBXItem148: TTBXItem
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
        object TBXItem15: TTBXItem
          Action = NonVisualDataModule.RemoteTreeAction
        end
        object TBXSeparatorItem3: TTBXSeparatorItem
        end
        object TBXItem16: TTBXItem
          Action = NonVisualDataModule.RemoteIconAction
        end
        object TBXItem17: TTBXItem
          Action = NonVisualDataModule.RemoteSmallIconAction
        end
        object TBXItem18: TTBXItem
          Action = NonVisualDataModule.RemoteListAction
        end
        object TBXItem19: TTBXItem
          Action = NonVisualDataModule.RemoteReportAction
        end
        object TBXItem170: TTBXItem
          Action = NonVisualDataModule.RemoteThumbnailAction
        end
        object TBXSeparatorItem4: TTBXSeparatorItem
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
        object TBXItem20: TTBXItem
          Action = NonVisualDataModule.RemoteRefreshAction
        end
        object TBXSubmenuItem16: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in panel'
          object TBXItem93: TTBXItem
            Action = NonVisualDataModule.RemoteSortAscendingAction2
          end
          object TBXSeparatorItem28: TTBXSeparatorItem
          end
          object TBXItem94: TTBXItem
            Action = NonVisualDataModule.RemoteSortByNameAction2
            GroupIndex = 1
          end
          object TBXItem95: TTBXItem
            Action = NonVisualDataModule.RemoteSortByExtAction2
            GroupIndex = 1
          end
          object TBXItem97: TTBXItem
            Action = NonVisualDataModule.RemoteSortBySizeAction2
            GroupIndex = 1
          end
          object TBXItem132: TTBXItem
            Action = NonVisualDataModule.RemoteSortByTypeAction2
            RadioItem = True
          end
          object TBXItem96: TTBXItem
            Action = NonVisualDataModule.RemoteSortByChangedAction2
            GroupIndex = 1
          end
          object TBXItem98: TTBXItem
            Action = NonVisualDataModule.RemoteSortByRightsAction2
            GroupIndex = 1
          end
          object TBXItem99: TTBXItem
            Action = NonVisualDataModule.RemoteSortByOwnerAction2
            GroupIndex = 1
          end
          object TBXItem100: TTBXItem
            Action = NonVisualDataModule.RemoteSortByGroupAction2
            GroupIndex = 1
          end
        end
        object ColumndsSubmenuItem: TTBXSubmenuItem
          Caption = '&Columns'
          HelpKeyword = 'ui_file_panel#selecting_columns'
          object TBXItem101: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteNameColumnAction2
          end
          object TBXItem102: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteSizeColumnAction2
          end
          object TBXItem131: TTBXItem
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
          object TBXItem76: TTBXItem
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
        object TBXItem138: TTBXItem
          Action = NonVisualDataModule.RemoteFilterAction
        end
        object TBXSeparatorItem5: TTBXSeparatorItem
        end
        object TBXItem21: TTBXItem
          Action = NonVisualDataModule.PreferencesAction
        end
      end
      object TBXSubmenuItem22: TTBXSubmenuItem
        Caption = '&Help'
        HelpKeyword = 'ui_explorer_menu#help'
        Hint = 'Help'
        object TBXItem116: TTBXItem
          Action = NonVisualDataModule.TableOfContentsAction
        end
        object TBXItem159: TTBXItem
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
    object ButtonsToolbar: TTBXToolbar
      Left = 0
      Top = 52
      Caption = 'Commands'
      DockPos = -8
      DockRow = 2
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      object BackButton: TTBXSubmenuItem
        Action = NonVisualDataModule.RemoteBackAction
        DropdownCombo = True
      end
      object ForwardButton: TTBXSubmenuItem
        Action = NonVisualDataModule.RemoteForwardAction
        DropdownCombo = True
      end
      object TBXSeparatorItem6: TTBXSeparatorItem
      end
      object TBXItem23: TTBXItem
        Action = NonVisualDataModule.RemoteParentDirAction
      end
      object TBXItem24: TTBXItem
        Action = NonVisualDataModule.RemoteRootDirAction
      end
      object TBXItem29: TTBXItem
        Action = NonVisualDataModule.RemoteHomeDirAction
      end
      object TBXItem37: TTBXItem
        Action = NonVisualDataModule.RemoteRefreshAction
      end
      object TBXSeparatorItem24: TTBXSeparatorItem
      end
      object TBXItem139: TTBXItem
        Action = NonVisualDataModule.RemoteFindFilesAction2
        DisplayMode = nbdmImageAndText
      end
      object TBXSeparatorItem15: TTBXSeparatorItem
      end
      object TBXItem141: TTBXSubmenuItem
        Action = NonVisualDataModule.RemoteCopyAction
        DisplayMode = nbdmImageAndText
        DropdownCombo = True
        object TBXItem155: TTBXItem
          Action = NonVisualDataModule.RemoteCopyNonQueueAction
        end
        object TBXItem157: TTBXItem
          Action = NonVisualDataModule.RemoteCopyQueueAction
        end
        object TBXSeparatorItem38: TTBXSeparatorItem
        end
        object TBXItem142: TTBXItem
          Action = NonVisualDataModule.RemoteMoveAction
        end
      end
      object TBXSeparatorItem27: TTBXSeparatorItem
      end
      object TBXItem42: TTBXSubmenuItem
        Action = NonVisualDataModule.RemoteEditAction2
        DisplayMode = nbdmImageAndText
        DropdownCombo = True
        OnPopup = EditMenuItemPopup
      end
      object TBXItem45: TTBXItem
        Action = NonVisualDataModule.CurrentOpenAction
      end
      object TBXItem58: TTBXItem
        Action = NonVisualDataModule.RemoteDeleteAction2
      end
      object TBXItem59: TTBXSubmenuItem
        Action = NonVisualDataModule.RemotePropertiesAction2
        DisplayMode = nbdmImageAndText
        DropdownCombo = True
        object TBXItem90: TTBXItem
          Action = NonVisualDataModule.RemotePropertiesAction2
          Options = [tboDefault]
        end
        object TBXSeparatorItem29: TTBXSeparatorItem
        end
        object TBXItem22: TTBXItem
          Action = NonVisualDataModule.RemoteCalculateDirectorySizesAction
        end
      end
      object TBXItem60: TTBXItem
        Action = NonVisualDataModule.RemoteRenameAction2
      end
      object TBXSeparatorItem16: TTBXSeparatorItem
      end
      object NewSubmenuItem: TTBXSubmenuItem
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
      object TBXItem63: TTBXItem
        Action = NonVisualDataModule.ConsoleAction
      end
      object TBXItem91: TTBXItem
        Action = NonVisualDataModule.PuttyAction
      end
      object TBXSeparatorItem17: TTBXSeparatorItem
      end
      object TBXItem64: TTBXItem
        Action = NonVisualDataModule.SynchronizeAction
      end
      object TBXItem65: TTBXItem
        Action = NonVisualDataModule.FullSynchronizeAction
        DisplayMode = nbdmImageAndText
      end
    end
    object SelectionToolbar: TTBXToolbar
      Left = 0
      Top = 78
      Caption = 'Selection'
      DockPos = -4
      DockRow = 3
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      object TBXItem66: TTBXItem
        Action = NonVisualDataModule.SelectAction
      end
      object TBXItem67: TTBXItem
        Action = NonVisualDataModule.UnselectAction
      end
      object TBXSeparatorItem18: TTBXSeparatorItem
      end
      object TBXItem68: TTBXItem
        Action = NonVisualDataModule.SelectAllAction
      end
      object TBXItem69: TTBXItem
        Action = NonVisualDataModule.InvertSelectionAction
      end
      object TBXItem70: TTBXItem
        Action = NonVisualDataModule.ClearSelectionAction
      end
      object TBXItem134: TTBXItem
        Action = NonVisualDataModule.RestoreSelectionAction
      end
    end
    object SessionToolbar2: TTBXToolbar
      Left = 0
      Top = 104
      Caption = 'Sessions and Tabs'
      DockPos = -4
      DockRow = 4
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      object TBXItem123: TTBXItem
        Action = NonVisualDataModule.NewTabAction
        DisplayMode = nbdmImageAndText
      end
      object TBXItem125: TTBXItem
        Action = NonVisualDataModule.SaveCurrentSessionAction2
      end
      object TBXSeparatorItem23: TTBXSeparatorItem
      end
      object TBXItem137: TTBXItem
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
    object PreferencesToolbar: TTBXToolbar
      Left = 0
      Top = 130
      Caption = 'Preferences'
      DockPos = -4
      DockRow = 5
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      object TBXItem126: TTBXItem
        Action = NonVisualDataModule.PreferencesAction
      end
      object TBXSeparatorItem36: TTBXSeparatorItem
      end
      object TBXSubmenuItem3: TTBXSubmenuItem
        Action = NonVisualDataModule.RemoteCycleStyleAction
        DropdownCombo = True
        object TBXItem72: TTBXItem
          Action = NonVisualDataModule.RemoteIconAction
        end
        object TBXItem73: TTBXItem
          Action = NonVisualDataModule.RemoteSmallIconAction
        end
        object TBXItem74: TTBXItem
          Action = NonVisualDataModule.RemoteListAction
        end
        object TBXItem75: TTBXItem
          Action = NonVisualDataModule.RemoteReportAction
        end
        object TBXItem279: TTBXItem
          Action = NonVisualDataModule.RemoteThumbnailAction
        end
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
        object TBXSeparatorItem44: TTBXSeparatorItem
        end
        object TBXItem165: TTBXItem
          Action = NonVisualDataModule.QueueFileListAction
        end
      end
      object TBXItem71: TTBXItem
        Action = NonVisualDataModule.RemoteTreeAction
      end
    end
    object SortToolbar: TTBXToolbar
      Left = 0
      Top = 156
      Caption = 'Sort'
      DockPos = 0
      DockRow = 6
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
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
      object TBXItem133: TTBXItem
        Action = NonVisualDataModule.CurrentSortByTypeAction2
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
    object AddressToolbar: TTBXToolbar
      Left = 0
      Top = 25
      Caption = 'Address'
      DockableTo = [dpTop, dpBottom]
      DockMode = dmCannotFloat
      DockPos = -8
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      PopupMenu = NonVisualDataModule.RemoteAddressPopup
      Resizable = False
      ShowHint = True
      Stretch = True
      TabOrder = 1
      OnResize = ToolBarResize
      OnGetBaseSize = ToolbarGetBaseSize
      OnEndModal = AddressToolbarEndModal
      object TBXLabelItem1: TTBXLabelItem
        Caption = 'Address'
        Margin = 2
      end
      object UnixPathComboBox: TTBXComboBoxItem
        EditWidth = 200
        OnAcceptText = UnixPathComboBoxAcceptText
        OnBeginEdit = UnixPathComboBoxBeginEdit
        ShowImage = True
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
        object TBXItem127: TTBXItem
          Action = NonVisualDataModule.RemoteFilterAction
        end
        object TBXSeparatorItem43: TTBXSeparatorItem
        end
        object TBXItem161: TTBXItem
          Action = NonVisualDataModule.FileColorsPreferencesAction
        end
      end
    end
    object UpdatesToolbar: TTBXToolbar
      Left = 0
      Top = 182
      Caption = 'Updates'
      DockPos = -7
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      object TBXSubmenuItem4: TTBXSubmenuItem
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
      Top = 182
      Caption = 'Transfer Settings'
      DockPos = 44
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      Options = [tboShowHint]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
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
      Top = 182
      Caption = 'Custom Commands'
      ChevronMenu = True
      ChevronPriorityForNewItems = tbcpLowest
      DockPos = 193
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      Visible = False
    end
  end
  inherited RemotePanel: TPanel
    Left = 9
    Top = 239
    Width = 614
    Height = 93
    Constraints.MinHeight = 92
    Constraints.MinWidth = 210
    inherited RemotePanelSplitter: TSplitter
      Height = 62
      Hint = 
        'Drag to resize directory tree. Double click to hide directory tr' +
        'ee.'
    end
    inherited RemoteStatusBar: TTBXStatusBar
      Top = 71
      Width = 614
      Height = 22
      Images = GlyphsModule.SessionImages
      Panels = <
        item
          Size = 130
          StretchPriority = 1
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taCenter
          Hint = 'Click to show hidden files'
          Size = 120
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taCenter
          Hint = 'Click to modify or clear the filter'
          Size = 120
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
      OnPanelClick = RemoteStatusBarPanelClick
      OnPanelDblClick = StatusBarPanelDblClick
    end
    inherited RemoteDirPanel: TPanel
      Width = 442
      Height = 62
      inherited RemoteDirView: TUnixDirView
        Width = 442
        Height = 62
        OnUpdateStatusBar = RemoteDirViewUpdateStatusBar
        OnPathChange = RemoteDirViewPathChange
      end
    end
    inherited RemoteDrivePanel: TPanel
      Height = 62
      Constraints.MinWidth = 40
      inherited RemoteDriveView: TUnixDriveView
        Height = 62
      end
    end
    object BottomDock: TTBXDock
      Left = 0
      Top = 62
      Width = 614
      Height = 9
      Color = clBtnFace
      FixAlign = True
      Position = dpBottom
    end
  end
  inherited QueuePanel: TPanel
    Top = 335
    Width = 632
    inherited QueueLabel: TPathLabel
      Width = 632
    end
    inherited QueueFileListSplitter: TSplitter
      Width = 632
    end
    inherited QueueView3: TListView
      Width = 632
    end
    inherited QueueDock: TTBXDock
      Width = 632
    end
    inherited QueueFileList: TListView
      Width = 632
    end
  end
  inherited SessionsPageControl: TThemePageControl
    Top = 218
    Width = 632
  end
  object LeftDock: TTBXDock [5]
    Left = 0
    Top = 239
    Width = 9
    Height = 93
    Position = dpLeft
  end
  object RightDock: TTBXDock [6]
    Left = 623
    Top = 239
    Width = 9
    Height = 93
    Position = dpRight
  end
  inherited MessageDock: TTBXDock
    Top = 209
    Width = 632
  end
end
