inherited ScpCommanderForm: TScpCommanderForm
  Left = 162
  Top = 0
  Width = 845
  Height = 708
  HelpType = htKeyword
  HelpKeyword = 'ui_commander'
  Caption = 'ScpCommanderForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter [0]
    Left = 313
    Top = 205
    Width = 5
    Height = 277
    Cursor = crHSplit
    Hint = 
      '|Drag to change ratio of file panels. Double click to make width' +
      ' of file panels equal.'
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
    OnMoved = SplitterMoved
  end
  inherited QueueSplitter: TSplitter
    Top = 533
    Width = 837
  end
  inherited TopDock: TTBXDock
    Width = 837
    Height = 205
    object MenuToolbar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Menu'
      CloseButton = False
      Images = GlyphsModule.ExplorerImages
      MenuBar = True
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 6
      object LocalMenuButton: TTBXSubmenuItem
        Caption = '&Local'
        HelpKeyword = 'ui_commander_menu#local'
        Hint = 'Change local panel layout or change displayed directory/drive'
        object TBXItem1: TTBXItem
          Action = NonVisualDataModule.LocalChangePathAction
        end
        object TBXSeparatorItem1: TTBXSeparatorItem
          Hint = 'E'
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
            Hint = 'E'
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
            Hint = 'E'
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
          Hint = 'E'
        end
        object TBXSubmenuItem3: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in local panel'
          object TBXItem12: TTBXItem
            Action = NonVisualDataModule.LocalSortAscendingAction
          end
          object TBXSeparatorItem5: TTBXSeparatorItem
            Hint = 'E'
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
        object TBXItem111: TTBXItem
          Action = NonVisualDataModule.InvertSelectionAction
        end
        object TBXItem112: TTBXItem
          Action = NonVisualDataModule.ClearSelectionAction
        end
        object TBXItem27: TTBXItem
          Action = NonVisualDataModule.RestoreSelectionAction
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
          Hint = 'E'
        end
        object TBXItem25: TTBXItem
          Action = NonVisualDataModule.CurrentOpenAction
        end
        object TBXItem26: TTBXItem
          Action = NonVisualDataModule.CurrentEditAction
        end
        object TBXSubmenuItem25: TTBXSubmenuItem
          Action = NonVisualDataModule.CurrentEditAlternativeAction
        end
        object TBXItem29: TTBXItem
          Action = NonVisualDataModule.AddEditLinkAction
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem30: TTBXItem
          Action = NonVisualDataModule.CurrentCopyAction
        end
        object TBXItem31: TTBXItem
          Action = NonVisualDataModule.RemoteCopyToAction
        end
        object TBXItem32: TTBXItem
          Action = NonVisualDataModule.CurrentMoveAction
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
        object TBXItem36: TTBXItem
          Action = NonVisualDataModule.PasteAction
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
          Hint = 'E'
        end
        object CustomCommandsMenu: TTBXSubmenuItem
          Action = NonVisualDataModule.CustomCommandsAction
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
            Action = NonVisualDataModule.UrlToClipboardAction
          end
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
          Hint = 'E'
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
          Action = NonVisualDataModule.FindFilesAction
        end
        object QueueSubmenuItem: TTBXSubmenuItem
          Caption = '&Queue'
          HelpKeyword = 'ui_queue#managing_the_queue'
          Hint = 'Queue list commands'
          OnPopup = QueueSubmenuItemPopup
          object TBXItem46: TTBXItem
            Action = NonVisualDataModule.QueueGoToAction
          end
          object TBXSeparatorItem10: TTBXSeparatorItem
            Hint = 'E'
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
            Hint = 'E'
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
            Hint = 'E'
          end
          object TBXItem52: TTBXItem
            Action = NonVisualDataModule.QueueItemUpAction
          end
          object TBXItem53: TTBXItem
            Action = NonVisualDataModule.QueueItemDownAction
          end
          object TBXSeparatorItem48: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXSubmenuItem13: TTBXSubmenuItem
            Caption = '&All'
            HelpKeyword = 'ui_queue#managing_the_queue'
            Hint = 'Mass queue management commands'
            object TBXItem198: TTBXItem
              Action = NonVisualDataModule.QueuePauseAllAction
            end
            object TBXItem199: TTBXItem
              Action = NonVisualDataModule.QueueResumeAllAction
            end
          end
        end
        object TBXSeparatorItem13: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem54: TTBXItem
          Action = NonVisualDataModule.ConsoleAction
        end
        object TBXItem55: TTBXItem
          Action = NonVisualDataModule.PuttyAction
        end
        object TBXSeparatorItem14: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem56: TTBXItem
          Action = NonVisualDataModule.FileSystemInfoAction
        end
        object TBXItem57: TTBXItem
          Action = NonVisualDataModule.ClearCachesAction
        end
        object TBXSeparatorItem15: TTBXSeparatorItem
          Hint = 'E'
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
        object TBXItem218: TTBXItem
          Action = NonVisualDataModule.DuplicateSessionAction
        end
        object TBXSubmenuItem20: TTBXSubmenuItem
          Action = NonVisualDataModule.SavedSessionsAction
        end
        object TBXSeparatorItem29: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXSubmenuItem21: TTBXSubmenuItem
          Action = NonVisualDataModule.OpenedSessionsAction
        end
        object TBXItem114: TTBXItem
          Action = NonVisualDataModule.SaveCurrentSessionAction
        end
        object TBXItem115: TTBXItem
          Action = NonVisualDataModule.CloseSessionAction
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
          object TBXItem59: TTBXItem
            Action = NonVisualDataModule.CommanderToolbarBandAction
          end
          object TBXItem60: TTBXItem
            Action = NonVisualDataModule.CommanderSessionBandAction
          end
          object TBXItem61: TTBXItem
            Action = NonVisualDataModule.CommanderSelectionBandAction
          end
          object TBXItem62: TTBXItem
            Action = NonVisualDataModule.CommanderPreferencesBandAction
          end
          object TBXItem63: TTBXItem
            Action = NonVisualDataModule.CommanderSortBandAction
          end
          object TBXItem64: TTBXItem
            Action = NonVisualDataModule.CommanderCommandsBandAction
          end
          object TBXItem186: TTBXItem
            Action = NonVisualDataModule.CommanderUpdatesBandAction
          end
          object TBXItem188: TTBXItem
            Action = NonVisualDataModule.CommanderTransferBandAction
          end
          object TBXItem214: TTBXItem
            Action = NonVisualDataModule.CommanderUploadDownloadBandAction
          end
          object TBXItem215: TTBXItem
            Action = NonVisualDataModule.CommanderCustomCommandsBandAction
          end
          object TBXSeparatorItem47: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem191: TTBXItem
            Action = NonVisualDataModule.LockToolbarsAction
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
          object TBXSeparatorItem16: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem67: TTBXItem
            Action = NonVisualDataModule.LocalTreeAction
          end
          object TBXSeparatorItem17: TTBXSeparatorItem
            Hint = 'E'
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
          object TBXSeparatorItem18: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem71: TTBXItem
            Action = NonVisualDataModule.RemoteTreeAction
          end
          object TBXSeparatorItem19: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem72: TTBXItem
            Action = NonVisualDataModule.RemoteStatusBarAction
          end
        end
        object TBXSeparatorItem20: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem73: TTBXItem
          Action = NonVisualDataModule.CommandLinePanelAction
        end
        object TBXItem74: TTBXItem
          Action = NonVisualDataModule.ToolBarAction
        end
        object TBXItem75: TTBXItem
          Action = NonVisualDataModule.StatusBarAction
        end
        object TBXItem76: TTBXItem
          Action = NonVisualDataModule.ViewLogAction
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
            Hint = 'E'
          end
          object TBXItem80: TTBXItem
            Action = NonVisualDataModule.QueueToolbarAction
          end
          object TBXSeparatorItem22: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXSubmenuItem8: TTBXSubmenuItem
            Action = NonVisualDataModule.QueueCycleOnceEmptyAction
            DropdownCombo = True
            object TBXItem222: TTBXItem
              Action = NonVisualDataModule.QueueIdleOnceEmptyAction
              RadioItem = True
            end
            object TBXItem223: TTBXItem
              Action = NonVisualDataModule.QueueDisconnectOnceEmptyAction
              RadioItem = True
            end
            object TBXItem224: TTBXItem
              Action = NonVisualDataModule.QueueShutDownOnceEmptyAction
              RadioItem = True
            end
          end
          object TBXItem81: TTBXItem
            Action = NonVisualDataModule.QueuePreferencesAction
          end
        end
        object TBXSeparatorItem23: TTBXSeparatorItem
          Hint = 'E'
        end
        object ColorMenuItem: TTBXColorItem
          Action = NonVisualDataModule.ColorMenuAction
          Color = clNone
          object TBXItem216: TTBXItem
            Action = NonVisualDataModule.ColorDefaultAction
          end
          object TBXSeparatorItem50: TTBXSeparatorItem
            Blank = True
          end
          object SessionColorPalette: TTBXColorPalette
            PaletteOptions = [tpoCustomImages]
            OnChange = SessionColorPaletteChange
          end
          object TBXSeparatorItem51: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem217: TTBXItem
            Action = NonVisualDataModule.ColorPickAction
          end
        end
        object TBXSeparatorItem49: TTBXSeparatorItem
          Hint = 'E'
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
          Hint = 'E'
        end
        object TBXSubmenuItem15: TTBXSubmenuItem
          Caption = '&Go To'
          HelpKeyword = 'task_navigate'
          Hint = 'Go to directory'
          object TBXItem84: TTBXItem
            Action = NonVisualDataModule.RemoteOpenDirAction
          end
          object TBXSeparatorItem25: TTBXSeparatorItem
            Hint = 'E'
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
            Hint = 'E'
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
          Hint = 'E'
        end
        object TBXSubmenuItem16: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in remote panel'
          object TBXItem93: TTBXItem
            Action = NonVisualDataModule.RemoteSortAscendingAction
          end
          object TBXSeparatorItem28: TTBXSeparatorItem
            Hint = 'E'
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
        object TBXSeparatorItem30: TTBXSeparatorItem
          Hint = 'E'
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
          Hint = 'E'
        end
        object TBXItem120: TTBXItem
          Action = NonVisualDataModule.CheckForUpdatesAction
        end
        object TBXSeparatorItem32: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem121: TTBXItem
          Action = NonVisualDataModule.DonatePageAction
        end
        object TBXSeparatorItem33: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem122: TTBXItem
          Action = NonVisualDataModule.AboutAction
        end
      end
    end
    object SelectionToolbar: TTBXToolbar
      Left = 0
      Top = 75
      Caption = 'Selection'
      DockPos = 0
      DockRow = 3
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      object TBXItem131: TTBXItem
        Action = NonVisualDataModule.SelectAction
      end
      object TBXItem132: TTBXItem
        Action = NonVisualDataModule.UnselectAction
      end
      object TBXSeparatorItem37: TTBXSeparatorItem
      end
      object TBXItem133: TTBXItem
        Action = NonVisualDataModule.SelectAllAction
      end
      object TBXItem134: TTBXItem
        Action = NonVisualDataModule.InvertSelectionAction
      end
      object TBXItem135: TTBXItem
        Action = NonVisualDataModule.ClearSelectionAction
      end
      object TBXItem200: TTBXItem
        Action = NonVisualDataModule.RestoreSelectionAction
      end
    end
    object PreferencesToolbar: TTBXToolbar
      Left = 0
      Top = 49
      Caption = 'Preferences'
      DockPos = 0
      DockRow = 2
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object TBXItem126: TTBXItem
        Action = NonVisualDataModule.PreferencesAction
      end
      object TBXSeparatorItem36: TTBXSeparatorItem
      end
      object TBXItem127: TTBXItem
        Action = NonVisualDataModule.ViewLogAction
      end
      object TBXSubmenuItem24: TTBXSubmenuItem
        Action = NonVisualDataModule.QueueToggleShowAction
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
      end
    end
    object SessionToolbar: TTBXToolbar
      Left = 0
      Top = 23
      Caption = 'Session'
      DockMode = dmCannotFloat
      DockPos = 0
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      Stretch = True
      TabOrder = 0
      OnResize = ToolBarResize
      OnGetBaseSize = ToolbarGetBaseSize
      object TBXItem123: TTBXItem
        Action = NonVisualDataModule.NewSessionAction
      end
      object TBXItem219: TTBXItem
        Action = NonVisualDataModule.DuplicateSessionAction
      end
      object TBXSeparatorItem34: TTBXSeparatorItem
      end
      object SessionCombo: TTBXComboBoxItem
        EditWidth = 114
        DropDownList = True
        MaxVisibleItems = 15
      end
      object TBXItem124: TTBXItem
        Action = NonVisualDataModule.CloseSessionAction
      end
      object TBXSeparatorItem35: TTBXSeparatorItem
      end
      object TBXSubmenuItem23: TTBXSubmenuItem
        Action = NonVisualDataModule.SavedSessionsAction
        Options = [tboDropdownArrow]
      end
      object TBXItem125: TTBXItem
        Action = NonVisualDataModule.SaveCurrentSessionAction
      end
    end
    object CommandToolbar: TTBXToolbar
      Left = 0
      Top = 101
      Caption = 'Standard'
      DockPos = -5
      DockRow = 4
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      object TBXItem136: TTBXItem
        Action = NonVisualDataModule.CurrentCopyAction
      end
      object TBXItem137: TTBXItem
        Action = NonVisualDataModule.CurrentMoveAction
      end
      object TBXSeparatorItem38: TTBXSeparatorItem
      end
      object TBXItem138: TTBXItem
        Action = NonVisualDataModule.CurrentEditAction
      end
      object TBXItem139: TTBXItem
        Action = NonVisualDataModule.CurrentOpenAction
      end
      object TBXItem140: TTBXItem
        Action = NonVisualDataModule.CurrentRenameAction
      end
      object TBXItem141: TTBXItem
        Action = NonVisualDataModule.CurrentDeleteAction
      end
      object TBXItem142: TTBXItem
        Action = NonVisualDataModule.CurrentPropertiesAction
      end
      object TBXSeparatorItem39: TTBXSeparatorItem
      end
      object TBXItem143: TTBXItem
        Action = NonVisualDataModule.CurrentCreateDirAction
      end
      object TBXItem144: TTBXItem
        Action = NonVisualDataModule.AddEditLinkAction
      end
    end
    object SortToolbar: TTBXToolbar
      Left = 0
      Top = 127
      Caption = 'Sort'
      DockPos = -8
      DockRow = 5
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
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
      Top = 153
      Caption = 'Commands'
      DockPos = 0
      DockRow = 6
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      object TBXItem154: TTBXItem
        Action = NonVisualDataModule.CompareDirectoriesAction
      end
      object TBXItem155: TTBXItem
        Action = NonVisualDataModule.SynchronizeAction
      end
      object TBXItem156: TTBXItem
        Action = NonVisualDataModule.FullSynchronizeAction
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
      object TBXItem227: TTBXItem
        Action = NonVisualDataModule.FindFilesAction
      end
    end
    object UpdatesToolbar: TTBXToolbar
      Left = 0
      Top = 179
      Caption = 'Updates'
      DockPos = -7
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      object TBXSubmenuItem1: TTBXSubmenuItem
        Action = NonVisualDataModule.ShowUpdatesAction
        DropdownCombo = True
        object TBXItem184: TTBXItem
          Action = NonVisualDataModule.CheckForUpdatesAction
        end
        object TBXSeparatorItem46: TTBXSeparatorItem
          Hint = 'E'
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
          Hint = 'E'
        end
        object TBXItem183: TTBXItem
          Action = NonVisualDataModule.UpdatesPreferencesAction
        end
      end
    end
    object TransferToolbar: TTBXToolbar
      Left = 46
      Top = 179
      Caption = 'Transfer settings'
      DockMode = dmCannotFloat
      DockPos = 44
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      Stretch = True
      TabOrder = 8
      OnResize = ToolBarResize
      OnGetBaseSize = ToolbarGetBaseSize
      object TransferDropDown: TTBXDropDownItem
        EditWidth = 114
        Hint = 'Select transfer settings preset'
        DropDownList = True
        object TransferList: TTBXStringList
          MaxVisibleItems = 15
          MinWidth = 350
        end
        object TransferLabel: TTBXLabelItem
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
    object UploadDownloadToolbar: TTBXToolbar
      Left = 170
      Top = 179
      Caption = 'Upload/Download'
      DockPos = 170
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      Visible = False
      object TBXItem213: TTBXItem
        Action = NonVisualDataModule.RemoteCopyAction
      end
      object TBXItem212: TTBXItem
        Action = NonVisualDataModule.LocalCopyAction
      end
    end
    object CustomCommandsToolbar: TTBXToolbar
      Left = 226
      Top = 179
      Caption = 'Custom Commands'
      DockPos = 225
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      Visible = False
    end
  end
  inherited RemotePanel: TPanel
    Left = 318
    Top = 205
    Width = 519
    Height = 277
    Constraints.MinHeight = 220
    Constraints.MinWidth = 185
    TabOrder = 1
    object RemotePathLabel: TPathLabel [0]
      Left = 0
      Top = 78
      Width = 519
      Height = 15
      UnixPath = True
      HotTrack = True
      OnGetStatus = RemotePathLabelGetStatus
      OnPathClick = RemotePathLabelPathClick
      AutoSize = False
      OnDblClick = PathLabelDblClick
    end
    inherited RemotePanelSplitter: TSplitter
      Left = 0
      Top = 138
      Width = 519
      Height = 3
      Cursor = crVSplit
      Hint = 
        'Drag to resize directory tree. Double click to make height of di' +
        'rectory trees equal.'
      Align = alTop
    end
    inherited RemoteStatusBar: TTBXStatusBar
      Top = 258
      Width = 519
      SimplePanel = True
    end
    inherited RemoteDirView: TUnixDirView
      Left = 0
      Top = 141
      Width = 519
      Height = 108
      Constraints.MinHeight = 70
      NortonLike = nlOn
      OnUpdateStatusBar = RemoteDirViewUpdateStatusBar
      PathLabel = RemotePathLabel
      AddParentDir = True
      OnDDFileOperationExecuted = RemoteFileControlDDFileOperationExecuted
      OnPathChange = RemoteDirViewPathChange
    end
    inherited RemoteDriveView: TUnixDriveView
      Top = 93
      Width = 519
      Height = 45
      Align = alTop
      Constraints.MinHeight = 30
      HideSelection = False
      TabStop = False
    end
    object RemoteTopDock: TTBXDock
      Left = 0
      Top = 0
      Width = 519
      Height = 78
      FixAlign = True
      object RemoteHistoryToolbar: TTBXToolbar
        Left = 0
        Top = 26
        Caption = 'Remote history'
        DockPos = -6
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
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
        Left = 0
        Top = 52
        Caption = 'Remote navigation'
        DockPos = 0
        DockRow = 2
        Images = GlyphsModule.ExplorerImages
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
        object TBXSeparatorItem44: TTBXSeparatorItem
        end
        object TBXItem170: TTBXItem
          Action = NonVisualDataModule.RemoteTreeAction
        end
      end
      object RemotePathToolbar: TTBXToolbar
        Left = 0
        Top = 0
        Caption = 'Remote path'
        DockableTo = [dpTop, dpBottom]
        DockMode = dmCannotFloat
        DockPos = 0
        Images = GlyphsModule.ExplorerImages
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
        object TBXItem169: TTBXItem
          Action = NonVisualDataModule.RemoteOpenDirAction
        end
      end
    end
    object RemoteBottomDock: TTBXDock
      Left = 0
      Top = 249
      Width = 519
      Height = 9
      FixAlign = True
      Position = dpBottom
    end
  end
  inherited QueuePanel: TPanel
    Top = 536
    Width = 837
    Height = 116
    TabOrder = 3
    inherited QueueView2: TListView
      Width = 837
      Height = 90
      TabStop = False
    end
    inherited QueueDock: TTBXDock
      Width = 837
    end
  end
  object LocalPanel: TPanel
    Left = 0
    Top = 205
    Width = 313
    Height = 277
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinHeight = 220
    Constraints.MinWidth = 185
    TabOrder = 0
    object LocalPathLabel: TPathLabel
      Left = 0
      Top = 78
      Width = 313
      Height = 15
      HotTrack = True
      OnGetStatus = LocalPathLabelGetStatus
      OnPathClick = LocalPathLabelPathClick
      AutoSize = False
      PopupMenu = NonVisualDataModule.LocalPanelPopup
      OnDblClick = PathLabelDblClick
    end
    object LocalPanelSplitter: TSplitter
      Left = 0
      Top = 138
      Width = 313
      Height = 3
      Cursor = crVSplit
      Hint = 
        'Drag to resize directory tree. Double click to make height of di' +
        'rectory trees equal.'
      Align = alTop
      AutoSnap = False
      MinSize = 70
      ResizeStyle = rsUpdate
    end
    object LocalStatusBar: TTBXStatusBar
      Left = 0
      Top = 258
      Width = 313
      Height = 19
      Panels = <>
      ParentShowHint = False
      SimplePanel = True
      ShowHint = True
      UseSystemFont = False
      OnClick = LocalStatusBarClick
    end
    object LocalDirView: TDirView
      Left = 0
      Top = 141
      Width = 313
      Height = 108
      Align = alClient
      Constraints.MinHeight = 70
      FullDrag = True
      HideSelection = False
      PopupMenu = NonVisualDataModule.LocalDirViewPopup
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnRightClick = DirViewColumnRightClick
      OnEditing = DirViewEditing
      OnEnter = LocalDirViewEnter
      DirColProperties.ExtVisible = False
      PathLabel = LocalPathLabel
      OnUpdateStatusBar = LocalDirViewUpdateStatusBar
      OnGetSelectFilter = RemoteDirViewGetSelectFilter
      HeaderImages = GlyphsModule.ArrowImages
      AddParentDir = True
      OnLoaded = DirViewLoaded
      OnDDDragEnter = LocalFileControlDDDragEnter
      OnDDDragLeave = FileControlDDDragLeave
      OnDDDragOver = LocalFileControlDDDragOver
      OnDDTargetHasDropHandler = LocalDirViewDDTargetHasDropHandler
      OnDDFileOperation = LocalFileControlDDFileOperation
      OnDDMenuPopup = LocalFileControlDDMenuPopup
      OnExecFile = LocalDirViewExecFile
      OnMatchMask = DirViewMatchMask
      OnGetOverlay = RemoteDirViewGetOverlay
      ConfirmDelete = False
      WatchForChanges = True
      OnFileIconForName = LocalDirViewFileIconForName
      OnHistoryChange = DirViewHistoryChange
      OnPathChange = LocalDirViewPathChange
    end
    object LocalTopDock: TTBXDock
      Left = 0
      Top = 0
      Width = 313
      Height = 78
      FixAlign = True
      object LocalHistoryToolbar: TTBXToolbar
        Left = 0
        Top = 26
        Caption = 'Local history'
        DockPos = -6
        DockRow = 1
        Images = GlyphsModule.ExplorerImages
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
        Left = 0
        Top = 52
        Caption = 'Local navigation'
        DockPos = 0
        DockRow = 2
        Images = GlyphsModule.ExplorerImages
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
        Caption = 'Local path'
        DockableTo = [dpTop, dpBottom]
        DockMode = dmCannotFloat
        DockPos = 0
        Images = GlyphsModule.ExplorerImages
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
        object TBXItem163: TTBXItem
          Action = NonVisualDataModule.LocalOpenDirAction
        end
      end
    end
    object LocalDriveView: TDriveView
      Left = 0
      Top = 93
      Width = 313
      Height = 45
      WatchDirectory = True
      DirView = LocalDirView
      OnRefreshDrives = LocalDriveViewRefreshDrives
      OnDDDragEnter = LocalFileControlDDDragEnter
      OnDDDragLeave = FileControlDDDragLeave
      OnDDDragOver = LocalFileControlDDDragOver
      OnDDFileOperation = LocalFileControlDDFileOperation
      OnDDMenuPopup = LocalFileControlDDMenuPopup
      Align = alTop
      Constraints.MinHeight = 30
      HideSelection = False
      Indent = 19
      ParentColor = False
      TabOrder = 2
      TabStop = False
      OnEnter = LocalDriveViewEnter
    end
    object LocalBottomDock: TTBXDock
      Left = 0
      Top = 249
      Width = 313
      Height = 9
      FixAlign = True
      Position = dpBottom
    end
  end
  object BottomDock: TTBXDock
    Left = 0
    Top = 482
    Width = 837
    Height = 51
    FixAlign = True
    Position = dpBottom
    object ToolbarToolbar: TTBXToolbar
      Left = 0
      Top = 25
      Caption = 'Commands'
      DockPos = 0
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = False
      Stretch = True
      TabOrder = 0
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
      object TBXItem173: TTBXItem
        Action = NonVisualDataModule.CurrentCopyAction
        DisplayMode = nbdmImageAndText
        Stretch = True
      end
      object TBXItem174: TTBXItem
        Action = NonVisualDataModule.CurrentMoveAction
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
  object StatusBar: TTBXStatusBar
    Left = 0
    Top = 652
    Width = 837
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
end
