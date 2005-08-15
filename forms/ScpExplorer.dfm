inherited ScpExplorerForm: TScpExplorerForm
  Left = 292
  Top = 148
  Width = 648
  Height = 513
  HelpType = htKeyword
  HelpKeyword = 'ui_explorer'
  ActiveControl = RemoteDirView
  Caption = 'ScpExplorerForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited QueueSplitter: TSplitter
    Top = 343
    Width = 640
  end
  inherited TopDock: TTBXDock
    Width = 640
    Height = 206
    object MenuToolbar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Menu'
      CloseButton = False
      Images = GlyphsModule.ExplorerImages
      MenuBar = True
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 0
      object TBXSubmenuItem5: TTBXSubmenuItem
        Caption = '&File'
        HelpKeyword = 'ui_explorer_menu#file'
        Hint = 'File operations'
        object TBXItem25: TTBXItem
          Action = NonVisualDataModule.CurrentOpenAction
        end
        object TBXItem26: TTBXItem
          Action = NonVisualDataModule.CurrentEditAction
        end
        object TBXSubmenuItem9: TTBXSubmenuItem
          Action = NonVisualDataModule.CurrentEditAlternativeAction
        end
        object TBXItem28: TTBXItem
          Action = NonVisualDataModule.EditNewAction
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem34: TTBXItem
          Action = NonVisualDataModule.CurrentDeleteAction
        end
        object TBXItem35: TTBXItem
          Action = NonVisualDataModule.CurrentRenameAction
        end
        object TBXItem41: TTBXItem
          Action = NonVisualDataModule.CurrentPropertiesAction
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
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
        object TBXItem36: TTBXItem
          Action = NonVisualDataModule.PasteAction
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
          Hint = 'E'
        end
        object CustomCommandsMenu: TTBXSubmenuItem
          Action = NonVisualDataModule.CustomCommandsAction
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
            Action = NonVisualDataModule.UrlToClipboardAction
          end
        end
        object TBXSeparatorItem1: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem1: TTBXItem
          Action = NonVisualDataModule.CloseSessionAction
        end
        object TBXItem2: TTBXItem
          Action = NonVisualDataModule.CloseApplicationAction
        end
      end
      object TBXSubmenuItem7: TTBXSubmenuItem
        Caption = '&Commands'
        HelpKeyword = 'ui_explorer_menu#commands'
        Hint = 'Other commands'
        object TBXItem3: TTBXItem
          Action = NonVisualDataModule.CurrentCreateDirAction
        end
        object TBXItem4: TTBXItem
          Action = NonVisualDataModule.AddEditLinkAction
        end
        object TBXItem43: TTBXItem
          Action = NonVisualDataModule.SynchronizeAction
        end
        object TBXItem44: TTBXItem
          Action = NonVisualDataModule.FullSynchronizeAction
        end
        object TBXSubmenuItem8: TTBXSubmenuItem
          Caption = '&Queue'
          HelpKeyword = 'ui_queue#managing_the_queue'
          Hint = 'Queue list commands'
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
        object TBXItem5: TTBXItem
          Action = NonVisualDataModule.RemoteAddBookmarkAction
        end
        object TBXItem6: TTBXItem
          Action = NonVisualDataModule.RemotePathToClipboardAction
        end
        object TBXSeparatorItem2: TTBXSeparatorItem
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
      object TBXSubmenuItem19: TTBXSubmenuItem
        Caption = '&Session'
        HelpKeyword = 'ui_explorer_menu#session'
        Hint = 'Session commands'
        object TBXItem113: TTBXItem
          Action = NonVisualDataModule.NewSessionAction
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
            Action = NonVisualDataModule.ExplorerSessionBandAction
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
          object TBXSeparatorItem19: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem92: TTBXItem
            Action = NonVisualDataModule.LockToolbarsAction
          end
        end
        object TBXItem13: TTBXItem
          Action = NonVisualDataModule.StatusBarAction
        end
        object TBXItem14: TTBXItem
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
          object TBXItem81: TTBXItem
            Action = NonVisualDataModule.QueuePreferencesAction
          end
        end
        object TBXItem15: TTBXItem
          Action = NonVisualDataModule.RemoteTreeAction
        end
        object TBXSeparatorItem3: TTBXSeparatorItem
          Hint = 'E'
        end
        object TBXItem16: TTBXItem
          Action = NonVisualDataModule.CurrentIconAction
        end
        object TBXItem17: TTBXItem
          Action = NonVisualDataModule.CurrentSmallIconAction
        end
        object TBXItem18: TTBXItem
          Action = NonVisualDataModule.CurrentListAction
        end
        object TBXItem19: TTBXItem
          Action = NonVisualDataModule.CurrentReportAction
        end
        object TBXSeparatorItem4: TTBXSeparatorItem
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
        object TBXItem20: TTBXItem
          Action = NonVisualDataModule.RemoteRefreshAction
        end
        object TBXSubmenuItem16: TTBXSubmenuItem
          Caption = '&Sort'
          HelpKeyword = 'ui_file_panel#sorting_files'
          Hint = 'Change file order in panel'
          object TBXItem93: TTBXItem
            Action = NonVisualDataModule.RemoteSortAscendingAction
          end
          object TBXSeparatorItem28: TTBXSeparatorItem
            Hint = 'E'
          end
          object TBXItem94: TTBXItem
            Action = NonVisualDataModule.RemoteSortByNameAction
            GroupIndex = 1
          end
          object TBXItem95: TTBXItem
            Action = NonVisualDataModule.RemoteSortByExtAction
            GroupIndex = 1
          end
          object TBXItem132: TTBXItem
            Action = NonVisualDataModule.RemoteSortByTypeAction
            RadioItem = True
          end
          object TBXItem96: TTBXItem
            Action = NonVisualDataModule.RemoteSortByChangedAction
            GroupIndex = 1
          end
          object TBXItem97: TTBXItem
            Action = NonVisualDataModule.RemoteSortBySizeAction
            GroupIndex = 1
          end
          object TBXItem98: TTBXItem
            Action = NonVisualDataModule.RemoteSortByRightsAction
            GroupIndex = 1
          end
          object TBXItem99: TTBXItem
            Action = NonVisualDataModule.RemoteSortByOwnerAction
            GroupIndex = 1
          end
          object TBXItem100: TTBXItem
            Action = NonVisualDataModule.RemoteSortByGroupAction
            GroupIndex = 1
          end
        end
        object TBXSubmenuItem17: TTBXSubmenuItem
          Caption = 'Show &Columns'
          HelpKeyword = 'ui_file_panel#selecting_columns'
          Hint = 'Select columns to show in panel'
          object TBXItem101: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteNameColumnAction
          end
          object TBXItem102: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteSizeColumnAction
          end
          object TBXItem131: TTBXItem
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
          object TBXItem76: TTBXItem
            Action = NonVisualDataModule.ShowHideRemoteLinkTargetColumnAction
          end
        end
        object TBXSeparatorItem5: TTBXSeparatorItem
          Hint = 'E'
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
    object ButtonsToolbar: TTBXToolbar
      Left = 0
      Top = 50
      Caption = 'Commands'
      DockPos = -8
      DockRow = 2
      Images = GlyphsModule.ExplorerImages
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
      object TBXSeparatorItem15: TTBXSeparatorItem
      end
      object TBXItem42: TTBXItem
        Action = NonVisualDataModule.CurrentEditAction
      end
      object TBXItem45: TTBXItem
        Action = NonVisualDataModule.CurrentOpenAction
      end
      object TBXItem58: TTBXItem
        Action = NonVisualDataModule.CurrentDeleteAction
      end
      object TBXItem59: TTBXItem
        Action = NonVisualDataModule.CurrentPropertiesAction
      end
      object TBXItem60: TTBXItem
        Action = NonVisualDataModule.CurrentRenameAction
      end
      object TBXSeparatorItem16: TTBXSeparatorItem
      end
      object TBXItem61: TTBXItem
        Action = NonVisualDataModule.CurrentCreateDirAction
      end
      object TBXItem62: TTBXItem
        Action = NonVisualDataModule.AddEditLinkAction
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
      end
    end
    object SelectionToolbar: TTBXToolbar
      Left = 0
      Top = 76
      Caption = 'Selection'
      DockPos = -4
      DockRow = 3
      Images = GlyphsModule.ExplorerImages
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
    object SessionToolbar: TTBXToolbar
      Left = 0
      Top = 102
      Caption = 'Session'
      DockPos = -4
      DockRow = 4
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      object TBXItem123: TTBXItem
        Action = NonVisualDataModule.NewSessionAction
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
    object PreferencesToolbar: TTBXToolbar
      Left = 0
      Top = 128
      Caption = 'Preferences'
      DockPos = -4
      DockRow = 5
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      object TBXItem126: TTBXItem
        Action = NonVisualDataModule.PreferencesAction
      end
      object TBXSeparatorItem36: TTBXSeparatorItem
      end
      object TBXSubmenuItem3: TTBXSubmenuItem
        Action = NonVisualDataModule.CurrentCycleStyleAction
        DropdownCombo = True
        object TBXItem72: TTBXItem
          Action = NonVisualDataModule.CurrentIconAction
        end
        object TBXItem73: TTBXItem
          Action = NonVisualDataModule.CurrentSmallIconAction
        end
        object TBXItem74: TTBXItem
          Action = NonVisualDataModule.CurrentListAction
        end
        object TBXItem75: TTBXItem
          Action = NonVisualDataModule.CurrentReportAction
        end
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
      object TBXItem71: TTBXItem
        Action = NonVisualDataModule.RemoteTreeAction
      end
    end
    object SortToolbar: TTBXToolbar
      Left = 0
      Top = 154
      Caption = 'Sort'
      DockPos = 0
      DockRow = 6
      Images = GlyphsModule.ExplorerImages
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
        Action = NonVisualDataModule.RemoteSortByTypeAction
        RadioItem = True
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
      Top = 23
      Caption = 'Address'
      DockableTo = [dpTop, dpBottom]
      DockMode = dmCannotFloat
      DockPos = -8
      DockRow = 1
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      Resizable = False
      ShowHint = True
      Stretch = True
      TabOrder = 1
      OnResize = ToolBarResize
      OnGetBaseSize = AddressToolbarGetBaseSize
      object TBXLabelItem1: TTBXLabelItem
        Caption = 'Address'
        Margin = 3
      end
      object TBControlItem1: TTBControlItem
        Control = UnixPathComboBox
      end
      object TBXItem22: TTBXItem
        Action = NonVisualDataModule.RemoteOpenDirAction
      end
      object UnixPathComboBox: TUnixPathComboBox
        Tag = 1
        Left = 49
        Top = 1
        Width = 555
        Height = 21
        Align = alClient
        Constraints.MinWidth = 50
        TabOrder = 0
      end
    end
    object UpdatesToolbar: TTBXToolbar
      Left = 0
      Top = 180
      Caption = 'Updates'
      DockPos = -7
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      object TBXSubmenuItem4: TTBXSubmenuItem
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
      Left = 44
      Top = 180
      Caption = 'Transfer settings'
      DockPos = 44
      DockRow = 7
      Images = GlyphsModule.ExplorerImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      object TransferCombo: TTBXComboBoxItem
        EditWidth = 114
        Hint = 'Select transfer settings preset'
        DropDownList = True
        MaxVisibleItems = 15
      end
      object TBXItem90: TTBXItem
        Action = NonVisualDataModule.PresetsPreferencesAction
      end
    end
  end
  inherited RemotePanel: TPanel
    Left = 9
    Top = 206
    Width = 622
    Height = 137
    Constraints.MinHeight = 100
    Constraints.MinWidth = 210
    inherited RemotePanelSplitter: TSplitter
      Height = 106
      Hint = 
        'Drag to resize directory tree. Double click to hide directory tr' +
        'ee.'
    end
    inherited RemoteStatusBar: TTBXStatusBar
      Tag = 1
      Top = 115
      Width = 622
      Height = 22
      Images = GlyphsModule.SessionImages
      Panels = <
        item
          MaxSize = 190
          Size = 190
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taCenter
          MaxSize = 70
          ViewPriority = 97
          Size = 70
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
          MaxSize = 25
          ViewPriority = 93
          Size = 25
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taCenter
          MaxSize = 25
          ViewPriority = 94
          Size = 25
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          MaxSize = 70
          ViewPriority = 95
          Size = 70
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taCenter
          MaxSize = 90
          ViewPriority = 96
          Size = 90
          Tag = 0
          TextTruncation = twEndEllipsis
        end
        item
          Alignment = taCenter
          MaxSize = 80
          ViewPriority = 99
          Size = 80
          Tag = 0
          TextTruncation = twEndEllipsis
        end>
      OnDblClick = RemoteStatusBarDblClick
    end
    inherited RemoteDirView: TUnixDirView
      Width = 450
      Height = 106
      PathComboBox = UnixPathComboBox
      OnUpdateStatusBar = RemoteDirViewUpdateStatusBar
    end
    inherited RemoteDriveView: TUnixDriveView
      Height = 106
      Constraints.MinWidth = 40
    end
    object BottomDock: TTBXDock
      Left = 0
      Top = 106
      Width = 622
      Height = 9
      FixAlign = True
      Position = dpBottom
    end
  end
  inherited QueuePanel: TPanel
    Top = 346
    Width = 640
    inherited QueueView: TListView
      Width = 640
    end
    inherited QueueDock: TTBXDock
      Width = 640
    end
  end
  object LeftDock: TTBXDock
    Left = 0
    Top = 206
    Width = 9
    Height = 137
    Position = dpLeft
  end
  object RightDock: TTBXDock
    Left = 631
    Top = 206
    Width = 9
    Height = 137
    Position = dpRight
  end
end
