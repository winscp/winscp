object NonVisualDataModule: TNonVisualDataModule
  Height = 502
  Width = 624
  object RemoteFilePopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 424
    Top = 336
    object TBXItem23: TTBXItem
      Action = CurrentAddEditLinkContextAction
    end
    object RemoteOpenMenuItem: TTBXItem
      Action = CurrentOpenAction
    end
    object RemoteEditMenuItem: TTBXSubmenuItem
      Action = CurrentEditFocusedAction
      DropdownCombo = True
      OnPopup = FocusedEditMenuItemPopup
    end
    object RemoteCopyMenuItem: TTBXSubmenuItem
      Action = RemoteCopyFocusedAction
      DropdownCombo = True
      object TBXItem72: TTBXItem
        Action = RemoteCopyFocusedNonQueueAction
      end
      object TBXItem69: TTBXItem
        Action = RemoteCopyFocusedQueueAction
      end
      object TBXSeparatorItem9: TTBXSeparatorItem
      end
      object Moveto1: TTBXItem
        Action = RemoteMoveFocusedAction
      end
    end
    object Duplicate3: TTBXItem
      Action = RemoteCopyToFocusedAction
    end
    object Moveto6: TTBXItem
      Action = RemoteMoveToFocusedAction
    end
    object Delete1: TTBXItem
      Action = CurrentDeleteFocusedAction
    end
    object Rename1: TTBXItem
      Action = CurrentRenameAction
    end
    object TBXSeparatorItem12: TTBXSeparatorItem
    end
    object TBXItem82: TTBXItem
      Action = CurrentCopyToClipboardFocusedAction2
    end
    object N45: TTBXSeparatorItem
    end
    object RemoteFilePopupCustomCommandsMenu: TTBXSubmenuItem
      Action = CustomCommandsFileAction
      object TTBXItem
      end
    end
    object FileNames3: TTBXSubmenuItem
      Caption = '&File Names'
      HelpKeyword = 'filenames'
      Hint = 'Operations with name(s) of selected file(s)'
      object InserttoCommandLine2: TTBXItem
        Action = FileListToCommandLineAction
      end
      object CopytoClipboard3: TTBXItem
        Action = FileListToClipboardAction
      end
      object CopytoClipboardIncludePaths3: TTBXItem
        Action = FullFileListToClipboardAction
      end
      object CopyURLtoClipboard3: TTBXItem
        Action = FileGenerateUrlAction2
      end
    end
    object N1: TTBXSeparatorItem
    end
    object Properties1: TTBXItem
      Action = CurrentPropertiesFocusedAction
    end
  end
  object ExplorerActions: TActionList
    Images = GlyphsModule.ExplorerImages
    OnExecute = ExplorerActionsExecute
    OnUpdate = ExplorerActionsUpdate
    Left = 440
    Top = 24
    object AutoSizeRemoteColumnsAction: TAction
      Tag = 12
      Category = 'Columns'
      Caption = 'Size &Automatically'
      HelpKeyword = 'ui_file_panel#width'
      Hint = 'Adjust columns width to fit their contents'
      ShortCut = 16491
    end
    object RemoteCopyQueueAction: TAction
      Tag = 14
      Category = 'Remote Selected Operation'
      Caption = 'Download in &Background...'
      HelpKeyword = 'task_download'
      Hint = 
        'Download selected remote file(s) to local directory in backgroun' +
        'd'
      ImageIndex = 107
    end
    object RemoteCopyFocusedQueueAction: TAction
      Tag = 12
      Category = 'Remote Focused Operation'
      Caption = 'Download in &Background...'
      HelpKeyword = 'task_download'
      Hint = 
        'Download selected remote file(s) to local directory in backgroun' +
        'd'
      ImageIndex = 107
    end
    object LocalCopyQueueAction: TAction
      Tag = 9
      Category = 'Local Selected Operation'
      Caption = 'Upload in &Background...'
      HelpKeyword = 'task_upload'
      Hint = 'Upload selected local file(s) to remote directory in background'
      ImageIndex = 108
    end
    object LocalCopyFocusedQueueAction: TAction
      Tag = 8
      Category = 'Local Focused Operation'
      Caption = 'Upload in &Background...'
      HelpKeyword = 'task_upload'
      Hint = 'Upload selected local file(s) to remote directory in background'
      ImageIndex = 108
    end
    object RemoteCopyNonQueueAction: TAction
      Tag = 14
      Category = 'Remote Selected Operation'
      Caption = 'Down&load...'
      HelpKeyword = 'task_download'
      Hint = 'Download|Download selected remote file(s) to local directory'
      ImageIndex = 89
    end
    object RemoteCopyFocusedNonQueueAction: TAction
      Tag = 12
      Category = 'Remote Focused Operation'
      Caption = 'Down&load...'
      HelpKeyword = 'task_download'
      Hint = 'Download|Download selected remote file(s) to local directory'
      ImageIndex = 89
    end
    object LocalCopyNonQueueAction: TAction
      Tag = 9
      Category = 'Local Selected Operation'
      Caption = 'Up&load...'
      HelpKeyword = 'task_upload'
      Hint = 'Upload|Upload selected local file(s) to remote directory'
      ImageIndex = 88
    end
    object LocalCopyFocusedNonQueueAction: TAction
      Tag = 8
      Category = 'Local Focused Operation'
      Caption = 'Up&load...'
      HelpKeyword = 'task_upload'
      Hint = 'Upload|Upload selected local file(s) to remote directory'
      ImageIndex = 88
    end
    object LocalCopyFocusedAction: TAction
      Tag = 8
      Category = 'Local Focused Operation'
      Caption = 'Up&load...'
      HelpKeyword = 'task_upload'
      Hint = 'Upload|Upload selected local file(s) to remote directory'
      ImageIndex = 88
    end
    object RemoteCopyFocusedAction: TAction
      Tag = 12
      Category = 'Remote Focused Operation'
      Caption = 'Down&load...'
      HelpKeyword = 'task_download'
      Hint = 'Download|Download selected remote file(s) to local directory'
      ImageIndex = 89
    end
    object RemoteMoveFocusedAction: TAction
      Tag = 12
      Category = 'Remote Focused Operation'
      Caption = 'Download and Dele&te...'
      HelpKeyword = 'task_download'
      Hint = 
        'Download and Delete|Download selected remote file(s) to local di' +
        'rectory and delete original'
      ImageIndex = 97
    end
    object RemoteCopyAction: TAction
      Tag = 14
      Category = 'Remote Selected Operation'
      Caption = 'Down&load...'
      HelpKeyword = 'task_download'
      Hint = 'Download|Download selected remote file(s) to local directory'
      ImageIndex = 89
    end
    object AutoSizeLocalColumnsAction: TAction
      Tag = 8
      Category = 'Columns'
      Caption = 'Size &Automatically'
      HelpKeyword = 'ui_file_panel#width'
      Hint = 'Adjust columns width to fit their contents'
      ShortCut = 16491
    end
    object ResetLayoutRemoteColumnsAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Reset Layout'
      HelpKeyword = 'ui_file_panel#width'
      Hint = 'Reset to the default layout of file panel columns'
    end
    object GoToTreeAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Go to Tree'
      HelpKeyword = 'ui_file_panel#directory_tree'
      Hint = 'Go to tree'
      ImageIndex = 76
      ShortCut = 49236
    end
    object LocalTreeAction: TAction
      Tag = 8
      Category = 'View'
      Caption = '&Tree'
      HelpKeyword = 'ui_file_panel#directory_tree'
      Hint = 'Hide/show directory tree'
      ImageIndex = 76
      ShortCut = 49236
    end
    object RemoteTreeAction: TAction
      Tag = 12
      Category = 'View'
      Caption = '&Tree'
      HelpKeyword = 'ui_file_panel#directory_tree'
      Hint = 'Hide/show directory tree'
      ImageIndex = 76
      ShortCut = 49236
    end
    object QueueItemQueryAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show Query'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Show pending query of selected queue item'
      ImageIndex = 67
    end
    object ResetLayoutLocalColumnsAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Reset Layout'
      HelpKeyword = 'ui_file_panel#width'
      Hint = 'Reset to the default layout of file panel columns'
    end
    object QueueItemErrorAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show Error'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Show pending error message of selected queue item'
      ImageIndex = 68
    end
    object QueueItemPromptAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show Prompt'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Show pending prompt of selected queue item'
      ImageIndex = 69
    end
    object GoToCommandLineAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Go to Comma&nd Line'
      HelpKeyword = 'ui_commander#command_line'
      Hint = 'Go to command line'
      ShortCut = 24653
    end
    object QueueItemDeleteAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Cancel'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Remove selected queue item'
      ImageIndex = 71
    end
    object QueueItemExecuteAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Execute Now'
      HelpKeyword = 'ui_queue#manage'
      Hint = 
        'Execute selected queue item immediately by granting it additiona' +
        'l connection'
      ImageIndex = 70
    end
    object SelectOneAction: TAction
      Tag = 12
      Category = 'Selection'
      Caption = '&Select/Unselect'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Select|Select/unselect focused file'
    end
    object CurrentRenameAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Rename'
      HelpKeyword = 'task_rename'
      Hint = 'Rename|Rename selected file'
      ImageIndex = 3
    end
    object LocalSortAscendingAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = '&Ascending'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Ascending/descending|Toggle ascending/descending sorting of file' +
        's in the panel'
      ImageIndex = 37
    end
    object CurrentEditAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Edit'
      HelpKeyword = 'task_edit'
      Hint = 'Edit|Edit selected file(s)'
      ImageIndex = 57
    end
    object HideColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Hide Column'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Hide column|Hide selected column'
    end
    object LocalBackAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Back'
      HelpKeyword = 'task_navigate#special_commands'
      ImageIndex = 6
      ShortCut = 32805
    end
    object RemoteCycleStyleAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = 'View'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'View|Cycle thru directory view styles'
      ImageIndex = 8
    end
    object RemoteIconAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = '&Large Icons'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'Large Icons|View large icons'
      ImageIndex = 8
    end
    object RemoteSmallIconAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = '&Small Icons'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'Small Icons|View small icons'
      ImageIndex = 9
    end
    object RemoteListAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = 'Lis&t'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'List|View list'
      ImageIndex = 10
    end
    object RemoteReportAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = '&Details'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'Details|View details'
      ImageIndex = 11
    end
    object RemoteMoveToAction: TAction
      Tag = 14
      Category = 'Remote Selected Operation'
      Caption = 'Mo&ve To...'
      HelpKeyword = 'task_move_duplicate#move'
      Hint = 
        'Move|Move selected remote file(s) to another remote directory or' +
        ' another name'
      ImageIndex = 100
    end
    object CurrentDeleteFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Delete'
      HelpKeyword = 'task_delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object CurrentPropertiesFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Properties'
      HelpKeyword = 'task_properties'
      Hint = 'Properties|Display/change properties of selected file(s)'
      ImageIndex = 4
    end
    object CurrentCreateDirAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Create Directory...'
      HelpKeyword = 'task_create_directory'
      Hint = 'Create directory|Create new directory'
      ImageIndex = 5
    end
    object CurrentDeleteAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Delete'
      HelpKeyword = 'task_delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object CurrentPropertiesAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Properties'
      HelpKeyword = 'task_properties'
      Hint = 'Properties|Display/change properties of selected file(s)'
      ImageIndex = 4
    end
    object RemoteBackAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Back'
      HelpKeyword = 'task_navigate#special_commands'
      ImageIndex = 6
      ShortCut = 32805
    end
    object RemoteForwardAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Forward'
      HelpKeyword = 'task_navigate#special_commands'
      ImageIndex = 7
      ShortCut = 32807
    end
    object CommandLinePanelAction: TAction
      Tag = 8
      Category = 'View'
      Caption = 'Comma&nd Line'
      HelpKeyword = 'ui_commander#command_line'
      Hint = 'Hide/show command line'
      ShortCut = 24653
    end
    object RemoteParentDirAction: TAction
      Tag = 12
      Category = 'Remote Directory'
      Caption = '&Parent Directory'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Parent directory|Go to parent directory'
      ImageIndex = 12
      ShortCut = 8
    end
    object RemoteRootDirAction: TAction
      Tag = 12
      Category = 'Remote Directory'
      Caption = '&Root Directory'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Root directory|Go to root directory'
      ImageIndex = 13
      ShortCut = 16604
    end
    object RemoteHomeDirAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Home Directory'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Home directory|Go to home directory'
      ImageIndex = 15
    end
    object RemoteRefreshAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Refresh'
      Hint = 'Refresh|Refresh directory content'
      ImageIndex = 16
    end
    object AboutAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&About...'
      HelpKeyword = 'ui_about'
      Hint = 'About|Show About box'
      ImageIndex = 65
    end
    object StatusBarAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Status &Bar'
      Hint = 'Hide/show status bar'
    end
    object SessionsTabsAction2: TAction
      Tag = 15
      Category = 'View'
      Caption = 'T&abs'
      Hint = 'Hide/show tabs'
    end
    object ExplorerAddressBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Address'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show address toolbar'
    end
    object ExplorerMenuBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Menu'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show menu'
    end
    object ExplorerToolbarBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Standard Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show standard toolbar'
    end
    object RemoteOpenDirAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Open Directory/Bookmark...'
      HelpKeyword = 'task_navigate#manual'
      Hint = 
        'Open directory/bookmark|Open specified directory or saved bookma' +
        'rk'
      ImageIndex = 18
    end
    object SelectAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'Sele&ct Files...'
      HelpKeyword = 'ui_select'
      Hint = 'Select|Select files by mask'
      ImageIndex = 19
      ShortCut = 107
    end
    object UnselectAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = '&Unselect Files...'
      HelpKeyword = 'ui_select'
      Hint = 'Unselect|Unselect files by mask'
      ImageIndex = 20
      ShortCut = 109
    end
    object SelectAllAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'Select &All'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Select all files'
      ImageIndex = 21
      ShortCut = 16449
    end
    object InvertSelectionAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = '&Invert Selection'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Invert selection'
      ImageIndex = 22
      ShortCut = 106
    end
    object ExplorerSelectionBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'Se&lection Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show selection toolbar'
    end
    object ClearSelectionAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'C&lear Selection'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Clear selection'
      ImageIndex = 23
      ShortCut = 24652
    end
    object ExplorerSessionBandAction2: TAction
      Tag = 7
      Category = 'View'
      Caption = 'Sessio&ns and Tabs Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show sessions and tabs toolbar'
    end
    object ExplorerPreferencesBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Preferences Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show preferences toolbar'
    end
    object ExplorerSortBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'So&rt Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show sort toolbar'
    end
    object ExplorerUpdatesBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Update Button'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show updates toolbar'
    end
    object ExplorerTransferBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Transfer Settings'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show transfer settings toolbar'
    end
    object ExplorerCustomCommandsBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'Custom Co&mmand Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show custom commands toolbar'
    end
    object SiteManagerAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = 'Site &Manager...'
      HelpKeyword = 'ui_login'
      Hint = 
        'Site Manager|Opens site manager (hold down Shift to open site ma' +
        'nager in new window)'
    end
    object CloseTabAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = '&Close Tab'
      HelpKeyword = 'ui_tabs#working'
      Hint = 'Close the current tab'
      ImageIndex = 26
      SecondaryShortCuts.Strings = (
        'Ctrl+W')
      ShortCut = 24644
    end
    object DisconnectSessionAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Disconnect Session'
      HelpKeyword = 'task_connections#closing'
      Hint = 'Disconnect the current session, but keep the tab opened'
      ImageIndex = 116
    end
    object ReconnectSessionAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Reconnect Session'
      HelpKeyword = 'task_connections'
      Hint = 'Reconnect the current disconnected session'
      ShortCut = 24658
    end
    object SavedSessionsAction2: TAction
      Tag = 15
      Category = 'Session'
      Caption = 'Si&tes'
      HelpKeyword = 'task_connections#opening_additional_connection'
      Hint = 'Open site'
      ImageIndex = 27
    end
    object WorkspacesAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = '&Workspaces'
      HelpKeyword = 'workspace'
      Hint = 'Open workspace'
      ImageIndex = 101
    end
    object PreferencesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Preferences...'
      HelpKeyword = 'ui_preferences'
      Hint = 'Preferences|Show/change user preferences'
      ImageIndex = 28
      ShortCut = 49232
    end
    object RemoteChangePathAction2: TAction
      Tag = 11
      Category = 'Remote Directory'
      Caption = '&Change Directory'
      HelpKeyword = 'task_navigate'
      Hint = 'Allows selection of different directory for panel'
      ImageIndex = 29
      ShortCut = 32881
    end
    object LocalForwardAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Forward'
      HelpKeyword = 'task_navigate#special_commands'
      ImageIndex = 7
      ShortCut = 32807
    end
    object LocalParentDirAction: TAction
      Tag = 8
      Category = 'Local Directory'
      Caption = '&Parent Directory'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Parent directory|Go to parent directory'
      ImageIndex = 12
      ShortCut = 8
    end
    object LocalRootDirAction: TAction
      Tag = 8
      Category = 'Local Directory'
      Caption = '&Root Directory'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Root directory|Go to root directory'
      ImageIndex = 14
      ShortCut = 16604
    end
    object LocalHomeDirAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Home Directory'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Home directory|Go to home directory'
      ImageIndex = 15
    end
    object LocalRefreshAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Refresh'
      Hint = 'Refresh|Refresh directory content'
      ImageIndex = 16
    end
    object LocalOpenDirAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Open Directory/Bookmark...'
      HelpKeyword = 'task_navigate#manual'
      Hint = 
        'Open directory/bookmark|Open specified directory or saved bookma' +
        'rk'
      ImageIndex = 18
    end
    object LocalChangePathAction2: TAction
      Tag = 11
      Category = 'Local Directory'
      Caption = '&Change Drive'
      HelpKeyword = 'task_navigate'
      Hint = 'Allows selection of different drive for panel'
      ImageIndex = 30
      ShortCut = 32880
    end
    object ToolBar2Action: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Hot Keys Toolbar'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show bottom hot keys toolbar'
    end
    object CommanderMenuBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Menu'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show menu'
    end
    object CommanderSessionBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Sessio&ns and Tabs Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show sessions and tabs toolbar'
    end
    object CommanderPreferencesBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Preferences Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show preferences toolbar'
    end
    object CommanderSortBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'So&rt Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show sort toolbar'
    end
    object CommanderUpdatesBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Update Button'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show updates toolbar'
    end
    object CommanderTransferBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Transfer Settings'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show transfer settings toolbar'
    end
    object CommanderCommandsBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Commands Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show commands toolbar'
    end
    object CommanderCustomCommandsBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Custom Co&mmand Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show custom commands toolbar'
    end
    object CommanderLocalHistoryBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = '&History Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show history toolbar'
    end
    object CommanderLocalNavigationBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Navigation Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show navigation toolbar'
    end
    object CommanderLocalFileBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = '&File Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show file toolbar'
    end
    object CommanderLocalSelectionBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Se&lection Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show selection toolbar'
    end
    object CommanderRemoteHistoryBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = '&History Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show history toolbar'
    end
    object CommanderRemoteNavigationBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Navigation Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show navigation toolbar'
    end
    object CommanderRemoteFileBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = '&File Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show file toolbar'
    end
    object CommanderRemoteSelectionBandAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Se&lection Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show selection toolbar'
    end
    object LocalStatusBarAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Status &Bar'
      Hint = 'Hide/show panel status bar'
    end
    object RemoteStatusBarAction2: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Status &Bar'
      Hint = 'Hide/show panel status bar'
    end
    object LocalSortByNameAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Name'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by name|Sort panel by name'
      ImageIndex = 31
      ShortCut = 16498
    end
    object LocalSortByExtAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Extension'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by extension|Sort panel by file name extension'
      ImageIndex = 32
      ShortCut = 16499
    end
    object LocalSortBySizeAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Size'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by size|Sort panel by file size'
      ImageIndex = 35
      ShortCut = 16501
    end
    object LocalSortByAttrAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By A&ttributes'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by attributes|Sort panel by attributes'
      ImageIndex = 36
      ShortCut = 16502
    end
    object LocalSortByTypeAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Type'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by type|Sort panel by file type'
      ImageIndex = 34
    end
    object LocalSortByChangedAction2: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Modification'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by time|Sort panel by last modification time'
      ImageIndex = 33
      ShortCut = 16500
    end
    object RemoteSortAscendingAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = '&Ascending'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Ascending/descending|Toggle ascending/descending sorting of file' +
        's in the panel'
      ImageIndex = 37
    end
    object RemoteSortByNameAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Name'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by name|Sort panel by name'
      ImageIndex = 31
      ShortCut = 16498
    end
    object RemoteSortByExtAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Extension'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by extension|Sort panel by file name extension'
      ImageIndex = 32
      ShortCut = 16499
    end
    object RemoteSortBySizeAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Size'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by size|Sort panel by file size'
      ImageIndex = 35
      ShortCut = 16501
    end
    object RemoteSortByRightsAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Permissions/Attributes'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Sort by attributes/permissions|Sort panel by attributes/permissi' +
        'ons'
      ImageIndex = 36
      ShortCut = 16502
    end
    object RemoteSortByChangedAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Modification'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by time|Sort panel by last modification time'
      ImageIndex = 33
      ShortCut = 16500
    end
    object RemoteSortByOwnerAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Owner'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by owner|Sort panel by file owner'
      ImageIndex = 38
      ShortCut = 16503
    end
    object RemoteSortByGroupAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Group'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by group|Sort panel by file group'
      ImageIndex = 39
      ShortCut = 16504
    end
    object RemoteSortByTypeAction2: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Type'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by type|Sort panel by file type'
      ImageIndex = 34
    end
    object CurrentSortAscendingAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = '&Ascending'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Ascending/descending|Toggle ascending/descending sort of current' +
        ' panel'
      ImageIndex = 37
    end
    object CurrentSortByNameAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Name'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by name|Sort current panel by name'
      ImageIndex = 31
      ShortCut = 16498
    end
    object CurrentSortByExtAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Extension'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by extension|Sort current panel by file name extension'
      ImageIndex = 32
      ShortCut = 16499
    end
    object CurrentSortBySizeAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Size'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by size|Sort current panel by file size'
      ImageIndex = 35
      ShortCut = 16501
    end
    object CurrentSortByTypeAction2: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Type'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by type|Sort current panel by file type'
      ImageIndex = 34
    end
    object CurrentSortByRightsAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Permissions/Attributes'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Sort by attributes/permissions|Sort current panel by attributes/' +
        'permissions'
      ImageIndex = 36
      ShortCut = 16502
    end
    object CurrentSortByChangedAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Modification'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort by time|Sort current panel by last modification time'
      ImageIndex = 33
      ShortCut = 16500
    end
    object CurrentSortByOwnerAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Owner'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Sort by owner|Sort current panel by file owner (remote panel onl' +
        'y)'
      ImageIndex = 38
      ShortCut = 16503
    end
    object CurrentSortByGroupAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Group'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 
        'Sort by group|Sort current panel by file group (remote panel onl' +
        'y)'
      ImageIndex = 39
      ShortCut = 16504
    end
    object SortColumnAscendingAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'Sort &Ascending'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort files ascending by selected column'
      ImageIndex = 41
    end
    object SortColumnDescendingAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'Sort &Descending'
      HelpKeyword = 'ui_file_panel#sorting_files'
      Hint = 'Sort files descending by selected column'
      ImageIndex = 40
    end
    object HomepageAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = 'Product &Homepage'
      Hint = 'Opens web browser and points it to project homepage '
      ImageIndex = 42
    end
    object HistoryPageAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&Version History'
      Hint = 'Opens web browser and points it to application history page'
    end
    object SaveCurrentSessionAction2: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Save Session as Site...'
      HelpKeyword = 'task_connections#saving'
      Hint = 'Save session as site|Save current session as site'
      ImageIndex = 43
    end
    object ShowHideRemoteNameColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Name'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide name column'
      ImageIndex = 44
    end
    object ShowHideRemoteExtColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Extension'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide extension column'
      ImageIndex = 45
    end
    object ShowHideRemoteSizeColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Size'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide size column'
      ImageIndex = 47
    end
    object ShowHideRemoteChangedColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Modification'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide modification column'
      ImageIndex = 48
    end
    object ShowHideRemoteRightsColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Permissions'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide permissions column'
      ImageIndex = 49
    end
    object ShowHideRemoteOwnerColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Owner'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide owner column'
      ImageIndex = 50
    end
    object ShowHideRemoteGroupColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Group'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide group column'
      ImageIndex = 51
    end
    object ShowHideRemoteLinkTargetColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Link Target'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide link target column'
      ImageIndex = 82
    end
    object ShowHideRemoteTypeColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Type'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide type column'
      ImageIndex = 46
    end
    object ShowHideLocalNameColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Name'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide name column'
      ImageIndex = 44
    end
    object ShowHideLocalExtColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Extension'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide extension column'
      ImageIndex = 45
    end
    object ShowHideLocalTypeColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Type'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide type column'
      ImageIndex = 46
    end
    object ShowHideLocalSizeColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Size'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide size column'
      ImageIndex = 47
    end
    object ShowHideLocalChangedColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Modification'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide modification column'
      ImageIndex = 48
    end
    object ShowHideLocalAttrColumnAction2: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Attributes'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      Hint = 'Show/hide attributes column'
      ImageIndex = 49
    end
    object CompareDirectoriesAction2: TAction
      Tag = 11
      Category = 'Command'
      Caption = '&Compare Directories'
      HelpKeyword = 'task_compare_directories'
      Hint = 'Compare directories|Mark different files between files panels'
      ImageIndex = 52
      ShortCut = 8305
    end
    object SynchronizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Keep Remote Directory up to Date...'
      HelpKeyword = 'task_keep_up_to_date'
      Hint = 
        'Keep remote directory up to date|Keep remote directory up to dat' +
        'e'
      ImageIndex = 53
      ShortCut = 16469
    end
    object ForumPageAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&Support Forum'
      Hint = 'Opens web browser and points it to support forum page'
    end
    object LocalAddBookmarkAction2: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Add Path to Bookmarks'
      HelpKeyword = 'task_navigate#bookmarks'
      Hint = 'Add to bookmarks|Add the current directory to bookmark list'
      ImageIndex = 54
      ShortCut = 16450
    end
    object RemoteAddBookmarkAction2: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Add Path to Bookmarks'
      HelpKeyword = 'task_navigate#bookmarks'
      Hint = 'Add to bookmarks|Add the current directory to bookmark list'
      ImageIndex = 54
      ShortCut = 16450
    end
    object ConsoleAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Open &Terminal'
      HelpKeyword = 'ui_console'
      Hint = 
        'Open terminal|Open terminal window that allow executing arbitrar' +
        'y command (with exception of commands that require user input)'
      ImageIndex = 55
      ShortCut = 24660
    end
    object PuttyAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Open in &PuTTY'
      HelpKeyword = 'integration_putty#open_putty'
      Hint = 
        'Open session in PuTTY|Execute PuTTY SSH terminal and opens curre' +
        'nt session with it'
      ImageIndex = 64
      ShortCut = 16464
    end
    object LocalExploreDirectoryAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Explore Directory'
      Hint = 'Opens Windows File Explorer with the current local directory'
      ImageIndex = 56
      ShortCut = 49221
    end
    object CurrentOpenAction: TAction
      Tag = 15
      Category = 'Focused Operation'
      Caption = '&Open'
      HelpKeyword = 'task_edit'
      Hint = 
        'Open document|Open selected document using application associate' +
        'd with document type'
      ImageIndex = 58
    end
    object SynchronizeBrowsingAction2: TAction
      Tag = 11
      Category = 'Command'
      AutoCheck = True
      Caption = 'Synchronize &Browsing'
      HelpKeyword = 'task_navigate#synchronize_browsing'
      Hint = 'Synchronize browsing|Synchronize browsing between both panels'
      ImageIndex = 59
      ShortCut = 49218
    end
    object CurrentAddEditLinkAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Edit &Link...'
      HelpKeyword = 'task_link'
      Hint = 
        'Add/edit link|Add new link/shortcut or edit selected link/shortc' +
        'ut'
      ImageIndex = 60
    end
    object CurrentAddEditLinkContextAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Edit &Link...'
      HelpKeyword = 'task_link'
      Hint = 'Edit link|Edit selected link/shortcut'
      ImageIndex = 60
    end
    object CloseApplicationAction2: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Quit'
      Hint = 
        'Exit application|Close application (any opened sessions are clos' +
        'ed)'
      ImageIndex = 61
    end
    object OpenedTabsAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = '&Opened Tabs'
      HelpKeyword = 'ui_tabs#switch'
      Hint = 'Select tab|Select tab to activate'
      ImageIndex = 62
    end
    object DuplicateTabAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = 'Du&plicate Tab'
      HelpKeyword = 'ui_tabs'
      Hint = 
        'Duplicate tab|Open new tab with the same folder (hold down Shift' +
        ' to open the tab in new window)'
      ImageIndex = 91
    end
    object NewLinkAction: TAction
      Tag = 12
      Category = 'Command'
      Caption = '&Link...'
      HelpKeyword = 'task_link'
      Hint = 'Create link|Create new link/shortcut'
      ImageIndex = 60
    end
    object CustomCommandsFileAction: TAction
      Tag = 14
      Category = 'Command'
      Caption = 'File &Custom Commands'
      HelpKeyword = 'custom_command'
      Hint = 'Execute custom commands with selected file(s)'
    end
    object CustomCommandsNonFileAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Static &Custom Commands'
      HelpKeyword = 'custom_command'
      Hint = 'Execute custom commands that do not operate with files'
    end
    object CustomCommandsCustomizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Customize...'
      HelpKeyword = 'ui_pref_commands'
      Hint = 'Customize custom commands'
      ImageIndex = 28
    end
    object CustomCommandsEnterAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Enter...'
      HelpKeyword = 'custom_command#executing_and_configuring'
      Hint = 'Enter ad hoc custom command'
      ImageIndex = 90
    end
    object CustomCommandsEnterFocusedAction: TAction
      Tag = 12
      Category = 'Command'
      Caption = '&Enter...'
      HelpKeyword = 'custom_command#executing_and_configuring'
      Hint = 'Enter ad hoc custom command'
      ImageIndex = 90
    end
    object CheckForUpdatesAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&Check for Updates'
      HelpKeyword = 'updates'
      Hint = 'Queries application homepage for updates'
      ImageIndex = 63
    end
    object DonatePageAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&Donate'
      Hint = 'Opens web browser and points it to project donation page'
    end
    object CustomCommandsLastAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'CustomCommandsLastAction'
      HelpKeyword = 'custom_command#executing_and_configuring'
    end
    object CustomCommandsLastFocusedAction: TAction
      Tag = 12
      Category = 'Command'
      Caption = 'CustomCommandsLastFocusedAction'
      HelpKeyword = 'custom_command#executing_and_configuring'
    end
    object FileSystemInfoAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Server/Protocol &Information'
      HelpKeyword = 'ui_fsinfo'
      Hint = 'Display server/protocol information'
      ImageIndex = 17
    end
    object ClearCachesAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Clea&r Caches'
      HelpKeyword = 'directory_cache'
      Hint = 'Clear directory listing and directory changes caches'
    end
    object FullSynchronizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Synchronize...'
      HelpKeyword = 'task_synchronize_full'
      Hint = 'Synchronize local directory with remote directory'
      ImageIndex = 66
      ShortCut = 16467
    end
    object RemoteMoveToFocusedAction: TAction
      Tag = 12
      Category = 'Remote Focused Operation'
      Caption = 'Mo&ve To...'
      HelpKeyword = 'task_move_duplicate#move'
      Hint = 
        'Move|Move selected remote file(s) to another remote directory or' +
        ' another name'
      ImageIndex = 100
    end
    object ShowHiddenFilesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Show/Hide &Hidden Files'
      HelpKeyword = 'ui_file_panel#hidden_files'
      Hint = 'Toggle showing hidden files in panel(s)'
      ShortCut = 49224
    end
    object FormatSizeBytesNoneAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Bytes'
      HelpKeyword = 'ui_pref_panels#common'
      Hint = 'Show files sizes in bytes'
    end
    object LocalPathToClipboardAction2: TAction
      Tag = 15
      Category = 'Local Directory'
      Caption = 'Copy &Path to Clipboard'
      HelpKeyword = 'filenames#cwd'
      Hint = 'Copy the current path to the clipboard'
    end
    object RemotePathToClipboardAction2: TAction
      Tag = 15
      Category = 'Remote Directory'
      Caption = 'Copy &Path to Clipboard'
      HelpKeyword = 'filenames#cwd'
      Hint = 'Copy the current path to the clipboard'
    end
    object FileListToCommandLineAction: TAction
      Tag = 11
      Category = 'Selected Operation'
      Caption = 'Insert to Command &Line'
      HelpKeyword = 'filenames#command_line'
      Hint = 'Insert name(s) of selected file(s) to command line'
      ShortCut = 16397
    end
    object FileListToClipboardAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Copy to &Clipboard'
      HelpKeyword = 'filenames#file_name'
      Hint = 'Copy name(s) of selected file(s) to clipboard'
      ShortCut = 24643
    end
    object FullFileListToClipboardAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Copy to Clipboard (Include &Paths)'
      HelpKeyword = 'filenames#file_name'
      Hint = 'Copy name(s) including path of selected file(s) to clipboard'
      ShortCut = 49219
    end
    object QueueGoToAction: TAction
      Tag = 15
      Category = 'Queue'
      Caption = '&Go To'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Go to transfer queue list'
      ImageIndex = 74
      ShortCut = 16465
    end
    object QueueItemUpAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Move &Up'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Move selected queue item up to be processed earlier'
      ImageIndex = 72
    end
    object QueueItemDownAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Move &Down'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Move selected queue item down to be processed later'
      ImageIndex = 73
    end
    object QueueToggleShowAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Queue'
      Hint = 'Show/hide queue list'
      ImageIndex = 74
    end
    object QueueShowAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show'
      HelpKeyword = 'ui_queue'
      Hint = 'Show queue list'
    end
    object QueueHideWhenEmptyAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Hide when &Empty'
      HelpKeyword = 'ui_queue'
      Hint = 'Hide queue list when it is empty'
    end
    object QueueHideAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Hide'
      HelpKeyword = 'ui_queue'
      Hint = 'Hide queue list'
    end
    object QueueToolbarAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Toolbar'
      Hint = 'Hide/show queue list toolbar (on queue list panel)'
    end
    object QueueFileListAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&File List'
      Hint = 'Hide/show full queue file list'
    end
    object QueueResetLayoutColumnsAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Reset Columns Layout'
      Hint = 'Reset to the default layout of list columns'
    end
    object QueuePreferencesAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Customize...'
      HelpKeyword = 'ui_pref_background'
      Hint = 'Customize queue list'
      ImageIndex = 28
    end
    object PasteAction3: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Paste from Clipboard'
      HelpKeyword = 'clipboard'
      Hint = 
        'Paste files from the clipboard to the current directory in the a' +
        'ctive panel; or open a path from the clipboard in the active pan' +
        'el; or open a session URL from the clipboard'
      ImageIndex = 75
      ShortCut = 16470
    end
    object NewFileAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&File...'
      HelpKeyword = 'task_edit'
      Hint = 'Create file|Create new file and open it in editor'
      ImageIndex = 77
    end
    object EditorListCustomizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Configure...'
      HelpKeyword = 'ui_pref_editor'
      Hint = 'Customize editors'
      ImageIndex = 28
    end
    object RemoteCopyToFocusedAction: TAction
      Tag = 12
      Category = 'Remote Focused Operation'
      Caption = '&Duplicate...'
      HelpKeyword = 'task_move_duplicate#duplicate'
      Hint = 
        'Duplicate|Duplicate selected remote file(s) to another remote di' +
        'rectory or another name'
      ImageIndex = 78
    end
    object RemoteCopyToAction: TAction
      Tag = 14
      Category = 'Remote Selected Operation'
      Caption = 'Du&plicate...'
      HelpKeyword = 'task_move_duplicate#duplicate'
      Hint = 
        'Duplicate|Duplicate selected remote file(s) to another remote di' +
        'rectory or another name'
      ImageIndex = 78
    end
    object FileGenerateUrlAction2: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Generate File &URL...'
      HelpKeyword = 'ui_generateurl'
      Hint = 'Generate URL'#39's of selected file(s)'
    end
    object TableOfContentsAction: TAction
      Tag = 12
      Category = 'Help'
      Caption = '&Contents'
      Hint = 
        'Opens web browser and points it to documentation table of conten' +
        'ts'
      ImageIndex = 79
      ShortCut = 112
    end
    object FileListFromClipboardAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Transfer Files in Clipboard'
      HelpKeyword = 'clipboard'
      Hint = 'Transfer files whose names are in clipboard'
    end
    object LocalCopyAction: TAction
      Tag = 9
      Category = 'Local Selected Operation'
      Caption = 'Up&load...'
      HelpKeyword = 'task_upload'
      Hint = 'Upload|Upload selected local file(s) to remote directory'
      ImageIndex = 88
    end
    object CurrentDeleteAlternativeAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Delete'
      HelpKeyword = 'task_delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object CurrentEditWithAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Edit &With...'
      HelpKeyword = 'task_edit'
      Hint = 'Edit With|Edit selected file(s) using editor of your choice'
    end
    object DownloadPageAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&Download'
      Hint = 'Opens web browser and points it to application download page'
    end
    object UpdatesPreferencesAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = 'Confi&gure...'
      HelpKeyword = 'ui_pref_updates'
      Hint = 'Configure automatic check for application updates'
      ImageIndex = 28
    end
    object FormatSizeBytesKilobytesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Kilobytes'
      HelpKeyword = 'ui_pref_panels#common'
      Hint = 'Show files sizes in kilobytes'
    end
    object FormatSizeBytesShortAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Short format'
      HelpKeyword = 'ui_pref_panels#common'
      Hint = 'Show files sizes in short format'
    end
    object PresetsPreferencesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Configure...'
      HelpKeyword = 'ui_pref_transfer'
      Hint = 'Configure transfers settings presets'
      ImageIndex = 28
    end
    object LockToolbarsAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Lock Toolbars'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Prevents moving and (un)docking of all toolbars'
    end
    object SelectiveToolbarTextAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Show Selective Text Labels'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Show text labels for selected important commands on toolbars'
    end
    object ToolbarIconSizeAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'T&oolbar Icons Size'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Change toolbar icons size'
    end
    object ToolbarIconSizeNormalAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Normal'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Show normal toolbar icons'
    end
    object ToolbarIconSizeLargeAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Large'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Show large toolbar icons'
    end
    object ToolbarIconSizeVeryLargeAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Very Large'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Show very large toolbar icons'
    end
    object CustomCommandsBandAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Custom Co&mmand Buttons'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Hide/show custom commands toolbar'
    end
    object ColorMenuAction2: TAction
      Tag = 15
      Category = 'View'
      Caption = 'C&olor'
      HelpKeyword = 'task_connections#session_color'
      Hint = 'Change color of the current tab'
    end
    object AutoReadDirectoryAfterOpAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Auto&matically Reload Directory'
      Hint = 'Toggle automatic reload of remote directory after operation'
      ShortCut = 49234
    end
    object QueueItemPauseAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Suspend'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Suspend selected queue item'
      ImageIndex = 83
    end
    object QueueItemResumeAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Resume'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Resume selected suspended queue item'
      ImageIndex = 70
    end
    object QueuePauseAllAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Suspend All'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Suspend all running queue items'
      ImageIndex = 84
    end
    object QueueResumeAllAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Resume All'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Resume all suspended queue items'
      ImageIndex = 85
    end
    object QueueDeleteAllDoneAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Delete All &Completed'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Remove all completed queue items'
      ImageIndex = 99
    end
    object QueueEnableAction: TAction
      Tag = 15
      Category = 'Queue'
      Caption = '&Process Queue'
      HelpKeyword = 'ui_queue#manage'
      Hint = 
        'Enable queue processing|Enable queue processing (pending queue i' +
        'tems will not start, when queue processing is disabled)'
      ImageIndex = 96
      ShortCut = 24657
    end
    object QueueDisconnectOnceEmptyAction2: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Disconnect Session'
      HelpKeyword = 'ui_queue'
      Hint = 'Disconnect the session once the queue is empty'
      ImageIndex = 87
    end
    object RestoreSelectionAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = '&Restore Selection'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Restore previous selection'
      ImageIndex = 86
      ShortCut = 24659
    end
    object LocalSelectAction2: TAction
      Tag = 12
      Category = 'Selection'
      Caption = 'Sele&ct Files...'
      HelpKeyword = 'ui_select'
      Hint = 'Select|Select files by mask'
      ImageIndex = 19
    end
    object LocalUnselectAction2: TAction
      Tag = 12
      Category = 'Selection'
      Caption = '&Unselect Files...'
      HelpKeyword = 'ui_select'
      Hint = 'Unselect|Unselect files by mask'
      ImageIndex = 20
    end
    object LocalSelectAllAction2: TAction
      Tag = 12
      Category = 'Selection'
      Caption = 'Select &All'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Select all files'
      ImageIndex = 21
    end
    object CurrentEditFocusedAction: TAction
      Tag = 15
      Category = 'Focused Operation'
      Caption = '&Edit'
      HelpKeyword = 'task_edit'
      Hint = 'Edit|Edit selected file(s)'
      ImageIndex = 57
    end
    object CurrentEditWithFocusedAction: TAction
      Tag = 15
      Category = 'Focused Operation'
      Caption = 'Edit &With...'
      HelpKeyword = 'task_edit'
      Hint = 'Edit With|Edit selected file(s) using editor of your choice'
    end
    object NewDirAction: TAction
      Tag = 12
      Category = 'Command'
      Caption = '&Directory...'
      HelpKeyword = 'task_create_directory'
      Hint = 'Create directory|Create new directory'
      ImageIndex = 5
    end
    object QueueShutDownOnceEmptyAction2: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Shut Down Computer'
      HelpKeyword = 'ui_queue'
      Hint = 'Shut down the computer once the queue is empty'
      ImageIndex = 93
    end
    object QueueSuspendOnceEmptyAction2: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Slee&p Computer'
      HelpKeyword = 'ui_queue'
      Hint = 'Put the computer into sleep mode once the queue is empty'
      ImageIndex = 105
    end
    object QueueIdleOnceEmptyAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Stay Idle'
      Checked = True
      HelpKeyword = 'ui_queue'
      Hint = 'Stay idle once the queue is empty'
      ImageIndex = 94
    end
    object QueueCycleOnceEmptyAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Once &Empty'
      HelpKeyword = 'ui_queue'
      Hint = 'Toggle action to perform once the queue list is empty'
      ImageIndex = 94
    end
    object QueueItemSpeedAction: TTBEditAction
      Tag = 12
      Category = 'Queue'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Change speed limit of selected queue item'
      ImageIndex = 109
      EditCaption = '&Speed'
    end
    object QueueDeleteAllAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Cancel All'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Remove all queue items'
      ImageIndex = 106
    end
    object LocalFilterAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Filter...'
      HelpKeyword = 'ui_file_panel#filtering'
      Hint = 'Filter|Filter displayed files'
      ImageIndex = 92
      ShortCut = 49222
    end
    object RemoteFilterAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Filter...'
      HelpKeyword = 'ui_file_panel#filtering'
      Hint = 'Filter|Filter displayed files'
      ImageIndex = 92
      ShortCut = 49222
    end
    object RemoteFindFilesAction2: TAction
      Tag = 15
      Category = 'Remote Directory'
      Caption = '&Find Files...'
      HelpKeyword = 'task_find'
      Hint = 'Find files|Find files and directories'
      ImageIndex = 95
    end
    object CurrentEditInternalAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Internal editor'
      HelpKeyword = 'task_edit'
      Hint = 'Edit (internal)|Edit selected file(s) using internal editor'
    end
    object SaveWorkspaceAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = 'Save Wor&kspace...'
      HelpKeyword = 'workspace'
      Hint = 'Save workspace|Save workspace'
      ImageIndex = 102
    end
    object LocalRenameAction2: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Rename'
      HelpKeyword = 'task_rename'
      Hint = 'Rename|Rename selected file'
      ImageIndex = 3
    end
    object LocalEditAction2: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Edit'
      HelpKeyword = 'task_edit'
      Hint = 'Edit|Edit selected file(s)'
      ImageIndex = 57
    end
    object LocalMoveAction: TAction
      Tag = 9
      Category = 'Local Selected Operation'
      Caption = 'Upload and Dele&te...'
      HelpKeyword = 'task_upload'
      Hint = 
        'Upload and Delete|Upload selected local file(s) to remote direct' +
        'ory and delete original'
      ImageIndex = 98
    end
    object LocalCreateDirAction3: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Directory...'
      HelpKeyword = 'task_create_directory'
      Hint = 'Create directory|Create new directory'
      ImageIndex = 5
    end
    object LocalDeleteAction2: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Delete'
      HelpKeyword = 'task_delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object LocalPropertiesAction2: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Properties'
      HelpKeyword = 'task_properties'
      Hint = 'Properties|Display/change properties of selected file(s)'
      ImageIndex = 4
    end
    object LocalAddEditLinkAction3: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Shortcut...'
      HelpKeyword = 'task_link'
      Hint = 
        'Add/edit link|Add new link/shortcut or edit selected link/shortc' +
        'ut'
      ImageIndex = 60
    end
    object RemoteRenameAction2: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Rename'
      HelpKeyword = 'task_rename'
      Hint = 'Rename|Rename selected file'
      ImageIndex = 3
    end
    object RemoteEditAction2: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Edit'
      HelpKeyword = 'task_edit'
      Hint = 'Edit|Edit selected file(s)'
      ImageIndex = 57
    end
    object RemoteMoveAction: TAction
      Tag = 14
      Category = 'Remote Selected Operation'
      Caption = 'Download and Dele&te...'
      HelpKeyword = 'task_download'
      Hint = 
        'Download and Delete|Download selected remote file(s) to local di' +
        'rectory and delete original'
      ImageIndex = 97
    end
    object RemoteCreateDirAction3: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Directory...'
      HelpKeyword = 'task_create_directory'
      Hint = 'Create directory|Create new directory'
      ImageIndex = 5
    end
    object RemoteDeleteAction2: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Delete'
      HelpKeyword = 'task_delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object RemotePropertiesAction2: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Properties'
      HelpKeyword = 'task_properties'
      Hint = 'Properties|Display/change properties of selected file(s)'
      ImageIndex = 4
    end
    object RemoteAddEditLinkAction3: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Link...'
      HelpKeyword = 'task_link'
      Hint = 
        'Add/edit link|Add new link/shortcut or edit selected link/shortc' +
        'ut'
      ImageIndex = 60
    end
    object RemoteSelectAction2: TAction
      Tag = 12
      Category = 'Selection'
      Caption = 'Sele&ct Files...'
      HelpKeyword = 'ui_select'
      Hint = 'Select|Select files by mask'
      ImageIndex = 19
    end
    object RemoteUnselectAction2: TAction
      Tag = 12
      Category = 'Selection'
      Caption = '&Unselect Files...'
      HelpKeyword = 'ui_select'
      Hint = 'Unselect|Unselect files by mask'
      ImageIndex = 20
    end
    object RemoteSelectAllAction2: TAction
      Tag = 12
      Category = 'Selection'
      Caption = 'Select &All'
      HelpKeyword = 'ui_file_panel#selecting_files'
      Hint = 'Select all files'
      ImageIndex = 21
    end
    object LocalMoveFocusedAction: TAction
      Tag = 8
      Category = 'Local Focused Operation'
      Caption = 'Upload and Dele&te...'
      HelpKeyword = 'task_upload'
      Hint = 
        'Upload and Delete|Upload selected local file(s) to remote direct' +
        'ory and delete original'
      ImageIndex = 98
    end
    object CurrentEditInternalFocusedAction: TAction
      Tag = 15
      Category = 'Focused Operation'
      Caption = '&Internal editor'
      HelpKeyword = 'task_edit'
      Hint = 'Edit (internal)|Edit selected file(s) using internal editor'
    end
    object CurrentSystemMenuFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&System Menu'
      Hint = 
        'Display system file context menu (in Properties you can opt to d' +
        'isplay it by default instead of built-in menu)'
    end
    object SessionGenerateUrlAction2: TAction
      Tag = 15
      Category = 'Session'
      Caption = 'Generate Session &URL/Code...'
      HelpKeyword = 'ui_generateurl'
      Hint = 'Generate URL or code for current session'
    end
    object SelectSameExtAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'Select Files with Same &Extension'
      Hint = 
        'Select all files with the same extension as currently focused fi' +
        'le'
      ShortCut = 32875
    end
    object UnselectSameExtAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'Unselect Files with Same E&xtension'
      Hint = 
        'Unselect all files with the same extension as currently focused ' +
        'file'
      ShortCut = 32877
    end
    object GoToAddressAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'GoToAddressAction'
      SecondaryShortCuts.Strings = (
        'Alt+D')
      ShortCut = 16460
    end
    object LockAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Lock'
      Hint = 'Lock selected file(s)'
    end
    object UnlockAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Unlock'
      Hint = 'Unlock selected file(s)'
    end
    object TipsAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = 'Show &Tips'
      HelpKeyword = 'ui_tips'
      Hint = 'Displays tips on using WinSCP'
      ImageIndex = 110
    end
    object ChangePasswordAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Change Password...'
      HelpKeyword = 'task_change_password'
      Hint = 'Change account password'
    end
    object PrivateKeyUploadAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Install Public Key into Server...'
      HelpKeyword = 'guide_public_key'
      Hint = 'Install public key for authentication into the server'
    end
    object RemoteNewFileAction: TAction
      Tag = 15
      Category = 'Remote Selected Operation'
      Caption = '&File...'
      HelpKeyword = 'task_edit'
      Hint = 'Create file|Create new file and open it in editor'
      ImageIndex = 77
    end
    object RemoteCalculateDirectorySizesAction: TAction
      Tag = 12
      Category = 'Remote Selected Operation'
      Caption = '&Calculate Directory Sizes'
      HelpKeyword = 'ui_file_panel#directory_sizes'
      Hint = 
        'Calculate sizes of the selected directories and display them in ' +
        'the file panel'
      ShortCut = 40973
    end
    object LocalNewFileAction: TAction
      Tag = 15
      Category = 'Local Selected Operation'
      Caption = '&File...'
      HelpKeyword = 'task_edit'
      Hint = 'Create file|Create new file and open it in editor'
      ImageIndex = 77
    end
    object CustomizeToolbarAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Customize Toolbar'
      HelpKeyword = 'ui_toolbars'
      Hint = 'Show/hide toolbar buttons'
    end
    object RenameTabAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = '&Rename Tab'
      HelpKeyword = 'ui_tabs#renaming'
      Hint = 'Rename tab|Change the name of the current tab'
    end
    object CurrentCopyToClipboardAction2: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Copy to Clipboard'
      HelpKeyword = 'clipboard#copy'
      Hint = 'Copy the selected files to the clipboard'
      ImageIndex = 111
      ShortCut = 16451
    end
    object FileColorsPreferencesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'File &Colors...'
      HelpKeyword = 'ui_pref_file_colors'
      Hint = 'Configure file color rules'
    end
    object CurrentCopyToClipboardFocusedAction2: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Copy to Clipboard'
      HelpKeyword = 'clipboard#copy'
      Hint = 'Copy the selected files to the clipboard'
      ImageIndex = 111
      ShortCut = 16451
    end
    object CommanderLocalPanelAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Left Panel'
      HelpKeyword = 'ui_file_panel'
      Hint = 'Change left panel layout'
    end
    object CommanderRemotePanelAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Right Panel'
      HelpKeyword = 'ui_file_panel'
      Hint = 'Change right panel layout'
    end
    object RemoteExploreDirectoryAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Explore Directory'
      Hint = 'Open Windows File Explorer with the current local directory'
      ImageIndex = 56
      ShortCut = 49221
    end
    object LocalLocalCopyAction: TAction
      Tag = 9
      Category = 'Local Selected Operation'
      Caption = '&Copy...'
      Hint = 'Copy the selected file(s) to another directory or another name'
      ImageIndex = 78
    end
    object LocalLocalMoveAction: TAction
      Tag = 9
      Category = 'Local Selected Operation'
      Caption = '&Move...'
      Hint = 'Move the selected file(s) to another directory or rename them'
      ImageIndex = 100
    end
    object LocalOtherCopyAction: TAction
      Tag = 14
      Category = 'Local Selected Operation'
      Caption = '&Copy...'
      Hint = 'Copy the selected file(s) to another directory or another name'
      ImageIndex = 112
    end
    object LocalOtherMoveAction: TAction
      Tag = 14
      Category = 'Local Selected Operation'
      Caption = '&Move...'
      Hint = 'Move the selected file(s) to another directory or rename them'
      ImageIndex = 113
    end
    object LocalCalculateDirectorySizesAction: TAction
      Tag = 12
      Category = 'Local Selected Operation'
      Caption = '&Calculate Directory Sizes'
      HelpKeyword = 'ui_file_panel#directory_sizes'
      Hint = 
        'Calculate sizes of the selected directories and display them in ' +
        'the file panel'
      ShortCut = 40973
    end
    object LocalLocalCopyFocusedAction: TAction
      Tag = 8
      Category = 'Local Focused Operation'
      Caption = '&Copy...'
      Hint = 'Copy the selected file(s) to another directory or another name'
      ImageIndex = 78
    end
    object LocalLocalMoveFocusedAction: TAction
      Tag = 8
      Category = 'Local Focused Operation'
      Caption = '&Move...'
      Hint = 'Move the selected file(s) to another directory or rename them'
      ImageIndex = 100
    end
    object NewTabAction: TAction
      Tag = 15
      Category = 'Tab'
      Caption = '&New Tab'
      HelpKeyword = 'ui_tabs#working'
      Hint = 'Open new tab'
      ImageIndex = 115
    end
    object NewLocalTabAction: TAction
      Tag = 11
      Category = 'Tab'
      Caption = '&Local Tab'
      HelpKeyword = 'ui_tabs#working'
      Hint = 'Open new tab with two local panels'
      ImageIndex = 115
      ShortCut = 24654
    end
    object NewRemoteTabAction: TAction
      Tag = 11
      Category = 'Tab'
      Caption = '&Remote Tab'
      HelpKeyword = 'ui_tabs#working'
      Hint = 'Open new tab with one local panel and one remote session panel'
      ImageIndex = 25
      ShortCut = 16462
    end
    object DefaultToNewRemoteTabAction: TAction
      Tag = 11
      Category = 'Tab'
      Caption = '&Default to Remote Tab'
      Hint = 
        'When turned on, the New Tab command opens a new remote tab. Othe' +
        'rwise it opens a new local tab.'
    end
    object CalculateDirectorySizesAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = '&Calculate Directory Sizes'
      HelpKeyword = 'ui_file_panel#directory_sizes'
      Hint = 
        'Calculate sizes of the selected directories and display them in ' +
        'the file panel'
      ShortCut = 40973
    end
    object LocalOtherDirAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = 'Path fro&m Other Panel'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Open the same directory as in the other panel'
      ShortCut = 16574
    end
    object RemoteOtherDirAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = 'Path fro&m Other Panel'
      HelpKeyword = 'task_navigate#special_commands'
      Hint = 'Open the same directory as in the other panel'
      ShortCut = 16574
    end
    object IncrementalSearchStartAction: TAction
      Tag = 11
      Category = 'Command'
      Caption = 'IncrementalSearchStartAction'
      ShortCut = 16454
    end
    object RemoteThumbnailAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = '&Thumbnails'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'Thumbnails|View thumbnails'
    end
    object LocalReportAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = '&Details'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'Details|View details'
      ImageIndex = 11
    end
    object LocalThumbnailAction: TAction
      Tag = 15
      Category = 'Style'
      Caption = '&Thumbnails'
      HelpKeyword = 'ui_file_panel#view_style'
      Hint = 'Thumbnails|View thumbnails'
    end
  end
  object ExplorerBarPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 192
    Top = 336
    object Address2: TTBXItem
      Action = ExplorerAddressBandAction
    end
    object StandardButtons1: TTBXItem
      Action = ExplorerToolbarBandAction
    end
    object SelectionButtons1: TTBXItem
      Action = ExplorerSelectionBandAction
    end
    object SessionButtons2: TTBXItem
      Action = ExplorerSessionBandAction2
    end
    object PreferencesButtons1: TTBXItem
      Action = ExplorerPreferencesBandAction
    end
    object SortButtons3: TTBXItem
      Action = ExplorerSortBandAction
    end
    object TBXItem3: TTBXItem
      Action = ExplorerUpdatesBandAction
    end
    object TBXItem4: TTBXItem
      Action = ExplorerTransferBandAction
    end
    object TBXItem16: TTBXItem
      Action = ExplorerCustomCommandsBandAction
    end
    object TBXSeparatorItem27: TTBXSeparatorItem
    end
    object TBXItem7: TTBXItem
      Action = LockToolbarsAction
    end
    object TBXItem48: TTBXItem
      Action = SelectiveToolbarTextAction
    end
    object TBXSubmenuItem15: TTBXSubmenuItem
      Action = ToolbarIconSizeAction
      object TBXItem127: TTBXItem
        Action = ToolbarIconSizeNormalAction
        RadioItem = True
      end
      object TBXItem120: TTBXItem
        Action = ToolbarIconSizeLargeAction
        RadioItem = True
      end
      object TBXItem128: TTBXItem
        Action = ToolbarIconSizeVeryLargeAction
        RadioItem = True
      end
    end
    object TBXSubmenuItem4: TTBXSubmenuItem
      Action = CustomizeToolbarAction
    end
    object N5: TTBXSeparatorItem
    end
    object SessionsTabs2: TTBXItem
      Action = SessionsTabsAction2
    end
    object StatusBar2: TTBXItem
      Action = StatusBarAction
    end
    object N72: TTBXSeparatorItem
    end
    object Queue7: TTBXSubmenuItem
      Caption = '&Queue'
      HelpKeyword = 'ui_queue'
      Hint = 'Configure queue list'
      object Show6: TTBXItem
        Action = QueueShowAction
        RadioItem = True
      end
      object HidewhenEmpty6: TTBXItem
        Action = QueueHideWhenEmptyAction
        RadioItem = True
      end
      object Hide5: TTBXItem
        Action = QueueHideAction
        RadioItem = True
      end
      object N71: TTBXSeparatorItem
      end
      object Toolbar5: TTBXItem
        Action = QueueToolbarAction
      end
      object TBXItem85: TTBXItem
        Action = QueueFileListAction
      end
      object N70: TTBXSeparatorItem
      end
      object Customize5: TTBXItem
        Action = QueuePreferencesAction
      end
    end
    object Tree4: TTBXItem
      Action = RemoteTreeAction
    end
  end
  object SessionIdleTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = SessionIdleTimerTimer
    Left = 32
    Top = 336
  end
  object CommanderBarPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 424
    Top = 264
    object CommandsButtons2: TTBXItem
      Action = CommanderCommandsBandAction
    end
    object SessionButtons5: TTBXItem
      Action = CommanderSessionBandAction2
    end
    object PreferencesButtons4: TTBXItem
      Action = CommanderPreferencesBandAction
    end
    object SortButtons2: TTBXItem
      Action = CommanderSortBandAction
    end
    object TBXItem2: TTBXItem
      Action = CommanderUpdatesBandAction
    end
    object TBXItem5: TTBXItem
      Action = CommanderTransferBandAction
    end
    object TBXItem15: TTBXItem
      Action = CommanderCustomCommandsBandAction
    end
    object TBXSeparatorItem24: TTBXSeparatorItem
    end
    object TBXItem6: TTBXItem
      Action = LockToolbarsAction
    end
    object TBXItem46: TTBXItem
      Action = SelectiveToolbarTextAction
    end
    object TBXSubmenuItem14: TTBXSubmenuItem
      Action = ToolbarIconSizeAction
      object TBXItem123: TTBXItem
        Action = ToolbarIconSizeNormalAction
        RadioItem = True
      end
      object TBXItem117: TTBXItem
        Action = ToolbarIconSizeLargeAction
        RadioItem = True
      end
      object TBXItem126: TTBXItem
        Action = ToolbarIconSizeVeryLargeAction
        RadioItem = True
      end
    end
    object TBXItem77: TTBXSubmenuItem
      Action = CustomizeToolbarAction
    end
    object N26: TTBXSeparatorItem
    end
    object SessionsTabs1: TTBXItem
      Action = SessionsTabsAction2
    end
    object CommandLine2: TTBXItem
      Action = CommandLinePanelAction
    end
    object CommandsToolbar1: TTBXItem
      Action = ToolBar2Action
    end
    object StatusBar8: TTBXItem
      Action = StatusBarAction
    end
    object N27: TTBXSeparatorItem
    end
    object LocalPanel1: TTBXSubmenuItem
      Action = CommanderLocalPanelAction
      object HistoryButtons3: TTBXItem
        Action = CommanderLocalHistoryBandAction2
      end
      object NavigationButtons3: TTBXItem
        Action = CommanderLocalNavigationBandAction2
      end
      object TBXItem40: TTBXItem
        Action = CommanderLocalFileBandAction2
      end
      object TBXItem43: TTBXItem
        Action = CommanderLocalSelectionBandAction2
      end
      object N23: TTBXSeparatorItem
      end
      object Tree7: TTBXItem
        Action = LocalTreeAction
      end
      object N77: TTBXSeparatorItem
      end
      object StatusBar6: TTBXItem
        Action = LocalStatusBarAction2
      end
    end
    object RemotePanel2: TTBXSubmenuItem
      Action = CommanderRemotePanelAction
      object HistoryButtons4: TTBXItem
        Action = CommanderRemoteHistoryBandAction2
      end
      object NavigationButtons4: TTBXItem
        Action = CommanderRemoteNavigationBandAction2
      end
      object TBXItem41: TTBXItem
        Action = CommanderRemoteFileBandAction2
      end
      object TBXItem42: TTBXItem
        Action = CommanderRemoteSelectionBandAction2
      end
      object N25: TTBXSeparatorItem
      end
      object Tree8: TTBXItem
        Action = RemoteTreeAction
      end
      object N78: TTBXSeparatorItem
      end
      object StatusBar7: TTBXItem
        Action = RemoteStatusBarAction2
      end
    end
    object Options1: TTBXSubmenuItem
      Caption = '&Queue'
      HelpKeyword = 'ui_queue'
      Hint = 'Configure queue list'
      object Show5: TTBXItem
        Action = QueueShowAction
        RadioItem = True
      end
      object HidewhenEmpty5: TTBXItem
        Action = QueueHideWhenEmptyAction
        RadioItem = True
      end
      object Hide4: TTBXItem
        Action = QueueHideAction
        RadioItem = True
      end
      object N69: TTBXSeparatorItem
      end
      object Toolbar4: TTBXItem
        Action = QueueToolbarAction
      end
      object TBXItem84: TTBXItem
        Action = QueueFileListAction
      end
      object N68: TTBXSeparatorItem
      end
      object Customize4: TTBXItem
        Action = QueuePreferencesAction
      end
    end
  end
  object RemotePanelPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 312
    Top = 264
    object TBXSubmenuItem8: TTBXSubmenuItem
      Caption = '&Go To'
      HelpKeyword = 'task_navigate'
      Hint = 'Go to directory'
      object TBXItem86: TTBXItem
        Action = RemoteOpenDirAction
      end
      object TBXItem99: TTBXItem
        Action = RemoteExploreDirectoryAction
      end
      object TBXSeparatorItem13: TTBXSeparatorItem
      end
      object TBXItem87: TTBXItem
        Action = RemoteParentDirAction
      end
      object TBXItem88: TTBXItem
        Action = RemoteRootDirAction
      end
      object TBXItem89: TTBXItem
        Action = RemoteHomeDirAction
      end
      object TBXItem109: TTBXItem
        Action = RemoteOtherDirAction
      end
      object TBXSeparatorItem14: TTBXSeparatorItem
      end
      object TBXItem90: TTBXItem
        Action = RemoteBackAction
      end
      object TBXItem91: TTBXItem
        Action = RemoteForwardAction
      end
    end
    object TBXItem32: TTBXItem
      Action = RemoteRefreshAction
    end
    object TBXItem30: TTBXItem
      Action = RemoteAddBookmarkAction2
    end
    object TBXItem26: TTBXItem
      Action = RemoteFilterAction
    end
    object CopyPathtoClipboard1: TTBXItem
      Action = RemotePathToClipboardAction2
    end
    object N51: TTBXSeparatorItem
    end
    object HistoryButtons5: TTBXItem
      Action = CommanderRemoteHistoryBandAction2
    end
    object NavigationButtons5: TTBXItem
      Action = CommanderRemoteNavigationBandAction2
    end
    object TBXItem14: TTBXItem
      Action = CommanderRemoteFileBandAction2
    end
    object TBXItem45: TTBXItem
      Action = CommanderRemoteSelectionBandAction2
    end
    object TBXSeparatorItem26: TTBXSeparatorItem
    end
    object TBXItem37: TTBXItem
      Action = LockToolbarsAction
    end
    object TBXItem49: TTBXItem
      Action = SelectiveToolbarTextAction
    end
    object TBXSubmenuItem13: TTBXSubmenuItem
      Action = ToolbarIconSizeAction
      object TBXItem121: TTBXItem
        Action = ToolbarIconSizeNormalAction
        RadioItem = True
      end
      object TBXItem119: TTBXItem
        Action = ToolbarIconSizeLargeAction
        RadioItem = True
      end
      object TBXItem122: TTBXItem
        Action = ToolbarIconSizeVeryLargeAction
        RadioItem = True
      end
    end
    object TBXSubmenuItem9: TTBXSubmenuItem
      Action = CustomizeToolbarAction
    end
    object N28: TTBXSeparatorItem
    end
    object Tree5: TTBXItem
      Action = RemoteTreeAction
    end
    object N75: TTBXSeparatorItem
    end
    object StatusBar9: TTBXItem
      Action = RemoteStatusBarAction2
    end
  end
  object LocalPanelPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 312
    Top = 336
    object TBXSubmenuItem10: TTBXSubmenuItem
      Caption = '&Go To'
      HelpKeyword = 'task_navigate'
      Hint = 'Go to directory'
      object TBXItem92: TTBXItem
        Action = LocalOpenDirAction
      end
      object TBXItem93: TTBXItem
        Action = LocalExploreDirectoryAction
      end
      object TBXSeparatorItem15: TTBXSeparatorItem
      end
      object TBXItem94: TTBXItem
        Action = LocalParentDirAction
      end
      object TBXItem95: TTBXItem
        Action = LocalRootDirAction
      end
      object TBXItem96: TTBXItem
        Action = LocalHomeDirAction
      end
      object TBXItem113: TTBXItem
        Action = LocalOtherDirAction
      end
      object TBXSeparatorItem16: TTBXSeparatorItem
      end
      object TBXItem97: TTBXItem
        Action = LocalBackAction
      end
      object TBXItem98: TTBXItem
        Action = LocalForwardAction
      end
    end
    object TBXItem34: TTBXItem
      Action = LocalRefreshAction
    end
    object TBXItem27: TTBXItem
      Action = LocalFilterAction
    end
    object TBXItem31: TTBXItem
      Action = LocalAddBookmarkAction2
    end
    object CopyPathtoClipboard2: TTBXItem
      Action = LocalPathToClipboardAction2
    end
    object N52: TTBXSeparatorItem
    end
    object HistoryButtons6: TTBXItem
      Action = CommanderLocalHistoryBandAction2
    end
    object NavigationButtons6: TTBXItem
      Action = CommanderLocalNavigationBandAction2
    end
    object TBXItem39: TTBXItem
      Action = CommanderLocalFileBandAction2
    end
    object TBXItem44: TTBXItem
      Action = CommanderLocalSelectionBandAction2
    end
    object TBXSeparatorItem25: TTBXSeparatorItem
    end
    object TBXItem38: TTBXItem
      Action = LockToolbarsAction
    end
    object TBXItem47: TTBXItem
      Action = SelectiveToolbarTextAction
    end
    object TBXSubmenuItem16: TTBXSubmenuItem
      Action = ToolbarIconSizeAction
      object TBXItem129: TTBXItem
        Action = ToolbarIconSizeNormalAction
        RadioItem = True
      end
      object TBXItem118: TTBXItem
        Action = ToolbarIconSizeLargeAction
        RadioItem = True
      end
      object TBXItem130: TTBXItem
        Action = ToolbarIconSizeVeryLargeAction
        RadioItem = True
      end
    end
    object TBXSubmenuItem6: TTBXSubmenuItem
      Action = CustomizeToolbarAction
    end
    object N29: TTBXSeparatorItem
    end
    object Tree6: TTBXItem
      Action = LocalTreeAction
    end
    object N76: TTBXSeparatorItem
    end
    object StatusBar10: TTBXItem
      Action = LocalStatusBarAction2
    end
  end
  object LocalDirViewColumnPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 248
    Top = 88
    object SortAscending1: TTBXItem
      Action = SortColumnAscendingAction
    end
    object SortDescending1: TTBXItem
      Action = SortColumnDescendingAction
    end
    object LocalSortByExtColumnPopupItem: TTBXItem
      Action = LocalSortByExtAction2
    end
    object Hidecolumn1: TTBXItem
      Action = HideColumnAction
    end
    object N37: TTBXSeparatorItem
    end
    object LocalFormatSizeBytesPopupItem: TTBXSubmenuItem
      Caption = 'Show File Si&zes In'
      HelpKeyword = 'ui_pref_panels#common'
      Hint = 'Select files sizes display format'
      object TBXItem64: TTBXItem
        Action = FormatSizeBytesNoneAction
      end
      object TBXItem65: TTBXItem
        Action = FormatSizeBytesKilobytesAction
      end
      object TBXItem66: TTBXItem
        Action = FormatSizeBytesShortAction
      end
    end
    object LocalCalculateDirectorySizesPopupItem: TTBXItem
      Action = LocalCalculateDirectorySizesAction
    end
    object TBXSeparatorItem8: TTBXSeparatorItem
    end
    object LocalColumnsSubmenuItem: TTBXSubmenuItem
      Caption = '&Columns'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      object Name3: TTBXItem
        Action = ShowHideLocalNameColumnAction2
      end
      object Size3: TTBXItem
        Action = ShowHideLocalSizeColumnAction2
      end
      object Type2: TTBXItem
        Action = ShowHideLocalTypeColumnAction2
      end
      object Modification3: TTBXItem
        Action = ShowHideLocalChangedColumnAction2
      end
      object Attributes3: TTBXItem
        Action = ShowHideLocalAttrColumnAction2
      end
      object TBXSeparatorItem73: TTBXSeparatorItem
      end
      object TBXItem264: TTBXItem
        Action = AutoSizeLocalColumnsAction
      end
      object TBXItem112: TTBXItem
        Action = ResetLayoutLocalColumnsAction
      end
    end
  end
  object RemoteDirViewColumnPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 424
    Top = 88
    object MenuItem1: TTBXItem
      Action = SortColumnAscendingAction
      RadioItem = True
    end
    object MenuItem2: TTBXItem
      Action = SortColumnDescendingAction
      RadioItem = True
    end
    object RemoteSortByExtColumnPopupItem: TTBXItem
      Action = RemoteSortByExtAction2
    end
    object Hidecolumn2: TTBXItem
      Action = HideColumnAction
    end
    object N38: TTBXSeparatorItem
    end
    object RemoteFormatSizeBytesPopupItem: TTBXSubmenuItem
      Caption = 'Show File Si&zes In'
      HelpKeyword = 'ui_pref_panels#common'
      Hint = 'Select files sizes display format'
      object TBXItem67: TTBXItem
        Action = FormatSizeBytesNoneAction
      end
      object TBXItem53: TTBXItem
        Action = FormatSizeBytesKilobytesAction
      end
      object TBXItem55: TTBXItem
        Action = FormatSizeBytesShortAction
      end
    end
    object RemoteCalculateDirectorySizesPopupItem: TTBXItem
      Action = RemoteCalculateDirectorySizesAction
    end
    object TBXSeparatorItem7: TTBXSeparatorItem
    end
    object RemoteColumnsSubmenuItem: TTBXSubmenuItem
      Caption = '&Columns'
      HelpKeyword = 'ui_file_panel#selecting_columns'
      object Name4: TTBXItem
        Action = ShowHideRemoteNameColumnAction2
      end
      object Size4: TTBXItem
        Action = ShowHideRemoteSizeColumnAction2
      end
      object TBXItem8: TTBXItem
        Action = ShowHideRemoteTypeColumnAction2
      end
      object Modification4: TTBXItem
        Action = ShowHideRemoteChangedColumnAction2
      end
      object Permissions1: TTBXItem
        Action = ShowHideRemoteRightsColumnAction2
      end
      object Owner2: TTBXItem
        Action = ShowHideRemoteOwnerColumnAction2
      end
      object Group2: TTBXItem
        Action = ShowHideRemoteGroupColumnAction2
      end
      object TBXItem1: TTBXItem
        Action = ShowHideRemoteLinkTargetColumnAction2
      end
      object TBXSeparatorItem20: TTBXSeparatorItem
      end
      object TBXItem114: TTBXItem
        Action = AutoSizeRemoteColumnsAction
      end
      object TBXItem115: TTBXItem
        Action = ResetLayoutRemoteColumnsAction
      end
    end
  end
  object QueuePopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    OnPopup = QueuePopupPopup
    Options = [tboShowHint]
    Left = 360
    Top = 176
    object ShowQuery1: TTBXItem
      Action = QueueItemQueryAction
    end
    object ShowError1: TTBXItem
      Action = QueueItemErrorAction
    end
    object ShowPrompt1: TTBXItem
      Action = QueueItemPromptAction
    end
    object N53: TTBXSeparatorItem
    end
    object ExecuteNow1: TTBXItem
      Action = QueueItemExecuteAction
    end
    object TBXItem9: TTBXItem
      Action = QueueItemPauseAction
    end
    object TBXItem10: TTBXItem
      Action = QueueItemResumeAction
    end
    object Delete4: TTBXItem
      Action = QueueItemDeleteAction
    end
    object QueuePopupSpeedComboBoxItem: TTBXComboBoxItem
      Action = QueueItemSpeedAction
      ShowImage = True
      OnAdjustImageIndex = QueuePopupSpeedComboBoxItemAdjustImageIndex
    end
    object N54: TTBXSeparatorItem
    end
    object MoveUp1: TTBXItem
      Action = QueueItemUpAction
    end
    object MoveDown1: TTBXItem
      Action = QueueItemDownAction
    end
    object N67: TTBXSeparatorItem
    end
    object QueueEnableItem: TTBXItem
      Action = QueueEnableAction
    end
    object TBXSubmenuItem1: TTBXSubmenuItem
      Caption = '&All'
      HelpKeyword = 'ui_queue#manage'
      Hint = 'Mass queue management commands'
      object TBXItem11: TTBXItem
        Action = QueuePauseAllAction
      end
      object TBXItem12: TTBXItem
        Action = QueueResumeAllAction
      end
      object TBXItem142: TTBXItem
        Action = QueueDeleteAllAction
      end
      object TBXSeparatorItem5: TTBXSeparatorItem
      end
      object TBXItem51: TTBXItem
        Action = QueueDeleteAllDoneAction
      end
    end
    object TBXSubmenuItem3: TTBXSubmenuItem
      Action = QueueCycleOnceEmptyAction
      DropdownCombo = True
      object TBXItem28: TTBXItem
        Action = QueueIdleOnceEmptyAction
        RadioItem = True
      end
      object TBXItem13: TTBXItem
        Action = QueueDisconnectOnceEmptyAction2
        RadioItem = True
      end
      object TBXItem68: TTBXItem
        Action = QueueSuspendOnceEmptyAction2
        RadioItem = True
      end
      object TBXItem29: TTBXItem
        Action = QueueShutDownOnceEmptyAction2
        RadioItem = True
      end
    end
    object Queue2: TTBXSubmenuItem
      Caption = '&Options'
      HelpKeyword = 'ui_queue'
      Hint = 'Configure queue list'
      object Show4: TTBXItem
        Action = QueueShowAction
        RadioItem = True
      end
      object HidewhenEmpty4: TTBXItem
        Action = QueueHideWhenEmptyAction
        RadioItem = True
      end
      object Hide3: TTBXItem
        Action = QueueHideAction
        RadioItem = True
      end
      object N66: TTBXSeparatorItem
      end
      object Toolbar3: TTBXItem
        Action = QueueToolbarAction
      end
      object TBXItem83: TTBXItem
        Action = QueueFileListAction
      end
      object TBXSeparatorItem23: TTBXSeparatorItem
      end
      object TBXItem116: TTBXItem
        Action = QueueResetLayoutColumnsAction
      end
      object N65: TTBXSeparatorItem
      end
      object Customize3: TTBXItem
        Action = QueuePreferencesAction
      end
    end
  end
  object RemoteDirViewPopup: TTBXPopupMenu
    AutoPopup = False
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 360
    Top = 400
    object GoTo4: TTBXSubmenuItem
      Caption = '&Go To'
      HelpKeyword = 'task_navigate'
      Hint = 'Go to directory'
      object OpenDirectoryBookmark3: TTBXItem
        Action = RemoteOpenDirAction
      end
      object TBXItem100: TTBXItem
        Action = RemoteExploreDirectoryAction
      end
      object N81: TTBXSeparatorItem
      end
      object ParentDirectory4: TTBXItem
        Action = RemoteParentDirAction
      end
      object RootDirectory4: TTBXItem
        Action = RemoteRootDirAction
      end
      object HomeDirectory4: TTBXItem
        Action = RemoteHomeDirAction
      end
      object N80: TTBXSeparatorItem
      end
      object Back4: TTBXItem
        Action = RemoteBackAction
      end
      object Forward4: TTBXItem
        Action = RemoteForwardAction
      end
    end
    object Refresh4: TTBXItem
      Action = RemoteRefreshAction
    end
    object AddToBookmarks4: TTBXItem
      Action = RemoteAddBookmarkAction2
    end
    object TBXItem35: TTBXItem
      Action = RemoteFilterAction
    end
    object CopyPathtoClipboard6: TTBXItem
      Action = RemotePathToClipboardAction2
    end
    object N79: TTBXSeparatorItem
    end
    object TBXSubmenuItem26: TTBXSubmenuItem
      Caption = '&New'
      HelpKeyword = 'task_index'
      Hint = 'Create object|Create new object'
      object TBXItem135: TTBXItem
        Action = NewFileAction
      end
      object TBXItem136: TTBXItem
        Action = NewDirAction
      end
      object TBXItem209: TTBXItem
        Action = NewLinkAction
      end
    end
    object TBXItem75: TTBXItem
      Action = PasteAction3
    end
    object RemoteDirViewPopupCustomCommandsMenu: TTBXSubmenuItem
      Action = CustomCommandsNonFileAction
    end
  end
  object LocalDirViewPopup: TTBXPopupMenu
    AutoPopup = False
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 480
    Top = 400
    object GoTo5: TTBXSubmenuItem
      Caption = '&Go To'
      HelpKeyword = 'task_navigate'
      Hint = 'Go to directory'
      object OpenDirectoryBookmark4: TTBXItem
        Action = LocalOpenDirAction
      end
      object ExploreDirectory2: TTBXItem
        Action = LocalExploreDirectoryAction
      end
      object N84: TTBXSeparatorItem
      end
      object ParentDirectory5: TTBXItem
        Action = LocalParentDirAction
      end
      object RootDirectory5: TTBXItem
        Action = LocalRootDirAction
      end
      object HomeDirectory5: TTBXItem
        Action = LocalHomeDirAction
      end
      object N83: TTBXSeparatorItem
      end
      object Back5: TTBXItem
        Action = LocalBackAction
      end
      object Forward5: TTBXItem
        Action = LocalForwardAction
      end
    end
    object Refresh5: TTBXItem
      Action = LocalRefreshAction
    end
    object TBXItem36: TTBXItem
      Action = LocalFilterAction
    end
    object AddToBookmarks5: TTBXItem
      Action = LocalAddBookmarkAction2
    end
    object CopyPathtoClipboard7: TTBXItem
      Action = LocalPathToClipboardAction2
    end
    object N82: TTBXSeparatorItem
    end
    object TBXSubmenuItem7: TTBXSubmenuItem
      Caption = '&New'
      HelpKeyword = 'task_index'
      Hint = 'Create object|Create new object'
      object TBXItem70: TTBXItem
        Action = NewFileAction
      end
      object TBXItem71: TTBXItem
        Action = NewDirAction
      end
    end
    object TBXItem76: TTBXItem
      Action = PasteAction3
    end
  end
  object RemoteAddressPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 248
    Top = 400
    object TBXItem33: TTBXItem
      Action = RemoteRefreshAction
    end
    object TBXItem24: TTBXItem
      Action = RemoteAddBookmarkAction2
    end
    object TBXItem25: TTBXItem
      Action = RemotePathToClipboardAction2
    end
    object TBXSeparatorItem1: TTBXSeparatorItem
    end
    object TBXItem17: TTBXItem
      Action = RemoteOpenDirAction
    end
    object TBXSubmenuItem2: TTBXSubmenuItem
      Caption = '&Go To'
      HelpKeyword = 'task_navigate'
      Hint = 'Go to directory'
      object TBXItem18: TTBXItem
        Action = RemoteParentDirAction
      end
      object TBXItem19: TTBXItem
        Action = RemoteRootDirAction
      end
      object TBXItem20: TTBXItem
        Action = RemoteHomeDirAction
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
      end
      object TBXItem21: TTBXItem
        Action = RemoteBackAction
      end
      object TBXItem22: TTBXItem
        Action = RemoteForwardAction
      end
    end
  end
  object SessionsPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 456
    Top = 176
    object TBXItem124: TTBXItem
      Action = CloseTabAction
    end
    object TBXItem219: TTBXItem
      Action = DuplicateTabAction
    end
    object TBXItem78: TTBXItem
      Action = RenameTabAction
    end
    object TBXSeparatorItem17: TTBXSeparatorItem
    end
    object ColorMenuItem: TTBXColorItem
      Action = ColorMenuAction2
      Color = clNone
    end
    object TBXSeparatorItem52: TTBXSeparatorItem
    end
    object TBXItem79: TTBXItem
      Action = DisconnectSessionAction
    end
    object TBXItem80: TTBXItem
      Action = ReconnectSessionAction
    end
    object TBXItem125: TTBXItem
      Action = SaveCurrentSessionAction2
    end
    object TBXSeparatorItem6: TTBXSeparatorItem
    end
    object TBXItem56: TTBXItem
      Action = FileSystemInfoAction
    end
    object TBXItem52: TTBXItem
      Action = SessionGenerateUrlAction2
    end
    object TBXSeparatorItem34: TTBXSeparatorItem
    end
    object SessionsNewTabItem: TTBXSubmenuItem
      Action = NewTabAction
      DropdownCombo = True
      object TBXItem104: TTBXItem
        Action = NewRemoteTabAction
      end
      object TBXItem105: TTBXItem
        Action = NewLocalTabAction
      end
      object TBXSeparatorItem18: TTBXSeparatorItem
      end
      object TBXItem106: TTBXItem
        Action = DefaultToNewRemoteTabAction
      end
    end
    object TBXSubmenuItem23: TTBXSubmenuItem
      Action = SavedSessionsAction2
      Options = [tboDropdownArrow]
    end
    object TBXSeparatorItem35: TTBXSeparatorItem
    end
    object SessionsTabs4: TTBXItem
      Action = SessionsTabsAction2
    end
  end
  object LocalFilePopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 536
    Top = 336
    object LocalOpenMenuItem: TTBXItem
      Action = CurrentOpenAction
    end
    object LocalEditMenuItem: TTBXSubmenuItem
      Action = CurrentEditFocusedAction
      DropdownCombo = True
      OnPopup = FocusedEditMenuItemPopup
    end
    object LocalCopyMenuItem: TTBXSubmenuItem
      Action = LocalCopyFocusedAction
      DropdownCombo = True
      object TBXItem73: TTBXItem
        Action = LocalCopyFocusedNonQueueAction
      end
      object TBXItem74: TTBXItem
        Action = LocalCopyFocusedQueueAction
      end
      object TBXSeparatorItem10: TTBXSeparatorItem
      end
      object TBXItem54: TTBXItem
        Action = LocalMoveFocusedAction
      end
    end
    object LocalLocalCopyMenuItem: TTBXItem
      Action = LocalLocalCopyFocusedAction
    end
    object TBXItem101: TTBXItem
      Action = LocalLocalMoveFocusedAction
    end
    object TBXItem57: TTBXItem
      Action = CurrentDeleteFocusedAction
    end
    object TBXItem58: TTBXItem
      Action = CurrentRenameAction
    end
    object TBXSeparatorItem11: TTBXSeparatorItem
    end
    object TBXItem81: TTBXItem
      Action = CurrentCopyToClipboardFocusedAction2
    end
    object TBXSeparatorItem3: TTBXSeparatorItem
    end
    object LocalFilePopupCustomCommandsMenu: TTBXSubmenuItem
      Action = CustomCommandsFileAction
      object TTBXItem
      end
    end
    object TBXSubmenuItem5: TTBXSubmenuItem
      Caption = '&File Names'
      HelpKeyword = 'filenames'
      Hint = 'Operations with name(s) of selected file(s)'
      object TBXItem59: TTBXItem
        Action = FileListToCommandLineAction
      end
      object TBXItem60: TTBXItem
        Action = FileListToClipboardAction
      end
      object TBXItem61: TTBXItem
        Action = FullFileListToClipboardAction
      end
    end
    object TBXSeparatorItem4: TTBXSeparatorItem
    end
    object TBXItem63: TTBXItem
      Action = CurrentPropertiesFocusedAction
    end
    object TBXItem50: TTBXItem
      Action = CurrentSystemMenuFocusedAction
    end
  end
  object LocalBrowserPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 552
    Top = 176
    object TBXItem62: TTBXItem
      Action = CloseTabAction
    end
    object TBXItem102: TTBXItem
      Action = DuplicateTabAction
    end
    object TBXItem103: TTBXItem
      Action = RenameTabAction
    end
    object TBXSeparatorItem21: TTBXSeparatorItem
    end
    object TBXSubmenuItem12: TTBXSubmenuItem
      Action = NewTabAction
      DropdownCombo = True
      object TBXItem107: TTBXItem
        Action = NewRemoteTabAction
      end
      object TBXItem108: TTBXItem
        Action = NewLocalTabAction
      end
      object TBXSeparatorItem19: TTBXSeparatorItem
      end
      object TBXItem111: TTBXItem
        Action = DefaultToNewRemoteTabAction
      end
    end
    object TBXSubmenuItem11: TTBXSubmenuItem
      Action = SavedSessionsAction2
      Options = [tboDropdownArrow]
    end
    object TBXSeparatorItem22: TTBXSeparatorItem
    end
    object TBXItem110: TTBXItem
      Action = SessionsTabsAction2
    end
  end
  object NewTabPopup: TTBXPopupMenu
    Images = GlyphsModule.ExplorerImages
    Options = [tboShowHint]
    Left = 256
    Top = 176
    object NewRemoteTabItem: TTBXItem
      Action = NewRemoteTabAction
    end
    object NewLocalTabItem: TTBXItem
      Action = NewLocalTabAction
    end
    object TBXSeparatorItem67: TTBXSeparatorItem
    end
    object TBXItem232: TTBXItem
      Action = DefaultToNewRemoteTabAction
    end
  end
end
