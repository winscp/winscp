object NonVisualDataModule: TNonVisualDataModule
  OldCreateOrder = False
  Left = 361
  Top = 156
  Height = 502
  Width = 624
  object LogActions: TActionList
    Images = LogImages
    OnExecute = LogActionsExecute
    OnUpdate = LogActionsUpdate
    Left = 32
    Top = 104
    object LogClearAction: TAction
      Category = 'LogMemo'
      Caption = 'C&lear'
      Hint = 'Clear log'
      ImageIndex = 1
      ShortCut = 16430
    end
    object LogSelectAllAction: TAction
      Category = 'LogMemo'
      Caption = 'Select &All'
      Hint = 'Select all'
      ImageIndex = 3
      ShortCut = 16449
    end
    object LogCopyAction: TAction
      Category = 'LogMemo'
      Caption = '&Copy'
      Hint = 'Copy to clipboard'
      ImageIndex = 2
      ShortCut = 16451
    end
    object LogCloseAction: TAction
      Category = 'LogForm'
      Caption = '&Close'
      Hint = 'Close log window'
      ImageIndex = 0
      ShortCut = 32883
    end
  end
  object LogMemoPopup: TPopupMenu
    Images = LogImages
    Left = 32
    Top = 152
    object Clear1: TMenuItem
      Action = LogClearAction
    end
    object Close1: TMenuItem
      Action = LogCopyAction
    end
    object Selectall1: TMenuItem
      Action = LogSelectAllAction
    end
  end
  object LogImages: TImageList
    ShareImages = True
    Left = 32
    Top = 8
  end
  object LogDisabledImages: TImageList
    ShareImages = True
    Left = 32
    Top = 56
  end
  object ExplorerImages: TImageList
    ShareImages = True
    Left = 232
    Top = 24
  end
  object RemoteFilePopup: TPopupMenu
    Images = ExplorerImages
    Left = 424
    Top = 336
    object CurrentOpenMenuItem: TMenuItem
      Action = CurrentOpenAction
    end
    object CurentEditMenuItem: TMenuItem
      Action = CurrentEditAction
    end
    object CurrentCopyMenuItem: TMenuItem
      Action = CurrentCopyFocusedAction
    end
    object Duplicate3: TMenuItem
      Action = RemoteCopyToAction
    end
    object Moveto1: TMenuItem
      Action = CurrentMoveFocusedAction
    end
    object Moveto6: TMenuItem
      Action = RemoteMoveToFocusedAction
    end
    object Delete1: TMenuItem
      Action = CurrentDeleteFocusedAction
    end
    object Rename1: TMenuItem
      Action = CurrentRenameAction
    end
    object N45: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object RemoteDirViewCustomCommandsMenu: TMenuItem
      Action = CustomCommandsAction
      object TMenuItem
      end
    end
    object N1: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Properties1: TMenuItem
      Action = CurrentPropertiesFocusedAction
    end
  end
  object ExplorerActions: TActionList
    Images = ExplorerImages
    OnExecute = ExplorerActionsExecute
    OnUpdate = ExplorerActionsUpdate
    Left = 440
    Top = 24
    object GoToTreeAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Go To Tree'
      Hint = 'Go to tree'
      ImageIndex = 76
      ShortCut = 49236
    end
    object LocalTreeAction: TAction
      Tag = 8
      Category = 'View'
      Caption = '&Tree'
      Hint = 'Hide/show directory tree'
      ImageIndex = 76
      ShortCut = 49236
    end
    object RemoteTreeAction: TAction
      Tag = 12
      Category = 'View'
      Caption = '&Tree'
      Hint = 'Hide/show directory tree'
      ImageIndex = 76
      ShortCut = 49236
    end
    object QueueItemQueryAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show Query'
      Hint = 'Show pending query of selected queue item'
      ImageIndex = 67
    end
    object QueueItemErrorAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show Error'
      Hint = 'Show pending error message of selected queue item'
      ImageIndex = 68
    end
    object QueueItemPromptAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Show Prompt'
      Hint = 'Show pending prompt of selected queue item'
      ImageIndex = 69
    end
    object GoToCommandLineAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Go To Comma&nd Line'
      Hint = 'Go to command line'
      ShortCut = 49230
    end
    object QueueItemDeleteAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Delete'
      Hint = 'Remove selected queue item'
      ImageIndex = 71
    end
    object QueueItemExecuteAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Execute Now'
      Hint = 
        'Execute selected queue item immediately by granting it additiona' +
        'l connection'
      ImageIndex = 70
    end
    object SelectOneAction: TAction
      Tag = 12
      Category = 'Selection'
      Caption = '&Select/Unselect'
      Hint = 'Select|Select/unselect focused file'
    end
    object CurrentRenameAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Rename'
      Hint = 'Rename|Rename selected file'
      ImageIndex = 3
    end
    object LocalSortAscendingAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = '&Ascending'
      Hint = 
        'Ascending/descending|Toggle ascending/descending sort of local p' +
        'anel'
      ImageIndex = 37
    end
    object CurrentEditAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Edit'
      Hint = 'Edit|Edit selected file'
      ImageIndex = 57
    end
    object HideColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Hide Column'
      Hint = 'Hide column|Hide selected column'
    end
    object LocalBackAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Back'
      ImageIndex = 6
      ShortCut = 32805
    end
    object CurrentCopyAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Copy ...'
      Hint = 'Copy|Copy selected file(s)'
      ImageIndex = 0
    end
    object CurrentMoveAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Move ...'
      Hint = 'Move|Move selected file(s)'
      ImageIndex = 1
    end
    object CurrentCycleStyleAction: TAction
      Tag = 7
      Category = 'Style'
      Caption = 'View'
      Hint = 'View|Cycle thru directory view styles'
      ImageIndex = 8
    end
    object CurrentIconAction: TAction
      Tag = 7
      Category = 'Style'
      Caption = '&Large Icons'
      Hint = 'Large Icons|View large icons'
      ImageIndex = 8
    end
    object CurrentSmallIconAction: TAction
      Tag = 7
      Category = 'Style'
      Caption = '&Small Icons'
      Hint = 'Small Icons|View small icons'
      ImageIndex = 9
    end
    object CurrentListAction: TAction
      Tag = 7
      Category = 'Style'
      Caption = 'Lis&t'
      Hint = 'List|View list'
      ImageIndex = 10
    end
    object CurrentReportAction: TAction
      Tag = 7
      Category = 'Style'
      Caption = '&Details'
      Hint = 'Details|View details'
      ImageIndex = 11
    end
    object CurrentCopyFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Copy ...'
      Hint = 'Copy|Copy selected file(s) to local directory'
      ImageIndex = 0
    end
    object RemoteMoveToAction: TAction
      Tag = 14
      Category = 'Selected Operation'
      Caption = 'Mo&ve to ...'
      Hint = 'Move|Move selected file(s) to remote directory'
    end
    object CurrentMoveFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Move ...'
      Hint = 'Move|Move selected file(s) to local directory'
      ImageIndex = 1
    end
    object CurrentDeleteFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object CurrentPropertiesFocusedAction: TAction
      Tag = 12
      Category = 'Focused Operation'
      Caption = '&Properties'
      Hint = 'Properties|Display/change properties of selected file(s)'
      ImageIndex = 4
    end
    object CurrentCreateDirAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Create Directory...'
      Hint = 'Create directory|Create new directory'
      ImageIndex = 5
    end
    object CurrentDeleteAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Delete'
      Hint = 'Delete|Delete selected file(s)'
      ImageIndex = 2
    end
    object CurrentPropertiesAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Properties'
      Hint = 'Properties|Display/change properties of selected file(s)'
      ImageIndex = 4
    end
    object RemoteBackAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Back'
      ImageIndex = 6
      ShortCut = 32805
    end
    object RemoteForwardAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Forward'
      ImageIndex = 7
      ShortCut = 32807
    end
    object CommandLinePanelAction: TAction
      Tag = 8
      Category = 'View'
      Caption = 'Comma&nd Line'
      Hint = 'Hide/show command line'
      ShortCut = 49230
    end
    object RemoteParentDirAction: TAction
      Tag = 12
      Category = 'Remote Directory'
      Caption = '&Parent Directory'
      Hint = 'Parent directory|Go to parent directory'
      ImageIndex = 12
      ShortCut = 8
    end
    object RemoteRootDirAction: TAction
      Tag = 12
      Category = 'Remote Directory'
      Caption = '&Root Directory'
      Hint = 'Root directory|Go to root directory'
      ImageIndex = 13
      ShortCut = 16604
    end
    object RemoteHomeDirAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Home Directory'
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
      Caption = '&About ...'
      Hint = 'About|Show About box'
      ImageIndex = 17
    end
    object StatusBarAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Status &Bar'
      Hint = 'Hide/show status bar'
    end
    object ExplorerAddressBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Address'
      Hint = 'Hide/show address toolbar'
    end
    object ExplorerMenuBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Menu'
      Hint = 'Hide/show menu'
    end
    object ExplorerToolbarBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Standard Buttons'
      Hint = 'Hide/show standard toolbar'
    end
    object RemoteOpenDirAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Open Directory/Bookmark...'
      Hint = 
        'Open directory/bookmark|Open specified directory or saved bookma' +
        'rk'
      ImageIndex = 18
    end
    object SelectAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'Sele&ct Files'
      Hint = 'Select|Select files by mask'
      ImageIndex = 19
    end
    object UnselectAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = '&Unselect Files'
      Hint = 'Unselect|Unselect files by mask'
      ImageIndex = 20
    end
    object SelectAllAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = 'Select &All'
      Hint = 'Select all'
      ImageIndex = 21
    end
    object InvertSelectionAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = '&Invert Selection'
      Hint = 'Invert selection'
      ImageIndex = 22
    end
    object ExplorerSelectionBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'Se&lection Buttons'
      Hint = 'Hide/show selection toolbar'
    end
    object ClearSelectionAction: TAction
      Tag = 15
      Category = 'Selection'
      Caption = '&Clear Selection'
      Hint = 'Clear selection'
      ImageIndex = 23
    end
    object ExplorerSessionBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'Sessio&n Buttons'
      Hint = 'Hide/show session toolbar'
    end
    object ExplorerPreferencesBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = '&Preferences Buttons'
      Hint = 'Hide/show preferences toolbar'
    end
    object ExplorerSortBandAction: TAction
      Tag = 7
      Category = 'View'
      Caption = 'So&rt Buttons'
      Hint = 'Hide/show sort toolbar'
    end
    object ViewLogAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Lo&g Window'
      Hint = 'Show/hide log window'
      ImageIndex = 24
    end
    object NewSessionAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&New Session...'
      Hint = 'New session|Opens new session'
      ImageIndex = 25
      ShortCut = 16462
    end
    object CloseSessionAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Disconnect'
      Hint = 'Close session|Terminate current session'
      ImageIndex = 26
      ShortCut = 24644
    end
    object SavedSessionsAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = 'Saved Sessions'
      Hint = 'Open saved session'
      ImageIndex = 27
    end
    object PreferencesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = '&Preferences...'
      Hint = 'Preferences|Show/change user preferences'
      ImageIndex = 28
      ShortCut = 49232
    end
    object RemoteChangePathAction: TAction
      Tag = 11
      Category = 'Remote Directory'
      Caption = '&Change Directory'
      Hint = 'Allows selection of different directory for remote panel'
      ImageIndex = 29
      ShortCut = 32881
    end
    object LocalForwardAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Forward'
      ImageIndex = 7
      ShortCut = 32807
    end
    object LocalParentDirAction: TAction
      Tag = 8
      Category = 'Local Directory'
      Caption = '&Parent Directory'
      Hint = 'Parent directory|Go to parent directory'
      ImageIndex = 12
      ShortCut = 8
    end
    object LocalRootDirAction: TAction
      Tag = 8
      Category = 'Local Directory'
      Caption = '&Root Directory'
      Hint = 'Root directory|Go to root directory'
      ImageIndex = 14
      ShortCut = 16604
    end
    object LocalHomeDirAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Home Directory'
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
      Hint = 
        'Open directory/bookmark|Open specified directory or saved bookma' +
        'rk'
      ImageIndex = 18
    end
    object LocalChangePathAction: TAction
      Tag = 11
      Category = 'Local Directory'
      Caption = '&Change Drive'
      Hint = 'Allows selection of different drive for local panel'
      ImageIndex = 30
      ShortCut = 32880
    end
    object ToolBarAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Commands Toolbar'
      Hint = 'Hide/show bottom commands toolbar'
    end
    object CommanderMenuBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Menu'
      Hint = 'Hide/show menu'
    end
    object CommanderSessionBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Sessio&n Buttons'
      Hint = 'Hide/show session toolbar'
    end
    object CommanderPreferencesBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Preferences Buttons'
      Hint = 'Hide/show preferences toolbar'
    end
    object CommanderSelectionBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Se&lection Buttons'
      Hint = 'Hide/show selection toolbar'
    end
    object CommanderToolbarBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Standard Buttons'
      Hint = 'Hide/show standard toolbar'
    end
    object CommanderSortBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'So&rt Buttons'
      Hint = 'Hide/show sort toolbar'
    end
    object CommanderCommandsBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Commands Buttons'
      Hint = 'Hide/show commands toolbar'
    end
    object CommanderLocalHistoryBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&History Buttons'
      Hint = 'Hide/show local history toolbar'
    end
    object CommanderLocalNavigationBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Navigation Buttons'
      Hint = 'Hide/show local navigation toolbar'
    end
    object CommanderRemoteHistoryBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&History Buttons'
      Hint = 'Hide/show remote history toolbar'
    end
    object CommanderRemoteNavigationBandAction: TAction
      Tag = 11
      Category = 'View'
      Caption = '&Navigation Buttons'
      Hint = 'Hide/show remote navigation toolbar'
    end
    object LocalStatusBarAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Status &Bar'
      Hint = 'Hide/show local panel status bar'
    end
    object RemoteStatusBarAction: TAction
      Tag = 11
      Category = 'View'
      Caption = 'Status &Bar'
      Hint = 'Hide/show remote panel status bar'
    end
    object LocalSortByNameAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Name'
      Hint = 'Sort by name|Sort local panel by name'
      ImageIndex = 31
      ShortCut = 16498
    end
    object LocalSortByExtAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Extension'
      Hint = 'Sort by extension|Sort local panel by file name extension'
      ImageIndex = 32
      ShortCut = 16499
    end
    object LocalSortBySizeAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Size'
      Hint = 'Sort by size|Sort local panel by file size'
      ImageIndex = 35
      ShortCut = 16501
    end
    object LocalSortByAttrAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By A&ttributes'
      Hint = 'Sort by attributes|Sort local panel by attributes'
      ImageIndex = 36
      ShortCut = 16502
    end
    object LocalSortByTypeAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Type'
      Hint = 'Sort by type|Sort local panel by file type'
      ImageIndex = 34
    end
    object LocalSortByChangedAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Modification'
      Hint = 'Sort by time|Sort local panel by last modification time'
      ImageIndex = 33
      ShortCut = 16500
    end
    object RemoteSortAscendingAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = '&Ascending'
      Hint = 
        'Ascending/descending|Toggle ascending/descending sort of remote ' +
        'panel'
      ImageIndex = 37
    end
    object RemoteSortByNameAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Name'
      Hint = 'Sort by name|Sort remote panel by name'
      ImageIndex = 31
      ShortCut = 16498
    end
    object RemoteSortByExtAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Extension'
      Hint = 'Sort by extension|Sort remote panel by file name extension'
      ImageIndex = 32
      ShortCut = 16499
    end
    object RemoteSortBySizeAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Size'
      Hint = 'Sort by size|Sort remote panel by file size'
      ImageIndex = 35
      ShortCut = 16501
    end
    object RemoteSortByRightsAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Permissions'
      Hint = 'Sort by permissions|Sort remote panel by permissions'
      ImageIndex = 36
      ShortCut = 16502
    end
    object RemoteSortByChangedAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Modification'
      Hint = 'Sort by time|Sort remote panel by last modification time'
      ImageIndex = 33
      ShortCut = 16500
    end
    object RemoteSortByOwnerAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Owner'
      Hint = 'Sort by owner|Sort remote panel by file owner'
      ImageIndex = 38
      ShortCut = 16503
    end
    object RemoteSortByGroupAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Group'
      Hint = 'Sort by group|Sort remote panel by file group'
      ImageIndex = 39
      ShortCut = 16504
    end
    object CurrentSortAscendingAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = '&Ascending'
      Hint = 
        'Ascending/descending|Toggle ascending/descending sort of current' +
        ' panel'
      ImageIndex = 37
    end
    object CurrentSortByNameAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Name'
      Hint = 'Sort by name|Sort current panel by name'
      ImageIndex = 31
      ShortCut = 16498
    end
    object CurrentSortByExtAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Extension'
      Hint = 'Sort by extension|Sort current panel by file name extension'
      ImageIndex = 32
      ShortCut = 16499
    end
    object CurrentSortBySizeAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Size'
      Hint = 'Sort by size|Sort current panel by file size'
      ImageIndex = 35
      ShortCut = 16501
    end
    object CurrentSortByTypeAction: TAction
      Tag = 9
      Category = 'Sort'
      Caption = 'By &Type'
      Hint = 'Sort by type|Sort current panel by file type (local panel only)'
      ImageIndex = 34
    end
    object CurrentSortByRightsAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'By &Attributes'
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
      Hint = 'Sort by time|Sort current panel by last modification time'
      ImageIndex = 33
      ShortCut = 16500
    end
    object CurrentSortByOwnerAction: TAction
      Tag = 14
      Category = 'Sort'
      Caption = 'By &Owner'
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
      Hint = 'Sort files ascending by selected column'
      ImageIndex = 41
    end
    object SortColumnDescendingAction: TAction
      Tag = 15
      Category = 'Sort'
      Caption = 'Sort &Descending'
      Hint = 'Sort files descending by selected column'
      ImageIndex = 40
    end
    object HomepageAction: TAction
      Tag = 12
      Category = 'Help'
      Caption = 'Product &Homepage'
      Hint = 'Opens web browser and points it to application homepage '
      ImageIndex = 42
    end
    object HistoryPageAction: TAction
      Tag = 12
      Category = 'Help'
      Caption = '&Version History'
      Hint = 'Opens web browser and points it to application history page'
    end
    object RequirementsPageAction: TAction
      Tag = 12
      Category = 'Help'
      Caption = 'Program &Requirements'
      Hint = 'Opens web browser and points it to program requirements page'
    end
    object SaveCurrentSessionAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = '&Save Session...'
      Hint = 'Save session|Save current session'
      ImageIndex = 43
    end
    object ShowHideRemoteNameColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Name'
      Hint = 'Show/hide name|Show/hide name column on remote panel'
      ImageIndex = 44
    end
    object ShowHideRemoteExtColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Extension'
      Hint = 'Show/hide extension|Show/hide extension column on remote panel'
      ImageIndex = 45
    end
    object ShowHideRemoteSizeColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Size'
      Hint = 'Show/hide size|Show/hide size column on remote panel'
      ImageIndex = 47
    end
    object ShowHideRemoteChangedColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Modification'
      Hint = 
        'Show/hide modification|Show/hide modification column on remote p' +
        'anel'
      ImageIndex = 48
    end
    object ShowHideRemoteRightsColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Permissions'
      Hint = 
        'Show/hide permissions|Show/hide permissions column on remote pan' +
        'el'
      ImageIndex = 49
    end
    object ShowHideRemoteOwnerColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Owner'
      Hint = 'Show/hide owner|Show/hide owner column on remote panel'
      ImageIndex = 50
    end
    object ShowHideRemoteGroupColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Group'
      Hint = 'Show/hide group|Show/hide group column on remote panel'
      ImageIndex = 51
    end
    object ShowHideLocalNameColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Name'
      Hint = 'Show/hide name|Show/hide name column on local panel'
      ImageIndex = 44
    end
    object ShowHideLocalExtColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Extension'
      Hint = 'Show/hide extension|Show/hide extension column on local panel'
      ImageIndex = 45
    end
    object ShowHideLocalTypeColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Type'
      Hint = 'Show/hide type|Show/hide type column on local panel'
      ImageIndex = 46
    end
    object ShowHideLocalSizeColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Size'
      Hint = 'Show/hide size|Show/hide size column on local panel'
      ImageIndex = 47
    end
    object ShowHideLocalChangedColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Modification'
      Hint = 
        'Show/hide modification|Show/hide modification column on local pa' +
        'nel'
      ImageIndex = 48
    end
    object ShowHideLocalAttrColumnAction: TAction
      Tag = 15
      Category = 'Columns'
      Caption = '&Attributes'
      Hint = 'Show/hide attributes|Show/hide attributes column on local panel'
      ImageIndex = 49
    end
    object CompareDirectoriesAction: TAction
      Tag = 11
      Category = 'Command'
      Caption = '&Compare Directories'
      Hint = 
        'Compare directories|Mark different files in local and remote dir' +
        'ectory'
      ImageIndex = 52
      ShortCut = 16451
    end
    object SynchronizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Keep Remote Directory Up To Date'
      Hint = 
        'Keep remote directory up to date|Keep remote directory up to dat' +
        'e'
      ImageIndex = 53
      ShortCut = 16469
    end
    object ForumPageAction: TAction
      Tag = 12
      Category = 'Help'
      Caption = '&Support Forum'
      Hint = 'Opens web browser and points it to support forum page'
    end
    object LocalAddBookmarkAction: TAction
      Tag = 9
      Category = 'Local Directory'
      Caption = '&Add To Bookmarks'
      Hint = 'Add to bookmarks|Add current local directory to bookmark list'
      ImageIndex = 54
      ShortCut = 16450
    end
    object RemoteAddBookmarkAction: TAction
      Tag = 14
      Category = 'Remote Directory'
      Caption = '&Add To Bookmarks'
      Hint = 'Add to bookmarks|Add current remote directory to bookmark list'
      ImageIndex = 54
      ShortCut = 16450
    end
    object ConsoleAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Open &Terminal'
      Hint = 
        'Open teminal|Open terminal window that allow executing arbitrary' +
        ' command (with exception of commands that require user input)'
      ImageIndex = 55
      ShortCut = 16468
    end
    object PuttyAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Open in &PuTTY'
      Hint = 
        'Open session in PuTTY|Execute PuTTY SSH terminal and opens curre' +
        'nt session with it'
      ImageIndex = 64
      ShortCut = 16464
    end
    object LocalExploreDirectoryAction: TAction
      Tag = 15
      Category = 'Local Directory'
      Caption = '&Explore Directory'
      Hint = 'Opens Windows Explorer with current local directory'
      ImageIndex = 56
      ShortCut = 49221
    end
    object CurrentEditAlternativeAction: TAction
      Tag = 15
      Category = 'Focused Operation'
      Caption = '&Edit (alternative)'
      Hint = 'Edit (alternative)|Edit selected file using alternative editor'
    end
    object CurrentOpenAction: TAction
      Tag = 15
      Category = 'Focused Operation'
      Caption = '&Open'
      Hint = 
        'Open document|Open selected document using application associate' +
        'd with document type'
      ImageIndex = 58
    end
    object SynchronizeBrowsingAction: TAction
      Tag = 11
      Category = 'Command'
      AutoCheck = True
      Caption = 'Synchronize &Browsing'
      Hint = 
        'Synchronize browsing|Synchronize local and remote directory brow' +
        'ing'
      ImageIndex = 59
      ShortCut = 49218
    end
    object AddEditLinkAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Add/Edit &Link...'
      Hint = 
        'Add/edit link|Add new link/shortcut or edit selected link/shortc' +
        'ut'
      ImageIndex = 60
    end
    object CloseApplicationAction: TAction
      Tag = 15
      Category = 'Toolbar Operation (selected + rename + mkdir + close)'
      Caption = '&Quit'
      Hint = 
        'Exit application|Terminate opened session(s) and close applicati' +
        'on'
      ImageIndex = 61
    end
    object OpenedSessionsAction: TAction
      Tag = 15
      Category = 'Session'
      Caption = 'Opened Sessions'
      Hint = 'Select session|Select opened session to activate'
      ImageIndex = 62
    end
    object CustomCommandsAction: TAction
      Tag = 14
      Category = 'Command'
      Caption = '&Custom Commands'
      Hint = 'Execute custom commands with selected file(s)'
    end
    object CustomCommandsCustomizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Customize...'
      Hint = 'Customize custom commands'
      ImageIndex = 28
    end
    object CheckForUpdatesAction: TAction
      Tag = 15
      Category = 'Help'
      Caption = '&Check For Updates'
      Hint = 'Queries application homepage for updates'
      ImageIndex = 63
    end
    object DonatePageAction: TAction
      Tag = 12
      Category = 'Help'
      Caption = '&Donate'
      Hint = 'Opens web browser and points it to program donation page'
    end
    object FileSystemInfoAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Server/protocol Information'
      Hint = 'Display server/protocol information'
      ImageIndex = 17
    end
    object ClearCachesAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Clea&r Caches'
      Hint = 'Clear directory listing and directory changes caches'
    end
    object FullSynchronizeAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Synchronize'
      Hint = 'Synchronize local directory with remote directory'
      ImageIndex = 66
      ShortCut = 16467
    end
    object RemoteMoveToFocusedAction: TAction
      Tag = 14
      Category = 'Focused Operation'
      Caption = 'Mo&ve to ...'
      Hint = 'Move|Move selected file(s) to remote directory'
    end
    object ShowHiddenFilesAction: TAction
      Tag = 15
      Category = 'View'
      Caption = 'Show/hide &hidden files'
      Hint = 'Toggle showing hidden files in panel(s)'
      ShortCut = 49224
    end
    object LocalPathToClipboardAction: TAction
      Tag = 15
      Category = 'Local Directory'
      Caption = 'Copy Path to &Clipboard'
      Hint = 'Copy current local path to clipboard'
    end
    object RemotePathToClipboardAction: TAction
      Tag = 15
      Category = 'Remote Directory'
      Caption = 'Copy Path to &Clipboard'
      Hint = 'Copy current remote path to clipboard'
    end
    object FileListToCommandLineAction: TAction
      Tag = 11
      Category = 'Selected Operation'
      Caption = 'Insert to Command &Line'
      Hint = 'Insert name(s) of selected file(s) to command line'
      ShortCut = 16397
    end
    object FileListToClipboardAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Copy to &Clipboard'
      Hint = 'Copy name(s) of selected file(s) to clipboard'
      ShortCut = 24643
    end
    object FullFileListToClipboardAction: TAction
      Tag = 15
      Category = 'Selected Operation'
      Caption = 'Copy to Clipboard (Include &Paths)'
      Hint = 'Copy name(s) including path of selected file(s) to clipboard'
      ShortCut = 49219
    end
    object QueueGoToAction: TAction
      Tag = 15
      Category = 'Queue'
      Caption = '&Go To'
      Hint = 'Go to transfer queue list'
      ImageIndex = 74
      ShortCut = 16465
    end
    object QueueItemUpAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Move &Up'
      Hint = 'Move selected queue item up to be processed earlier'
      ImageIndex = 72
    end
    object QueueItemDownAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Move &Down'
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
      Hint = 'Show queue list'
    end
    object QueueHideWhenEmptyAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = 'Hide when &Empty'
      Hint = 'Hide queue list when it is empty'
    end
    object QueueHideAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Hide'
      Hint = 'Hide queue list'
    end
    object QueueToolbarAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Toolbar'
      Hint = 'Hide/show queue list toolbar (on queue list panel)'
    end
    object QueuePreferencesAction: TAction
      Tag = 12
      Category = 'Queue'
      Caption = '&Customize...'
      Hint = 'Customize queue list'
      ImageIndex = 28
    end
    object PasteAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = '&Paste'
      Hint = 'Paste files from clipboard to current directory in active panel'
      ImageIndex = 75
      ShortCut = 16470
    end
    object EditNewAction: TAction
      Tag = 15
      Category = 'Command'
      Caption = 'Edit &new file ...'
      Hint = 'Edit new file|Create new file and open it in editor'
      ImageIndex = 77
    end
    object RemoteCopyToFocusedAction: TAction
      Tag = 14
      Category = 'Focused Operation'
      Caption = '&Duplicate ...'
      Hint = 'Duplicate|Duplicate selected file(s) to remote directory'
      ImageIndex = 78
    end
    object RemoteCopyToAction: TAction
      Tag = 14
      Category = 'Selected Operation'
      Caption = '&Duplicate ...'
      Hint = 'Duplicate|Duplicate selected file(s) to remote directory'
      ImageIndex = 78
    end
  end
  object ExplorerDisabledImages: TImageList
    ShareImages = True
    Left = 336
    Top = 24
  end
  object ExplorerMenu: TMainMenu
    Images = ExplorerImages
    Left = 256
    Top = 400
    object ExplorerFileMenu: TMenuItem
      Caption = '&File'
      Hint = 'File operations'
      object Open3: TMenuItem
        Action = CurrentOpenAction
      end
      object Edit3: TMenuItem
        Action = CurrentEditAction
      end
      object Editalternative1: TMenuItem
        Action = CurrentEditAlternativeAction
      end
      object Editnewfile1: TMenuItem
        Action = EditNewAction
      end
      object N42: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Delete2: TMenuItem
        Action = CurrentDeleteAction
      end
      object Rename3: TMenuItem
        Action = CurrentRenameAction
      end
      object Properties2: TMenuItem
        Action = CurrentPropertiesAction
      end
      object N2: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Copyto2: TMenuItem
        Action = CurrentCopyAction
      end
      object Duplicate1: TMenuItem
        Action = RemoteCopyToAction
      end
      object Moveto2: TMenuItem
        Action = CurrentMoveAction
      end
      object Moveto5: TMenuItem
        Action = RemoteMoveToAction
      end
      object Paste2: TMenuItem
        Action = PasteAction
      end
      object N39: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object CustomCommands1: TMenuItem
        Action = CustomCommandsAction
        object TMenuItem
        end
      end
      object FileNames2: TMenuItem
        Caption = '&File Names'
        Hint = 'Operations with name(s) of selected file(s)'
        object CopytoClipboard2: TMenuItem
          Action = FileListToClipboardAction
        end
        object CopytoClipboardIncludePaths2: TMenuItem
          Action = FullFileListToClipboardAction
        end
      end
      object N10: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Disconnect2: TMenuItem
        Action = CloseSessionAction
      end
      object Quit2: TMenuItem
        Action = CloseApplicationAction
      end
    end
    object ExporerCommandsMenu: TMenuItem
      Caption = '&Commands'
      Hint = 'Other commands'
      object Createdirectory2: TMenuItem
        Action = CurrentCreateDirAction
      end
      object Addeditlink2: TMenuItem
        Action = AddEditLinkAction
      end
      object KeepRemoteDirectoryUpToDate2: TMenuItem
        Action = SynchronizeAction
      end
      object Synchronize2: TMenuItem
        Action = FullSynchronizeAction
      end
      object Queue4: TMenuItem
        Caption = '&Queue'
        Hint = 'Queue list commands'
        object QueueGoToAction2: TMenuItem
          Action = QueueGoToAction
        end
        object N60: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ShowQuery3: TMenuItem
          Action = QueueItemQueryAction
        end
        object ShowError3: TMenuItem
          Action = QueueItemErrorAction
        end
        object ShowPrompt3: TMenuItem
          Action = QueueItemPromptAction
        end
        object N59: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ExecuteNow3: TMenuItem
          Action = QueueItemExecuteAction
        end
        object Delete6: TMenuItem
          Action = QueueItemDeleteAction
        end
        object N58: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object MoveUp3: TMenuItem
          Action = QueueItemUpAction
        end
        object MoveDown3: TMenuItem
          Action = QueueItemDownAction
        end
      end
      object N40: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Addtobookmarks3: TMenuItem
        Action = RemoteAddBookmarkAction
      end
      object CopyPathtoClipboard5: TMenuItem
        Action = RemotePathToClipboardAction
      end
      object N3: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Openterminal2: TMenuItem
        Action = ConsoleAction
      end
      object OpeninPuTTY2: TMenuItem
        Action = PuttyAction
      end
      object N50: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Serverprotocolinformation2: TMenuItem
        Action = FileSystemInfoAction
      end
      object ClearCaches2: TMenuItem
        Action = ClearCachesAction
      end
    end
    object ExplorerViewMenu: TMenuItem
      Caption = '&View'
      Hint = 'Change program layout'
      object Toolbars1: TMenuItem
        Caption = '&Toolbars'
        Hint = 'Show/hide toolbars'
        object Address1: TMenuItem
          Action = ExplorerAddressBandAction
        end
        object Buttons1: TMenuItem
          Action = ExplorerToolbarBandAction
        end
        object SelectionButtons2: TMenuItem
          Action = ExplorerSelectionBandAction
        end
        object SessionButtons1: TMenuItem
          Action = ExplorerSessionBandAction
        end
        object PreferencesButtons2: TMenuItem
          Action = ExplorerPreferencesBandAction
        end
        object SortButtons4: TMenuItem
          Action = ExplorerSortBandAction
        end
      end
      object StatusBar1: TMenuItem
        Action = StatusBarAction
      end
      object LogWindow1: TMenuItem
        Action = ViewLogAction
      end
      object Queue6: TMenuItem
        Caption = '&Queue'
        Hint = 'Configure queue list'
        object Show3: TMenuItem
          Action = QueueShowAction
        end
        object HidewhenEmpty3: TMenuItem
          Action = QueueHideWhenEmptyAction
        end
        object Hide2: TMenuItem
          Action = QueueHideAction
        end
        object N64: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Toolbar2: TMenuItem
          Action = QueueToolbarAction
        end
        object N63: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Customize2: TMenuItem
          Action = QueuePreferencesAction
        end
      end
      object Tree3: TMenuItem
        Action = RemoteTreeAction
      end
      object N4: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object LargeIcons2: TMenuItem
        Action = CurrentIconAction
        RadioItem = True
      end
      object SmallIcons2: TMenuItem
        Action = CurrentSmallIconAction
        RadioItem = True
      end
      object List2: TMenuItem
        Action = CurrentListAction
        RadioItem = True
      end
      object Details2: TMenuItem
        Action = CurrentReportAction
        RadioItem = True
      end
      object N6: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Goto1: TMenuItem
        Caption = '&Go To'
        Hint = 'Go to directory'
        object OpenDirectory1: TMenuItem
          Action = RemoteOpenDirAction
        end
        object N8: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Parentdirectory1: TMenuItem
          Action = RemoteParentDirAction
        end
        object Rootdirectory1: TMenuItem
          Action = RemoteRootDirAction
        end
        object Homedirectory1: TMenuItem
          Action = RemoteHomeDirAction
        end
        object N7: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Back1: TMenuItem
          Action = RemoteBackAction
        end
        object Forward1: TMenuItem
          Action = RemoteForwardAction
        end
      end
      object Refresh1: TMenuItem
        Action = RemoteRefreshAction
      end
      object Sort3: TMenuItem
        Caption = '&Sort'
        Hint = 'Change file order in panel'
        object Ascending2: TMenuItem
          Action = RemoteSortAscendingAction
        end
        object N35: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ByName2: TMenuItem
          Action = RemoteSortByNameAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByExtension1: TMenuItem
          Action = RemoteSortByExtAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByModification3: TMenuItem
          Action = RemoteSortByChangedAction
          GroupIndex = 1
          RadioItem = True
        end
        object BySize3: TMenuItem
          Action = RemoteSortBySizeAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByPermissions1: TMenuItem
          Action = RemoteSortByRightsAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByOwner2: TMenuItem
          Action = RemoteSortByOwnerAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByGroup2: TMenuItem
          Action = RemoteSortByGroupAction
          GroupIndex = 1
          RadioItem = True
        end
      end
      object Showcolumns5: TMenuItem
        Caption = 'Show &Columns'
        object Name5: TMenuItem
          Action = ShowHideRemoteNameColumnAction
        end
        object Size5: TMenuItem
          Action = ShowHideRemoteSizeColumnAction
        end
        object Modification5: TMenuItem
          Action = ShowHideRemoteChangedColumnAction
        end
        object Permissions2: TMenuItem
          Action = ShowHideRemoteRightsColumnAction
        end
        object Owner3: TMenuItem
          Action = ShowHideRemoteOwnerColumnAction
        end
        object Group3: TMenuItem
          Action = ShowHideRemoteGroupColumnAction
        end
      end
      object N36: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Preferences1: TMenuItem
        Action = PreferencesAction
      end
    end
  end
  object ExplorerBarPopup: TPopupMenu
    Images = ExplorerImages
    Left = 192
    Top = 336
    object Address2: TMenuItem
      Action = ExplorerAddressBandAction
    end
    object StandardButtons1: TMenuItem
      Action = ExplorerToolbarBandAction
    end
    object SelectionButtons1: TMenuItem
      Action = ExplorerSelectionBandAction
    end
    object SessionButtons2: TMenuItem
      Action = ExplorerSessionBandAction
    end
    object PreferencesButtons1: TMenuItem
      Action = ExplorerPreferencesBandAction
    end
    object SortButtons3: TMenuItem
      Action = ExplorerSortBandAction
    end
    object N5: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object StatusBar2: TMenuItem
      Action = StatusBarAction
    end
    object N72: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Queue7: TMenuItem
      Caption = '&Queue'
      Hint = 'Configure queue list'
      object Show6: TMenuItem
        Action = QueueShowAction
      end
      object HidewhenEmpty6: TMenuItem
        Action = QueueHideWhenEmptyAction
      end
      object Hide5: TMenuItem
        Action = QueueHideAction
      end
      object N71: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Toolbar5: TMenuItem
        Action = QueueToolbarAction
      end
      object N70: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Customize5: TMenuItem
        Action = QueuePreferencesAction
      end
    end
    object Tree4: TMenuItem
      Action = RemoteTreeAction
    end
  end
  object ExplorerStylePopup: TPopupMenu
    Images = ExplorerImages
    Left = 192
    Top = 264
    object CurrentIconAction1: TMenuItem
      Action = CurrentIconAction
      GroupIndex = 1
      RadioItem = True
    end
    object CurrentSmallIconAction1: TMenuItem
      Action = CurrentSmallIconAction
      GroupIndex = 1
      RadioItem = True
    end
    object CurrentListAction1: TMenuItem
      Action = CurrentListAction
      GroupIndex = 1
      RadioItem = True
    end
    object CurrentReportAction1: TMenuItem
      Action = CurrentReportAction
      GroupIndex = 1
      RadioItem = True
    end
  end
  object SessionIdleTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = SessionIdleTimerTimer
    Left = 32
    Top = 336
  end
  object SessionImages: TImageList
    ShareImages = True
    Left = 240
    Top = 176
  end
  object CommonScpMenu: TMainMenu
    Images = ExplorerImages
    Left = 48
    Top = 400
    object CommonSessionMenu: TMenuItem
      Caption = '&Session'
      Hint = 'Session commands'
      object Newsession1: TMenuItem
        Action = NewSessionAction
      end
      object SavedSessionsMenu: TMenuItem
        Action = SavedSessionsAction
        object TMenuItem
        end
      end
      object N9: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object OpenedSessionsMenu: TMenuItem
        Action = OpenedSessionsAction
        object TMenuItem
        end
      end
      object Savesession1: TMenuItem
        Action = SaveCurrentSessionAction
      end
      object Disconnect1: TMenuItem
        Action = CloseSessionAction
      end
    end
    object CommonMarkMenu: TMenuItem
      Caption = '&Mark'
      Hint = 'Mark commands'
      object SelectUnselect1: TMenuItem
        Action = SelectOneAction
      end
      object SelectFiles1: TMenuItem
        Action = SelectAction
      end
      object UnselectFiles1: TMenuItem
        Action = UnselectAction
      end
      object SelectAll2: TMenuItem
        Action = SelectAllAction
      end
      object InvertSelection1: TMenuItem
        Action = InvertSelectionAction
      end
      object ClearSelection1: TMenuItem
        Action = ClearSelectionAction
      end
    end
    object CommonHelpMenu: TMenuItem
      Caption = '&Help'
      Hint = 'Help'
      object WinSCPhomepage1: TMenuItem
        Action = HomepageAction
      end
      object Supportforum1: TMenuItem
        Action = ForumPageAction
      end
      object Versionhistory1: TMenuItem
        Action = HistoryPageAction
      end
      object Programrequirements1: TMenuItem
        Action = RequirementsPageAction
      end
      object N48: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object CheckForUpdates1: TMenuItem
        Action = CheckForUpdatesAction
      end
      object N11: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Donate1: TMenuItem
        Action = DonatePageAction
      end
      object N44: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object About1: TMenuItem
        Action = AboutAction
      end
    end
  end
  object CommanderMenu: TMainMenu
    Images = ExplorerImages
    Left = 152
    Top = 400
    object CommanderLocalMenu: TMenuItem
      Caption = '&Local'
      Hint = 'Change local panel layout or change displayed directory/drive'
      object Changedrive1: TMenuItem
        Action = LocalChangePathAction
      end
      object N16: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object GoTo2: TMenuItem
        Caption = '&Go To'
        Hint = 'Go to directory'
        object OpenDirectory2: TMenuItem
          Action = LocalOpenDirAction
        end
        object Exploredirectory1: TMenuItem
          Action = LocalExploreDirectoryAction
        end
        object N15: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ParentDirectory2: TMenuItem
          Action = LocalParentDirAction
        end
        object RootDirectory2: TMenuItem
          Action = LocalRootDirAction
        end
        object HomeDirectory2: TMenuItem
          Action = LocalHomeDirAction
        end
        object N14: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Back2: TMenuItem
          Action = LocalBackAction
        end
        object Forward2: TMenuItem
          Action = LocalForwardAction
        end
      end
      object Refresh2: TMenuItem
        Action = LocalRefreshAction
      end
      object Addtobookmarks1: TMenuItem
        Action = LocalAddBookmarkAction
      end
      object CopyPathtoClipboard3: TMenuItem
        Action = LocalPathToClipboardAction
      end
      object N30: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Sort1: TMenuItem
        Caption = '&Sort'
        Hint = 'Change file order in local panel'
        object Vzestupn1: TMenuItem
          Action = LocalSortAscendingAction
        end
        object N32: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object N31: TMenuItem
          Action = LocalSortByNameAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByExtension2: TMenuItem
          Action = LocalSortByExtAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByType1: TMenuItem
          Action = LocalSortByTypeAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByModification1: TMenuItem
          Action = LocalSortByChangedAction
          GroupIndex = 1
          RadioItem = True
        end
        object BySize1: TMenuItem
          Action = LocalSortBySizeAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByAttributes1: TMenuItem
          Action = LocalSortByAttrAction
          GroupIndex = 1
          RadioItem = True
        end
      end
      object Showcolumns1: TMenuItem
        Caption = 'Show &Columns'
        object Name1: TMenuItem
          Action = ShowHideLocalNameColumnAction
        end
        object Size1: TMenuItem
          Action = ShowHideLocalSizeColumnAction
        end
        object Type1: TMenuItem
          Action = ShowHideLocalTypeColumnAction
        end
        object Modification1: TMenuItem
          Action = ShowHideLocalChangedColumnAction
        end
        object Attributes1: TMenuItem
          Action = ShowHideLocalAttrColumnAction
        end
      end
    end
    object CommanderFilesMenu: TMenuItem
      Caption = '&Files'
      Hint = 'File operation commands'
      object Createdirectory1: TMenuItem
        Action = CurrentCreateDirAction
      end
      object N13: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Open2: TMenuItem
        Action = CurrentOpenAction
      end
      object Edit2: TMenuItem
        Action = CurrentEditAction
      end
      object Edit4: TMenuItem
        Action = CurrentEditAlternativeAction
      end
      object Editnewfile2: TMenuItem
        Action = EditNewAction
      end
      object Addeditlink1: TMenuItem
        Action = AddEditLinkAction
      end
      object N41: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Copyto3: TMenuItem
        Action = CurrentCopyAction
      end
      object Duplicate2: TMenuItem
        Action = RemoteCopyToAction
      end
      object Moveto3: TMenuItem
        Action = CurrentMoveAction
      end
      object Moveto4: TMenuItem
        Action = RemoteMoveToAction
      end
      object Delete3: TMenuItem
        Action = CurrentDeleteAction
      end
      object Rename2: TMenuItem
        Action = CurrentRenameAction
      end
      object Paste1: TMenuItem
        Action = PasteAction
      end
      object N12: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object CustomCommandsMenu: TMenuItem
        Action = CustomCommandsAction
        object TMenuItem
        end
      end
      object FileNames1: TMenuItem
        Caption = '&File Names'
        Hint = 'Operations with name(s) of selected file(s)'
        object InserttoCommandLine1: TMenuItem
          Action = FileListToCommandLineAction
        end
        object CopytoClipboard1: TMenuItem
          Action = FileListToClipboardAction
        end
        object CopytoClipboardIncludePaths1: TMenuItem
          Action = FullFileListToClipboardAction
        end
      end
      object N43: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Properties3: TMenuItem
        Action = CurrentPropertiesAction
      end
    end
    object CommanderCommandsMenu: TMenuItem
      Caption = '&Commands'
      Hint = 'Other commands'
      object Comparedirectories1: TMenuItem
        Action = CompareDirectoriesAction
      end
      object Keepremotedirectoryuptodate1: TMenuItem
        Action = SynchronizeAction
      end
      object Synchronize1: TMenuItem
        Action = FullSynchronizeAction
      end
      object Synchronizebrowsing1: TMenuItem
        Action = SynchronizeBrowsingAction
        AutoCheck = True
      end
      object Queue3: TMenuItem
        Caption = '&Queue'
        Hint = 'Queue list commands'
        object QueueGoToAction1: TMenuItem
          Action = QueueGoToAction
        end
        object N57: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ShowQuery2: TMenuItem
          Action = QueueItemQueryAction
        end
        object ShowError2: TMenuItem
          Action = QueueItemErrorAction
        end
        object ShowPrompt2: TMenuItem
          Action = QueueItemPromptAction
        end
        object N55: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ExecuteNow2: TMenuItem
          Action = QueueItemExecuteAction
        end
        object Delete5: TMenuItem
          Action = QueueItemDeleteAction
        end
        object N56: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object MoveUp2: TMenuItem
          Action = QueueItemUpAction
        end
        object MoveDown2: TMenuItem
          Action = QueueItemDownAction
        end
      end
      object N47: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object OpenTerminal1: TMenuItem
        Action = ConsoleAction
      end
      object OpeninPuTTY1: TMenuItem
        Action = PuttyAction
      end
      object N49: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Serverprotocolinformation1: TMenuItem
        Action = FileSystemInfoAction
      end
      object ClearCaches1: TMenuItem
        Action = ClearCachesAction
      end
      object N46: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Quit1: TMenuItem
        Action = CloseApplicationAction
      end
    end
    object CommanderOptionsMenu: TMenuItem
      Caption = '&Options'
      Hint = 'Change program layout/preferences'
      object Toolbars2: TMenuItem
        Caption = '&Toolbars'
        Hint = 'Show/hide toolbars'
        object StandardButtons2: TMenuItem
          Action = CommanderToolbarBandAction
        end
        object SessionButtons3: TMenuItem
          Action = CommanderSessionBandAction
        end
        object SessionButtons4: TMenuItem
          Action = CommanderSelectionBandAction
        end
        object PreferencesButtons3: TMenuItem
          Action = CommanderPreferencesBandAction
        end
        object Sortbuttons1: TMenuItem
          Action = CommanderSortBandAction
        end
        object CommandsButtons1: TMenuItem
          Action = CommanderCommandsBandAction
        end
      end
      object Leftpanel1: TMenuItem
        Caption = '&Local Panel'
        Hint = 'Change local panel layout'
        object HistoryButtons1: TMenuItem
          Action = CommanderLocalHistoryBandAction
        end
        object NavigationButtons1: TMenuItem
          Action = CommanderLocalNavigationBandAction
        end
        object N21: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Tree1: TMenuItem
          Action = LocalTreeAction
        end
        object N73: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object StatusBar4: TMenuItem
          Action = LocalStatusBarAction
        end
      end
      object RemotePanel1: TMenuItem
        Caption = '&Remote Panel'
        Hint = 'Change remote panel layout'
        object HistoryButtons2: TMenuItem
          Action = CommanderRemoteHistoryBandAction
        end
        object NavigationButtons2: TMenuItem
          Action = CommanderRemoteNavigationBandAction
        end
        object N22: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Tree2: TMenuItem
          Action = RemoteTreeAction
        end
        object N74: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object StatusBar5: TMenuItem
          Action = RemoteStatusBarAction
        end
      end
      object N20: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object CommandLine1: TMenuItem
        Action = CommandLinePanelAction
      end
      object CommandToolbar1: TMenuItem
        Action = ToolBarAction
      end
      object StatusBar3: TMenuItem
        Action = StatusBarAction
      end
      object LogWindow2: TMenuItem
        Action = ViewLogAction
      end
      object Queue5: TMenuItem
        Caption = '&Queue'
        Hint = 'Configure queue list'
        object Show2: TMenuItem
          Action = QueueShowAction
        end
        object HidewhenEmpty2: TMenuItem
          Action = QueueHideWhenEmptyAction
        end
        object Queue1: TMenuItem
          Action = QueueHideAction
        end
        object N61: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Toolbar1: TMenuItem
          Action = QueueToolbarAction
        end
        object N62: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Customize1: TMenuItem
          Action = QueuePreferencesAction
        end
      end
      object N24: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Preferences2: TMenuItem
        Action = PreferencesAction
      end
    end
    object CommanderRemoteMenu: TMenuItem
      Caption = '&Remote'
      Hint = 'Change remote panel layout or change displayed directory'
      object Changedirectory1: TMenuItem
        Action = RemoteChangePathAction
      end
      object N17: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object GoTo3: TMenuItem
        Caption = '&Go To'
        Hint = 'Go to directory'
        object OpenDirectory3: TMenuItem
          Action = RemoteOpenDirAction
        end
        object N19: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ParentDirectory3: TMenuItem
          Action = RemoteParentDirAction
        end
        object RootDirectory3: TMenuItem
          Action = RemoteRootDirAction
        end
        object HomeDirectory3: TMenuItem
          Action = RemoteHomeDirAction
        end
        object N18: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object Back3: TMenuItem
          Action = RemoteBackAction
        end
        object Forward3: TMenuItem
          Action = RemoteForwardAction
        end
      end
      object Refresh3: TMenuItem
        Action = RemoteRefreshAction
      end
      object Addtobookmarks2: TMenuItem
        Action = RemoteAddBookmarkAction
      end
      object CopyPathtoClipboard4: TMenuItem
        Action = RemotePathToClipboardAction
      end
      object N33: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Sort2: TMenuItem
        Caption = '&Sort'
        Hint = 'Change file order in remote panel'
        object Ascending1: TMenuItem
          Action = RemoteSortAscendingAction
        end
        object N34: TMenuItem
          Caption = '-'
          Hint = 'E'
        end
        object ByName1: TMenuItem
          Action = RemoteSortByNameAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByExtension3: TMenuItem
          Action = RemoteSortByExtAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByModification2: TMenuItem
          Action = RemoteSortByChangedAction
          GroupIndex = 1
          RadioItem = True
        end
        object BySize2: TMenuItem
          Action = RemoteSortBySizeAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByAttributes2: TMenuItem
          Action = RemoteSortByRightsAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByOwner1: TMenuItem
          Action = RemoteSortByOwnerAction
          GroupIndex = 1
          RadioItem = True
        end
        object ByGroup1: TMenuItem
          Action = RemoteSortByGroupAction
          GroupIndex = 1
          RadioItem = True
        end
      end
      object Showcolumns2: TMenuItem
        Caption = 'Show &Columns'
        object Name2: TMenuItem
          Action = ShowHideRemoteNameColumnAction
        end
        object Size2: TMenuItem
          Action = ShowHideRemoteSizeColumnAction
        end
        object Modification2: TMenuItem
          Action = ShowHideRemoteChangedColumnAction
        end
        object Attributes2: TMenuItem
          Action = ShowHideRemoteRightsColumnAction
        end
        object Owner1: TMenuItem
          Action = ShowHideRemoteOwnerColumnAction
        end
        object Group1: TMenuItem
          Action = ShowHideRemoteGroupColumnAction
        end
      end
    end
  end
  object CommanderBarPopup: TPopupMenu
    Images = ExplorerImages
    Left = 424
    Top = 264
    object StandardButtons3: TMenuItem
      Action = CommanderToolbarBandAction
    end
    object SessionButtons5: TMenuItem
      Action = CommanderSessionBandAction
    end
    object SelectionButtons3: TMenuItem
      Action = CommanderSelectionBandAction
    end
    object PreferencesButtons4: TMenuItem
      Action = CommanderPreferencesBandAction
    end
    object SortButtons2: TMenuItem
      Action = CommanderSortBandAction
    end
    object CommandsButtons2: TMenuItem
      Action = CommanderCommandsBandAction
    end
    object N26: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object CommandLine2: TMenuItem
      Action = CommandLinePanelAction
    end
    object CommandsToolbar1: TMenuItem
      Action = ToolBarAction
    end
    object StatusBar8: TMenuItem
      Action = StatusBarAction
    end
    object N27: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object LocalPanel1: TMenuItem
      Caption = '&Local Panel'
      Hint = 'Change local panel layout'
      object HistoryButtons3: TMenuItem
        Action = CommanderLocalHistoryBandAction
      end
      object NavigationButtons3: TMenuItem
        Action = CommanderLocalNavigationBandAction
      end
      object N23: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Tree7: TMenuItem
        Action = LocalTreeAction
      end
      object N77: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object StatusBar6: TMenuItem
        Action = LocalStatusBarAction
      end
    end
    object RemotePanel2: TMenuItem
      Caption = '&Remote Panel'
      Hint = 'Change remote panel layout'
      object HistoryButtons4: TMenuItem
        Action = CommanderRemoteHistoryBandAction
      end
      object NavigationButtons4: TMenuItem
        Action = CommanderRemoteNavigationBandAction
      end
      object N25: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Tree8: TMenuItem
        Action = RemoteTreeAction
      end
      object N78: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object StatusBar7: TMenuItem
        Action = RemoteStatusBarAction
      end
    end
    object Options1: TMenuItem
      Caption = '&Queue'
      Hint = 'Configure queue list'
      object Show5: TMenuItem
        Action = QueueShowAction
      end
      object HidewhenEmpty5: TMenuItem
        Action = QueueHideWhenEmptyAction
      end
      object Hide4: TMenuItem
        Action = QueueHideAction
      end
      object N69: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Toolbar4: TMenuItem
        Action = QueueToolbarAction
      end
      object N68: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Customize4: TMenuItem
        Action = QueuePreferencesAction
      end
    end
  end
  object RemotePanelPopup: TPopupMenu
    Images = ExplorerImages
    Left = 312
    Top = 264
    object CopyPathtoClipboard1: TMenuItem
      Action = RemotePathToClipboardAction
    end
    object OpenDirectoryBookmark1: TMenuItem
      Action = RemoteOpenDirAction
    end
    object N51: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object HistoryButtons5: TMenuItem
      Action = CommanderRemoteHistoryBandAction
    end
    object NavigationButtons5: TMenuItem
      Action = CommanderRemoteNavigationBandAction
    end
    object N28: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Tree5: TMenuItem
      Action = RemoteTreeAction
    end
    object N75: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object StatusBar9: TMenuItem
      Action = RemoteStatusBarAction
    end
  end
  object LocalPanelPopup: TPopupMenu
    Images = ExplorerImages
    Left = 312
    Top = 336
    object CopyPathtoClipboard2: TMenuItem
      Action = LocalPathToClipboardAction
    end
    object OpenDirectoryBookmark2: TMenuItem
      Action = LocalOpenDirAction
    end
    object N52: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object HistoryButtons6: TMenuItem
      Action = CommanderLocalHistoryBandAction
    end
    object NavigationButtons6: TMenuItem
      Action = CommanderLocalNavigationBandAction
    end
    object N29: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Tree6: TMenuItem
      Action = LocalTreeAction
    end
    object N76: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object StatusBar10: TMenuItem
      Action = LocalStatusBarAction
    end
  end
  object LocalDirViewColumnPopup: TPopupMenu
    Images = ExplorerImages
    Left = 248
    Top = 88
    object SortAscending1: TMenuItem
      Action = SortColumnAscendingAction
    end
    object SortDescending1: TMenuItem
      Action = SortColumnDescendingAction
    end
    object Hidecolumn1: TMenuItem
      Action = HideColumnAction
    end
    object N37: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Showcolumns3: TMenuItem
      Caption = 'Show &Columns'
      object Name3: TMenuItem
        Action = ShowHideLocalNameColumnAction
      end
      object Size3: TMenuItem
        Action = ShowHideLocalSizeColumnAction
      end
      object Type2: TMenuItem
        Action = ShowHideLocalTypeColumnAction
      end
      object Modification3: TMenuItem
        Action = ShowHideLocalChangedColumnAction
      end
      object Attributes3: TMenuItem
        Action = ShowHideLocalAttrColumnAction
      end
    end
  end
  object RemoteDirViewColumnPopup: TPopupMenu
    Images = ExplorerImages
    Left = 424
    Top = 88
    object MenuItem1: TMenuItem
      Action = SortColumnAscendingAction
    end
    object MenuItem2: TMenuItem
      Action = SortColumnDescendingAction
    end
    object Hidecolumn2: TMenuItem
      Action = HideColumnAction
    end
    object N38: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Showcolumns4: TMenuItem
      Caption = 'Show &Columns'
      object Name4: TMenuItem
        Action = ShowHideRemoteNameColumnAction
      end
      object Size4: TMenuItem
        Action = ShowHideRemoteSizeColumnAction
      end
      object Modification4: TMenuItem
        Action = ShowHideRemoteChangedColumnAction
      end
      object Permissions1: TMenuItem
        Action = ShowHideRemoteRightsColumnAction
      end
      object Owner2: TMenuItem
        Action = ShowHideRemoteOwnerColumnAction
      end
      object Group2: TMenuItem
        Action = ShowHideRemoteGroupColumnAction
      end
    end
  end
  object ArrowImages: TImageList
    ShareImages = True
    Left = 168
    Top = 176
  end
  object QueueImages: TImageList
    ShareImages = True
    Left = 320
    Top = 176
  end
  object QueuePopup: TPopupMenu
    AutoLineReduction = maManual
    Images = ExplorerImages
    OnPopup = QueuePopupPopup
    Left = 392
    Top = 176
    object ShowQuery1: TMenuItem
      Action = QueueItemQueryAction
    end
    object ShowError1: TMenuItem
      Action = QueueItemErrorAction
    end
    object ShowPrompt1: TMenuItem
      Action = QueueItemPromptAction
    end
    object N53: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object ExecuteNow1: TMenuItem
      Action = QueueItemExecuteAction
    end
    object Delete4: TMenuItem
      Action = QueueItemDeleteAction
    end
    object N54: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object MoveUp1: TMenuItem
      Action = QueueItemUpAction
    end
    object MoveDown1: TMenuItem
      Action = QueueItemDownAction
    end
    object N67: TMenuItem
      Caption = '-'
      Hint = 'E'
    end
    object Queue2: TMenuItem
      Caption = '&Options'
      Hint = 'Configure queue list'
      object Show4: TMenuItem
        Action = QueueShowAction
      end
      object HidewhenEmpty4: TMenuItem
        Action = QueueHideWhenEmptyAction
      end
      object Hide3: TMenuItem
        Action = QueueHideAction
      end
      object N66: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Toolbar3: TMenuItem
        Action = QueueToolbarAction
      end
      object N65: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Customize3: TMenuItem
        Action = QueuePreferencesAction
      end
    end
  end
  object QueueShowPopup: TPopupMenu
    Images = ExplorerImages
    Left = 88
    Top = 264
    object Show1: TMenuItem
      Action = QueueShowAction
    end
    object HidewhenEmpty1: TMenuItem
      Action = QueueHideWhenEmptyAction
    end
    object Hide1: TMenuItem
      Action = QueueHideAction
    end
  end
  object RemoteDirViewPopup: TPopupMenu
    Images = ExplorerImages
    Left = 360
    Top = 400
    object GoTo4: TMenuItem
      Caption = '&Go To'
      Hint = 'Go to directory'
      object OpenDirectoryBookmark3: TMenuItem
        Action = RemoteOpenDirAction
      end
      object N81: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object ParentDirectory4: TMenuItem
        Action = RemoteParentDirAction
      end
      object RootDirectory4: TMenuItem
        Action = RemoteRootDirAction
      end
      object HomeDirectory4: TMenuItem
        Action = RemoteHomeDirAction
      end
      object N80: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Back4: TMenuItem
        Action = RemoteBackAction
      end
      object Forward4: TMenuItem
        Action = RemoteForwardAction
      end
    end
    object Refresh4: TMenuItem
      Action = RemoteRefreshAction
    end
    object AddToBookmarks4: TMenuItem
      Action = RemoteAddBookmarkAction
    end
    object CopyPathtoClipboard6: TMenuItem
      Action = RemotePathToClipboardAction
    end
  end
  object LocalDirViewPopup: TPopupMenu
    Images = ExplorerImages
    Left = 472
    Top = 400
    object GoTo5: TMenuItem
      Caption = '&Go To'
      Hint = 'Go to directory'
      object OpenDirectoryBookmark4: TMenuItem
        Action = LocalOpenDirAction
      end
      object ExploreDirectory2: TMenuItem
        Action = LocalExploreDirectoryAction
      end
      object N84: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object ParentDirectory5: TMenuItem
        Action = LocalParentDirAction
      end
      object RootDirectory5: TMenuItem
        Action = LocalRootDirAction
      end
      object HomeDirectory5: TMenuItem
        Action = LocalHomeDirAction
      end
      object N83: TMenuItem
        Caption = '-'
        Hint = 'E'
      end
      object Back5: TMenuItem
        Action = LocalBackAction
      end
      object Forward5: TMenuItem
        Action = LocalForwardAction
      end
    end
    object Refresh5: TMenuItem
      Action = LocalRefreshAction
    end
    object AddToBookmarks5: TMenuItem
      Action = LocalAddBookmarkAction
    end
    object CopyPathtoClipboard7: TMenuItem
      Action = LocalPathToClipboardAction
    end
  end
end
