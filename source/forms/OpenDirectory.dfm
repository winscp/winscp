object OpenDirectoryDialog: TOpenDirectoryDialog
  Left = 511
  Top = 239
  HelpType = htKeyword
  HelpKeyword = 'ui_opendir'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Open directory'
  ClientHeight = 342
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    450
    342)
  TextHeight = 15
  object EditLabel: TLabel
    Left = 46
    Top = 8
    Width = 82
    Height = 15
    Caption = '&Open directory:'
  end
  object Image: TImage
    Left = 8
    Top = 11
    Width = 32
    Height = 32
    AutoSize = True
  end
  object LocalDirectoryEdit: THistoryComboBox
    Left = 46
    Top = 26
    Width = 310
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'LocalDirectoryEdit'
    OnChange = DirectoryEditChange
    SaveOn = []
  end
  object RemoteDirectoryEdit: THistoryComboBox
    Left = 46
    Top = 26
    Width = 396
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    MaxLength = 1000
    TabOrder = 0
    Text = 'RemoteDirectoryEdit'
    OnChange = DirectoryEditChange
    SaveOn = []
  end
  object OKBtn: TButton
    Left = 190
    Top = 309
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object CancelBtn: TButton
    Left = 276
    Top = 309
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object PageControl: TPageControl
    Left = 8
    Top = 55
    Width = 434
    Height = 248
    ActivePage = SessionBookmarksSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnChange = PageControlChange
    object SessionBookmarksSheet: TTabSheet
      Tag = 1
      Caption = 'Site bookmarks'
      DesignSize = (
        426
        218)
      object SessionBookmarksList: TListBox
        Tag = 1
        Left = 5
        Top = 5
        Width = 328
        Height = 207
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        ItemHeight = 15
        TabOrder = 0
        OnClick = BookmarksListClick
        OnDblClick = BookmarksListDblClick
        OnDragDrop = BookmarksListDragDrop
        OnDragOver = BookmarksListDragOver
        OnEndDrag = BookmarksListEndDrag
        OnKeyDown = BookmarksListKeyDown
        OnStartDrag = BookmarksListStartDrag
      end
      object AddSessionBookmarkButton: TButton
        Tag = 1
        Left = 339
        Top = 5
        Width = 80
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Add'
        TabOrder = 1
        OnClick = AddBookmarkButtonClick
      end
      object RemoveSessionBookmarkButton: TButton
        Tag = 1
        Left = 339
        Top = 36
        Width = 80
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Remo&ve'
        TabOrder = 2
        OnClick = RemoveBookmarkButtonClick
      end
      object UpSessionBookmarkButton: TButton
        Tag = -1
        Left = 339
        Top = 156
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Up'
        TabOrder = 3
        OnClick = BookmarkButtonClick
      end
      object DownSessionBookmarkButton: TButton
        Tag = 1
        Left = 339
        Top = 187
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Down'
        TabOrder = 4
        OnClick = BookmarkButtonClick
      end
    end
    object SharedBookmarksSheet: TTabSheet
      Tag = 2
      Caption = 'Shared bookmarks'
      ImageIndex = 1
      DesignSize = (
        426
        218)
      object SharedBookmarksList: TListBox
        Tag = 2
        Left = 5
        Top = 5
        Width = 328
        Height = 207
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        ItemHeight = 15
        TabOrder = 0
        OnClick = BookmarksListClick
        OnDblClick = BookmarksListDblClick
        OnDragDrop = BookmarksListDragDrop
        OnDragOver = BookmarksListDragOver
        OnEndDrag = BookmarksListEndDrag
        OnKeyDown = BookmarksListKeyDown
        OnStartDrag = BookmarksListStartDrag
      end
      object AddSharedBookmarkButton: TButton
        Tag = 2
        Left = 339
        Top = 5
        Width = 80
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Add'
        TabOrder = 1
        OnClick = AddBookmarkButtonClick
      end
      object RemoveSharedBookmarkButton: TButton
        Tag = 2
        Left = 339
        Top = 36
        Width = 80
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Remo&ve'
        TabOrder = 2
        OnClick = RemoveBookmarkButtonClick
      end
      object UpSharedBookmarkButton: TButton
        Tag = -2
        Left = 339
        Top = 156
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Up'
        TabOrder = 4
        OnClick = BookmarkButtonClick
      end
      object DownSharedBookmarkButton: TButton
        Tag = 2
        Left = 339
        Top = 187
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Down'
        TabOrder = 5
        OnClick = BookmarkButtonClick
      end
      object ShortCutSharedBookmarkButton: TButton
        Tag = 2
        Left = 339
        Top = 67
        Width = 80
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Shortcut...'
        TabOrder = 3
        OnClick = ShortCutBookmarkButtonClick
      end
    end
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 362
    Top = 25
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'B&rowse...'
    TabOrder = 2
    OnClick = LocalDirectoryBrowseButtonClick
  end
  object SwitchButton: TButton
    Left = 8
    Top = 309
    Width = 134
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Location Profiles...'
    ModalResult = 2
    TabOrder = 4
    OnClick = SwitchButtonClick
  end
  object HelpButton: TButton
    Left = 362
    Top = 309
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpButtonClick
  end
end
