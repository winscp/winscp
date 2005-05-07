object OpenDirectoryDialog: TOpenDirectoryDialog
  Left = 408
  Top = 195
  HelpType = htKeyword
  HelpKeyword = 'ui_opendir'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Open directory'
  ClientHeight = 308
  ClientWidth = 405
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    405
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object EditLabel: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = '&Open directory:'
  end
  object LocalDirectoryEdit: TIEComboBox
    Left = 8
    Top = 25
    Width = 308
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    Text = 'LocalDirectoryEdit'
    OnChange = DirectoryEditChange
  end
  object RemoteDirectoryEdit: TIEComboBox
    Left = 8
    Top = 25
    Width = 389
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    MaxLength = 1000
    TabOrder = 0
    Text = 'RemoteDirectoryEdit'
    OnChange = DirectoryEditChange
  end
  object OKBtn: TButton
    Left = 159
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object CancelBtn: TButton
    Left = 239
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object BookmarksGroup: TXPGroupBox
    Left = 8
    Top = 56
    Width = 386
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Bookmarks'
    TabOrder = 3
    DesignSize = (
      386
      209)
    object Label1: TLabel
      Left = 16
      Top = 22
      Width = 83
      Height = 13
      Caption = 'Select &bookmark:'
      FocusControl = BookmarksList
    end
    object BookmarksList: TListBox
      Left = 16
      Top = 38
      Width = 266
      Height = 155
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 0
      OnClick = BookmarksListClick
      OnDblClick = BookmarksListDblClick
      OnDragDrop = BookmarksListDragDrop
      OnDragOver = BookmarksListDragOver
      OnKeyDown = BookmarksListKeyDown
      OnStartDrag = BookmarksListStartDrag
    end
    object AddBookmarkButton: TButton
      Left = 289
      Top = 38
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      TabOrder = 1
      OnClick = AddBookmarkButtonClick
    end
    object RemoveBookmarkButton: TButton
      Left = 289
      Top = 70
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      TabOrder = 2
      OnClick = RemoveBookmarkButtonClick
    end
    object DownBookmarkButton: TButton
      Left = 289
      Top = 168
      Width = 83
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Down'
      TabOrder = 3
      OnClick = BookmarkButtonClick
    end
    object UpBookmarkButton: TButton
      Left = 289
      Top = 136
      Width = 83
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Up'
      TabOrder = 4
      OnClick = BookmarkButtonClick
    end
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 320
    Top = 23
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'B&rowse...'
    TabOrder = 2
    OnClick = LocalDirectoryBrowseButtonClick
  end
  object SwitchButton: TButton
    Left = 8
    Top = 274
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Location Profiles...'
    ModalResult = 2
    TabOrder = 4
    OnClick = SwitchButtonClick
  end
  object HelpButton: TButton
    Left = 320
    Top = 274
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpButtonClick
  end
end
