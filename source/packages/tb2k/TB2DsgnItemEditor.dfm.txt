object TBItemEditForm: TTBItemEditForm
  Left = 200
  Top = 104
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 247
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 129
    Top = 19
    Width = 3
    Height = 228
    Cursor = crHSplit
    ResizeStyle = rsUpdate
  end
  object TreeView: TTreeView
    Left = 0
    Top = 19
    Width = 129
    Height = 228
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    ShowRoot = False
    TabOrder = 2
    OnChange = TreeViewChange
    OnDragDrop = TreeViewDragDrop
    OnDragOver = TreeViewDragOver
    OnEnter = TreeViewEnter
    OnKeyDown = TreeViewKeyDown
    OnKeyPress = TreeViewKeyPress
  end
  object ListView: TListView
    Left = 132
    Top = 19
    Width = 308
    Height = 228
    Align = alClient
    Columns = <
      item
        Caption = 'Caption'
        Width = 160
      end
      item
        Caption = 'Type'
        Width = 120
      end>
    ColumnClick = False
    DragMode = dmAutomatic
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = TBPopupMenu1
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnDblClick = ListViewDblClick
    OnEnter = ListViewEnter
    OnDragDrop = ListViewDragDrop
    OnDragOver = ListViewDragOver
    OnKeyDown = ListViewKeyDown
    OnKeyPress = ListViewKeyPress
  end
  object Toolbar: TTBToolbar
    Left = 0
    Top = 0
    Width = 440
    Height = 19
    Align = alTop
    Caption = 'Toolbar'
    DockPos = 0
    FullSize = True
    LinkSubitems = ToolbarItems
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object TBPopupMenu1: TTBPopupMenu
    LinkSubitems = ToolbarItems
    Left = 256
    Top = 120
  end
  object TBItemContainer1: TTBItemContainer
    Left = 224
    Top = 120
    object ToolbarItems: TTBSubmenuItem
      object NewItemButton: TTBItem
        Caption = 'New &Item'
        Hint = 'New Item'
        ImageIndex = 0
        ShortCut = 45
        OnClick = NewItemButtonClick
      end
      object NewSubmenuButton: TTBItem
        Caption = 'New &Submenu'
        Hint = 'New Submenu'
        ImageIndex = 1
        ShortCut = 16429
        OnClick = NewSubmenuButtonClick
      end
      object NewSepButton: TTBItem
        Caption = 'New Se&parator'
        Hint = 'New Separator'
        ImageIndex = 2
        ShortCut = 189
        OnClick = NewSepButtonClick
      end
      object MoreMenu: TTBSubmenuItem
        Caption = '&More'
        Options = [tboDropdownArrow]
      end
      object TBSeparatorItem1: TTBSeparatorItem
      end
      object CutButton: TTBItem
        Caption = 'Cu&t'
        Enabled = False
        Hint = 'Cut'
        ImageIndex = 5
        OnClick = CutButtonClick
      end
      object CopyButton: TTBItem
        Caption = '&Copy'
        Enabled = False
        Hint = 'Copy'
        ImageIndex = 4
        OnClick = CopyButtonClick
      end
      object PasteButton: TTBItem
        Caption = '&Paste'
        Hint = 'Paste'
        ImageIndex = 6
        OnClick = PasteButtonClick
      end
      object DeleteButton: TTBItem
        Caption = '&Delete Item'
        Enabled = False
        Hint = 'Delete Item'
        ImageIndex = 3
        ShortCut = 46
        OnClick = DeleteButtonClick
      end
      object TBSeparatorItem2: TTBSeparatorItem
      end
      object MoveUpButton: TTBItem
        Caption = 'Move &Up'
        Hint = 'Move Up'
        ImageIndex = 7
        ShortCut = 32806
        OnClick = MoveUpButtonClick
      end
      object MoveDownButton: TTBItem
        Caption = 'Move D&own'
        Hint = 'Move Down'
        ImageIndex = 8
        ShortCut = 32808
        OnClick = MoveDownButtonClick
      end
      object TBSeparatorItem3: TTBSeparatorItem
      end
      object TBSubmenuItem1: TTBSubmenuItem
        Caption = '&Tools'
        Options = [tboDropdownArrow]
        object TConvertMenu: TTBItem
          Caption = '&Convert TMainMenu/TPopupMenu...'
          OnClick = TConvertMenuClick
        end
      end
    end
  end
end
