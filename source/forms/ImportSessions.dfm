object ImportSessionsDialog: TImportSessionsDialog
  Left = 362
  Top = 186
  HelpType = htKeyword
  HelpKeyword = 'ui_import'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Import sitesX'
  ClientHeight = 307
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    418
    307)
  TextHeight = 15
  object Label: TLabel
    Left = 8
    Top = 12
    Width = 68
    Height = 15
    Caption = '&Import from:'
    FocusControl = SourceComboBox
  end
  object OKButton: TButton
    Left = 158
    Top = 274
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object CancelButton: TButton
    Left = 244
    Top = 274
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object SessionListView2: TListView
    Left = 8
    Top = 39
    Width = 402
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Width = 240
      end>
    ColumnClick = False
    DoubleBuffered = True
    HideSelection = False
    ReadOnly = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowColumnHeaders = False
    ShowHint = True
    TabOrder = 3
    ViewStyle = vsReport
    OnInfoTip = SessionListView2InfoTip
    OnKeyUp = SessionListView2KeyUp
    OnMouseDown = SessionListView2MouseDown
  end
  object CheckAllButton: TButton
    Left = 8
    Top = 274
    Width = 125
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Un/check &all'
    TabOrder = 4
    OnClick = CheckAllButtonClick
  end
  object HelpButton: TButton
    Left = 330
    Top = 274
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpButtonClick
  end
  object SourceComboBox: TComboBox
    Left = 116
    Top = 9
    Width = 133
    Height = 23
    Style = csDropDownList
    TabOrder = 0
    OnSelect = SourceComboBoxSelect
    Items.Strings = (
      'PuTTY'
      'KiTTY'
      'FileZilla'
      'OpenSSH'
      'INI file'
      'known_hosts')
  end
  object ErrorPanel: TPanel
    Left = 48
    Top = 92
    Width = 321
    Height = 125
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 8
    object ErrorLabel: TLabel
      Left = 0
      Top = 0
      Width = 321
      Height = 125
      Align = alClient
      Alignment = taCenter
      Caption = 'ErrorLabel'
      ShowAccelChar = False
      Layout = tlCenter
      WordWrap = True
    end
  end
  object PasteButton: TButton
    Left = 255
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Paste'
    TabOrder = 1
    OnClick = PasteButtonClick
  end
  object BrowseButton: TButton
    Left = 255
    Top = 8
    Width = 80
    Height = 25
    Caption = 'B&rowse...'
    TabOrder = 2
    OnClick = BrowseButtonClick
  end
end
