object FileFindDialog: TFileFindDialog
  Left = 367
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_find'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'FindX'
  ClientHeight = 417
  ClientWidth = 562
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    562
    417)
  PixelsPerInch = 96
  TextHeight = 13
  object FilterGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 461
    Height = 127
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Filter'
    TabOrder = 0
    DesignSize = (
      461
      127)
    object MaskLabel: TLabel
      Left = 11
      Top = 20
      Width = 47
      Height = 13
      Caption = '&File mask:'
      FocusControl = MaskEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 11
      Top = 71
      Width = 48
      Height = 13
      Caption = 'Sear&ch in:'
      FocusControl = RemoteDirectoryEdit
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 87
      Width = 439
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 3
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object MaskEdit: THistoryComboBox
      Left = 11
      Top = 36
      Width = 361
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'MaskEdit'
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object MaskHintText: TStaticText
      Left = 275
      Top = 59
      Width = 97
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'mask hi&nts'
      TabOrder = 1
      TabStop = True
    end
    object MaskButton: TButton
      Left = 378
      Top = 33
      Width = 72
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = MaskButtonClick
    end
  end
  object CancelButton: TButton
    Left = 476
    Top = 43
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
  end
  object StartStopButton: TButton
    Left = 476
    Top = 11
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&StartX'
    Default = True
    TabOrder = 1
    OnClick = StartStopButtonClick
  end
  object HelpButton: TButton
    Left = 476
    Top = 75
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object FileView: TIEListView
    Left = 8
    Top = 142
    Width = 461
    Height = 252
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColumnClick = False
    FullDrag = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnDblClick = FileViewDblClick
    NortonLike = nlOff
    Columns = <
      item
        Caption = 'Name'
        Width = 80
      end
      item
        Caption = 'Directory'
        Width = 120
      end
      item
        Alignment = taRightJustify
        Caption = 'Size'
        Width = 80
      end
      item
        Caption = 'Changed'
        Width = 90
      end>
    MultiSelect = False
    OnSelectItem = FileViewSelectItem
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 398
    Width = 562
    Height = 19
    Panels = <>
    ParentShowHint = False
    ShowHint = True
    SimplePanel = True
  end
  object FocusButton: TButton
    Left = 476
    Top = 142
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Fo&cus'
    ModalResult = 1
    TabOrder = 6
    OnClick = FocusButtonClick
  end
  object MinimizeButton: TButton
    Left = 476
    Top = 43
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 3
    OnClick = MinimizeButtonClick
  end
end
