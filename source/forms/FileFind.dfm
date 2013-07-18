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
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    000000000000000000000000000000000000736152CE736052FF736052FF7360
    52FF736152CE00000000736152CE736052FF736052FF736052FF736152CE0000
    000000000000000000000000000000000000736052FFD3C9C2FFFFFFFFFFA590
    80FF736052FF00000000736052FFB9A89CFFFFFFFFFFEDE9E6FF736052FF0000
    00000000000000000000000000000000000035302EF5292626FF292626FF2926
    26FF35302EF50000000035302DF6292626FF292626FF292626FF35302DF60000
    000000000000000000000000000000000000292626E751443AFF99816FFFB9AD
    A5FF292626E700000000292626E7B8ACA4FF99816FFF51443AFF292626E70000
    0000000000000000000000000000000000002A2727A54D423BFF8E7867FF8E84
    7DFF2A2727A5000000002A2727A58E847DFF8E7867FF4D423BFF2A2727A51D84
    B1F31E84B2FF1E84B2FF1E84B2FF1E84B2FF226381FF423A35FF5D5047FF4B46
    44FF225F7BFF1E84B2FF235D78F84B4644FF5D5047FF423A35FF2825255A1E84
    B2FF84E9FEFF84E9FEFF84E9FEFF84E9FEFF4F7780FF292626FF292626FF2926
    26FF292626FF292626FF292626FF292626FF292626FF292626FF292525961E84
    B2FF3AC6F0FF37C5F0FF37C5F0FF37C5F0FF2E5E6DFF65564CFFBBAB9FFF9981
    6FFFB4A396FFBBAEA4FF8A796DFF99816FFFBBAB9FFF65564CFF292626A71E84
    B2FF56E3FFFF49E1FFFF49E1FFFF48E1FFFF3EA3B8FF453C37FFBBAB9FFF9981
    6FFF292626FF292626FF292626FF99816FFFBBAB9FFF453C37FF2B2727541E84
    B2FF65E8FFFF4EE4FFFF4EE4FFFF4EE4FFFF4BDAF5FF2E3E42FF292626FF2926
    26FF292626FF2B3031FF292626FF292626FF292626FF292626DC2B2B2B0C1E84
    B2FF73EBFFFF54E7FFFF53E7FFFF53E7FFFF53E6FFFF52E6FFFF292626FFBAAA
    9EFF292626FF51E5FFFF292626FFBAAA9EFF292626FF00000000000000001E84
    B2FFEAFDFFFFEAFDFFFFEAFDFFFFEAFDFFFFEAFDFFFFEAFDFFFF555757FF2926
    26FF535556FFEAFDFFFF263B46FF292626FF292626C700000000000000001E84
    B2FF1E84B2FF1E84B2FF1E84B2FF1E84B2FF1E84B2FF1E84B2FF1E84B2FF1E84
    B2FF1E84B2FF1E84B2FF1E84B2FF000000000000000000000000000000001E84
    B2FF34CCF6FF34CCF6FF34CCF6FF34CCF6FF34CCF6FF34CCF6FF34CCF6FF34CC
    F6FF34CCF6FF34CCF6FF1E84B2FF000000000000000000000000000000001E85
    B2CA31B4DBFF41DDFFFF41DDFFFF41DDFFFF2EADD6FF1E84B2FF1E84B2FF1E84
    B2FF1E84B2FF1E84B2FF1E84B2C3000000000000000000000000000000001A80
    B3141E84B2C91E84B2FF1E84B2FF1E84B2FF1D84B2C71B80B61C000000000000
    000000000000000000000000000000000000000000000000000000000000F820
    0000F8200000F8200000F8200000F82000000000000000000000000000000000
    0000000000000003000000030000000F0000000F0000000F000001FF0000}
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
