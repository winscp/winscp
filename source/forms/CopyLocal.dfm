object CopyLocalDialog: TCopyLocalDialog
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'ui_copy_local'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyLocalDialog'
  ClientHeight = 126
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    511
    126)
  TextHeight = 15
  object Image: TImage
    Left = 8
    Top = 11
    Width = 32
    Height = 32
    AutoSize = True
  end
  object DirectoryLabel: TLabel
    Left = 46
    Top = 8
    Width = 63
    Height = 15
    Caption = '&Target path:'
    FocusControl = DirectoryEdit
  end
  object DirectoryEdit: THistoryComboBox
    Left = 46
    Top = 26
    Width = 371
    Height = 23
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    TabOrder = 0
    Text = 'DirectoryEdit'
    OnExit = DirectoryEditExit
  end
  object OkButton: TButton
    Left = 251
    Top = 55
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 337
    Top = 55
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 423
    Top = 25
    Width = 80
    Height = 25
    Caption = 'B&rowse...'
    TabOrder = 1
    OnClick = LocalDirectoryBrowseButtonClick
  end
  object HelpButton: TButton
    Left = 423
    Top = 55
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 5
    OnClick = HelpButtonClick
  end
  object NeverShowAgainCheck: TCheckBox
    Left = 10
    Top = 59
    Width = 237
    Height = 17
    Caption = '&Do not show this dialog box again'
    TabOrder = 2
  end
  object ShortCutHintPanel: TPanel
    Left = 0
    Top = 88
    Width = 511
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 6
    DesignSize = (
      511
      38)
    object ShortCutHintLabel: TLabel
      Left = 8
      Top = 3
      Width = 495
      Height = 32
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 
        'In Commander interface the keyboard shortcut F5 is used to trans' +
        'fer files. Should you want to use it to refresh a file panel, cl' +
        'ick here to go to preferences.'
      ShowAccelChar = False
      WordWrap = True
      OnClick = ShortCutHintLabelClick
    end
  end
end
