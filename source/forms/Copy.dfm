object CopyDialog: TCopyDialog
  Left = 363
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_copy'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyDialog'
  ClientHeight = 235
  ClientWidth = 567
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
    567
    235)
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
    Width = 212
    Height = 15
    Caption = 'Copy 2 selected files to remote directory'
  end
  object LocalDirectoryEdit: THistoryComboBox
    Left = 46
    Top = 26
    Width = 427
    Height = 23
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    TabOrder = 0
    Text = 'LocalDirectoryEdit'
    OnChange = ControlChange
    OnExit = LocalDirectoryEditExit
  end
  object RemoteDirectoryEdit: THistoryComboBox
    Left = 46
    Top = 26
    Width = 513
    Height = 23
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    MaxLength = 1000
    TabOrder = 2
    Text = 'RemoteDirectoryEdit'
    OnChange = ControlChange
  end
  object OkButton: TButton
    Left = 307
    Top = 143
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnDropDownClick = OkButtonDropDownClick
  end
  object CancelButton: TButton
    Left = 393
    Top = 143
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 479
    Top = 25
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'B&rowse...'
    TabOrder = 1
    OnClick = LocalDirectoryBrowseButtonClick
  end
  object QueueCheck2: TCheckBox
    Left = 10
    Top = 120
    Width = 317
    Height = 17
    Caption = 'Transfer in &background (add to transfer queue) X'
    TabOrder = 4
    OnClick = ControlChange
  end
  object HelpButton: TButton
    Left = 479
    Top = 143
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 8
    OnClick = HelpButtonClick
  end
  object NeverShowAgainCheck: TCheckBox
    Left = 10
    Top = 174
    Width = 253
    Height = 17
    Caption = '&Do not show this dialog box again'
    TabOrder = 10
    OnClick = NeverShowAgainCheckClick
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 143
    Width = 175
    Height = 25
    Caption = 'Transfer settin&gs...'
    Style = bsSplitButton
    TabOrder = 5
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 55
    Width = 551
    Height = 59
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 3
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      551
      59)
    object CopyParamLabel: TLabel
      Left = 9
      Top = 20
      Width = 533
      Height = 35
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      ShowAccelChar = False
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object ShortCutHintPanel: TPanel
    Left = 0
    Top = 197
    Width = 567
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 9
    DesignSize = (
      567
      38)
    object ShortCutHintLabel: TLabel
      Left = 8
      Top = 3
      Width = 551
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
  object OkMenu: TPopupMenu
    Left = 456
    Top = 69
    object DownloadItem: TMenuItem
      Caption = '&Download'
      Default = True
      OnClick = DownloadItemClick
    end
    object BrowseItem: TMenuItem
      Caption = '&Browse'
      OnClick = BrowseItemClick
    end
  end
end
