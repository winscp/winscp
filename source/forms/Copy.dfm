object CopyDialog: TCopyDialog
  Left = 363
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_copy'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyDialog'
  ClientHeight = 225
  ClientWidth = 511
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    511
    225)
  PixelsPerInch = 96
  TextHeight = 13
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
    Width = 195
    Height = 13
    Caption = 'Copy 2 selected files to remote directory'
  end
  object LocalDirectoryEdit: THistoryComboBox
    Left = 46
    Top = 25
    Width = 372
    Height = 21
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
    Top = 25
    Width = 456
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    MaxLength = 1000
    TabOrder = 2
    Text = 'RemoteDirectoryEdit'
    OnChange = ControlChange
  end
  object OkButton: TButton
    Left = 260
    Top = 136
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnDropDownClick = OkButtonDropDownClick
  end
  object CancelButton: TButton
    Left = 343
    Top = 136
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 427
    Top = 23
    Width = 75
    Height = 25
    Caption = 'B&rowse...'
    TabOrder = 1
    OnClick = LocalDirectoryBrowseButtonClick
  end
  object QueueCheck2: TCheckBox
    Left = 12
    Top = 112
    Width = 317
    Height = 17
    Caption = 'Transfer on background (add to transfer &queue) X'
    TabOrder = 4
    OnClick = ControlChange
  end
  object HelpButton: TButton
    Left = 427
    Top = 136
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 8
    OnClick = HelpButtonClick
  end
  object NeverShowAgainCheck: TCheckBox
    Left = 12
    Top = 167
    Width = 493
    Height = 17
    Caption = '&Do not show this dialog box again'
    TabOrder = 10
    OnClick = NeverShowAgainCheckClick
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 136
    Width = 161
    Height = 25
    Caption = 'Transfer settin&gs...'
    Style = bsSplitButton
    TabOrder = 5
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 53
    Width = 496
    Height = 50
    Caption = 'Transfer settings'
    TabOrder = 3
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      496
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 482
      Height = 26
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
    Top = 191
    Width = 511
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 9
    object ShortCutHintLabel: TLabel
      Left = 12
      Top = 3
      Width = 490
      Height = 28
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
