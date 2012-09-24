object CopyDialog: TCopyDialog
  Left = 363
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_copy'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyDialog'
  ClientHeight = 189
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    511
    189)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoryLabel: TLabel
    Left = 8
    Top = 8
    Width = 187
    Height = 13
    Caption = 'Copy 2 selected files to remote directory'
  end
  object LocalDirectoryEdit: THistoryComboBox
    Left = 8
    Top = 25
    Width = 410
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'LocalDirectoryEdit'
    OnChange = ControlChange
  end
  object RemoteDirectoryEdit: THistoryComboBox
    Left = 8
    Top = 25
    Width = 494
    Height = 21
    AutoComplete = False
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 1000
    TabOrder = 2
    Text = 'RemoteDirectoryEdit'
    OnChange = ControlChange
  end
  object CopyButton: TButton
    Left = 260
    Top = 159
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Copy'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object CancelButton: TButton
    Left = 343
    Top = 159
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
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
    Left = 8
    Top = 135
    Width = 301
    Height = 17
    Caption = 'Transfer on background (add to transfer &queue) X'
    TabOrder = 6
    OnClick = ControlChange
  end
  object QueueIndividuallyCheck: TCheckBox
    Left = 312
    Top = 135
    Width = 193
    Height = 17
    Caption = '&Transfer each file individually'
    TabOrder = 7
    OnClick = ControlChange
  end
  object HelpButton: TButton
    Left = 427
    Top = 159
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 11
    OnClick = HelpButtonClick
  end
  object NewerOnlyCheck: TCheckBox
    Left = 8
    Top = 112
    Width = 301
    Height = 17
    Caption = '&New and updated file(s) only'
    TabOrder = 4
  end
  object NeverShowAgainCheck: TCheckBox
    Left = 312
    Top = 112
    Width = 193
    Height = 17
    Caption = '&Do not show this dialog box again'
    TabOrder = 5
    OnClick = ControlChange
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 159
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Transfer settin&gs...'
    TabOrder = 8
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
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
end
