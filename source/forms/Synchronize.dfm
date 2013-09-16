object SynchronizeDialog: TSynchronizeDialog
  Left = 367
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_keepuptodate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Keep remote directory up to date X'
  ClientHeight = 421
  ClientWidth = 436
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    436
    421)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 421
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      421
      119)
    object LocalDirectoryLabel: TLabel
      Left = 11
      Top = 19
      Width = 235
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Watch for changes in the local directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 11
      Top = 68
      Width = 321
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '... &and automatically reflect them on the remote directory:'
      FocusControl = RemoteDirectoryEdit
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 84
      Width = 399
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 11
      Top = 35
      Width = 316
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 335
      Top = 33
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'B&rowse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object StopButton: TButton
    Left = 192
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Stop'
    TabOrder = 4
    OnClick = StopButtonClick
  end
  object CancelButton: TButton
    Left = 272
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 6
  end
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 130
    Width = 421
    Height = 95
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Synchronize options'
    TabOrder = 1
    DesignSize = (
      421
      95)
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 196
      Height = 17
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SaveSettingsCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 196
      Height = 17
      Caption = 'Use same &options next time'
      TabOrder = 4
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 213
      Top = 20
      Width = 197
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Existing files only'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeRecursiveCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 196
      Height = 17
      Caption = 'Update s&ubdirectories'
      TabOrder = 2
      OnClick = ControlChange
    end
    object SynchronizeSynchronizeCheck: TGrayedCheckBox
      Left = 213
      Top = 68
      Width = 197
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Synchronize on s&tart'
      TabOrder = 5
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 213
      Top = 44
      Width = 197
      Height = 17
      Caption = 'Selected files o&nly'
      TabOrder = 3
      OnClick = ControlChange
    end
  end
  object StartButton: TButton
    Left = 192
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Start'
    Default = True
    TabOrder = 3
    OnClick = StartButtonClick
  end
  object MinimizeButton: TButton
    Left = 273
    Top = 288
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 5
    OnClick = MinimizeButtonClick
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 288
    Width = 161
    Height = 25
    Caption = 'Transfer settin&gs...'
    TabOrder = 2
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 230
    Width = 421
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 7
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      421
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 407
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object HelpButton: TButton
    Left = 353
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 8
    OnClick = HelpButtonClick
  end
  object LogPanel: TPanel
    Left = 0
    Top = 321
    Width = 436
    Height = 100
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    DesignSize = (
      436
      100)
    object LogView: TListView
      Left = 8
      Top = 2
      Width = 420
      Height = 90
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Width = -1
          WidthType = (
            -1)
        end
        item
          Width = -1
          WidthType = (
            -1)
        end>
      DoubleBuffered = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnKeyDown = LogViewKeyDown
    end
  end
end
