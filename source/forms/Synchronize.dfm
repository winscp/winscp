object SynchronizeDialog: TSynchronizeDialog
  Left = 367
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_keepuptodate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Keep remote directory up to date X'
  ClientHeight = 445
  ClientWidth = 468
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    468
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 453
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      453
      119)
    object LocalDirectoryLabel: TLabel
      Left = 49
      Top = 19
      Width = 195
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Watch for changes in the local directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 49
      Top = 68
      Width = 281
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '... &and automatically reflect them on the remote directory:'
      FocusControl = RemoteDirectoryEdit
    end
    object Image: TImage
      Left = 11
      Top = 22
      Width = 32
      Height = 32
      AutoSize = True
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 84
      Width = 393
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 35
      Width = 312
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 367
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
    Left = 183
    Top = 312
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Stop'
    TabOrder = 4
    OnClick = StopButtonClick
  end
  object CancelButton: TButton
    Left = 277
    Top = 312
    Width = 88
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
    Width = 453
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Synchronize options'
    TabOrder = 1
    DesignSize = (
      453
      119)
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
      Top = 92
      Width = 196
      Height = 17
      Caption = 'Use same &options next time'
      TabOrder = 6
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 229
      Top = 20
      Width = 213
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
      Left = 229
      Top = 68
      Width = 213
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Synchronize on s&tart'
      TabOrder = 5
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 229
      Top = 44
      Width = 213
      Height = 17
      Caption = 'Selected files o&nly'
      TabOrder = 3
      OnClick = ControlChange
    end
    object ContinueOnErrorCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 196
      Height = 17
      Caption = 'Continue on &error'
      TabOrder = 4
      OnClick = ControlChange
    end
  end
  object StartButton: TButton
    Left = 183
    Top = 312
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Start'
    Default = True
    TabOrder = 3
    OnClick = StartButtonClick
    OnDropDownClick = StartButtonDropDownClick
  end
  object MinimizeButton: TButton
    Left = 277
    Top = 312
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 5
    OnClick = MinimizeButtonClick
    OnDropDownClick = MinimizeButtonDropDownClick
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 312
    Width = 161
    Height = 25
    Caption = 'Transfer settin&gs...'
    TabOrder = 2
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 254
    Width = 453
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 7
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      453
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 439
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      ShowAccelChar = False
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object HelpButton: TButton
    Left = 371
    Top = 312
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 8
    OnClick = HelpButtonClick
  end
  object LogPanel: TPanel
    Left = 0
    Top = 345
    Width = 468
    Height = 100
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    DesignSize = (
      468
      100)
    object LogView: TListView
      Left = 8
      Top = 2
      Width = 452
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
      OnCustomDrawItem = LogViewCustomDrawItem
      OnDblClick = LogViewDblClick
      OnDeletion = LogViewDeletion
      OnKeyDown = LogViewKeyDown
    end
  end
  object MinimizeMenu: TPopupMenu
    Left = 376
    Top = 376
    object Minimize1: TMenuItem
      Caption = '&Minimize'
      Default = True
      OnClick = Minimize1Click
    end
    object MinimizetoTray1: TMenuItem
      Caption = 'Minimize to System &Tray'
      OnClick = MinimizetoTray1Click
    end
  end
  object StartMenu: TPopupMenu
    Left = 288
    Top = 376
    object Start1: TMenuItem
      Caption = '&Start'
      Default = True
      OnClick = StartButtonClick
    end
    object StartInNewWindow1: TMenuItem
      Caption = 'Start in &New Window'
      OnClick = StartInNewWindow1Click
    end
  end
end
