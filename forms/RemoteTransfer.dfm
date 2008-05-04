object RemoteTransferDialog: TRemoteTransferDialog
  Left = 296
  Top = 235
  HelpType = htKeyword
  HelpKeyword = 'task_move_duplicate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'RemoteTransferDialog'
  ClientHeight = 183
  ClientWidth = 330
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    330
    183)
  PixelsPerInch = 96
  TextHeight = 13
  object SymlinkGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 314
    Height = 135
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      314
      135)
    object SessionLabel: TLabel
      Left = 11
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Target &session:'
      FocusControl = SessionCombo
    end
    object Label2: TLabel
      Left = 11
      Top = 64
      Width = 112
      Height = 13
      Caption = 'Target remote &directory:'
      FocusControl = DirectoryEdit
    end
    object SessionCombo: TComboBox
      Left = 11
      Top = 32
      Width = 292
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MaxLength = 250
      TabOrder = 0
      OnChange = SessionComboChange
    end
    object DirectoryEdit: THistoryComboBox
      Left = 11
      Top = 80
      Width = 292
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
    end
    object NotDirectCopyCheck: TCheckBox
      Left = 17
      Top = 109
      Width = 288
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Duplicate via local &temporary copy'
      TabOrder = 2
      OnClick = NotDirectCopyCheckClick
    end
  end
  object OkButton: TButton
    Left = 78
    Top = 150
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 162
    Top = 150
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 246
    Top = 150
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
