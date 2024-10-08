object RemoteTransferDialog: TRemoteTransferDialog
  Left = 296
  Top = 235
  HelpType = htKeyword
  HelpKeyword = 'ui_duplicate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'RemoteTransferDialog'
  ClientHeight = 179
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    420
    179)
  TextHeight = 13
  object Group: TGroupBox
    Left = 8
    Top = 6
    Width = 404
    Height = 131
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      404
      131)
    object SessionLabel: TLabel
      Left = 49
      Top = 12
      Width = 74
      Height = 13
      Caption = 'Target &session:'
      FocusControl = SessionCombo
    end
    object Label3: TLabel
      Left = 49
      Top = 60
      Width = 98
      Height = 13
      Caption = 'Target remote &path:'
      FocusControl = DirectoryEdit
    end
    object Image: TImage
      Left = 11
      Top = 15
      Width = 32
      Height = 32
      AutoSize = True
    end
    object SessionCombo: TComboBox
      Left = 49
      Top = 28
      Width = 344
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      MaxLength = 250
      TabOrder = 0
      OnChange = SessionComboChange
    end
    object DirectoryEdit: THistoryComboBox
      Left = 49
      Top = 76
      Width = 344
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
    end
    object NotDirectCopyCheck: TCheckBox
      Left = 55
      Top = 105
      Width = 340
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Duplicate via local &temporary copy'
      TabOrder = 2
      OnClick = NotDirectCopyCheckClick
    end
  end
  object OkButton: TButton
    Left = 168
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 252
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 336
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
