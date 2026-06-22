object RemoteTransferDialog: TRemoteTransferDialog
  Left = 296
  Top = 235
  HelpType = htKeyword
  HelpKeyword = 'ui_duplicate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'RemoteTransferDialog'
  ClientHeight = 181
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    464
    181)
  TextHeight = 15
  object Group: TGroupBox
    Left = 8
    Top = 8
    Width = 448
    Height = 133
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      448
      133)
    object SessionLabel: TLabel
      Left = 47
      Top = 9
      Width = 77
      Height = 15
      Caption = 'Target &session:'
      FocusControl = SessionCombo
    end
    object Label3: TLabel
      Left = 47
      Top = 59
      Width = 104
      Height = 15
      Caption = 'Target remote &path:'
      FocusControl = DirectoryEdit
    end
    object Image: TImage
      Left = 9
      Top = 12
      Width = 32
      Height = 32
      AutoSize = True
    end
    object SessionCombo: TComboBox
      Left = 47
      Top = 30
      Width = 392
      Height = 23
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      MaxLength = 250
      TabOrder = 0
      OnChange = SessionComboChange
    end
    object DirectoryEdit: THistoryComboBox
      Left = 47
      Top = 77
      Width = 392
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
    end
    object NotDirectCopyCheck: TCheckBox
      Left = 49
      Top = 106
      Width = 390
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Duplicate via local &temporary copy'
      TabOrder = 2
      OnClick = NotDirectCopyCheckClick
    end
  end
  object OkButton: TButton
    Left = 204
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 290
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 376
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
