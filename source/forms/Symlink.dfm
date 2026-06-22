object SymlinkDialog: TSymlinkDialog
  Left = 384
  Top = 214
  HelpType = htKeyword
  HelpKeyword = 'ui_symlink'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'SymlinkDialog'
  ClientHeight = 177
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    417
    177)
  TextHeight = 15
  object SymlinkGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 401
    Height = 130
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      401
      130)
    object FileNameLabel: TLabel
      Left = 9
      Top = 9
      Width = 93
      Height = 15
      Caption = '&Link/shortcut file:'
      FocusControl = FileNameEdit
    end
    object Label1: TLabel
      Left = 9
      Top = 56
      Width = 116
      Height = 15
      Caption = '&Point link/shortcut to:'
      FocusControl = PointToEdit
    end
    object FileNameEdit: TEdit
      Left = 9
      Top = 27
      Width = 383
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
    end
    object PointToEdit: TEdit
      Left = 9
      Top = 74
      Width = 383
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
    end
    object HardLinkCheck: TCheckBox
      Left = 11
      Top = 103
      Width = 401
      Height = 17
      Caption = '&Hard link'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object OkButton: TButton
    Left = 157
    Top = 144
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 243
    Top = 144
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 329
    Top = 144
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
