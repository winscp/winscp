object PasswordDialog: TPasswordDialog
  Left = 390
  Top = 251
  HelpType = htKeyword
  HelpKeyword = 'ui_password'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Enter password'
  ClientHeight = 151
  ClientWidth = 404
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  DesignSize = (
    404
    151)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 235
    Top = 119
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 323
    Top = 119
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PasswordPanel: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      404
      50)
    object PasswordLabel: TLabel
      Left = 8
      Top = 8
      Width = 389
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '&Password:'
      FocusControl = PasswordEdit
    end
    object PasswordEdit: TPasswordEdit
      Left = 8
      Top = 24
      Width = 390
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
    end
  end
  object ServerPromptPanel: TPanel
    Left = 0
    Top = 50
    Width = 404
    Height = 69
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      404
      69)
    object ServerPromptLabel: TLabel
      Left = 8
      Top = 24
      Width = 388
      Height = 44
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 
        'Note: This prompt is issued by the server. It is part of either ' +
        'keyboard-interactive, TIS or Cryptocard authentication.'
      WordWrap = True
    end
    object HideTypingCheck: TCheckBox
      Left = 14
      Top = 2
      Width = 275
      Height = 17
      Caption = '&Hide typing'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = HideTypingCheckClick
    end
  end
end
