object PasswordDialog: TPasswordDialog
  Left = 390
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Enter password'
  ClientHeight = 86
  ClientWidth = 300
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    300
    86)
  PixelsPerInch = 96
  TextHeight = 13
  object PasswordLabel: TLabel
    Left = 8
    Top = 8
    Width = 280
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '&Password:'
    FocusControl = PasswordEdit
  end
  object OKButton: TButton
    Left = 131
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 219
    Top = 54
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PasswordEdit: TEdit
    Left = 8
    Top = 24
    Width = 286
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 250
    PasswordChar = '*'
    TabOrder = 0
  end
end
