object TBConvertOptionsForm: TTBConvertOptionsForm
  Left = 225
  Top = 133
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Convert Menu'
  ClientHeight = 90
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Caption = '&Menu to convert:'
    FocusControl = MenuCombo
  end
  object MenuCombo: TComboBox
    Left = 8
    Top = 24
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object ConvertButton: TButton
    Left = 8
    Top = 57
    Width = 73
    Height = 23
    Caption = '&Convert'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object HelpButton: TButton
    Left = 168
    Top = 57
    Width = 73
    Height = 23
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpButtonClick
  end
  object Button1: TButton
    Left = 88
    Top = 57
    Width = 73
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
