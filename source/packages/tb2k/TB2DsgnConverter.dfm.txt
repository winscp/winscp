object TBConverterForm: TTBConverterForm
  Left = 200
  Top = 104
  AutoScroll = False
  Caption = 'Conversion Status'
  ClientHeight = 218
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object MessageList: TListBox
    Left = 8
    Top = 8
    Width = 409
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object CloseButton: TButton
    Left = 176
    Top = 185
    Width = 73
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    Enabled = False
    TabOrder = 1
    OnClick = CloseButtonClick
  end
  object CopyButton: TButton
    Left = 256
    Top = 185
    Width = 161
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'C&opy Messages to Clipboard'
    Enabled = False
    TabOrder = 2
    OnClick = CopyButtonClick
  end
end
