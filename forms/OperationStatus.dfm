object OperationStatusForm: TOperationStatusForm
  Left = 375
  Top = 233
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 46
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    366
    46)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 366
    Height = 46
    Align = alClient
    Style = bsRaised
  end
  object StatusLabel: TLabel
    Left = 16
    Top = 16
    Width = 335
    Height = 28
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'StatusLabel'
    ShowAccelChar = False
    WordWrap = True
  end
end
