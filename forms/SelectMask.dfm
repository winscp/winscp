object SelectMaskDialog: TSelectMaskDialog
  Left = 369
  Top = 257
  BorderStyle = bsDialog
  Caption = 'Select'
  ClientHeight = 115
  ClientWidth = 338
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 53
    Height = 13
    Caption = 'Enter &mask'
    FocusControl = MaskEdit
  end
  object Label2: TLabel
    Left = 55
    Top = 61
    Width = 274
    Height = 13
    Alignment = taRightJustify
    Caption = 'File masks are separated by semicolon (*.txt; picture[1-9].*)'
  end
  object OKBtn: TButton
    Left = 167
    Top = 83
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 255
    Top = 83
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object MaskEdit: THistoryComboBox
    Left = 88
    Top = 8
    Width = 241
    Height = 21
    ItemHeight = 13
    MaxLength = 255
    TabOrder = 0
    Text = '*.*'
  end
  object IncludingDirectoriesCheck: TCheckBox
    Left = 88
    Top = 37
    Width = 241
    Height = 17
    Caption = 'Including &directories'
    TabOrder = 1
  end
end
