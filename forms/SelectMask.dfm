object SelectMaskDialog: TSelectMaskDialog
  Left = 369
  Top = 257
  BorderStyle = bsDialog
  Caption = 'Select'
  ClientHeight = 145
  ClientWidth = 361
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object MaskGroup: TXPGroupBox
    Left = 8
    Top = 6
    Width = 345
    Height = 96
    TabOrder = 2
    object Label1: TLabel
      Left = 11
      Top = 19
      Width = 56
      Height = 13
      Caption = 'Enter &mask:'
      FocusControl = MaskEdit
    end
    object Label2: TLabel
      Left = 58
      Top = 68
      Width = 274
      Height = 13
      Alignment = taRightJustify
      Caption = 'File masks are separated by semicolon (*.txt; picture[1-9].*)'
    end
    object MaskEdit: THistoryComboBox
      Left = 91
      Top = 15
      Width = 241
      Height = 21
      ItemHeight = 13
      MaxLength = 1000
      TabOrder = 0
      Text = '*.*'
      OnExit = MaskEditExit
    end
    object IncludingDirectoriesCheck: TCheckBox
      Left = 91
      Top = 44
      Width = 241
      Height = 17
      Caption = 'Including &directories'
      TabOrder = 1
    end
  end
  object OKBtn: TButton
    Left = 191
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 279
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
