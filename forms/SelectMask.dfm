object SelectMaskDialog: TSelectMaskDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_select'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Select'
  ClientHeight = 142
  ClientWidth = 361
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    361
    142)
  PixelsPerInch = 96
  TextHeight = 13
  object MaskGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 345
    Height = 94
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      345
      94)
    object Label3: TLabel
      Left = 16
      Top = 19
      Width = 47
      Height = 13
      Caption = 'File &mask:'
      FocusControl = MaskEdit
    end
    object IncludingDirectoriesCheck: TCheckBox
      Left = 16
      Top = 63
      Width = 217
      Height = 17
      Caption = 'Including &directories'
      TabOrder = 2
    end
    object MaskEdit: THistoryComboBox
      Left = 16
      Top = 36
      Width = 313
      Height = 21
      AutoComplete = False
      ItemHeight = 13
      MaxLength = 1000
      TabOrder = 0
      Text = '*.*'
      OnExit = MaskEditExit
    end
    object HintText: TStaticText
      Left = 232
      Top = 64
      Width = 97
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'mask &hints'
      TabOrder = 1
      TabStop = True
    end
  end
  object OKBtn: TButton
    Left = 109
    Top = 109
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 194
    Top = 109
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 277
    Top = 109
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
