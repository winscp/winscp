object SelectMaskDialog: TSelectMaskDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_select'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'SelectX'
  ClientHeight = 186
  ClientWidth = 417
  Color = clBtnFace
  ParentFont = True
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    417
    186)
  TextHeight = 13
  object MaskGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 401
    Height = 138
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      401
      138)
    object Label3: TLabel
      Left = 16
      Top = 19
      Width = 47
      Height = 13
      Caption = 'File &mask:'
      FocusControl = MaskEdit
    end
    object ColorFileNamesLabel: TLabel
      Left = 16
      Top = 86
      Width = 150
      Height = 37
      Anchors = [akLeft, akTop, akBottom]
      AutoSize = False
      Caption = 'about.html'#13#10'index.html'#13#10'photo.jpg'
      Color = clWindow
      ParentColor = False
      ShowAccelChar = False
      Transparent = False
      WordWrap = True
    end
    object ColorSizesLabel: TLabel
      Left = 166
      Top = 86
      Width = 75
      Height = 37
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akBottom]
      AutoSize = False
      Caption = 'ColorSizesLabel'
      Color = clWindow
      ParentColor = False
      ShowAccelChar = False
      Transparent = False
      WordWrap = True
    end
    object ColorPaddingLabel: TLabel
      Left = 241
      Top = 86
      Width = 58
      Height = 37
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Color = clWindow
      ParentColor = False
      ShowAccelChar = False
      Transparent = False
      WordWrap = True
    end
    object ApplyToDirectoriesCheck: TCheckBox
      Left = 16
      Top = 63
      Width = 209
      Height = 17
      Caption = 'Apply to &directories'
      TabOrder = 3
    end
    object MaskEdit: THistoryComboBox
      Left = 16
      Top = 36
      Width = 283
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = '*.*'
      OnChange = MaskEditChange
      OnExit = MaskEditExit
    end
    object HintText: TStaticText
      Left = 194
      Top = 64
      Width = 105
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'mask hi&nts'
      TabOrder = 1
      TabStop = True
    end
    object MaskButton: TButton
      Left = 305
      Top = 34
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = MaskButtonClick
    end
    object ColorButton: TButton
      Left = 305
      Top = 86
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Color'
      TabOrder = 4
      OnClick = ColorButtonClick
    end
  end
  object OKBtn: TButton
    Left = 165
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 250
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 333
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object ClearButton: TButton
    Left = 84
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    ModalResult = 1
    TabOrder = 1
    OnClick = ClearButtonClick
  end
end
