object SelectMaskDialog: TSelectMaskDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_select'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'SelectX'
  ClientHeight = 181
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    460
    181)
  TextHeight = 15
  object MaskGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 444
    Height = 134
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      444
      134)
    object Label3: TLabel
      Left = 9
      Top = 9
      Width = 52
      Height = 15
      Caption = 'File &mask:'
      FocusControl = MaskEdit
    end
    object ColorFileNamesLabel: TLabel
      Left = 9
      Top = 77
      Width = 165
      Height = 47
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
      Left = 174
      Top = 77
      Width = 82
      Height = 47
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
      Left = 256
      Top = 77
      Width = 94
      Height = 47
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
      Left = 11
      Top = 54
      Width = 209
      Height = 17
      Caption = 'Apply to &directories'
      TabOrder = 3
    end
    object MaskEdit: THistoryComboBox
      Left = 9
      Top = 25
      Width = 341
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = '*.*'
      OnChange = MaskEditChange
      OnExit = MaskEditExit
    end
    object HintText: TStaticText
      Left = 235
      Top = 48
      Width = 115
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'mask hi&nts'
      TabOrder = 1
      TabStop = True
    end
    object MaskButton: TButton
      Left = 356
      Top = 24
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = MaskButtonClick
    end
    object ColorButton: TButton
      Left = 356
      Top = 77
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Color'
      TabOrder = 4
      OnClick = ColorButtonClick
    end
  end
  object OKBtn: TButton
    Left = 200
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 286
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 372
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object ClearButton: TButton
    Left = 114
    Top = 148
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    ModalResult = 1
    TabOrder = 1
    OnClick = ClearButtonClick
  end
end
