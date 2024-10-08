object EditorPreferencesDialog: TEditorPreferencesDialog
  Left = 303
  Top = 145
  HelpType = htKeyword
  HelpKeyword = 'ui_editor_preferences'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'EditorPreferencesDialog'
  ClientHeight = 389
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    403
    389)
  TextHeight = 13
  object ExternalEditorGroup: TGroupBox
    Left = 8
    Top = 250
    Width = 388
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'External editor options (affects editing remote files only)'
    TabOrder = 2
    object ExternalEditorTextCheck: TCheckBox
      Left = 16
      Top = 45
      Width = 337
      Height = 17
      Caption = 'Force &text transfer mode for files edited in external editor'
      TabOrder = 1
    end
    object SDIExternalEditorCheck: TCheckBox
      Left = 16
      Top = 21
      Width = 337
      Height = 17
      Caption = 'E&xternal editor opens each file in separate window (process)'
      TabOrder = 0
    end
  end
  object EditorGroup2: TGroupBox
    Left = 8
    Top = 8
    Width = 388
    Height = 155
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Editor'
    TabOrder = 0
    DesignSize = (
      388
      155)
    object EditorInternalButton: TRadioButton
      Left = 16
      Top = 21
      Width = 145
      Height = 17
      Caption = '&Internal editor'
      TabOrder = 0
      OnClick = ControlChange
    end
    object EditorExternalButton: TRadioButton
      Left = 16
      Top = 45
      Width = 145
      Height = 17
      Caption = '&External editor:'
      TabOrder = 1
      OnClick = ControlChange
    end
    object ExternalEditorEdit: THistoryComboBox
      Left = 32
      Top = 69
      Width = 267
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'ExternalEditorEdit'
      OnChange = ControlChange
      OnExit = ExternalEditorEditExit
    end
    object ExternalEditorBrowseButton: TButton
      Left = 305
      Top = 67
      Width = 75
      Height = 25
      Caption = 'B&rowse...'
      TabOrder = 3
      OnClick = ExternalEditorBrowseButtonClick
    end
    object EditorOpenButton: TRadioButton
      Left = 16
      Top = 97
      Width = 145
      Height = 17
      Caption = '&Associated application'
      TabOrder = 4
      OnClick = ControlChange
    end
    object DefaultButton: TButton
      Left = 16
      Top = 120
      Width = 193
      Height = 25
      Caption = 'Use default system editor'
      TabOrder = 5
      OnClick = DefaultButtonClick
    end
  end
  object MaskGroup: TGroupBox
    Left = 8
    Top = 170
    Width = 388
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Editor autoselection'
    TabOrder = 1
    DesignSize = (
      388
      73)
    object MaskLabel: TLabel
      Left = 11
      Top = 20
      Width = 157
      Height = 13
      Caption = 'Use this editor for &following files:'
      FocusControl = MaskEdit
    end
    object MaskEdit: THistoryComboBox
      Left = 11
      Top = 39
      Width = 367
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = '*.*'
      OnExit = MaskEditExit
    end
  end
  object OkButton: TButton
    Left = 151
    Top = 356
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 235
    Top = 356
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object HelpButton: TButton
    Left = 319
    Top = 356
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
  object RememberCheck: TCheckBox
    Left = 24
    Top = 332
    Width = 337
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Remember this editor'
    TabOrder = 3
  end
end
