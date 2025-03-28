object EditorPreferencesDialog: TEditorPreferencesDialog
  Left = 303
  Top = 145
  HelpType = htKeyword
  HelpKeyword = 'ui_editor_preferences'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'EditorPreferencesDialog'
  ClientHeight = 382
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    447
    382)
  TextHeight = 15
  object ExternalEditorGroup: TGroupBox
    Left = 8
    Top = 247
    Width = 431
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'External editor options (affects editing remote files only)'
    TabOrder = 2
    DesignSize = (
      431
      73)
    object ExternalEditorTextCheck: TCheckBox
      Left = 11
      Top = 45
      Width = 411
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Force &text transfer mode for files edited in external editor'
      TabOrder = 1
    end
    object SDIExternalEditorCheck: TCheckBox
      Left = 11
      Top = 22
      Width = 411
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'E&xternal editor opens each file in separate window (process)'
      TabOrder = 0
    end
  end
  object EditorGroup2: TGroupBox
    Left = 8
    Top = 8
    Width = 431
    Height = 154
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Editor'
    TabOrder = 0
    DesignSize = (
      431
      154)
    object EditorInternalButton: TRadioButton
      Left = 11
      Top = 22
      Width = 212
      Height = 17
      Caption = '&Internal editor'
      TabOrder = 0
      OnClick = ControlChange
    end
    object EditorExternalButton: TRadioButton
      Left = 11
      Top = 45
      Width = 212
      Height = 17
      Caption = '&External editor:'
      TabOrder = 1
      OnClick = ControlChange
    end
    object ExternalEditorEdit: THistoryComboBox
      Left = 26
      Top = 68
      Width = 310
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'ExternalEditorEdit'
      OnChange = ControlChange
      OnExit = ExternalEditorEditExit
    end
    object ExternalEditorBrowseButton: TButton
      Left = 342
      Top = 67
      Width = 80
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'B&rowse...'
      TabOrder = 3
      OnClick = ExternalEditorBrowseButtonClick
    end
    object EditorOpenButton: TRadioButton
      Left = 11
      Top = 97
      Width = 212
      Height = 17
      Caption = '&Associated application'
      TabOrder = 4
      OnClick = ControlChange
    end
    object DefaultButton: TButton
      Left = 9
      Top = 120
      Width = 214
      Height = 25
      Caption = 'Use default system editor'
      TabOrder = 5
      OnClick = DefaultButtonClick
    end
  end
  object MaskGroup: TGroupBox
    Left = 8
    Top = 168
    Width = 431
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Editor autoselection'
    TabOrder = 1
    DesignSize = (
      431
      73)
    object MaskLabel: TLabel
      Left = 9
      Top = 22
      Width = 173
      Height = 15
      Caption = 'Use this editor for &following files:'
      FocusControl = MaskEdit
    end
    object MaskEdit: THistoryComboBox
      Left = 9
      Top = 40
      Width = 413
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = '*.*'
      OnExit = MaskEditExit
    end
  end
  object OkButton: TButton
    Left = 187
    Top = 349
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 273
    Top = 349
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object HelpButton: TButton
    Left = 359
    Top = 349
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
  object RememberCheck: TCheckBox
    Left = 19
    Top = 326
    Width = 420
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&Remember this editor'
    TabOrder = 3
  end
end
