object CustomCommandDialog: TCustomCommandDialog
  Left = 384
  Top = 214
  HelpType = htKeyword
  HelpKeyword = 'ui_customcommand'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CustomCommandDialog'
  ClientHeight = 309
  ClientWidth = 416
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
    416
    309)
  TextHeight = 13
  object Group: TGroupBox
    Left = 8
    Top = 8
    Width = 400
    Height = 259
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      400
      259)
    object DescriptionLabel: TLabel
      Left = 11
      Top = 16
      Width = 57
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Description:'
      FocusControl = DescriptionEdit
    end
    object Label1: TLabel
      Left = 11
      Top = 64
      Width = 88
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Custom command:'
      FocusControl = CommandEdit
    end
    object ShortCutLabel: TLabel
      Left = 16
      Top = 231
      Width = 93
      Height = 13
      Caption = '&Keyboard shortcut:'
      FocusControl = ShortCutCombo
    end
    object DescriptionEdit: TEdit
      Left = 11
      Top = 32
      Width = 378
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
    end
    object CommandEdit: THistoryComboBox
      Left = 11
      Top = 80
      Width = 378
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 2048
      TabOrder = 1
      OnChange = ControlChange
      OnGetData = CommandEditGetData
      OnSetData = CommandEditSetData
    end
    object ApplyToDirectoriesCheck: TCheckBox
      Left = 16
      Top = 149
      Width = 181
      Height = 17
      Caption = '&Apply to directories'
      TabOrder = 5
      OnClick = ControlChange
    end
    object RecursiveCheck: TCheckBox
      Left = 203
      Top = 149
      Width = 185
      Height = 17
      Caption = '&Execute recursively'
      TabOrder = 6
      OnClick = ControlChange
    end
    object LocalCommandButton: TRadioButton
      Left = 203
      Top = 122
      Width = 185
      Height = 17
      Caption = '&Local command'
      TabOrder = 4
      OnClick = ControlChange
    end
    object RemoteCommandButton: TRadioButton
      Left = 16
      Top = 122
      Width = 181
      Height = 17
      Caption = '&Remote command'
      TabOrder = 3
      OnClick = ControlChange
    end
    object ShowResultsCheck: TCheckBox
      Left = 16
      Top = 176
      Width = 181
      Height = 17
      Caption = '&Show results in terminal'
      TabOrder = 7
      OnClick = ControlChange
    end
    object CopyResultsCheck: TCheckBox
      Left = 16
      Top = 203
      Width = 181
      Height = 17
      Caption = 'Copy results to clip&board'
      TabOrder = 9
      OnClick = ControlChange
    end
    object HintText: TStaticText
      Left = 272
      Top = 103
      Width = 117
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&patterns'
      TabOrder = 2
      TabStop = True
    end
    object ShortCutCombo: TComboBox
      Left = 203
      Top = 226
      Width = 184
      Height = 21
      TabOrder = 10
    end
    object RemoteFilesCheck: TCheckBox
      Left = 203
      Top = 176
      Width = 181
      Height = 17
      Caption = '&Use remote files'
      TabOrder = 8
      OnClick = ControlChange
    end
  end
  object OkButton: TButton
    Left = 164
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 248
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 332
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
