object CustomCommandDialog: TCustomCommandDialog
  Left = 384
  Top = 214
  HelpType = htKeyword
  HelpKeyword = 'ui_customcommand'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CustomCommandDialog'
  ClientHeight = 287
  ClientWidth = 464
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
    464
    287)
  TextHeight = 15
  object Group: TGroupBox
    Left = 8
    Top = 8
    Width = 448
    Height = 240
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      448
      240)
    object DescriptionLabel: TLabel
      Left = 9
      Top = 9
      Width = 63
      Height = 15
      Caption = '&Description:'
      FocusControl = DescriptionEdit
    end
    object Label1: TLabel
      Left = 9
      Top = 56
      Width = 103
      Height = 15
      Caption = '&Custom command:'
      FocusControl = CommandEdit
    end
    object ShortCutLabel: TLabel
      Left = 9
      Top = 210
      Width = 100
      Height = 15
      Caption = '&Keyboard shortcut:'
      FocusControl = ShortCutCombo
    end
    object DescriptionEdit: TEdit
      Left = 9
      Top = 27
      Width = 430
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
    end
    object CommandEdit: THistoryComboBox
      Left = 9
      Top = 74
      Width = 430
      Height = 23
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 2048
      TabOrder = 1
      OnChange = ControlChange
      OnGetData = CommandEditGetData
      OnSetData = CommandEditSetData
    end
    object ApplyToDirectoriesCheck: TCheckBox
      Left = 11
      Top = 138
      Width = 200
      Height = 17
      Caption = '&Apply to directories'
      TabOrder = 5
      OnClick = ControlChange
    end
    object RecursiveCheck: TCheckBox
      Left = 224
      Top = 138
      Width = 200
      Height = 17
      Caption = '&Execute recursively'
      TabOrder = 6
      OnClick = ControlChange
    end
    object LocalCommandButton: TRadioButton
      Left = 224
      Top = 115
      Width = 200
      Height = 17
      Caption = '&Local command'
      TabOrder = 4
      OnClick = ControlChange
    end
    object RemoteCommandButton: TRadioButton
      Left = 11
      Top = 115
      Width = 200
      Height = 17
      Caption = '&Remote command'
      TabOrder = 3
      OnClick = ControlChange
    end
    object ShowResultsCheck: TCheckBox
      Left = 11
      Top = 161
      Width = 200
      Height = 17
      Caption = '&Show results in terminal'
      TabOrder = 7
      OnClick = ControlChange
    end
    object CopyResultsCheck: TCheckBox
      Left = 11
      Top = 184
      Width = 200
      Height = 17
      Caption = 'Copy results to clip&board'
      TabOrder = 9
      OnClick = ControlChange
    end
    object HintText: TStaticText
      Left = 321
      Top = 97
      Width = 118
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&patterns'
      TabOrder = 2
      TabStop = True
    end
    object ShortCutCombo: TComboBox
      Left = 224
      Top = 207
      Width = 215
      Height = 23
      TabOrder = 10
    end
    object RemoteFilesCheck: TCheckBox
      Left = 224
      Top = 161
      Width = 200
      Height = 17
      Caption = '&Use remote files'
      TabOrder = 8
      OnClick = ControlChange
    end
  end
  object OkButton: TButton
    Left = 204
    Top = 254
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 290
    Top = 254
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 376
    Top = 254
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
