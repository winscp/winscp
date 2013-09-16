object CustomCommandDialog: TCustomCommandDialog
  Left = 384
  Top = 214
  HelpType = htKeyword
  HelpKeyword = 'ui_customcommand'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CustomCommandDialog'
  ClientHeight = 282
  ClientWidth = 396
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    396
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object Group: TGroupBox
    Left = 8
    Top = 8
    Width = 380
    Height = 232
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      380
      232)
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
      Top = 205
      Width = 93
      Height = 13
      Caption = '&Keyboard shortcut:'
      FocusControl = ShortCutCombo
    end
    object DescriptionEdit: TEdit
      Left = 11
      Top = 32
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
    end
    object CommandEdit: THistoryComboBox
      Left = 11
      Top = 80
      Width = 358
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
      OnGetData = CommandEditGetData
      OnSetData = CommandEditSetData
    end
    object ApplyToDirectoriesCheck: TCheckBox
      Left = 16
      Top = 149
      Width = 161
      Height = 17
      Caption = '&Apply to directories'
      TabOrder = 5
      OnClick = ControlChange
    end
    object RecursiveCheck: TCheckBox
      Left = 184
      Top = 149
      Width = 185
      Height = 17
      Caption = '&Execute recursively'
      TabOrder = 6
      OnClick = ControlChange
    end
    object LocalCommandButton: TRadioButton
      Left = 184
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
      Width = 161
      Height = 17
      Caption = '&Remote command'
      TabOrder = 3
      OnClick = ControlChange
    end
    object ShowResultsCheck: TCheckBox
      Left = 16
      Top = 176
      Width = 161
      Height = 17
      Caption = '&Show results in terminal'
      TabOrder = 7
      OnClick = ControlChange
    end
    object CopyResultsCheck: TCheckBox
      Left = 184
      Top = 176
      Width = 185
      Height = 17
      Caption = 'Copy results to clip&board'
      TabOrder = 8
      OnClick = ControlChange
    end
    object HintText: TStaticText
      Left = 290
      Top = 103
      Width = 79
      Height = 16
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&patterns'
      TabOrder = 2
      TabStop = True
    end
    object ShortCutCombo: TComboBox
      Left = 184
      Top = 200
      Width = 184
      Height = 21
      TabOrder = 9
    end
  end
  object OkButton: TButton
    Left = 144
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 228
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 312
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
