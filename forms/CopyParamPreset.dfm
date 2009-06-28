object CopyParamPresetDialog: TCopyParamPresetDialog
  Left = 264
  Top = 122
  HelpType = htKeyword
  HelpKeyword = 'ui_transfer_preset'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyParamPresetDialog'
  ClientHeight = 441
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    632
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 13
    Width = 87
    Height = 13
    Caption = 'Preset &description:'
    FocusControl = DescriptionEdit
  end
  object OkButton: TButton
    Left = 380
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 464
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object DescriptionEdit: TEdit
    Left = 11
    Top = 29
    Width = 366
    Height = 21
    MaxLength = 250
    TabOrder = 0
    OnChange = ControlChange
  end
  inline CopyParamsFrame: TCopyParamsFrame
    Left = 8
    Top = 51
    Width = 377
    Height = 355
    HelpType = htKeyword
    TabOrder = 1
  end
  object RuleGroup: TGroupBox
    Left = 388
    Top = 91
    Width = 235
    Height = 306
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Autoselection rule'
    TabOrder = 3
    DesignSize = (
      235
      306)
    object Label2: TLabel
      Left = 10
      Top = 20
      Width = 79
      Height = 13
      Caption = 'Hostna&me mask:'
      FocusControl = HostNameEdit
    end
    object Label3: TLabel
      Left = 10
      Top = 68
      Width = 79
      Height = 13
      Caption = 'Us&ername mask:'
      FocusControl = UserNameEdit
    end
    object Label4: TLabel
      Left = 10
      Top = 116
      Width = 111
      Height = 13
      Caption = 'Remote director&y mask:'
      FocusControl = RemoteDirectoryEdit
    end
    object Label5: TLabel
      Left = 10
      Top = 164
      Width = 100
      Height = 13
      Caption = '&Local directory mask:'
      FocusControl = LocalDirectoryEdit
    end
    object HostNameEdit: TEdit
      Left = 10
      Top = 36
      Width = 215
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object UserNameEdit: TEdit
      Left = 10
      Top = 84
      Width = 215
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object RemoteDirectoryEdit: TEdit
      Left = 10
      Top = 132
      Width = 215
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 2
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object LocalDirectoryEdit: TEdit
      Left = 10
      Top = 180
      Width = 215
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 3
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object CurrentRuleButton: TButton
      Left = 10
      Top = 208
      Width = 75
      Height = 25
      Caption = 'Current'
      TabOrder = 4
      OnClick = CurrentRuleButtonClick
    end
    object RuleMaskHintText: TStaticText
      Left = 127
      Top = 207
      Width = 97
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'mask hints'
      TabOrder = 5
      TabStop = True
    end
  end
  object HasRuleCheck: TCheckBox
    Left = 395
    Top = 66
    Width = 212
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Automatically select this preset when'
    TabOrder = 2
    OnClick = ControlChange
  end
  object HelpButton: TButton
    Left = 548
    Top = 408
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
end
