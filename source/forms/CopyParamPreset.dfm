object CopyParamPresetDialog: TCopyParamPresetDialog
  Left = 264
  Top = 122
  HelpType = htKeyword
  HelpKeyword = 'ui_transfer_preset'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyParamPresetDialog'
  ClientHeight = 560
  ClientWidth = 675
  Color = clBtnFace
  ParentFont = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    675
    560)
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 13
    Width = 90
    Height = 13
    Caption = 'Preset &description:'
    FocusControl = DescriptionEdit
  end
  object OkButton: TButton
    Left = 423
    Top = 527
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 507
    Top = 527
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object DescriptionEdit: TEdit
    Left = 10
    Top = 29
    Width = 405
    Height = 21
    MaxLength = 250
    TabOrder = 0
    OnChange = ControlChange
  end
  inline CopyParamsFrame: TCopyParamsFrame
    Left = 2
    Top = 51
    Width = 420
    Height = 477
    HelpType = htKeyword
    TabOrder = 1
  end
  object RuleGroup: TGroupBox
    Left = 426
    Top = 91
    Width = 240
    Height = 430
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Autoselection rule'
    TabOrder = 3
    DesignSize = (
      240
      430)
    object Label2: TLabel
      Left = 16
      Top = 20
      Width = 79
      Height = 13
      Caption = 'Hostna&me mask:'
      FocusControl = HostNameEdit
    end
    object Label3: TLabel
      Left = 16
      Top = 68
      Width = 79
      Height = 13
      Caption = 'Us&ername mask:'
      FocusControl = UserNameEdit
    end
    object Label4: TLabel
      Left = 16
      Top = 116
      Width = 114
      Height = 13
      Caption = 'Remote director&y mask:'
      FocusControl = RemoteDirectoryEdit
    end
    object Label5: TLabel
      Left = 16
      Top = 164
      Width = 101
      Height = 13
      Caption = '&Local directory mask:'
      FocusControl = LocalDirectoryEdit
    end
    object HostNameEdit: TEdit
      Left = 16
      Top = 36
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object UserNameEdit: TEdit
      Left = 16
      Top = 84
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object RemoteDirectoryEdit: TEdit
      Left = 16
      Top = 132
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 2
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object LocalDirectoryEdit: TEdit
      Left = 16
      Top = 180
      Width = 208
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 3
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object CurrentRuleButton: TButton
      Left = 16
      Top = 211
      Width = 73
      Height = 25
      Caption = 'Current'
      TabOrder = 4
      OnClick = CurrentRuleButtonClick
    end
    object RuleMaskHintText: TStaticText
      Left = 95
      Top = 207
      Width = 129
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
    Left = 433
    Top = 66
    Width = 216
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Automatically select this preset when'
    TabOrder = 2
    OnClick = ControlChange
  end
  object HelpButton: TButton
    Left = 591
    Top = 527
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
end
