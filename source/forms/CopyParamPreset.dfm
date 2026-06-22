object CopyParamPresetDialog: TCopyParamPresetDialog
  Left = 264
  Top = 122
  HelpType = htKeyword
  HelpKeyword = 'ui_transfer_preset'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'CopyParamPresetDialog'
  ClientHeight = 556
  ClientWidth = 744
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
    744
    556)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 97
    Height = 15
    Caption = 'Preset &description:'
    FocusControl = DescriptionEdit
  end
  object OkButton: TButton
    Left = 484
    Top = 523
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 570
    Top = 523
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object DescriptionEdit: TEdit
    Left = 8
    Top = 24
    Width = 450
    Height = 23
    MaxLength = 250
    TabOrder = 0
    OnChange = ControlChange
  end
  inline CopyParamsFrame: TCopyParamsFrame
    Left = 5
    Top = 50
    Width = 456
    Height = 471
    HelpType = htKeyword
    TabOrder = 1
  end
  object RuleGroup: TGroupBox
    Left = 472
    Top = 89
    Width = 264
    Height = 428
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Autoselection rule'
    TabOrder = 3
    DesignSize = (
      264
      428)
    object Label2: TLabel
      Left = 9
      Top = 22
      Width = 89
      Height = 15
      Caption = 'Hostna&me mask:'
      FocusControl = HostNameEdit
    end
    object Label3: TLabel
      Left = 9
      Top = 69
      Width = 87
      Height = 15
      Caption = 'Us&ername mask:'
      FocusControl = UserNameEdit
    end
    object Label4: TLabel
      Left = 9
      Top = 116
      Width = 125
      Height = 15
      Caption = 'Remote director&y mask:'
      FocusControl = RemoteDirectoryEdit
    end
    object Label5: TLabel
      Left = 10
      Top = 163
      Width = 112
      Height = 15
      Caption = '&Local directory mask:'
      FocusControl = LocalDirectoryEdit
    end
    object HostNameEdit: TEdit
      Left = 9
      Top = 40
      Width = 246
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 0
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object UserNameEdit: TEdit
      Left = 9
      Top = 87
      Width = 246
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 1
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object RemoteDirectoryEdit: TEdit
      Left = 9
      Top = 134
      Width = 246
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 2
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object LocalDirectoryEdit: TEdit
      Left = 9
      Top = 181
      Width = 246
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 250
      TabOrder = 3
      OnChange = ControlChange
      OnExit = MaskEditExit
    end
    object CurrentRuleButton: TButton
      Left = 9
      Top = 214
      Width = 80
      Height = 25
      Caption = 'Current'
      TabOrder = 5
      OnClick = CurrentRuleButtonClick
    end
    object RuleMaskHintText: TStaticText
      Left = 126
      Top = 204
      Width = 129
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'mask hints'
      TabOrder = 4
      TabStop = True
    end
  end
  object HasRuleCheck: TCheckBox
    Left = 474
    Top = 66
    Width = 262
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Automatically select this preset when'
    TabOrder = 2
    OnClick = ControlChange
  end
  object HelpButton: TButton
    Left = 656
    Top = 523
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
end
