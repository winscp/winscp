object CopyParamCustomDialog: TCopyParamCustomDialog
  Left = 264
  Top = 122
  HelpType = htKeyword
  HelpKeyword = 'ui_transfer_custom'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Transfer settings'
  ClientHeight = 387
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    377
    387)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 125
    Top = 354
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 209
    Top = 354
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  inline CopyParamsFrame: TCopyParamsFrame
    Left = 0
    Top = 0
    Width = 377
    Height = 345
    HelpType = htKeyword
    TabOrder = 0
    inherited CommonPropertiesGroup: TGroupBox
      Left = 197
      Top = 209
      Height = 73
      Caption = 'Common options'
      DesignSize = (
        173
        73)
      inherited CommonPreserveTimestampCheck: TCheckBox
        Top = 19
      end
    end
    inherited LocalPropertiesGroup: TGroupBox
      Left = 197
      Top = 156
      Height = 48
      Caption = 'Download options'
      DesignSize = (
        173
        48)
      inherited PreserveReadOnlyCheck: TCheckBox
        Top = 20
      end
      inherited LocalPreserveTimeCheck: TCheckBox
        Top = 92
      end
    end
    inherited RemotePropertiesGroup: TGroupBox
      Left = 8
      Top = 156
      Width = 182
      Height = 126
      Caption = 'Upload options'
    end
    inherited ChangeCaseGroup: TGroupBox
      Left = 247
      Top = 8
      Width = 123
      DesignSize = (
        123
        146)
      inherited CCLowerCaseShortButton: TRadioButton
        Width = 110
      end
      inherited CCNoChangeButton: TRadioButton
        Width = 110
      end
      inherited CCUpperCaseButton: TRadioButton
        Width = 110
      end
      inherited CCLowerCaseButton: TRadioButton
        Width = 110
      end
      inherited CCFirstUpperCaseButton: TRadioButton
        Width = 110
      end
    end
    inherited TransferModeGroup: TGroupBox
      Left = 8
      Top = 8
      Width = 231
      DesignSize = (
        231
        146)
      inherited TMTextButton: TRadioButton
        Width = 219
      end
      inherited TMBinaryButton: TRadioButton
        Width = 219
      end
      inherited TMAutomaticButton: TRadioButton
        Width = 219
      end
      inherited AsciiFileMaskCombo: THistoryComboBox
        Width = 213
      end
    end
    inherited OtherGroup: TGroupBox
      Left = 8
      Top = 284
      Width = 362
      DesignSize = (
        362
        61)
      inherited ExcludeFileMaskCombo: THistoryComboBox
        Width = 217
      end
      inherited ExcludeFileMaskHintText: TStaticText
        Left = 256
      end
    end
  end
  object HelpButton: TButton
    Left = 293
    Top = 354
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
