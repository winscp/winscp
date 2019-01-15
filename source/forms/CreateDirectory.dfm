object CreateDirectoryDialog: TCreateDirectoryDialog
  Left = 408
  Top = 195
  HelpType = htKeyword
  HelpKeyword = 'ui_create_directory'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Create folder'
  ClientHeight = 253
  ClientWidth = 337
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    337
    253)
  PixelsPerInch = 96
  TextHeight = 13
  object EditLabel: TLabel
    Left = 8
    Top = 8
    Width = 85
    Height = 13
    Caption = 'New &folder name:'
    FocusControl = DirectoryEdit
  end
  object DirectoryEdit: TEdit
    Left = 8
    Top = 25
    Width = 321
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 1000
    TabOrder = 0
    Text = 'DirectoryEdit'
    OnChange = DirectoryEditChange
  end
  object MorePanel: TPanel
    Left = 0
    Top = 50
    Width = 337
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      337
      169)
    object AttributesGroup: TGroupBox
      Left = 8
      Top = 3
      Width = 322
      Height = 157
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Attributes'
      TabOrder = 0
      inline RightsFrame: TRightsFrame
        Left = 7
        Top = 36
        Width = 239
        Height = 87
        TabOrder = 1
        inherited DirectoriesXCheck: TCheckBox
          Visible = False
        end
      end
      object SetRightsCheck: TCheckBox
        Left = 12
        Top = 16
        Width = 156
        Height = 17
        Caption = 'Set pe&rmissions'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = ControlChange
      end
      object SaveSettingsCheck: TCheckBox
        Left = 12
        Top = 129
        Width = 301
        Height = 17
        Caption = 'Use &same settings next time'
        TabOrder = 2
      end
    end
  end
  object OKBtn: TButton
    Left = 91
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 171
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 252
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
end
