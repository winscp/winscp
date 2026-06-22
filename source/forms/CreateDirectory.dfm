object CreateDirectoryDialog: TCreateDirectoryDialog
  Left = 408
  Top = 195
  HelpType = htKeyword
  HelpKeyword = 'ui_create_directory'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Create folder'
  ClientHeight = 263
  ClientWidth = 337
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
    337
    263)
  TextHeight = 15
  object EditLabel: TLabel
    Left = 8
    Top = 8
    Width = 94
    Height = 15
    Caption = 'New &folder name:'
    FocusControl = DirectoryEdit
  end
  object DirectoryEdit: TEdit
    Left = 8
    Top = 25
    Width = 321
    Height = 23
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
    Height = 179
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      337
      179)
    object AttributesGroup: TGroupBox
      Left = 8
      Top = 4
      Width = 321
      Height = 170
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Attributes'
      TabOrder = 0
      inline RightsFrame: TRightsFrame
        Left = 2
        Top = 39
        Width = 258
        Height = 98
        TabOrder = 1
        inherited DirectoriesXCheck: TCheckBox
          Visible = False
        end
      end
      object SetRightsCheck: TCheckBox
        Left = 11
        Top = 22
        Width = 156
        Height = 17
        Caption = 'Set pe&rmissions'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = ControlChange
      end
      object SaveSettingsCheck: TCheckBox
        Left = 11
        Top = 143
        Width = 301
        Height = 17
        Caption = 'Use &same settings next time'
        TabOrder = 2
      end
    end
  end
  object OKBtn: TButton
    Left = 77
    Top = 230
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 163
    Top = 230
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpButton: TButton
    Left = 249
    Top = 230
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
end
