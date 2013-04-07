object AuthenticateForm: TAuthenticateForm
  Left = 304
  Top = 113
  HelpType = htKeyword
  HelpKeyword = 'ui_authenticate'
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'AuthenticateForm'
  ClientHeight = 316
  ClientWidth = 375
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 280
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LogView: TListView
    Left = 0
    Top = 0
    Width = 375
    Height = 26
    Align = alClient
    Columns = <
      item
        Width = 100
      end>
    DoubleBuffered = True
    Items.ItemData = {
      033C0000000100000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      001141007500740068006E0065007400690063006100740069006E0067002E00
      2E002E00}
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PasswordPanel: TPanel
    Left = 0
    Top = 26
    Width = 375
    Height = 208
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object PromptEditPanel: TPanel
      Left = 0
      Top = 0
      Width = 375
      Height = 139
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        375
        139)
      object InstructionsLabel: TLabel
        Left = 8
        Top = 8
        Width = 360
        Height = 39
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Instructions for authentication. Please fill in your credentials' +
          ' carefully. Enter all required information, including your sessi' +
          'on username and session password.X'
        FocusControl = PromptEdit1
        WordWrap = True
      end
      object PromptLabel1: TLabel
        Left = 8
        Top = 56
        Width = 360
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '&UsernameX:'
        FocusControl = PromptEdit1
        WordWrap = True
      end
      object PromptLabel2: TLabel
        Left = 8
        Top = 101
        Width = 360
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '&PasswordX:'
        FocusControl = PromptEdit2
        WordWrap = True
      end
      object PromptEdit1: TPasswordEdit
        Left = 8
        Top = 73
        Width = 361
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 250
        TabOrder = 0
      end
      object PromptEdit2: TPasswordEdit
        Left = 8
        Top = 118
        Width = 361
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 250
        TabOrder = 1
      end
    end
    object SavePasswordPanel: TPanel
      Left = 0
      Top = 139
      Width = 375
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SavePasswordCheck: TCheckBox
        Left = 14
        Top = 6
        Width = 275
        Height = 17
        Caption = '&Change stored password to this one'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
    object ButtonsPanel: TPanel
      Left = 0
      Top = 164
      Width = 375
      Height = 44
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        375
        44)
      object PasswordOKButton: TButton
        Left = 118
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
      object PasswordCancelButton: TButton
        Left = 206
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object PasswordHelpButton: TButton
        Left = 294
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Help'
        TabOrder = 2
        OnClick = HelpButtonClick
      end
    end
  end
  object BannerPanel: TPanel
    Left = 0
    Top = 234
    Width = 375
    Height = 82
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    DesignSize = (
      375
      82)
    object BannerMemo: TMemo
      Left = 8
      Top = 8
      Width = 360
      Height = 34
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WantReturns = False
    end
    object NeverShowAgainCheck: TCheckBox
      Left = 15
      Top = 53
      Width = 188
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = '&Never show this banner again'
      TabOrder = 1
    end
    object BannerCloseButton: TButton
      Left = 206
      Top = 47
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Continue'
      ModalResult = 1
      TabOrder = 2
    end
    object BannerHelpButton: TButton
      Left = 292
      Top = 47
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
  end
end
