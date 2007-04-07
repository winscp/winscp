object AuthenticateForm: TAuthenticateForm
  Left = 304
  Top = 113
  HelpType = htKeyword
  HelpKeyword = 'ui_authenticate'
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'AuthenticateForm'
  ClientHeight = 270
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
  Position = poMainFormCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LogView: TListView
    Left = 0
    Top = 0
    Width = 375
    Height = 35
    Align = alClient
    Columns = <
      item
        Width = -1
        WidthType = (
          -1)
      end>
    Items.Data = {
      2E0000000100000000000000FFFFFFFFFFFFFFFF000000000000000011417574
      686E657469636174696E672E2E2E}
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PasswordPanel: TPanel
    Left = 0
    Top = 35
    Width = 375
    Height = 153
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    DesignSize = (
      375
      153)
    object PasswordEditPanel: TPanel
      Left = 0
      Top = 0
      Width = 375
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        375
        50)
      object PasswordLabel: TLabel
        Left = 8
        Top = 8
        Width = 360
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '&PasswordX:'
        FocusControl = PasswordEdit
        WordWrap = True
      end
      object PasswordEdit: TPasswordEdit
        Left = 8
        Top = 24
        Width = 361
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        MaxLength = 250
        TabOrder = 0
      end
    end
    object ServerPromptPanel: TPanel
      Left = 0
      Top = 50
      Width = 375
      Height = 69
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        375
        69)
      object ServerPromptLabel: TLabel
        Left = 8
        Top = 24
        Width = 359
        Height = 44
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = 
          'Note: This prompt is issued by the server. It is part of either ' +
          'keyboard-interactive, TIS or Cryptocard authentication.'
        WordWrap = True
      end
      object HideTypingCheck: TCheckBox
        Left = 14
        Top = 2
        Width = 275
        Height = 17
        Caption = 'Hide &typing'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = HideTypingCheckClick
      end
    end
    object PasswordOKButton: TButton
      Left = 118
      Top = 119
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
    end
    object PasswordCancelButton: TButton
      Left = 206
      Top = 119
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object PasswordHelpButton: TButton
      Left = 294
      Top = 119
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 4
      OnClick = HelpButtonClick
    end
  end
  object BannerPanel: TPanel
    Left = 0
    Top = 188
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
