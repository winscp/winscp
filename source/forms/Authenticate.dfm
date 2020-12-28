object AuthenticateForm: TAuthenticateForm
  Left = 304
  Top = 113
  HelpType = htKeyword
  HelpKeyword = 'ui_authenticate'
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'AuthenticateForm'
  ClientHeight = 380
  ClientWidth = 375
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 280
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 375
    Height = 65
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object LogView: TListBox
      Left = 48
      Top = 0
      Width = 327
      Height = 65
      Style = lbOwnerDrawVariable
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnDrawItem = LogViewDrawItem
      OnMeasureItem = LogViewMeasureItem
      OnMouseMove = LogViewMouseMove
    end
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 48
      Height = 65
      Align = alLeft
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      object AnimationPaintBox: TPaintBox
        Left = 8
        Top = 11
        Width = 32
        Height = 32
      end
    end
  end
  object PasswordPanel: TPanel
    Left = 0
    Top = 65
    Width = 375
    Height = 233
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
      Top = 164
      Width = 375
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
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
      Top = 189
      Width = 375
      Height = 44
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
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
    object SessionRememberPasswordPanel: TPanel
      Left = 0
      Top = 139
      Width = 375
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SessionRememberPasswordCheck: TCheckBox
        Left = 14
        Top = 6
        Width = 275
        Height = 17
        Caption = '&Remember password for this session'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
    end
  end
  object BannerPanel: TPanel
    Left = 0
    Top = 298
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
      PopupMenu = BannerPopupMenu
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WantReturns = False
      OnContextPopup = BannerMemoContextPopup
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
  object ActionList: TActionList
    Left = 32
    Top = 304
    object EditCopyAction: TEditCopy
      Category = 'Banner'
      Caption = '&Copy'
      ImageIndex = 0
      ShortCut = 16451
    end
    object EditSelectAllAction: TEditSelectAll
      Category = 'Banner'
      Caption = 'Select &All'
      ImageIndex = 1
      ShortCut = 16449
    end
    object BannerMonospacedFontAction: TAction
      Category = 'Banner'
      Caption = 'Use &Monospaced Font'
      OnExecute = BannerMonospacedFontActionExecute
    end
    object LabelCopyAction: TAction
      Category = 'Label'
      Caption = '&Copy'
      OnExecute = LabelCopyActionExecute
    end
    object LabelOpenLinkAction: TAction
      Category = 'Label'
      Caption = '&Open'
      OnExecute = LabelOpenLinkActionExecute
    end
  end
  object BannerPopupMenu: TPopupMenu
    Left = 144
    Top = 304
    object CopyItem: TMenuItem
      Action = EditCopyAction
    end
    object SelectAllItem: TMenuItem
      Action = EditSelectAllAction
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AdjustWindowItem: TMenuItem
      Action = BannerMonospacedFontAction
    end
  end
  object LabelPopupMenu: TPopupMenu
    Left = 56
    Top = 72
    object Copy1: TMenuItem
      Action = LabelCopyAction
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Open1: TMenuItem
      Action = LabelOpenLinkAction
      Default = True
    end
  end
end
