object EditMaskDialog: TEditMaskDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_editmask'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Edit file mask'
  ClientHeight = 537
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    474
    537)
  TextHeight = 15
  object FilesGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 458
    Height = 192
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Files masks'
    TabOrder = 0
    DesignSize = (
      458
      192)
    object Label3: TLabel
      Left = 9
      Top = 22
      Width = 66
      Height = 15
      Caption = '&Include files:'
      FocusControl = IncludeFileMasksMemo
    end
    object Label1: TLabel
      Left = 232
      Top = 22
      Width = 67
      Height = 15
      Caption = '&Exclude files:'
      FocusControl = ExcludeFileMasksMemo
    end
    object IncludeFileMasksMemo: TMemo
      Left = 9
      Top = 40
      Width = 217
      Height = 142
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'IncludeFileMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = ControlChange
      OnExit = FileMasksMemoExit
    end
    object ExcludeFileMasksMemo: TMemo
      Left = 232
      Top = 40
      Width = 217
      Height = 142
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'ExcludeFileMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 1
      OnChange = ControlChange
      OnExit = FileMasksMemoExit
    end
  end
  object OKBtn: TButton
    Left = 214
    Top = 504
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelBtn: TButton
    Left = 300
    Top = 504
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object HelpButton: TButton
    Left = 386
    Top = 504
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
  object ClearButton: TButton
    Left = 128
    Top = 504
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 3
    OnClick = ClearButtonClick
  end
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 206
    Width = 458
    Height = 186
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories masks'
    TabOrder = 1
    DesignSize = (
      458
      186)
    object Label2: TLabel
      Left = 9
      Top = 22
      Width = 100
      Height = 15
      Caption = 'I&nclude directories:'
      FocusControl = IncludeDirectoryMasksMemo
    end
    object Label4: TLabel
      Left = 232
      Top = 22
      Width = 101
      Height = 15
      Caption = 'E&xclude directories:'
      FocusControl = ExcludeDirectoryMasksMemo
    end
    object IncludeDirectoryMasksMemo: TMemo
      Left = 9
      Top = 40
      Width = 217
      Height = 115
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'IncludeDirectoryMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = ControlChange
      OnExit = DirectoryMasksMemoExit
    end
    object ExcludeDirectoryMasksMemo: TMemo
      Left = 232
      Top = 40
      Width = 217
      Height = 115
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'ExcludeDirectoryMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 1
      OnChange = ExcludeDirectoryMasksMemoChange
      OnExit = DirectoryMasksMemoExit
    end
    object ExcludeDirectoryAllCheck: TCheckBox
      Left = 234
      Top = 161
      Width = 217
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = '&All (do not recurse)'
      TabOrder = 2
      OnClick = ExcludeDirectoryAllCheckClick
    end
  end
  object MaskGroup: TGroupBox
    Left = 8
    Top = 409
    Width = 458
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mask'
    TabOrder = 7
    DesignSize = (
      458
      89)
    object MaskMemo: TMemo
      Left = 9
      Top = 21
      Width = 440
      Height = 58
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Lines.Strings = (
        'MaskMemo')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object MaskHintText: TStaticText
    Left = 337
    Top = 391
    Width = 129
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'mask hints'
    TabOrder = 2
    TabStop = True
  end
end
