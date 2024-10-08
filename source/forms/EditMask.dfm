object EditMaskDialog: TEditMaskDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_editmask'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Edit file mask'
  ClientHeight = 500
  ClientWidth = 425
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    425
    500)
  TextHeight = 13
  object FilesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 409
    Height = 179
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Files masks'
    TabOrder = 0
    DesignSize = (
      409
      179)
    object Label3: TLabel
      Left = 16
      Top = 19
      Width = 61
      Height = 13
      Caption = '&Include files:'
      FocusControl = IncludeFileMasksMemo
    end
    object Label1: TLabel
      Left = 212
      Top = 19
      Width = 63
      Height = 13
      Caption = '&Exclude files:'
      FocusControl = ExcludeFileMasksMemo
    end
    object IncludeFileMasksMemo: TMemo
      Left = 16
      Top = 35
      Width = 181
      Height = 129
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'IncludeFileMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = ControlChange
      OnExit = FileMasksMemoExit
    end
    object ExcludeFileMasksMemo: TMemo
      Left = 212
      Top = 35
      Width = 181
      Height = 129
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
    Left = 174
    Top = 467
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelBtn: TButton
    Left = 259
    Top = 467
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object HelpButton: TButton
    Left = 342
    Top = 467
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 5
    OnClick = HelpButtonClick
  end
  object ClearButton: TButton
    Left = 89
    Top = 467
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 2
    OnClick = ClearButtonClick
  end
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 191
    Width = 409
    Height = 172
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories masks'
    TabOrder = 1
    DesignSize = (
      409
      172)
    object Label2: TLabel
      Left = 16
      Top = 19
      Width = 92
      Height = 13
      Caption = 'I&nclude directories:'
      FocusControl = IncludeDirectoryMasksMemo
    end
    object Label4: TLabel
      Left = 212
      Top = 19
      Width = 94
      Height = 13
      Caption = 'E&xclude directories:'
      FocusControl = ExcludeDirectoryMasksMemo
    end
    object IncludeDirectoryMasksMemo: TMemo
      Left = 16
      Top = 35
      Width = 181
      Height = 104
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'IncludeDirectoryMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = ControlChange
      OnExit = DirectoryMasksMemoExit
    end
    object ExcludeDirectoryMasksMemo: TMemo
      Left = 212
      Top = 35
      Width = 181
      Height = 104
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'ExcludeDirectoryMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 1
      OnChange = ExcludeDirectoryMasksMemoChange
      OnExit = DirectoryMasksMemoExit
    end
    object ExcludeDirectoryAllCheck: TCheckBox
      Left = 212
      Top = 145
      Width = 181
      Height = 17
      Caption = '&All (do not recurse)'
      TabOrder = 2
      OnClick = ExcludeDirectoryAllCheckClick
    end
  end
  object MaskGroup: TGroupBox
    Left = 8
    Top = 385
    Width = 409
    Height = 76
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Mask'
    TabOrder = 6
    DesignSize = (
      409
      76)
    object MaskMemo: TMemo
      Left = 7
      Top = 15
      Width = 395
      Height = 52
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
    Left = 288
    Top = 369
    Width = 129
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'mask hints'
    TabOrder = 7
    TabStop = True
  end
end
