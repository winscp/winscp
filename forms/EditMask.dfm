object EditMaskDialog: TEditMaskDialog
  Left = 369
  Top = 257
  HelpType = htKeyword
  HelpKeyword = 'ui_editmask'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Edit file mask'
  ClientHeight = 484
  ClientWidth = 425
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  DesignSize = (
    425
    484)
  PixelsPerInch = 96
  TextHeight = 13
  object FilesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 409
    Height = 195
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Files masks'
    TabOrder = 0
    DesignSize = (
      409
      195)
    object Label3: TLabel
      Left = 16
      Top = 19
      Width = 61
      Height = 13
      Caption = '&Include files:'
    end
    object Label1: TLabel
      Left = 212
      Top = 19
      Width = 63
      Height = 13
      Caption = '&Exclude files:'
    end
    object IncludeFileMasksMemo: TMemo
      Left = 16
      Top = 35
      Width = 181
      Height = 135
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
      Height = 135
      Anchors = [akLeft, akTop, akBottom]
      Lines.Strings = (
        'ExcludeFileMasksMemo')
      ScrollBars = ssVertical
      TabOrder = 1
      OnChange = ControlChange
      OnExit = FileMasksMemoExit
    end
    object MaskHintText: TStaticText
      Left = 288
      Top = 174
      Width = 105
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'mask hints'
      TabOrder = 2
      TabStop = True
    end
  end
  object OKBtn: TButton
    Left = 174
    Top = 451
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
    Top = 451
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
    Top = 451
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 5
    OnClick = HelpButtonClick
  end
  object ClearButton: TButton
    Left = 89
    Top = 451
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 2
    OnClick = ClearButtonClick
  end
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 207
    Width = 409
    Height = 156
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories masks'
    TabOrder = 1
    DesignSize = (
      409
      156)
    object Label2: TLabel
      Left = 16
      Top = 19
      Width = 92
      Height = 13
      Caption = 'I&nclude directories:'
    end
    object Label4: TLabel
      Left = 212
      Top = 19
      Width = 94
      Height = 13
      Caption = 'E&xclude directories:'
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
      OnChange = ControlChange
      OnExit = DirectoryMasksMemoExit
    end
  end
  object MaskGroup: TGroupBox
    Left = 8
    Top = 369
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
      WantReturns = False
    end
  end
end
