object SynchronizeDialog: TSynchronizeDialog
  Left = 343
  Top = 210
  BorderStyle = bsDialog
  Caption = 'Keep remote directory up to date'
  ClientHeight = 284
  ClientWidth = 511
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    511
    284)
  PixelsPerInch = 96
  TextHeight = 13
  object StatusLabel: TLabel
    Left = 8
    Top = 8
    Width = 496
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Watch for new or changed files in local directory XXX and automa' +
      'tically upload them to remote directory XXX.'
    WordWrap = True
  end
  object StartButton: TButton
    Left = 37
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Start'
    TabOrder = 0
    OnClick = StartButtonClick
  end
  object StopButton: TButton
    Left = 127
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Stop'
    TabOrder = 1
    OnClick = StopButtonClick
  end
  object CloseButton: TButton
    Left = 305
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 2
  end
  object MorePanel: TPanel
    Left = 0
    Top = 52
    Width = 510
    Height = 190
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    inline CopyParamsFrame: TCopyParamsFrame
      Left = 2
      Top = 0
      Width = 508
      Height = 150
      TabOrder = 0
      inherited TransferModeGroup: TXPGroupBox
        inherited AsciiFileMaskLabel: TLabel
          Caption = 'Transfer following files in text m&ode'
        end
      end
    end
    object SaveSettingsCheck: TCheckBox
      Left = 8
      Top = 172
      Width = 409
      Height = 17
      Caption = 'Use &same settings next time'
      TabOrder = 1
    end
    object ExistingOnlyCheck: TCheckBox
      Left = 8
      Top = 152
      Width = 393
      Height = 17
      Caption = 'Update only &existing files'
      TabOrder = 2
    end
  end
  object MoreButton: TMoreButton
    Left = 395
    Top = 249
    Width = 75
    Height = 25
    Caption = '<< &Less'
    TabOrder = 4
    Panel = MorePanel
    RepositionForm = True
  end
  object MinimizeButton: TButton
    Left = 215
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Minimi&ze'
    TabOrder = 5
    OnClick = MinimizeButtonClick
  end
end
