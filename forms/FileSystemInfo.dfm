object FileSystemInfoDialog: TFileSystemInfoDialog
  Left = 345
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Server and protocol information'
  ClientHeight = 372
  ClientWidth = 367
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  DesignSize = (
    367
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object CloseButton: TButton
    Left = 283
    Top = 338
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ServerGroup: TXPGroupBox
    Left = 8
    Top = 8
    Width = 351
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Server information'
    TabOrder = 1
    DesignSize = (
      351
      89)
    object Label1: TLabel
      Left = 10
      Top = 18
      Width = 103
      Height = 13
      Caption = 'SSH protocol version:'
    end
    object Label2: TLabel
      Left = 10
      Top = 34
      Width = 98
      Height = 13
      Caption = 'Encryption algorithm:'
    end
    object Label3: TLabel
      Left = 10
      Top = 50
      Width = 63
      Height = 13
      Caption = 'Compression:'
    end
    object Label7: TLabel
      Left = 10
      Top = 66
      Width = 98
      Height = 13
      Caption = 'File transfer protocol:'
    end
    object SshVersionEdit: TEdit
      Left = 214
      Top = 18
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'SshVersionEdit'
    end
    object CipherEdit: TEdit
      Left = 214
      Top = 34
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'CipherEdit'
    end
    object CompressionEdit: TEdit
      Left = 214
      Top = 50
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'CompressionEdit'
    end
    object FSProtocolEdit: TEdit
      Left = 214
      Top = 66
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'FSProtocolEdit'
    end
  end
  object ProtocolGroup: TXPGroupBox
    Left = 8
    Top = 104
    Width = 351
    Height = 223
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Protocol capabilities/information'
    TabOrder = 2
    DesignSize = (
      351
      223)
    object Label4: TLabel
      Left = 10
      Top = 18
      Width = 118
      Height = 13
      Caption = 'Can change permissions:'
    end
    object Label5: TLabel
      Left = 10
      Top = 34
      Width = 125
      Height = 13
      Caption = 'Can change owner/group:'
    end
    object Label6: TLabel
      Left = 10
      Top = 50
      Width = 152
      Height = 13
      Caption = 'Can execute arbitrary command:'
    end
    object Label8: TLabel
      Left = 10
      Top = 66
      Width = 134
      Height = 13
      Caption = 'Can create symlink/hardlink:'
    end
    object Label9: TLabel
      Left = 10
      Top = 98
      Width = 162
      Height = 13
      Caption = 'Native text (ASCII) mode transfers:'
    end
    object Label10: TLabel
      Left = 10
      Top = 82
      Width = 115
      Height = 13
      Caption = 'Can lookup user groups:'
    end
    object ModeChangingEdit: TEdit
      Left = 214
      Top = 18
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'ModeChangingEdit'
    end
    object OwnerGroupChangingEdit: TEdit
      Left = 214
      Top = 34
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
      Text = 'OwnerGroupChangingEdit'
    end
    object AnyCommandEdit: TEdit
      Left = 214
      Top = 50
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
      Text = 'AnyCommandEdit'
    end
    object SymbolicHardLinkEdit: TEdit
      Left = 214
      Top = 66
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 3
      Text = 'SymbolicHardLinkEdit'
    end
    object NativeTextModeEdit: TEdit
      Left = 214
      Top = 98
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 4
      Text = 'NativeTextModeEdit'
    end
    object UserGroupListingEdit: TEdit
      Left = 214
      Top = 82
      Width = 129
      Height = 17
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 5
      Text = 'UserGroupListingEdit'
    end
    object InfoMemo: TMemo
      Left = 8
      Top = 120
      Width = 335
      Height = 93
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBtnFace
      Lines.Strings = (
        'InfoMemo')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 6
      WantReturns = False
      WordWrap = False
    end
  end
end
