object FileSystemInfoDialog: TFileSystemInfoDialog
  Left = 320
  Top = 130
  HelpType = htKeyword
  HelpKeyword = 'ui_fsinfo'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Server and protocol information'
  ClientHeight = 357
  ClientWidth = 371
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    371
    357)
  PixelsPerInch = 96
  TextHeight = 13
  object CloseButton: TButton
    Left = 204
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 287
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 371
    Height = 311
    ActivePage = SshSheet
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    OnChange = PageControlChange
    object SshSheet: TTabSheet
      Caption = 'SSH'
      DesignSize = (
        363
        283)
      object HostKeyGroup: TXPGroupBox
        Left = 6
        Top = 161
        Width = 351
        Height = 41
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Server host key fingerprint'
        TabOrder = 0
        DesignSize = (
          351
          41)
        object HostKeyFingerprintEdit: TEdit
          Left = 10
          Top = 18
          Width = 334
          Height = 17
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = 'HostKeyFingerprintEdit'
        end
      end
      object ServerView: TListView
        Left = 6
        Top = 8
        Width = 351
        Height = 146
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = -2
            WidthType = (
              -2)
          end
          item
            Caption = 'Value'
            Width = -2
            WidthType = (
              -2)
          end>
        ColumnClick = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = ListViewMenu
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object ProtocolSheet: TTabSheet
      Caption = 'Protocol'
      ImageIndex = 1
      DesignSize = (
        363
        283)
      object InfoGroup: TXPGroupBox
        Left = 6
        Top = 161
        Width = 351
        Height = 114
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Additional protocol information'
        TabOrder = 0
        DesignSize = (
          351
          114)
        object InfoMemo: TMemo
          Left = 9
          Top = 17
          Width = 333
          Height = 87
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnFace
          Lines.Strings = (
            'InfoMemo')
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          WantReturns = False
          WordWrap = False
        end
      end
      object ProtocolView: TListView
        Left = 6
        Top = 8
        Width = 351
        Height = 146
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = -2
            WidthType = (
              -2)
          end
          item
            Caption = 'Value'
            Width = -2
            WidthType = (
              -2)
          end>
        ColumnClick = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = ListViewMenu
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object SpaceAvailableSheet: TTabSheet
      Caption = 'Space available'
      ImageIndex = 2
      DesignSize = (
        363
        283)
      object Label1: TLabel
        Left = 13
        Top = 13
        Width = 22
        Height = 13
        Caption = '&Path'
        FocusControl = SpaceAvailablePathEdit
      end
      object SpaceAvailableView: TListView
        Left = 6
        Top = 40
        Width = 351
        Height = 113
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = -2
            WidthType = (
              -2)
          end
          item
            Caption = 'Value'
            Width = -2
            WidthType = (
              -2)
          end>
        ColumnClick = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = ListViewMenu
        TabOrder = 2
        ViewStyle = vsReport
      end
      object SpaceAvailablePathEdit: TEdit
        Left = 56
        Top = 9
        Width = 193
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 250
        TabOrder = 0
        OnChange = ControlChange
      end
      object SpaceAvailableButton: TButton
        Left = 256
        Top = 7
        Width = 99
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Check &space'
        Default = True
        TabOrder = 1
        OnClick = SpaceAvailableButtonClick
      end
    end
  end
  object ClipboardButton: TButton
    Left = 8
    Top = 323
    Width = 121
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Copy to Clipboard'
    TabOrder = 1
    OnClick = ClipboardButtonClick
  end
  object ListViewMenu: TPopupMenu
    Left = 144
    Top = 320
    object Copy: TMenuItem
      Caption = '&Copy'
      OnClick = CopyClick
    end
  end
end
