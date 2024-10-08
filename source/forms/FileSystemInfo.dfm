object FileSystemInfoDialog: TFileSystemInfoDialog
  Left = 320
  Top = 130
  HelpType = htKeyword
  HelpKeyword = 'ui_fsinfo'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Server and protocol information'
  ClientHeight = 398
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    371
    398)
  TextHeight = 13
  object CloseButton: TButton
    Left = 204
    Top = 364
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
    Top = 364
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
    Height = 352
    ActivePage = ProtocolSheet
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControlChange
    object ProtocolSheet: TTabSheet
      Caption = 'Protocol'
      DesignSize = (
        363
        324)
      object HostKeyGroup: TGroupBox
        Left = 6
        Top = 201
        Width = 351
        Height = 87
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Server host key fingerprints'
        TabOrder = 1
        DesignSize = (
          351
          87)
        object Label2: TLabel
          Left = 10
          Top = 18
          Width = 49
          Height = 13
          Caption = 'Algorithm:'
          FocusControl = HostKeyAlgorithmEdit
        end
        object Label3: TLabel
          Left = 10
          Top = 41
          Width = 46
          Height = 13
          Caption = 'SHA-256:'
          FocusControl = HostKeyFingerprintSHA256Edit
        end
        object Label4: TLabel
          Left = 10
          Top = 64
          Width = 25
          Height = 13
          Caption = 'MD5:'
          FocusControl = HostKeyFingerprintMD5Edit
        end
        object HostKeyFingerprintSHA256Edit: TEdit
          Left = 65
          Top = 41
          Width = 279
          Height = 17
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          PopupMenu = FingerprintPopupMenu
          ReadOnly = True
          TabOrder = 1
          Text = 'HostKeyFingerprintSHA256Edit'
          OnContextPopup = HostKeyFingerprintSHA256EditContextPopup
        end
        object HostKeyAlgorithmEdit: TEdit
          Left = 65
          Top = 18
          Width = 279
          Height = 17
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = 'HostKeyAlgorithmEdit'
        end
        object HostKeyFingerprintMD5Edit: TEdit
          Left = 65
          Top = 64
          Width = 279
          Height = 17
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          PopupMenu = FingerprintPopupMenu
          ReadOnly = True
          TabOrder = 2
          Text = 'HostKeyFingerprintMD5Edit'
          OnContextPopup = HostKeyFingerprintSHA256EditContextPopup
        end
      end
      object ServerView: TListView
        Left = 6
        Top = 8
        Width = 351
        Height = 187
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = 150
          end
          item
            Caption = 'Value'
            Width = 150
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = ListViewMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnContextPopup = ControlContextPopup
      end
      object CertificateGroup: TGroupBox
        Left = 6
        Top = 294
        Width = 351
        Height = 104
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Certificate fingerprint'
        TabOrder = 2
        DesignSize = (
          351
          104)
        object Label5: TLabel
          Left = 10
          Top = 20
          Width = 46
          Height = 13
          Caption = 'SHA-256:'
          FocusControl = CertificateFingerprintSha256Edit
        end
        object Label6: TLabel
          Left = 10
          Top = 43
          Width = 34
          Height = 13
          Caption = 'SHA-1:'
          FocusControl = CertificateFingerprintSha1Edit
        end
        object CertificateFingerprintSha256Edit: TEdit
          Left = 65
          Top = 20
          Width = 279
          Height = 17
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = 'CertificateFingerprintSha256Edit'
        end
        object CertificateViewButton: TButton
          Left = 10
          Top = 66
          Width = 121
          Height = 25
          Caption = '&Full certificate'
          TabOrder = 2
          OnClick = CertificateViewButtonClick
        end
        object CertificateFingerprintSha1Edit: TEdit
          Left = 65
          Top = 43
          Width = 279
          Height = 17
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          PopupMenu = FingerprintPopupMenu
          ReadOnly = True
          TabOrder = 1
          Text = 'CertificateFingerprintSha1Edit'
          OnContextPopup = HostKeyFingerprintSHA256EditContextPopup
        end
      end
    end
    object CapabilitiesSheet: TTabSheet
      Caption = 'Capabilities'
      ImageIndex = 1
      DesignSize = (
        363
        324)
      object InfoGroup: TGroupBox
        Left = 6
        Top = 202
        Width = 351
        Height = 114
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Additional information'
        TabOrder = 1
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
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
      object ProtocolView: TListView
        Left = 6
        Top = 8
        Width = 351
        Height = 187
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = 150
          end
          item
            Caption = 'Value'
            Width = 150
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = ListViewMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnContextPopup = ControlContextPopup
      end
    end
    object SpaceAvailableSheet: TTabSheet
      Caption = 'Space available'
      ImageIndex = 2
      DesignSize = (
        363
        324)
      object Label1: TLabel
        Left = 13
        Top = 13
        Width = 26
        Height = 13
        Caption = '&Path:'
        FocusControl = SpaceAvailablePathEdit
      end
      object SpaceAvailableView: TListView
        Left = 6
        Top = 40
        Width = 351
        Height = 155
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = 150
          end
          item
            Caption = 'Value'
            Width = 150
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = ListViewMenu
        TabOrder = 2
        ViewStyle = vsReport
        OnContextPopup = ControlContextPopup
        OnCustomDrawItem = SpaceAvailableViewCustomDrawItem
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
        OnEnter = SpaceAvailablePathEditEnter
        OnExit = SpaceAvailablePathEditExit
      end
      object SpaceAvailableButton: TButton
        Left = 256
        Top = 7
        Width = 99
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Check &space'
        TabOrder = 1
        OnClick = SpaceAvailableButtonClick
      end
    end
  end
  object ClipboardButton: TButton
    Left = 8
    Top = 364
    Width = 161
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Copy to Clipboard'
    TabOrder = 1
    OnClick = ClipboardButtonClick
  end
  object ListViewMenu: TPopupMenu
    Left = 151
    Top = 354
    object Copy: TMenuItem
      Caption = '&Copy'
      OnClick = CopyClick
    end
  end
  object FingerprintPopupMenu: TPopupMenu
    Left = 240
    Top = 354
    object Copy1: TMenuItem
      Action = EditCopyAction
    end
    object TMenuItem
      Action = EditSelectAllAction
    end
  end
  object FingerprintActionList: TActionList
    Left = 328
    Top = 354
    object EditCopyAction: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      ShortCut = 16451
      OnExecute = EditCopyActionExecute
      OnUpdate = EditCopyActionUpdate
    end
    object EditSelectAllAction: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      ShortCut = 16449
    end
  end
end
