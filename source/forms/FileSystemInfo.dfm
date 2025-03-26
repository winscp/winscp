object FileSystemInfoDialog: TFileSystemInfoDialog
  Left = 320
  Top = 130
  HelpType = htKeyword
  HelpKeyword = 'ui_fsinfo'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Server and protocol information'
  ClientHeight = 444
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    432
    444)
  TextHeight = 15
  object CloseButton: TButton
    Left = 258
    Top = 411
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 344
    Top = 411
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 432
    Height = 405
    ActivePage = ProtocolSheet
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControlChange
    object ProtocolSheet: TTabSheet
      Caption = 'Protocol'
      DesignSize = (
        424
        375)
      object HostKeyGroup: TGroupBox
        Left = 5
        Top = 236
        Width = 411
        Height = 95
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Server host key fingerprints'
        TabOrder = 1
        DesignSize = (
          411
          95)
        object Label2: TLabel
          Left = 8
          Top = 22
          Width = 57
          Height = 15
          Caption = 'Algorithm:'
          FocusControl = HostKeyAlgorithmEdit
        end
        object Label3: TLabel
          Left = 8
          Top = 45
          Width = 49
          Height = 15
          Caption = 'SHA-256:'
          FocusControl = HostKeyFingerprintSHA256Edit
        end
        object Label4: TLabel
          Left = 8
          Top = 68
          Width = 28
          Height = 15
          Caption = 'MD5:'
          FocusControl = HostKeyFingerprintMD5Edit
        end
        object HostKeyFingerprintSHA256Edit: TEdit
          Left = 88
          Top = 45
          Width = 314
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
          Left = 88
          Top = 22
          Width = 314
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
          Left = 88
          Top = 68
          Width = 314
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
        Left = 5
        Top = 5
        Width = 412
        Height = 225
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
        Left = 5
        Top = 337
        Width = 411
        Height = 102
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Certificate fingerprint'
        TabOrder = 2
        DesignSize = (
          411
          102)
        object Label5: TLabel
          Left = 8
          Top = 22
          Width = 49
          Height = 15
          Caption = 'SHA-256:'
          FocusControl = CertificateFingerprintSha256Edit
        end
        object Label6: TLabel
          Left = 8
          Top = 45
          Width = 37
          Height = 15
          Caption = 'SHA-1:'
          FocusControl = CertificateFingerprintSha1Edit
        end
        object CertificateFingerprintSha256Edit: TEdit
          Left = 88
          Top = 22
          Width = 314
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
          Left = 8
          Top = 68
          Width = 134
          Height = 25
          Caption = '&Full certificate'
          TabOrder = 2
          OnClick = CertificateViewButtonClick
        end
        object CertificateFingerprintSha1Edit: TEdit
          Left = 88
          Top = 45
          Width = 314
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
        424
        375)
      object InfoGroup: TGroupBox
        Left = 5
        Top = 236
        Width = 412
        Height = 134
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Additional information'
        TabOrder = 1
        DesignSize = (
          412
          134)
        object InfoMemo: TMemo
          Left = 9
          Top = 22
          Width = 394
          Height = 102
          TabStop = False
          Anchors = [akLeft, akTop, akRight, akBottom]
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
        Left = 5
        Top = 5
        Width = 412
        Height = 225
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
        424
        375)
      object Label1: TLabel
        Left = 5
        Top = 8
        Width = 27
        Height = 15
        Caption = '&Path:'
        FocusControl = SpaceAvailablePathEdit
      end
      object SpaceAvailableView: TListView
        Left = 5
        Top = 34
        Width = 412
        Height = 196
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
        Top = 5
        Width = 246
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 250
        TabOrder = 0
        OnChange = ControlChange
        OnEnter = SpaceAvailablePathEditEnter
        OnExit = SpaceAvailablePathEditExit
      end
      object SpaceAvailableButton: TButton
        Left = 308
        Top = 4
        Width = 109
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
    Top = 411
    Width = 179
    Height = 25
    Anchors = [akLeft, akBottom]
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
