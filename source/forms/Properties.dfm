object PropertiesDialog: TPropertiesDialog
  Left = 416
  Top = 133
  HelpType = htKeyword
  HelpKeyword = 'ui_properties'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Properties'
  ClientHeight = 424
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    398
    424)
  TextHeight = 15
  object OkButton: TButton
    Left = 138
    Top = 391
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 224
    Top = 391
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 398
    Height = 385
    ActivePage = CommonSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControlChange
    object CommonSheet: TTabSheet
      Caption = 'Common'
      DesignSize = (
        390
        355)
      object Bevel1: TBevel
        Left = 5
        Top = 44
        Width = 378
        Height = 9
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Label1: TLabel
        Left = 5
        Top = 55
        Width = 49
        Height = 15
        Caption = 'Location:'
        ShowAccelChar = False
      end
      object Label2: TLabel
        Left = 5
        Top = 78
        Width = 23
        Height = 15
        Caption = 'Size:'
        ShowAccelChar = False
      end
      object LinksToLabelLabel: TLabel
        Left = 5
        Top = 101
        Width = 44
        Height = 15
        Caption = 'Links to:'
        ShowAccelChar = False
      end
      object Bevel2: TBevel
        Left = 5
        Top = 124
        Width = 378
        Height = 9
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object RightsLabel: TLabel
        Left = 5
        Top = 201
        Width = 66
        Height = 15
        Caption = 'Permissions:'
        FocusControl = RightsFrame
      end
      object GroupOwnerRightsBevel: TBevel
        Left = 5
        Top = 190
        Width = 378
        Height = 9
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object GroupLabel: TLabel
        Left = 5
        Top = 162
        Width = 36
        Height = 15
        Caption = 'Group:'
        FocusControl = GroupComboBox
      end
      object OwnerLabel: TLabel
        Left = 5
        Top = 136
        Width = 38
        Height = 15
        Caption = 'Owner:'
        FocusControl = OwnerComboBox
      end
      object FileIconImage: TImage
        Left = 8
        Top = 5
        Width = 32
        Height = 32
        AutoSize = True
      end
      object RecursiveBevel: TBevel
        Left = 5
        Top = 322
        Width = 378
        Height = 9
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object LocationLabel: TEdit
        Left = 90
        Top = 55
        Width = 293
        Height = 17
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = bsNone
        TabOrder = 7
        Text = 'LocationLabel'
      end
      object FileLabel: TEdit
        Left = 90
        Top = 13
        Width = 293
        Height = 17
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = bsNone
        TabOrder = 8
        Text = 'FileLabel'
      end
      object SizeLabel: TEdit
        Left = 90
        Top = 78
        Width = 215
        Height = 17
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = bsNone
        TabOrder = 9
        Text = 'SizeLabel'
      end
      object LinksToLabel: TEdit
        Left = 90
        Top = 101
        Width = 293
        Height = 17
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = bsNone
        TabOrder = 10
        Text = 'LinksToLabel'
      end
      object GroupView: TEdit
        Left = 90
        Top = 162
        Width = 293
        Height = 17
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        MaxLength = 50
        TabOrder = 4
        Text = 'GroupView'
      end
      object OwnerView: TEdit
        Left = 90
        Top = 136
        Width = 293
        Height = 17
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        MaxLength = 50
        TabOrder = 2
        Text = 'OwnerView'
      end
      inline RightsFrame: TRightsFrame
        Left = 89
        Top = 194
        Width = 258
        Height = 128
        TabOrder = 5
      end
      object GroupComboBox: TComboBox
        Left = 90
        Top = 159
        Width = 179
        Height = 23
        DropDownCount = 16
        MaxLength = 50
        TabOrder = 3
        Text = 'GroupComboBox'
        OnChange = ControlChange
        OnExit = GroupComboBoxExit
      end
      object OwnerComboBox: TComboBox
        Left = 90
        Top = 133
        Width = 179
        Height = 23
        DropDownCount = 16
        MaxLength = 50
        TabOrder = 1
        Text = 'OwnerComboBox'
        OnChange = ControlChange
        OnExit = OwnerComboBoxExit
      end
      object RecursiveCheck2: TCheckBox
        Left = 8
        Top = 332
        Width = 375
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Set owner, group and permissions &recursively'
        TabOrder = 6
        OnClick = ControlChange
      end
      object CalculateSizeButton: TButton
        Left = 303
        Top = 72
        Width = 80
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'C&alculate'
        TabOrder = 0
        OnClick = CalculateSizeButtonClick
      end
    end
    object ChecksumSheet: TTabSheet
      Caption = 'Checksum'
      ImageIndex = 1
      DesignSize = (
        390
        355)
      object Label6: TLabel
        Left = 5
        Top = 8
        Width = 57
        Height = 15
        Caption = '&Algorithm:'
        FocusControl = ChecksumAlgEdit
      end
      object ChecksumView: TListView
        Left = 5
        Top = 34
        Width = 378
        Height = 315
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'File'
            Width = 100
          end
          item
            Caption = 'Checksum'
            Width = 100
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
        OnContextPopup = ListViewContextPopup
      end
      object ChecksumAlgEdit: TComboBox
        Left = 90
        Top = 5
        Width = 152
        Height = 23
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 250
        TabOrder = 0
        OnChange = ChecksumAlgEditChange
        OnEnter = ControlChange
        OnExit = ControlChange
        Items.Strings = (
          'Xmd5')
      end
      object ChecksumButton: TButton
        Left = 248
        Top = 4
        Width = 135
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Calculate checksum'
        TabOrder = 1
        OnClick = ChecksumButtonClick
      end
      object ChecksumGroup: TGroupBox
        Left = 5
        Top = 34
        Width = 378
        Height = 51
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Checksum'
        TabOrder = 3
        DesignSize = (
          378
          51)
        object ChecksumUnknownLabel: TLabel
          Left = 10
          Top = 18
          Width = 135
          Height = 15
          Caption = 'ChecksumUnknownLabel'
          ShowAccelChar = False
        end
        object ChecksumEdit: TEdit
          Left = 9
          Top = 22
          Width = 358
          Height = 19
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
          Text = 'ChecksumEdit'
        end
      end
    end
    object TagsSheet: TTabSheet
      Caption = 'Tags'
      ImageIndex = 2
      DesignSize = (
        390
        355)
      object TagsView: TListView
        Left = 3
        Top = 5
        Width = 382
        Height = 227
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Key'
            Width = 100
          end
          item
            Caption = 'Value'
            Width = 100
          end>
        ColumnClick = False
        DoubleBuffered = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = ListViewMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnContextPopup = ListViewContextPopup
        OnDblClick = TagsViewDblClick
        OnKeyDown = TagsViewKeyDown
        OnSelectItem = TagsViewSelectItem
      end
      object AddTagButton: TButton
        Left = 133
        Top = 238
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Add...'
        TabOrder = 1
        OnClick = AddTagButtonClick
      end
      object RemoveTagButton: TButton
        Left = 305
        Top = 238
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Remove'
        TabOrder = 3
        OnClick = RemoveTagButtonClick
      end
      object EditTagButton: TButton
        Left = 219
        Top = 238
        Width = 80
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Edit...'
        TabOrder = 2
        OnClick = EditTagButtonClick
      end
    end
  end
  object HelpButton: TButton
    Left = 310
    Top = 391
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
  object ListViewMenu: TPopupMenu
    Left = 16
    Top = 400
    object Copy: TMenuItem
      Caption = '&Copy'
      OnClick = CopyClick
    end
  end
end
