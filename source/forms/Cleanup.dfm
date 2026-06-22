object CleanupDialog: TCleanupDialog
  Left = 356
  Top = 218
  HelpType = htKeyword
  HelpKeyword = 'ui_cleanup'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Cleanup application data'
  ClientHeight = 323
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    489
    323)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 473
    Height = 108
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Following list contains all application data possibly stored on ' +
      'this computer. Check those you want to remove.'#13#10#13#10'If another ins' +
      'tance of application is running, please close it before cleaning' +
      ' data.'#13#10#13#10'Notice: Opening session and/or next execution of appli' +
      'cation will recreate some of data.'
    ShowAccelChar = False
    WordWrap = True
  end
  object OKButton: TButton
    Left = 231
    Top = 290
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 317
    Top = 290
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
  end
  object DataListView: TListView
    Left = 8
    Top = 117
    Width = 473
    Height = 167
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Data'
        Width = 160
      end
      item
        Caption = 'Location'
        Width = 500
      end>
    ColumnClick = False
    DoubleBuffered = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnInfoTip = DataListViewInfoTip
    OnKeyUp = DataListViewKeyUp
    OnMouseDown = DataListViewMouseDown
  end
  object CheckAllButton: TButton
    Left = 8
    Top = 290
    Width = 120
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Un/check &all'
    TabOrder = 1
    OnClick = CheckAllButtonClick
  end
  object HelpButton: TButton
    Left = 401
    Top = 290
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
end
