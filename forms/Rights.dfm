object RightsFrame: TRightsFrame
  Left = 0
  Top = 0
  Width = 163
  Height = 109
  PopupMenu = NonVisualDataModule.RightsPopup
  TabOrder = 0
  OnContextPopup = FrameContextPopup
  OnEnter = FrameEnter
  object OwnerLabel: TLabel
    Left = 4
    Top = 4
    Width = 31
    Height = 13
    Caption = '&Owner'
    FocusControl = OwnerReadCheck
  end
  object GroupLabel: TLabel
    Left = 4
    Top = 24
    Width = 29
    Height = 13
    Caption = '&Group'
    FocusControl = GroupReadCheck
  end
  object OthersLabel: TLabel
    Left = 4
    Top = 44
    Width = 31
    Height = 13
    Caption = 'Ot&hers'
    FocusControl = OthersReadCheck
  end
  object OthersButton: TSpeedButton
    Tag = 3
    Left = 0
    Top = 41
    Width = 53
    Height = 19
    Flat = True
    OnClick = RightsButtonsClick
  end
  object GroupButton: TSpeedButton
    Tag = 2
    Left = 0
    Top = 21
    Width = 53
    Height = 19
    Flat = True
    OnClick = RightsButtonsClick
  end
  object OwnerButton: TSpeedButton
    Tag = 1
    Left = 0
    Top = 1
    Width = 53
    Height = 19
    Flat = True
    OnClick = RightsButtonsClick
  end
  object OctalLabel: TLabel
    Left = 4
    Top = 68
    Width = 25
    Height = 13
    Caption = 'O&ctal'
    FocusControl = OctalEdit
  end
  object OwnerReadCheck: TCheckBox
    Tag = 1
    Left = 55
    Top = 3
    Width = 34
    Height = 17
    Caption = 'R'
    TabOrder = 0
    OnClick = ControlChange
  end
  object OwnerWriteCheck: TCheckBox
    Tag = 2
    Left = 92
    Top = 3
    Width = 34
    Height = 17
    Caption = 'W'
    TabOrder = 1
    OnClick = ControlChange
  end
  object OwnerExecuteCheck: TCheckBox
    Tag = 3
    Left = 129
    Top = 3
    Width = 34
    Height = 17
    Caption = 'X'
    TabOrder = 2
    OnClick = ControlChange
  end
  object GroupReadCheck: TCheckBox
    Tag = 4
    Left = 55
    Top = 23
    Width = 34
    Height = 17
    Caption = 'R'
    TabOrder = 3
    OnClick = ControlChange
  end
  object GroupWriteCheck: TCheckBox
    Tag = 5
    Left = 92
    Top = 23
    Width = 33
    Height = 17
    Caption = 'W'
    TabOrder = 4
    OnClick = ControlChange
  end
  object GroupExecuteCheck: TCheckBox
    Tag = 6
    Left = 129
    Top = 23
    Width = 34
    Height = 17
    Caption = 'X'
    TabOrder = 5
    OnClick = ControlChange
  end
  object OthersReadCheck: TCheckBox
    Tag = 7
    Left = 55
    Top = 43
    Width = 34
    Height = 17
    Caption = 'R'
    TabOrder = 6
    OnClick = ControlChange
  end
  object OthersWriteCheck: TCheckBox
    Tag = 8
    Left = 92
    Top = 43
    Width = 33
    Height = 17
    Caption = 'W'
    TabOrder = 7
    OnClick = ControlChange
  end
  object OthersExecuteCheck: TCheckBox
    Tag = 9
    Left = 129
    Top = 43
    Width = 34
    Height = 17
    Caption = 'X'
    TabOrder = 8
    OnClick = ControlChange
  end
  object OctalEdit: TEdit
    Left = 55
    Top = 64
    Width = 64
    Height = 21
    MaxLength = 3
    TabOrder = 9
    Text = 'OctalEdit'
    OnChange = OctalEditChange
    OnExit = OctalEditExit
  end
  object DirectoriesXCheck: TCheckBox
    Left = 5
    Top = 89
    Width = 156
    Height = 17
    Caption = 'Add &X to directories'
    TabOrder = 10
    OnClick = ControlChange
  end
end
