inherited RightsExtFrame: TRightsExtFrame
  Width = 239
  Height = 109
  object OctalLabel: TLabel [6]
    Left = 4
    Top = 68
    Width = 25
    Height = 13
    Caption = 'O&ctal'
    FocusControl = OctalEdit
  end
  inherited GroupReadCheck: TCheckBox
    TabOrder = 4
  end
  inherited GroupWriteCheck: TCheckBox
    TabOrder = 5
  end
  inherited GroupExecuteCheck: TCheckBox
    TabOrder = 6
  end
  inherited OthersReadCheck: TCheckBox
    TabOrder = 8
  end
  inherited OthersWriteCheck: TCheckBox
    TabOrder = 9
  end
  inherited OthersExecuteCheck: TCheckBox
    TabOrder = 10
  end
  inherited DirectoriesXCheck: TCheckBox
    Top = 89
    TabOrder = 12
  end
  object OctalEdit: TEdit [17]
    Left = 55
    Top = 64
    Width = 64
    Height = 21
    MaxLength = 4
    TabOrder = 13
    Text = 'OctalEdit'
    OnChange = OctalEditChange
    OnExit = OctalEditExit
  end
  object SetUidCheck: TCheckBox [18]
    Tag = 2048
    Left = 169
    Top = 3
    Width = 70
    Height = 17
    Caption = 'Set UID'
    TabOrder = 3
    OnClick = ControlChange
  end
  object SetGIDCheck: TCheckBox [19]
    Tag = 1024
    Left = 169
    Top = 23
    Width = 70
    Height = 17
    Caption = 'Set GID'
    TabOrder = 7
    OnClick = ControlChange
  end
  object StickyBitCheck: TCheckBox [20]
    Tag = 512
    Left = 169
    Top = 43
    Width = 70
    Height = 17
    Caption = 'Sticky bit'
    TabOrder = 11
    OnClick = ControlChange
  end
end
