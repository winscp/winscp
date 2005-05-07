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
  inherited GroupReadCheck: TGrayedCheckBox
    TabOrder = 4
  end
  inherited GroupWriteCheck: TGrayedCheckBox
    TabOrder = 5
  end
  inherited GroupExecuteCheck: TGrayedCheckBox
    TabOrder = 6
  end
  inherited OthersReadCheck: TGrayedCheckBox
    TabOrder = 8
  end
  inherited OthersWriteCheck: TGrayedCheckBox
    TabOrder = 9
  end
  inherited OthersExecuteCheck: TGrayedCheckBox
    TabOrder = 10
  end
  inherited DirectoriesXCheck: TCheckBox
    Top = 89
    TabOrder = 13
  end
  object OctalEdit: TEdit [17]
    Left = 55
    Top = 64
    Width = 64
    Height = 21
    MaxLength = 4
    TabOrder = 12
    Text = 'OctalEdit'
    OnChange = OctalEditChange
    OnExit = OctalEditExit
  end
  object SetUidCheck: TGrayedCheckBox [18]
    Tag = 2048
    Left = 169
    Top = 3
    Width = 70
    Height = 17
    Caption = 'Set UID'
    TabOrder = 3
    OnClick = ControlChange
  end
  object SetGIDCheck: TGrayedCheckBox [19]
    Tag = 1024
    Left = 169
    Top = 23
    Width = 70
    Height = 17
    Caption = 'Set GID'
    TabOrder = 7
    OnClick = ControlChange
  end
  object StickyBitCheck: TGrayedCheckBox [20]
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
