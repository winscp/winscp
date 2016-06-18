object FullSynchronizeDialog: TFullSynchronizeDialog
  Left = 365
  Top = 185
  HelpType = htKeyword
  HelpKeyword = 'ui_synchronize'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Synchronize'
  ClientHeight = 429
  ClientWidth = 481
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    481
    429)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 465
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      465
      119)
    object LocalDirectoryLabel: TLabel
      Left = 49
      Top = 19
      Width = 74
      Height = 13
      Caption = 'Lo&cal directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 49
      Top = 68
      Width = 87
      Height = 13
      Caption = 'R&emote directory:'
      FocusControl = RemoteDirectoryEdit
    end
    object Image: TImage
      Left = 11
      Top = 22
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
        00200806000000737A7AF4000000097048597300000EC300000EC301C76FA864
        000004644944415478DAC5967B6C53551CC7BFB75D6F5BBA479D6C80B2510662
        C2D0B8D52862A26C3190986D3E1018C448DC80A8114532C42DD3346609319068
        E23F12F610354498A05234A2C669A6A38A5BC15912B375CF6ED9CAD695B66B7B
        6F7BEFF1AC2D48D7DB76D32E9EA4B9F79EF3EDEFFBE9F73C6E19FCCF8DB971D3
        66D0A9DC69421BED5A2F2D25A68CA0BCA4C430E05F1080730D79470A4BB6D514
        AC3F2429EC33BD034BDBE9A315F5C307530EF0C5DBCB1F5469541D25CF3E2A63
        D5ACA490F7F168FBE427D13FEDDFF0E45BB65F530A70AE21BF5BBFF99E7577AC
        CA4D281EB5DAD179A1FB5F5A11134DEFA17800A4FC797DAA7E946433B674A2A2
        7E8899DD7F13E0B1B2E50B0AF0FD795B628005758FB48400E5B5BF2FA8B9F1F0
        FD49005EFF0A701F8D1E25B38391088A488517DB673CD69E04E0E06714E0FD39
        170C7725D1927F6E8CC73B9200D47C0CB88ECDB9E07C75C64653128003D4DC7D
        629EC644E2511ACED8742909C0FE7701CF2909E3F9FF5A29ADB139C93950FE6A
        0305F872CE05A38749529DF14373128097EB80E90B732E28A5EBBAE6C6D9BE71
        5CB2BBE0F007206318ACD12EC2A6BC6C682EDAF15A9A679D1CCC0F8410E740AD
        E3EE688097F603DE1F257C936F456F208843262BBE197342BB340399D999502B
        5550C859701E1E93E353F08F4FC20D622F2E2ACEEDECEA040560A2015ED80DF8
        7E8B631A2715AAF306053CFDED558CB004594B32403840C92A91A5CDA2106AB0
        7225FDB09876F9C0FB7914E417E0F4E7AD12007B7750803F12CF7BD463B86FDF
        2F567C37E5093DE6100685B72D82930FA26BD28B652B9661F55DABA04C538520
        14B230CCC933272500AACAE94BBF37BEB1C4E2EC71F9B1E96B0BD834191AF479
        D8A2BB1DB28866CC17C0BE8E018CA9B350745F5104800D25F251EB8958808D8F
        3F00D1674BB008635309D02938629D803E538DCD39E951A3562F8FEAEE51E8D6
        AEC5CA150537A76206A2F9549344022F1E0025908C395E2A240EA78BE7507AB6
        19F96B564397BF326C1C01903372347E7A3C16E091D23C10914FBCE824876275
        D783222ACD2318E382714BC40094EDD99B30EE581F828296F7E27EE3CEF44C18
        2B76404BB7E34C3BDFD894F02072D14B06E6D95E917BB0BB720F042280177804
        042E741DB40DA2E77237DE0CA8A08EFCF126209E27EA87633C98F99ADEDA7487
        B349F5F66AF074EA668C790AD03FD8872B57FE9C10206E1C7AC36199A5DF4E4F
        C187D3F9A91A8B017C4A009EDBBA2B641C8840982F9B313C3CF2B3203095B6FA
        C99190D000998ECDDEC5C8990F88485891A070A8CE713525003BB7EC0CC72F86
        E3E7821C7A7B7A611B180D8A44686708E3A0CB47AFD4287414009C8B3FD35FEB
        78266553B0EDA9ADE81FEA8742A580869E0737A6C2CFFBE09CBA0E8E6E494649
        77C6B81B1EBBAF5BC3C937580CD73C2903D017EBD165364FD06526E62C599CBB
        786936588D82261280CFEF83CBE18273CC8D2027B452F3AA5BCD5301F017C330
        5A01A434C32F1F9C66C52A0A5246ABDE4B97BD825EEDF4F5D00E112D83758E8B
        5235FE13402ADADF2739523FC86AC9150000000049454E44AE426082}
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 84
      Width = 405
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 2
      Text = 'RemoteDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 35
      Width = 322
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 378
      Top = 33
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Bro&wse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object OkButton: TButton
    Left = 233
    Top = 396
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object CancelButton: TButton
    Left = 315
    Top = 396
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 238
    Width = 303
    Height = 73
    Caption = 'Synchronize options'
    TabOrder = 3
    DesignSize = (
      303
      73)
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 130
      Height = 17
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 155
      Top = 44
      Width = 141
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Selected files o&nly'
      TabOrder = 3
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 155
      Top = 20
      Width = 141
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Existing files only'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizePreviewChangesCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 130
      Height = 17
      Caption = 'Pre&view changes'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 396
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Transfer settin&gs...'
    TabOrder = 7
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object DirectionGroup: TGroupBox
    Left = 8
    Top = 130
    Width = 465
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Direction/Target directory'
    TabOrder = 1
    object SynchronizeBothButton: TRadioButton
      Left = 11
      Top = 20
      Width = 138
      Height = 17
      Caption = '&Both'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeRemoteButton: TRadioButton
      Left = 155
      Top = 20
      Width = 143
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeLocalButton: TRadioButton
      Left = 304
      Top = 20
      Width = 154
      Height = 17
      Caption = '&Local'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
  object CompareCriterionsGroup: TGroupBox
    Left = 317
    Top = 238
    Width = 156
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Comparison criteria'
    TabOrder = 4
    DesignSize = (
      156
      73)
    object SynchronizeByTimeCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 138
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'M&odification time'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SynchronizeBySizeCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 138
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File si&ze'
      TabOrder = 1
      OnClick = ControlChange
    end
  end
  object SaveSettingsCheck: TCheckBox
    Left = 19
    Top = 317
    Width = 454
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Use &same options next time'
    TabOrder = 5
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 338
    Width = 465
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 6
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      465
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 451
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object HelpButton: TButton
    Left = 397
    Top = 396
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 10
    OnClick = HelpButtonClick
  end
  object ModeGroup: TGroupBox
    Left = 8
    Top = 184
    Width = 465
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Mode'
    TabOrder = 2
    object SynchronizeFilesButton: TRadioButton
      Left = 11
      Top = 20
      Width = 135
      Height = 17
      Caption = 'Synchronize &files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object MirrorFilesButton: TRadioButton
      Left = 155
      Top = 20
      Width = 143
      Height = 17
      Caption = '&Mirror files'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeTimestampsButton: TRadioButton
      Left = 304
      Top = 20
      Width = 154
      Height = 17
      Caption = 'Synchronize &timestamps'
      TabOrder = 2
      OnClick = ControlChange
    end
  end
end
