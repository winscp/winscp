object SynchronizeDialog: TSynchronizeDialog
  Left = 367
  Top = 198
  HelpType = htKeyword
  HelpKeyword = 'ui_keepuptodate'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Keep remote directory up to date X'
  ClientHeight = 445
  ClientWidth = 436
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    436
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoriesGroup: TGroupBox
    Left = 8
    Top = 6
    Width = 421
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directories'
    TabOrder = 0
    DesignSize = (
      421
      119)
    object LocalDirectoryLabel: TLabel
      Left = 49
      Top = 19
      Width = 195
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Watch for changes in the local directory:'
      FocusControl = LocalDirectoryEdit
    end
    object RemoteDirectoryLabel: TLabel
      Left = 49
      Top = 68
      Width = 281
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = '... &and automatically reflect them on the remote directory:'
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
        000005024944415478DAC5976B4C1C5514C7FF7766DF0FBA0B4B79B680B6D55A
        62B5BC9A16D2C646692B85B6980A4DD4DA686B7C3469D40F881F34A9C8071322
        D198DA8606B5A1EF8AC5A05F4CB10D228FF8A2481F140A6D0ABBB82CECC2B2AF
        19EF4CD9856597B2BBD47836937B66E6CE3DBF7BEE99FB9F25F89F8D789DFA8F
        967C4678F2C6EC0E3CE13F2F2A1F78F33F05F8AE2279836E71EAC5BC3D27E915
        E98CE82E5CFEAA142383373716BE77BBE941045C56A189F5F02AA6B7DC38340D
        7068E9959C82271F5B9C620878C0786B18BF36FC36E78063E0F13B71A39DB8AD
        7DC463A697E2E8314A8F5EC2F33F313CEA7ACA473A85BE2995D1EB58C2FCE8E1
        38CBAD32F3929900FCB6175685359309B707D59D77507BDD08995E812883064A
        B50A4A8512842370D9DDB05AC63174C7C8391DCEB384C729C232C7D666E7689A
        5B7E415F9999F8018413DC42677D84B1C36C90C190AA835CAE808491424A0F09
        23117DEF39437F77FAEFA2E7C64D64656622392119A7CF9F0904D856D61E5270
        9BD38682DA9D18559B2155483169718AB3250C8142A980213E06B1B10648D969
        10014AC88A42A614CF4F9C3B1104E0DD4660F0ED79015E69BA86669B1D1E1787
        471432942C8BC54A9D0A177FE845353BF13A38B247ABD364A7AF5E05AD5A2B06
        97B2523F98E3678E070138F80D05F8E0BEC15B8C36EC6EEA814C42F0E1134978
        3E2DC677EF42DD0D14BEDF2F8E975A117340AE9455AD5BBF96D1A8B47E4B23F8
        B5A76B8300BC55057EA8EABE003D56075E6D1EC0BE15312849D3FBDD6B38D5EF
        0310AB9D61BECFCECAD225C425CCAA0B096A4ED6040178AD0CBCA926AC37C10F
        E0EC9008F050A53E17846DCCC85CA3498A4FF44BBD307B96B6474F1C090428D8
        BB17309F8F1CE0DB111C60C7D21889B4332B33431DBF382E20B8E00B161C60F7
        16BAABFC1C3940C3B80800B0AD740B8D9DAF7F2040F16ABABB74450ED0E8F2D5
        4038E603D8901B07CE618A18E0521BB7308082D2A2B01EEC6CFF137DD77B03AE
        87AB9E5E8011DAE8C201D027A622F7C5BA05AB67D8299B02FE9BAAE7A391A8E7
        8CC81D85E5FD99910284AD9EB3EDC2D757C49AF1011C8ED286A288268E61B393
        DE89EA0DA1EFBC1600B0635781E8F32E37E07289ADE8BBDD300D9BD1D6DD33EE
        044917004255CF3933F0716670009E7E6888C16950DE792FF8A07118EDDD3D36
        8EE3B7ECB75A2F87A39E732F41571080E2AD74C62EDFAC85D668FA07AD5DD72D
        3CC73FBBCF6A6D16FA0653CFEE7326FC513B88FC4F974397AA981F604A3DFD00
        B66FCF170130957AC19F1CB7E352E7556ED2E53EB87F6CACDA0730433DAFD69B
        D152791B6E178FAD871F464286665E00AF7AFA03146CF25B776F36EC14A2A36F
        00A3F6C956FAFA1C8D2ED17E99F7DC4B186E3B43835B30D9ED80820EA55CAF42
        FE27C9212D81573DFD008A366F1403BA1D0EB01C37558CF780380A33343A86BB
        D6714C08603C0F3561104B87B07B3C184A60B0E35812646A263400AA9E81004F
        E76170D0848EAE6B589E188FB4683DFD96F3F8BF11AEE9CC389C2E5CA5B0EE15
        123C53110395810DB90805F50C00C8797C25DAFEEAB679C0BD4C7866975CCA16
        2F5914C518E432A86857A987C3A4C309ABD309230D6C241EACDCA9C49A3D6A48
        E4E1ED695EF59C06D06A071886E868B5E77BABFD0B8D269D21A49427788A76A4
        5A0FE13BECAE3C459292BE390649194E28F4A1A57CB679D5D3077044AD8E6309
        E1F6DA6CF36A7224EA199081BA7A7F8070ACFED0520B7D70D18208E8FF1B0AA0
        8F08E041DABFD644903F11991EFB0000000049454E44AE426082}
    end
    object RemoteDirectoryEdit: THistoryComboBox
      Left = 49
      Top = 84
      Width = 361
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
      Width = 280
      Height = 21
      AutoComplete = False
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 1000
      TabOrder = 0
      Text = 'LocalDirectoryEdit'
      OnChange = ControlChange
    end
    object LocalDirectoryBrowseButton: TButton
      Left = 335
      Top = 33
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'B&rowse...'
      TabOrder = 1
      OnClick = LocalDirectoryBrowseButtonClick
    end
  end
  object StopButton: TButton
    Left = 192
    Top = 312
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Stop'
    TabOrder = 4
    OnClick = StopButtonClick
  end
  object CancelButton: TButton
    Left = 272
    Top = 312
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 6
  end
  object OptionsGroup: TGroupBox
    Left = 8
    Top = 130
    Width = 421
    Height = 119
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Synchronize options'
    TabOrder = 1
    DesignSize = (
      421
      119)
    object SynchronizeDeleteCheck: TCheckBox
      Left = 11
      Top = 20
      Width = 196
      Height = 17
      Caption = '&Delete files'
      TabOrder = 0
      OnClick = ControlChange
    end
    object SaveSettingsCheck: TCheckBox
      Left = 11
      Top = 92
      Width = 196
      Height = 17
      Caption = 'Use same &options next time'
      TabOrder = 6
      OnClick = ControlChange
    end
    object SynchronizeExistingOnlyCheck: TCheckBox
      Left = 213
      Top = 20
      Width = 197
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Existing files only'
      TabOrder = 1
      OnClick = ControlChange
    end
    object SynchronizeRecursiveCheck: TCheckBox
      Left = 11
      Top = 44
      Width = 196
      Height = 17
      Caption = 'Update s&ubdirectories'
      TabOrder = 2
      OnClick = ControlChange
    end
    object SynchronizeSynchronizeCheck: TGrayedCheckBox
      Left = 213
      Top = 68
      Width = 197
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Synchronize on s&tart'
      TabOrder = 5
      OnClick = ControlChange
    end
    object SynchronizeSelectedOnlyCheck: TCheckBox
      Left = 213
      Top = 44
      Width = 197
      Height = 17
      Caption = 'Selected files o&nly'
      TabOrder = 3
      OnClick = ControlChange
    end
    object ContinueOnErrorCheck: TCheckBox
      Left = 11
      Top = 68
      Width = 196
      Height = 17
      Caption = 'Continue on &error'
      TabOrder = 4
      OnClick = ControlChange
    end
  end
  object StartButton: TButton
    Left = 192
    Top = 312
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Start'
    Default = True
    TabOrder = 3
    OnClick = StartButtonClick
  end
  object MinimizeButton: TButton
    Left = 273
    Top = 312
    Width = 74
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Minimize'
    TabOrder = 5
    OnClick = MinimizeButtonClick
  end
  object TransferSettingsButton: TButton
    Left = 8
    Top = 312
    Width = 161
    Height = 25
    Caption = 'Transfer settin&gs...'
    TabOrder = 2
    OnClick = TransferSettingsButtonClick
    OnDropDownClick = TransferSettingsButtonDropDownClick
  end
  object CopyParamGroup: TGroupBox
    Left = 8
    Top = 254
    Width = 421
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Transfer settings'
    TabOrder = 7
    OnClick = CopyParamGroupClick
    OnContextPopup = CopyParamGroupContextPopup
    DesignSize = (
      421
      50)
    object CopyParamLabel: TLabel
      Left = 7
      Top = 15
      Width = 407
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'CopyParamLabel'
      WordWrap = True
      OnClick = CopyParamGroupClick
    end
  end
  object HelpButton: TButton
    Left = 353
    Top = 312
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 8
    OnClick = HelpButtonClick
  end
  object LogPanel: TPanel
    Left = 0
    Top = 345
    Width = 436
    Height = 100
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    DesignSize = (
      436
      100)
    object LogView: TListView
      Left = 8
      Top = 2
      Width = 420
      Height = 90
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Width = -1
          WidthType = (
            -1)
        end
        item
          Width = -1
          WidthType = (
            -1)
        end>
      DoubleBuffered = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnCustomDrawItem = LogViewCustomDrawItem
      OnDblClick = LogViewDblClick
      OnDeletion = LogViewDeletion
      OnKeyDown = LogViewKeyDown
    end
  end
end
