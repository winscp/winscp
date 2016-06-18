object OpenDirectoryDialog: TOpenDirectoryDialog
  Left = 511
  Top = 239
  HelpType = htKeyword
  HelpKeyword = 'ui_opendir'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Open directory'
  ClientHeight = 334
  ClientWidth = 405
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    405
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object EditLabel: TLabel
    Left = 46
    Top = 8
    Width = 76
    Height = 13
    Caption = '&Open directory:'
  end
  object Image: TImage
    Left = 8
    Top = 11
    Width = 32
    Height = 32
    AutoSize = True
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000200000
      00200806000000737A7AF4000000097048597300000EC300000EC301C76FA864
      000004D24944415478DAED967B4C5B551CC7BFF7DEF6B60865942E83CD1658A7
      8950960D90F0C8D0E1A66CEB06E21F46E33460E27FBE6296CC6C23D685A828FE
      6DE25FFBC368F40F37610F35C68263D83D904DC0C9BB0F86E3614BC756E8E3DE
      E301CACAA5B7B031C8FEF1A4CD39F79EDF39DFCFFDFE7EF7C1E061340B5823BF
      FEB18123E33DCC6AECB7D302855DAD3383904A02E4D14DD3E9E90001F1D0FE77
      164C63BADF7DB2C98290C902DEA74AFE9E6159B32008E6070648FF446B660853
      AFD6F04F68D62720519B80B8B838B050000270C7EBC3BF236E4C78BC8384901A
      0AF3B25E6FA0C20C5C371CAFAE1C80DA98CE277FA85473C7749BD7CD0A2B5865
      F8AF98ED95F3634E89C989DBB8DED90D6D5212F2F3F2D1D6FE079C2EFBCA0132
      3ED27DACD228DE4F3268E0F706E19F0C8288044A951249DA44A43E9A02B52A4E
      02439D02AF5041C9F1B8DC761976D78014C06AC9504F2A042BC014CACB129B26
      C49556F1DE67143CD7A84AE4599F7BDACD10F285C8E002C7706382480C74D3E7
      388EAD7A3C6B8B5AAFD7537185C49D1918DB958B1874F64B011A6A0D9F994A5F
      3C642C3C2C2B3F60AB4397F5BBFAB7B93B2F703C6B1402C20F2195F0FAD07BB7
      DC51B551A7CB6444723A2B3BD368CCD82C05A02969BDD48A01675F04E0D4717D
      813A5EDD5A7AF069968FE3650102530158BF6A160F073CBF4C80B0097EB7B9CB
      82805CEC4CB5DFE1931B5237A59615E4E7DF0598AF8B964B2DE877F446001A6A
      D33AF2CAB6666FDAB261C9DC0FF78FA2EDA78E2563FC20F8929D864F9F82DC9C
      DCD92B5E5C94CDB6A6280072A03A6FA53529696F583B719DA890B37DFB6CC1CD
      8947EA80655834D9ACD100FB5F32AE0AC0BB179D68704E2C17E623225B220578
      5EBB2A00B1DAE9531E941F734A0A5F02B0EFD9B9C3B6AB0423636BCA32D3DA29
      4CAE04A0ECA92042F4F179BE5D8BB2779AD64E9A0470E6D31DD3FB8FD8E32400
      BB8B43F078198CB1C5D8B6F72D60FA1BB9D5F44722FDC271D49C181ECFAC13EF
      CE794626D1D2D079853A902F01D8592060E8268384CCD760306503C1F3F729B4
      54CCFC5884FDEF3174B43A4E50806A0940491E41F72083CC0347E9CBC50B847A
      1F4828D65CFB6F0E0C0DBAABCB8FBA4E48008AB601D7BA8192EA3AF0DC65FA3A
      753F9050743F9786E633FDF0BAA7F22B8EB9AE48009E3431B8DAA3C09E37EBC1
      F84E62F685BE287FCB0A2DE392288838F76D6F283EC0684A2DF669094066068B
      31C180A2CA83C09435C6D5227C2CDE436D44BB746BC28FE673437FD1FC9B66B6
      9200185258A8F4B9C82CA0B9F0FF791FD51E3B1D6451FC0DFB6DB4DBC6BF2EAF
      71BD120590AC61612CDE858D692A7A07DC90CDDF9C03B1F34EE4F2BE20A6EB9A
      17037DBE4315471D9F4701A879068515E5D0C48F528727EF21EF629849BC67B7
      5A9B3D708F877695D7387E8D0260E9D19EAA4A70C11E1A2CC8DA4A96145A5023
      318AF1C7D31360A705DD1ECB903B0A609DF611E41499204EB9C20FB0F9C590F6
      8BCFCBC620221E7E820683226C977C4E9AFFF4795D09C0669309A61DBBA9A353
      0B72BD686312D9502A42225A58E84424D673D3850B3FB79CA577805916206BAB
      0EBA64FA3D4F428B84620048C672805297FE1915D037287E40018ECB029454EC
      C53A5D6A4C0BE52D8F01B4707D38B6BDA505C3F651F3811AC7D92880C6DAB461
      1AB7116BDBFC8A10BF619FA5EF5614C0C36AFF03FC07D24654E8F935DA1B0000
      000049454E44AE426082}
  end
  object LocalDirectoryEdit: TIEComboBox
    Left = 46
    Top = 25
    Width = 270
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'LocalDirectoryEdit'
    OnChange = DirectoryEditChange
  end
  object RemoteDirectoryEdit: TIEComboBox
    Left = 46
    Top = 25
    Width = 351
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 1000
    TabOrder = 0
    Text = 'RemoteDirectoryEdit'
    OnChange = DirectoryEditChange
  end
  object OKBtn: TButton
    Left = 159
    Top = 300
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object CancelBtn: TButton
    Left = 239
    Top = 300
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object PageControl: TPageControl
    Left = 8
    Top = 56
    Width = 386
    Height = 235
    ActivePage = SessionBookmarksSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnChange = PageControlChange
    object SessionBookmarksSheet: TTabSheet
      Tag = 1
      Caption = 'Site bookmarks'
      DesignSize = (
        378
        207)
      object SessionBookmarksList: TListBox
        Tag = 1
        Left = 10
        Top = 9
        Width = 268
        Height = 187
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 0
        OnClick = BookmarksListClick
        OnDblClick = BookmarksListDblClick
        OnDragDrop = BookmarksListDragDrop
        OnDragOver = BookmarksListDragOver
        OnEndDrag = BookmarksListEndDrag
        OnKeyDown = BookmarksListKeyDown
        OnStartDrag = BookmarksListStartDrag
      end
      object AddSessionBookmarkButton: TButton
        Tag = 1
        Left = 285
        Top = 9
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Add'
        TabOrder = 1
        OnClick = AddBookmarkButtonClick
      end
      object RemoveSessionBookmarkButton: TButton
        Tag = 1
        Left = 285
        Top = 41
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Remo&ve'
        TabOrder = 2
        OnClick = RemoveBookmarkButtonClick
      end
      object UpSessionBookmarkButton: TButton
        Tag = -1
        Left = 285
        Top = 139
        Width = 83
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Up'
        TabOrder = 3
        OnClick = BookmarkButtonClick
      end
      object DownSessionBookmarkButton: TButton
        Tag = 1
        Left = 285
        Top = 171
        Width = 83
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Down'
        TabOrder = 4
        OnClick = BookmarkButtonClick
      end
    end
    object SharedBookmarksSheet: TTabSheet
      Tag = 2
      Caption = 'Shared bookmarks'
      ImageIndex = 1
      DesignSize = (
        378
        207)
      object SharedBookmarksList: TListBox
        Tag = 2
        Left = 10
        Top = 9
        Width = 268
        Height = 187
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 0
        OnClick = BookmarksListClick
        OnDblClick = BookmarksListDblClick
        OnDragDrop = BookmarksListDragDrop
        OnDragOver = BookmarksListDragOver
        OnEndDrag = BookmarksListEndDrag
        OnKeyDown = BookmarksListKeyDown
        OnStartDrag = BookmarksListStartDrag
      end
      object AddSharedBookmarkButton: TButton
        Tag = 2
        Left = 285
        Top = 9
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Add'
        TabOrder = 1
        OnClick = AddBookmarkButtonClick
      end
      object RemoveSharedBookmarkButton: TButton
        Tag = 2
        Left = 285
        Top = 41
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Remove'
        TabOrder = 2
        OnClick = RemoveBookmarkButtonClick
      end
      object UpSharedBookmarkButton: TButton
        Tag = -2
        Left = 285
        Top = 139
        Width = 83
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Up'
        TabOrder = 4
        OnClick = BookmarkButtonClick
      end
      object DownSharedBookmarkButton: TButton
        Tag = 2
        Left = 285
        Top = 171
        Width = 83
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Down'
        TabOrder = 5
        OnClick = BookmarkButtonClick
      end
      object ShortCutSharedBookmarkButton: TButton
        Tag = 2
        Left = 285
        Top = 73
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Shortcut...'
        TabOrder = 3
        OnClick = ShortCutBookmarkButtonClick
      end
    end
  end
  object LocalDirectoryBrowseButton: TButton
    Left = 320
    Top = 23
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'B&rowse...'
    TabOrder = 2
    OnClick = LocalDirectoryBrowseButtonClick
  end
  object SwitchButton: TButton
    Left = 8
    Top = 300
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Location Profiles...'
    ModalResult = 2
    TabOrder = 4
    OnClick = SwitchButtonClick
  end
  object HelpButton: TButton
    Left = 320
    Top = 300
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = HelpButtonClick
  end
end
