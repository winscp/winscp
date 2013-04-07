object LogForm: TLogForm
  Left = 356
  Top = 178
  HelpType = htKeyword
  HelpKeyword = 'ui_log'
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'Log'
  ClientHeight = 303
  ClientWidth = 451
  Color = clBtnFace
  Constraints.MinHeight = 170
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TTBXStatusBar
    Left = 0
    Top = 284
    Width = 451
    Height = 19
    Panels = <
      item
        Framed = False
        MaxSize = 200
        Size = 200
        StretchPriority = 100
        Tag = 0
      end>
    UseSystemFont = False
  end
  object TopDock: TTBXDock
    Left = 0
    Top = 0
    Width = 451
    Height = 26
    AllowDrag = False
    object Toolbar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Toolbar'
      FullSize = True
      Images = GlyphsModule.LogImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object TBXItem2: TTBXItem
        Action = NonVisualDataModule.LogClearAction
      end
      object TBXItem3: TTBXItem
        Action = NonVisualDataModule.LogCopyAction
      end
      object TBXItem4: TTBXItem
        Action = NonVisualDataModule.LogSelectAllAction
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
      end
      object TBXItem5: TTBXItem
        Action = NonVisualDataModule.LogPreferencesAction
      end
    end
  end
end
