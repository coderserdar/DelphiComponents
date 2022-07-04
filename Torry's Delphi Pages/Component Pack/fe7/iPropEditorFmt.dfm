object psPropertyEditorFmt: TpsPropertyEditorFmt
  Left = 115
  Top = 20
  Width = 326
  Height = 466
  Caption = 'PSOFT property editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PNL: TPanel
    Left = 0
    Top = 0
    Width = 318
    Height = 22
    Align = alTop
    TabOrder = 0
    object CB_OBJECTS: TComboBox
      Left = 0
      Top = 0
      Width = 270
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object PG: TPageControl
    Left = 0
    Top = 22
    Width = 318
    Height = 398
    ActivePage = SH_PROP
    Align = alClient
    TabOrder = 1
    object SH_PROP: TTabSheet
      Caption = 'Properties'
      object HC: THeaderControl
        Left = 0
        Top = 0
        Width = 310
        Height = 17
        DragReorder = False
        Sections = <
          item
            AllowClick = False
            ImageIndex = -1
            Text = 'Name'
            Width = 150
          end
          item
            AllowClick = False
            ImageIndex = -1
            Text = 'Value'
            Width = 1000
          end>
        Style = hsFlat
        OnSectionResize = HCSectionResize
      end
      object TV: TpsPropTree
        Left = 0
        Top = 17
        Width = 310
        Height = 353
        Align = alClient
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        NameColumnWidth = 100
      end
    end
  end
  object SB: TStatusBar
    Left = 0
    Top = 420
    Width = 318
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 150
      end
      item
        Alignment = taCenter
        Width = 70
      end
      item
        Alignment = taRightJustify
        Width = 50
      end>
    SimplePanel = False
  end
end
