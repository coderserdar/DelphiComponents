object fce_simple_data: Tfce_simple_data
  Left = 197
  Top = 88
  Width = 500
  Height = 408
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = '  TSivak3SimpleTable, data preview.'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object grid: TStringGrid
    Left = 0
    Top = 0
    Width = 484
    Height = 347
    Align = alClient
    BorderStyle = bsNone
    DefaultColWidth = 48
    DefaultRowHeight = 18
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
    TabOrder = 0
  end
  object Status: TStatusBar
    Left = 0
    Top = 347
    Width = 484
    Height = 19
    Panels = <
      item
        Width = 128
      end
      item
        Width = 50
      end>
  end
end
