object Form1: TForm1
  Left = 192
  Top = 55
  AutoScroll = False
  Caption = 'Update Demo'
  ClientHeight = 423
  ClientWidth = 492
  Color = clBtnFace
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 14
  object Plot1: TPlot
    Left = 0
    Top = 0
    Width = 265
    Height = 423
    Align = alClient
    XAxis.Max = 20.000000000000000000
    XAxis.Pen.Color = clBtnText
    XAxis.Font.Charset = DEFAULT_CHARSET
    XAxis.Font.Color = clBtnText
    XAxis.Font.Height = -11
    XAxis.Font.Name = 'Arial'
    XAxis.Font.Style = []
    XAxis.MinorTicks = 2
    XAxis.MajorTicks = 5
    XAxis.Title = 'Time'
    YAxis.Min = -1.000000000000000000
    YAxis.Max = 1.500000000000000000
    YAxis.Pen.Color = clBtnText
    YAxis.Font.Charset = DEFAULT_CHARSET
    YAxis.Font.Color = clBtnText
    YAxis.Font.Height = -11
    YAxis.Font.Name = 'Arial'
    YAxis.Font.Style = []
    YAxis.MinorTicks = 2
    YAxis.MajorTicks = 5
    YAxis.Title = 'Signal'
    XAxis2.Max = 10.000000000000000000
    XAxis2.Font.Charset = DEFAULT_CHARSET
    XAxis2.Font.Color = clWindowText
    XAxis2.Font.Height = -11
    XAxis2.Font.Name = 'MS Sans Serif'
    XAxis2.Font.Style = []
    XAxis2.Visible = False
    YAxis2.Max = 10.000000000000000000
    YAxis2.Font.Charset = DEFAULT_CHARSET
    YAxis2.Font.Color = clWindowText
    YAxis2.Font.Height = -11
    YAxis2.Font.Name = 'MS Sans Serif'
    YAxis2.Font.Style = []
    YAxis2.Visible = False
    Series = <
      item
        PointType = ptCircle
        PointSize = 7
        XColumn = 1
        YColumn = 2
        Pen.Color = clBtnText
        Brush.Color = clBtnHighlight
        Container = Container1
        IsRecording = True
      end>
    SerieIndex = 0
    LeftMargin = 47
    RightMargin = 8
    TopMargin = 7
    BottomMargin = 45
    Labels = <>
  end
  object SpeedButton1: TSpeedButton
    Left = 88
    Top = 8
    Width = 145
    Height = 26
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'Run add points cycle'
    Flat = True
    Glyph.Data = {
      96030000424D9603000000000000760000002800000050000000140000000100
      0400000000002003000000000000000000001000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777777777777777777777777777777
      7777777777177777777777777777778777777777777777777777777777777777
      777777D7777777777777777777D1777777777777777777D87777777777777777
      71777777777777777777771D7777777777777777777D1777777777777777777D
      87777777777777777D1887777777777777777771D777777777777777777D9177
      777777777777777D587777777777777777D1887777777777777777719D777777
      777777777777D9177777777777777777D58777777777777777D9188777777777
      7777777719D77777777777777777D9917777777777777777D558777777777777
      777D91887777777777777777199D77777777777777777D991777777777777777
      7D55877777777777777D991887777777777777777199D77777777777D9999999
      917777777777D55555555877777777777777D991887777777777199999999D77
      777777777D9999999917777777777D55555555877777777D9999999918877777
      77777199999999D7777777777D9999111111777777777D555588888877777777
      D9999999918777777777719999DDDDDD7777777777D9991777777777777777D5
      5587777777777777D9999111111777777777771999D777777777777777D99991
      77777777777777D555587777777777777D9991887777777777777719999D7777
      77777777777D9999177777777777777D55558777777777777D99991887777777
      777777719999D77777777777777D9999917777777777777D5555587777777777
      77D99991887777777777777199999D77777777777777D9999917777777777777
      D55555877777777777D999991887777777777777199999D7777777777777D999
      9991777777777777D555555877777777777D999991887777777777771999999D
      7777777777777DDDDDDDD777777777777DDDDDDDD7777777777D999999187777
      7777777771111111D77777777777777777777777777777777777777777777777
      7777DDDDDDDD7777777777777777777777777777777777777777777777777777
      7777777777777777777777777777777777777777777777777777}
    Margin = 4
    NumGlyphs = 4
  end
  object Splitter1: TSplitter
    Left = 265
    Top = 0
    Height = 423
    Align = alRight
    AutoSnap = False
  end
  object Worksheet1: TWorksheet
    Left = 268
    Top = 0
    Width = 224
    Height = 423
    Align = alRight
    Color = clBtnFace
    ColCount = 27
    DefaultColWidth = 48
    DefaultRowHeight = 18
    FixedColor = clBtnHighlight
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
    Container = Container1
    BlockColorF = clBtnText
    BlockColorB = clBtnHighlight
    AlignRight = False
    DrawHeaders = True
    Header.Strings = (
      'Time'
      'Signal')
    ColWidths = (
      48
      61
      85
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48
      48)
  end
  object CheckBox1: TCheckBox
    Left = 88
    Top = 36
    Width = 145
    Height = 17
    Caption = 'Scroll Worksheet'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object Container1: TContainer
    DataType = dtRealData
    UpdateCaption = False
    AutoLoad = False
    Left = 116
    Top = 68
    Data = {0100000000}
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 88
    Top = 68
  end
end
