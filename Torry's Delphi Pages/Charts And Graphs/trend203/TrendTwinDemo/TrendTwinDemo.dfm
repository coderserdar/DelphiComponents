object Form1: TForm1
  Left = 332
  Top = 163
  Width = 448
  Height = 148
  Caption = 'Trend Twin Option Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 23
    Top = 23
    Width = 108
    Height = 75
  end
  object Bevel2: TBevel
    Left = 9
    Top = 11
    Width = 135
    Height = 99
    Style = bsRaised
  end
  object Bevel3: TBevel
    Left = 167
    Top = 23
    Width = 108
    Height = 75
  end
  object Bevel4: TBevel
    Left = 153
    Top = 11
    Width = 135
    Height = 99
    Style = bsRaised
  end
  object Bevel5: TBevel
    Left = 311
    Top = 23
    Width = 108
    Height = 75
  end
  object Bevel6: TBevel
    Left = 297
    Top = 11
    Width = 135
    Height = 99
    Style = bsRaised
  end
  object Trend3: TJBKTrend
    Left = 24
    Top = 24
    Width = 105
    Height = 73
    Caption = 'Trend1'
    Color = 33023
    TabOrder = 0
    MinVal = -100
    Divisions = 30
    Options = [toTwin]
    Style = ts3D
    BackGroundColor = clInfoBk
    GridYstep = 20
  end
  object Trend1: TJBKTrend
    Left = 168
    Top = 24
    Width = 105
    Height = 73
    Caption = 'Trend1'
    Color = 33023
    TabOrder = 1
    MinVal = -100
    Divisions = 30
    Options = [toTwin]
    Style = tsLine
    GridStyle = gsYGrid
    GridXstep = 15
    GridYstep = 40
  end
  object Trend2: TJBKTrend
    Left = 312
    Top = 24
    Width = 105
    Height = 73
    Caption = 'Trend1'
    Color = 12615680
    TabOrder = 2
    MaxVal = 10000
    Divisions = 30
    Options = [toTwin]
    Style = ts3D
    GridStyle = gsNone
  end
  object Timer1: TTimer
    Interval = 330
    OnTimer = Timer1Timer
    Left = 136
    Top = 80
  end
end
