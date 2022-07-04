object Form1: TForm1
  Left = 388
  Top = 227
  Width = 429
  Height = 483
  Caption = 'Native Delphi Trend Component Demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 200
    Top = 176
    Width = 209
    Height = 145
    Caption = ' Trend Types '
    TabOrder = 0
  end
  object RadioButton1: TRadioButton
    Left = 216
    Top = 208
    Width = 113
    Height = 17
    Caption = 'Line'
    TabOrder = 1
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 216
    Top = 224
    Width = 113
    Height = 17
    Caption = 'Scatter'
    TabOrder = 2
    OnClick = RadioButton2Click
  end
  object RadioButton3: TRadioButton
    Left = 216
    Top = 240
    Width = 113
    Height = 17
    Caption = '3D'
    TabOrder = 3
    OnClick = RadioButton3Click
  end
  object RadioButton4: TRadioButton
    Left = 216
    Top = 256
    Width = 113
    Height = 17
    Caption = 'Bar'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = RadioButton4Click
  end
  object RadioButton5: TRadioButton
    Left = 216
    Top = 272
    Width = 113
    Height = 17
    Caption = 'Filled Line'
    TabOrder = 5
    OnClick = RadioButton5Click
  end
  object Trend1: TJBKTrend
    Left = 8
    Top = 8
    Width = 401
    Height = 73
    BevelOuter = bvNone
    BevelWidth = 2
    Caption = 'Trend1'
    Color = clRed
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Options = [toValues]
    Style = tsBar
    ColorBase = clLime
    BackGroundColor = clInfoBk
    GridXstep = 50
    GridYstep = 20
    AutoScale = True
  end
  object Trend2: TJBKTrend
    Left = 8
    Top = 88
    Width = 401
    Height = 81
    Caption = 'Trend1'
    Color = 33023
    TabOrder = 7
    Options = []
    Style = tsBar
  end
  object Trend3: TJBKTrend
    Left = 8
    Top = 184
    Width = 177
    Height = 137
    Caption = 'Trend3'
    Color = 65408
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    Options = [toValues, toTransparent, toRotated, toNoZeros]
    Style = tsBar
    GridStyle = gsNone
  end
  object Trend4: TJBKTrend
    Left = 8
    Top = 328
    Width = 49
    Height = 41
    Caption = 'Trend4'
    Color = clBlue
    TabOrder = 9
    Divisions = 9
    Options = []
    Style = tsBar
    GridStyle = gsYGrid
  end
  object Trend5: TJBKTrend
    Left = 16
    Top = 376
    Width = 201
    Height = 33
    Caption = 'Trend5'
    Color = clSilver
    TabOrder = 10
    Options = []
    Style = tsBar
    GridStyle = gsXGrid
  end
  object Trend6: TJBKTrend
    Left = 128
    Top = 328
    Width = 281
    Height = 33
    Caption = 'Trend6'
    Color = clOlive
    TabOrder = 11
    Options = []
    Style = tsBar
  end
  object Trend7: TJBKTrend
    Left = 224
    Top = 368
    Width = 185
    Height = 73
    Caption = 'Trend7'
    Color = 12615680
    TabOrder = 12
    Divisions = 50
    Options = []
    Style = tsBar
  end
  object CheckBox1: TCheckBox
    Left = 216
    Top = 296
    Width = 97
    Height = 17
    Caption = 'Twin'
    TabOrder = 13
    OnClick = CheckBox1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 360
    Top = 272
  end
end
