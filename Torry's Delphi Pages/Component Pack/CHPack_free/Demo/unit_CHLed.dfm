object frmCHLed: TfrmCHLed
  Left = 333
  Top = 357
  Width = 540
  Height = 395
  Caption = 'CH Led'
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 33
    Height = 13
    Caption = 'Normal'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 80
    Width = 27
    Height = 13
    Caption = 'Clock'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 136
    Width = 97
    Height = 13
    Caption = 'Thousand Seperator'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 320
    Top = 24
    Width = 59
    Height = 13
    Caption = 'Comma Two'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 320
    Top = 136
    Width = 65
    Height = 13
    Caption = 'Custom Mode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 432
    Top = 24
    Width = 58
    Height = 13
    Caption = 'Comma One'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 320
    Top = 80
    Width = 66
    Height = 13
    Caption = 'Comma Three'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 16
    Top = 200
    Width = 23
    Height = 13
    Caption = 'small'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 16
    Top = 262
    Width = 14
    Height = 13
    Caption = 'big'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 184
    Top = 262
    Width = 100
    Height = 13
    Caption = 'more pitch (Segment)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 360
    Top = 262
    Width = 76
    Height = 13
    Caption = 'more pitch (Led)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label12: TLabel
    Left = 184
    Top = 200
    Width = 97
    Height = 13
    Caption = 'Don´t show inactive '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object CHLed1: TCHLed
    Left = 16
    Top = 40
    Width = 227
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmNone
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '0123456789'
  end
  object CHLed2: TCHLed
    Left = 16
    Top = 96
    Width = 149
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmClock
    Led.Mask = 'xx|xx|xx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsColon
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '123356'
  end
  object CHLed3: TCHLed
    Left = 16
    Top = 152
    Width = 142
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmThousandSep
    Led.Mask = 'xxx|xxx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '100345'
  end
  object CHLed4: TCHLed
    Left = 320
    Top = 40
    Width = 73
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCommaTwo
    Led.Mask = 'x|xx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '584'
  end
  object CHLed5: TCHLed
    Left = 320
    Top = 152
    Width = 195
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCustom
    Led.Mask = 'xxxxx|xx|x'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '123356'
  end
  object CHLed6: TCHLed
    Left = 432
    Top = 40
    Width = 50
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCommaOne
    Led.Mask = 'x|x'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '71'
  end
  object CHLed7: TCHLed
    Left = 320
    Top = 96
    Width = 96
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCommaThree
    Led.Mask = 'x|xxx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '2082'
  end
  object CHLed8: TCHLed
    Left = 16
    Top = 218
    Width = 55
    Height = 20
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCommaTwo
    Led.Mask = 'x|xx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 14
    Led.Height = 20
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '584'
  end
  object CHLed9: TCHLed
    Left = 16
    Top = 280
    Width = 133
    Height = 60
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCommaTwo
    Led.Mask = 'x|xx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 40
    Led.Height = 60
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 5
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '584'
  end
  object CHLed10: TCHLed
    Left = 184
    Top = 280
    Width = 133
    Height = 60
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmCommaTwo
    Led.Mask = 'x|xx'
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 40
    Led.Height = 60
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 5
    Segment.Pitch = 4
    Segment.ShowInActive = True
    Value = '584'
  end
  object CHLed11: TCHLed
    Left = 360
    Top = 278
    Width = 160
    Height = 60
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmNone
    Led.Pitch = 20
    Led.Placeholder = 0
    Led.Width = 40
    Led.Height = 60
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 5
    Segment.Pitch = 1
    Segment.ShowInActive = True
    Value = '584'
  end
  object CHLed12: TCHLed
    Left = 184
    Top = 216
    Width = 89
    Height = 30
    Autosize = True
    Alignment = laLeft
    Buffered = False
    Color = clBlack
    Fill.Transparent = False
    Led.Mode = lmNone
    Led.Pitch = 3
    Led.Placeholder = 0
    Led.Width = 20
    Led.Height = 30
    Seperator.Seperator = lsPoint
    Seperator.Width = 10
    Segment.ActiveColor = clLime
    Segment.InActiveColor = clGreen
    Segment.Width = 3
    Segment.Pitch = 1
    Segment.ShowInActive = False
    Value = '2847'
  end
  object MainMenu1: TMainMenu
    Left = 494
    Top = 2
    object close1: TMenuItem
      Caption = 'Close'
      OnClick = close1Click
    end
    object Info1: TMenuItem
      Caption = 'Info'
      OnClick = Info1Click
    end
  end
end
