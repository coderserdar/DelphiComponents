object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 704
  ClientWidth = 763
  Color = 15986666
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ALButton1: TALButton
    Left = 16
    Top = 16
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStringReplace (AnsiString)'
    TabOrder = 0
    OnClick = ALButton1Click
    OnPaint = ALButtonPaint
  end
  object ALButton2: TALButton
    Left = 266
    Top = 16
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StringReplace (Unicode)'
    TabOrder = 1
    OnClick = ALButton2Click
    OnPaint = ALButtonPaint
  end
  object ALButton3: TALButton
    Left = 16
    Top = 48
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPosEx (AnsiString)'
    TabOrder = 2
    OnClick = ALButton3Click
    OnPaint = ALButtonPaint
  end
  object ALButton4: TALButton
    Left = 266
    Top = 48
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi PosEx (Unicode)'
    TabOrder = 3
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
  object ALButton5: TALButton
    Left = 16
    Top = 80
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPos (AnsiString)'
    TabOrder = 4
    OnClick = ALButton5Click
    OnPaint = ALButtonPaint
  end
  object ALButton6: TALButton
    Left = 266
    Top = 80
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Pos (Unicode)'
    TabOrder = 5
    OnClick = ALButton6Click
    OnPaint = ALButtonPaint
  end
  object ALButton7: TALButton
    Left = 16
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark ALCompareText (AnsiString)'
    TabOrder = 6
    OnClick = ALButton7Click
    OnPaint = ALButtonPaint
  end
  object ALButton8: TALButton
    Left = 266
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi CompareText (Unicode)'
    TabOrder = 7
    OnClick = ALButton8Click
    OnPaint = ALButtonPaint
  end
  object ALButton9: TALButton
    Left = 16
    Top = 144
    Width = 230
    Height = 25
    Caption = 'Benchmark ALUpperCase (AnsiString)'
    TabOrder = 8
    OnClick = ALButton9Click
    OnPaint = ALButtonPaint
  end
  object ALButton10: TALButton
    Left = 266
    Top = 145
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi UpperCase (Unicode)'
    TabOrder = 9
    OnClick = ALButton10Click
    OnPaint = ALButtonPaint
  end
  object ALButton11: TALButton
    Left = 16
    Top = 176
    Width = 230
    Height = 25
    Caption = 'Benchmark ALLowerCase (AnsiString)'
    TabOrder = 10
    OnClick = ALButton11Click
    OnPaint = ALButtonPaint
  end
  object ALButton12: TALButton
    Left = 266
    Top = 176
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi LowerCase (Unicode)'
    TabOrder = 11
    OnClick = ALButton12Click
    OnPaint = ALButtonPaint
  end
  object ALButton13: TALButton
    Left = 16
    Top = 208
    Width = 230
    Height = 25
    Caption = 'Benchmark ALCopyStr (AnsiString)'
    TabOrder = 12
    OnClick = ALButton13Click
    OnPaint = ALButtonPaint
  end
  object ALButton14: TALButton
    Left = 266
    Top = 208
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Copy (Unicode)'
    TabOrder = 13
    OnClick = ALButton14Click
    OnPaint = ALButtonPaint
  end
  object Panel2: TPanel
    Left = 16
    Top = 537
    Width = 345
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 14
    object Label7: TLabel
      Left = 10
      Top = 14
      Width = 152
      Height = 45
      Caption = 'Please help us to keep the development of these components free'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label8: TLabel
      Left = 10
      Top = 77
      Width = 168
      Height = 60
      Caption = 
        'If you like these components please simply click on each button ' +
        'below ... thanks for your support !'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Panel3: TPanel
      Left = 201
      Top = 8
      Width = 130
      Height = 134
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Color = clWhite
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 0
      object PanelWebBrowser: TPanel
        Left = -5
        Top = -23
        Width = 133
        Height = 159
        BevelOuter = bvNone
        Color = clMedGray
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 0
      end
    end
  end
  object ALButton15: TALButton
    Left = 16
    Top = 240
    Width = 230
    Height = 25
    Caption = 'Benchmark ALIntToStr (AnsiString)'
    TabOrder = 15
    OnClick = ALButton15Click
    OnPaint = ALButtonPaint
  end
  object ALButton16: TALButton
    Left = 266
    Top = 240
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi IntToStr (Unicode)'
    TabOrder = 16
    OnClick = ALButton16Click
    OnPaint = ALButtonPaint
  end
  object ALButton17: TALButton
    Left = 16
    Top = 272
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToInt (AnsiString)'
    TabOrder = 17
    OnClick = ALButton17Click
    OnPaint = ALButtonPaint
  end
  object ALButton18: TALButton
    Left = 266
    Top = 272
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt (Unicode)'
    TabOrder = 18
    OnClick = ALButton18Click
    OnPaint = ALButtonPaint
  end
  object ALButton19: TALButton
    Left = 16
    Top = 304
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToInt64 (AnsiString)'
    TabOrder = 19
    OnClick = ALButton19Click
    OnPaint = ALButtonPaint
  end
  object ALButton20: TALButton
    Left = 266
    Top = 304
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt64 (Unicode)'
    TabOrder = 20
    OnClick = ALButton20Click
    OnPaint = ALButtonPaint
  end
  object ALButton21: TALButton
    Left = 16
    Top = 336
    Width = 230
    Height = 25
    Caption = 'Benchmark ALDateToStr (AnsiString)'
    TabOrder = 21
    OnClick = ALButton21Click
    OnPaint = ALButtonPaint
  end
  object ALButton22: TALButton
    Left = 266
    Top = 336
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi DateToStr (Unicode)'
    TabOrder = 22
    OnClick = ALButton22Click
    OnPaint = ALButtonPaint
  end
  object ALButton23: TALButton
    Left = 16
    Top = 496
    Width = 230
    Height = 25
    Caption = 'Benchmark AnsiString Memory Usage'
    TabOrder = 23
    OnClick = ALButton23Click
    OnPaint = ALButtonPaint
  end
  object ALButton24: TALButton
    Left = 266
    Top = 496
    Width = 230
    Height = 25
    Caption = 'Benchmark UnicodeString Memory Usage'
    TabOrder = 24
    OnClick = ALButton24Click
    OnPaint = ALButtonPaint
  end
  object ALButton25: TALButton
    Left = 16
    Top = 400
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFloatToStr (AnsiString)'
    TabOrder = 25
    OnClick = ALButton25Click
    OnPaint = ALButtonPaint
  end
  object ALButton26: TALButton
    Left = 266
    Top = 400
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi FloatToStr (Unicode)'
    TabOrder = 26
    OnClick = ALButton26Click
    OnPaint = ALButtonPaint
  end
  object ALButton27: TALButton
    Left = 16
    Top = 368
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToDateTime (AnsiString)'
    TabOrder = 27
    OnClick = ALButton27Click
    OnPaint = ALButtonPaint
  end
  object ALButton28: TALButton
    Left = 266
    Top = 368
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToDateTime (Unicode)'
    TabOrder = 28
    OnClick = ALButton28Click
    OnPaint = ALButtonPaint
  end
  object ALButton29: TALButton
    Left = 516
    Top = 16
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StringReplace (AnsiString)'
    TabOrder = 29
    OnClick = ALButton29Click
    OnPaint = ALButtonPaint
  end
  object ALButton30: TALButton
    Left = 516
    Top = 48
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi PosEx (AnsiString)'
    TabOrder = 30
    OnClick = ALButton30Click
    OnPaint = ALButtonPaint
  end
  object ALButton31: TALButton
    Left = 516
    Top = 80
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Pos (AnsiString)'
    TabOrder = 31
    OnClick = ALButton31Click
    OnPaint = ALButtonPaint
  end
  object ALButton32: TALButton
    Left = 516
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi CompareText (AnsiString)'
    TabOrder = 32
    OnClick = ALButton32Click
    OnPaint = ALButtonPaint
  end
  object ALButton33: TALButton
    Left = 516
    Top = 145
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi UpperCase (AnsiString)'
    TabOrder = 33
    OnClick = ALButton33Click
    OnPaint = ALButtonPaint
  end
  object ALButton34: TALButton
    Left = 516
    Top = 176
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi LowerCase (AnsiString)'
    TabOrder = 34
    OnClick = ALButton34Click
    OnPaint = ALButtonPaint
  end
  object ALButton35: TALButton
    Left = 516
    Top = 208
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Copy (AnsiString)'
    TabOrder = 35
    OnClick = ALButton35Click
    OnPaint = ALButtonPaint
  end
  object ALButton36: TALButton
    Left = 516
    Top = 240
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi IntToStr (AnsiString)'
    TabOrder = 36
    OnClick = ALButton36Click
    OnPaint = ALButtonPaint
  end
  object ALButton37: TALButton
    Left = 516
    Top = 272
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt (AnsiString)'
    TabOrder = 37
    OnClick = ALButton37Click
    OnPaint = ALButtonPaint
  end
  object ALButton38: TALButton
    Left = 516
    Top = 304
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt64 (AnsiString)'
    TabOrder = 38
    OnClick = ALButton38Click
    OnPaint = ALButtonPaint
  end
  object ALButton39: TALButton
    Left = 516
    Top = 336
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi DateToStr (AnsiString)'
    TabOrder = 39
    OnClick = ALButton39Click
    OnPaint = ALButtonPaint
  end
  object ALButton41: TALButton
    Left = 516
    Top = 400
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi FloatToStr (AnsiString)'
    TabOrder = 40
    OnClick = ALButton41Click
    OnPaint = ALButtonPaint
  end
  object ALButton42: TALButton
    Left = 516
    Top = 368
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToDateTime (AnsiString)'
    TabOrder = 41
    OnClick = ALButton42Click
    OnPaint = ALButtonPaint
  end
  object ALButton40: TALButton
    Left = 16
    Top = 432
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFormat (AnsiString)'
    TabOrder = 42
    OnClick = ALButton40Click
    OnPaint = ALButtonPaint
  end
  object ALButton43: TALButton
    Left = 266
    Top = 432
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Format (Unicode)'
    TabOrder = 43
    OnClick = ALButton43Click
    OnPaint = ALButtonPaint
  end
  object ALButton44: TALButton
    Left = 516
    Top = 432
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Format (AnsiString)'
    TabOrder = 44
    OnClick = ALButton44Click
    OnPaint = ALButtonPaint
  end
  object ALButton45: TALButton
    Left = 16
    Top = 464
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToFloat (AnsiString)'
    TabOrder = 45
    OnClick = ALButton45Click
    OnPaint = ALButtonPaint
  end
  object ALButton46: TALButton
    Left = 266
    Top = 464
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToFloat (Unicode)'
    TabOrder = 46
    OnClick = ALButton46Click
    OnPaint = ALButtonPaint
  end
  object ALButton47: TALButton
    Left = 516
    Top = 464
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToFloat (AnsiString)'
    TabOrder = 47
    OnClick = ALButton47Click
    OnPaint = ALButtonPaint
  end
end
