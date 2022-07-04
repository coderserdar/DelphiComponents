object frmCHShape: TfrmCHShape
  Left = 234
  Top = 231
  Width = 874
  Height = 527
  Caption = 'CH Shape'
  Color = clBtnFace
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
  object CHShape1: TCHShape
    Left = 42
    Top = 28
    Width = 100
    Height = 100
    Shape = stRectangle
    ShapeDirection = sdLeft
  end
  object CHShape2: TCHShape
    Left = 161
    Top = 28
    Width = 100
    Height = 100
    Shape = stRectangleRound
    ShapeDirection = sdLeft
  end
  object CHShape3: TCHShape
    Left = 280
    Top = 28
    Width = 100
    Height = 100
    Shape = stCircle
    ShapeDirection = sdLeft
  end
  object CHShape4: TCHShape
    Left = 406
    Top = 28
    Width = 183
    Height = 100
    Shape = stEllipse
    ShapeDirection = sdLeft
  end
  object CHShape5: TCHShape
    Left = 42
    Top = 147
    Width = 100
    Height = 100
    Brush.Color = clRed
    Shape = stHexagon
    ShapeDirection = sdLeft
  end
  object CHShape6: TCHShape
    Left = 455
    Top = 140
    Width = 100
    Height = 100
    Shape = stOctagon
    ShapeDirection = sdDown
  end
  object CHShape7: TCHShape
    Left = 399
    Top = 294
    Width = 100
    Height = 100
    Brush.Color = clBlue
    Shape = stArrowLarge
    ShapeDirection = sdLeft
  end
  object CHShape8: TCHShape
    Left = 238
    Top = 294
    Width = 100
    Height = 100
    Brush.Color = clBlue
    Shape = stArrowLarge
    ShapeDirection = sdRight
  end
  object CHShape9: TCHShape
    Left = 133
    Top = 350
    Width = 78
    Height = 36
    Pen.Color = clNavy
    Pen.Width = 5
    Shape = stArrowSmall
    ShapeDirection = sdLeft
  end
  object CHShape10: TCHShape
    Left = 322
    Top = 238
    Width = 100
    Height = 100
    Brush.Color = clBlue
    Shape = stArrowLarge
    ShapeDirection = sdDown
  end
  object CHShape11: TCHShape
    Left = 322
    Top = 350
    Width = 100
    Height = 100
    Brush.Color = clBlue
    Shape = stArrowLarge
    ShapeDirection = sdUp
  end
  object CHShape12: TCHShape
    Left = 21
    Top = 350
    Width = 75
    Height = 35
    Pen.Color = clNavy
    Pen.Width = 5
    Shape = stArrowSmall
    ShapeDirection = sdRight
  end
  object CHShape13: TCHShape
    Left = 98
    Top = 280
    Width = 35
    Height = 75
    Pen.Color = clNavy
    Pen.Width = 5
    Shape = stArrowSmall
    ShapeDirection = sdDown
  end
  object CHShape14: TCHShape
    Left = 98
    Top = 378
    Width = 35
    Height = 75
    Pen.Color = clNavy
    Pen.Width = 5
    Shape = stArrowSmall
    ShapeDirection = sdUp
  end
  object CHShape15: TCHShape
    Left = 161
    Top = 147
    Width = 100
    Height = 100
    Brush.Color = clRed
    Shape = stHexagon
    ShapeDirection = sdDown
  end
  object CHShape16: TCHShape
    Left = 609
    Top = 28
    Width = 100
    Height = 100
    Brush.Color = clPurple
    Shape = stTrapezoid
    ShapeDirection = sdLeft
  end
  object CHShape17: TCHShape
    Left = 742
    Top = 35
    Width = 100
    Height = 100
    Brush.Color = clYellow
    Shape = stTriangle
    ShapeDirection = sdLeft
  end
  object CHShape18: TCHShape
    Left = 742
    Top = 147
    Width = 100
    Height = 100
    Brush.Color = clYellow
    Shape = stTriangle
    ShapeDirection = sdRight
  end
  object CHShape19: TCHShape
    Left = 742
    Top = 259
    Width = 100
    Height = 100
    Brush.Color = clYellow
    Shape = stTriangle
    ShapeDirection = sdDown
  end
  object CHShape20: TCHShape
    Left = 742
    Top = 371
    Width = 100
    Height = 100
    Brush.Color = clYellow
    Shape = stTriangle
    ShapeDirection = sdUp
  end
  object CHShape21: TCHShape
    Left = 301
    Top = 175
    Width = 127
    Height = 29
    Pen.Width = 5
    Shape = stLine
    ShapeDirection = sdLeft
  end
  object CHShape22: TCHShape
    Left = 609
    Top = 147
    Width = 100
    Height = 100
    Brush.Color = clPurple
    Shape = stTrapezoid
    ShapeDirection = sdRight
  end
  object CHShape23: TCHShape
    Left = 609
    Top = 266
    Width = 100
    Height = 100
    Brush.Color = clPurple
    Shape = stTrapezoid
    ShapeDirection = sdDown
  end
  object CHShape24: TCHShape
    Left = 609
    Top = 371
    Width = 100
    Height = 100
    Brush.Color = clPurple
    Shape = stTrapezoid
    ShapeDirection = sdUp
  end
  object CHShape25: TCHShape
    Left = 539
    Top = 245
    Width = 29
    Height = 218
    Pen.Width = 5
    Shape = stLine
    ShapeDirection = sdDown
  end
  object MainMenu1: TMainMenu
    Left = 750
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
