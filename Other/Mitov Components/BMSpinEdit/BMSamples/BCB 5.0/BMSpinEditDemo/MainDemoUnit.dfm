object Form1: TForm1
  Left = 444
  Top = 197
  Width = 282
  Height = 186
  Caption = 'BMSpinEdit demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BMSpinEdit1: TBMSpinEdit
    Left = 24
    Top = 16
    Width = 169
    Height = 28
    Cursor = crArrow
    TabStop = True
    EditorEnabled = False
    ParentColor = False
    TabOrder = 0
    Increment = 1
    IncrementGauge = 1
    MaxValue = 100
  end
  object BMSpinEdit2: TBMSpinEdit
    Left = 24
    Top = 64
    Width = 169
    Height = 28
    Cursor = crArrow
    TabStop = True
    EditorEnabled = False
    ParentColor = False
    TabOrder = 1
    Increment = 1
    MaxValue = 100
    TrackBarOrientation = trVertical
  end
  object BMSpinEdit3: TBMSpinEdit
    Left = 24
    Top = 112
    Width = 169
    Height = 28
    Cursor = crArrow
    TabStop = True
    EditorEnabled = False
    ParentColor = False
    TabOrder = 2
    Increment = 1
    IncrementGauge = 1
    MaxValue = 100
    MinValue = -100
    TrackBarOrientation = trVertical
  end
end
