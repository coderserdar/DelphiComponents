object Form1: TForm1
  Left = 352
  Top = 168
  Width = 320
  Height = 209
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object BMSpinEdit1: TBMDSpinEdit
    Left = 16
    Top = 16
    Width = 217
    Height = 28
    Cursor = crArrow
    EditorEnabled = False
    TabOrder = 0
    Increment = 1
    IncrementGauge = 1
    MaxValue = 100
  end
  object BMSpinEdit2: TBMDSpinEdit
    Left = 16
    Top = 72
    Width = 217
    Height = 28
    Cursor = crArrow
    EditorEnabled = False
    TabOrder = 1
    Increment = 1
    MaxValue = 100
    TrackBarOrientation = trVertical
  end
  object BMSpinEdit3: TBMDSpinEdit
    Left = 16
    Top = 128
    Width = 217
    Height = 28
    Cursor = crArrow
    EditorEnabled = False
    TabOrder = 2
    Increment = 1
    IncrementGauge = 1
    MaxValue = 100
    MinValue = -100
    TrackBarOrientation = trVertical
  end
end
