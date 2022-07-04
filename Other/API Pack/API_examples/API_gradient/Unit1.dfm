object Form1: TForm1
  Left = 241
  Top = 402
  Caption = 'Gradient Example'
  ClientHeight = 104
  ClientWidth = 230
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object API_gradient1: TAPI_gradient
    Left = 0
    Top = 0
    Width = 230
    Height = 83
    Align = alClient
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    ColorStart = clBlack
    ColorEnd = clYellow
    NumberofColors = 255
    GradientStyle = gsHorizontal
    AutoHide = False
    ExplicitHeight = 104
  end
  object ComboBox1: TComboBox
    Left = 0
    Top = 83
    Width = 230
    Height = 21
    Align = alBottom
    ItemHeight = 13
    ItemIndex = 2
    TabOrder = 0
    Text = 'Ellipse'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Horizontal'
      'Vertical'
      'Ellipse'
      'HorMiddle'
      'VerMiddle')
    ExplicitLeft = 64
    ExplicitTop = 80
    ExplicitWidth = 113
  end
end
