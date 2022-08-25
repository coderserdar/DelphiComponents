object Form1: TForm1
  Left = 221
  Top = 124
  Width = 565
  Height = 370
  Caption = 'IcsRandomInt vs System.Random'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 15
    Top = 17
    Width = 256
    Height = 256
  end
  object Image2: TImage
    Left = 284
    Top = 17
    Width = 256
    Height = 256
  end
  object IcsRandomIntButton32: TButton
    Left = 85
    Top = 293
    Width = 116
    Height = 25
    Caption = 'IcsRandomInt'
    TabOrder = 0
    OnClick = IcsRandomIntButton32Click
  end
  object RtlRandButton: TButton
    Left = 354
    Top = 293
    Width = 116
    Height = 25
    Caption = 'RTL Random'
    TabOrder = 1
    OnClick = RtlRandButtonClick
  end
end
