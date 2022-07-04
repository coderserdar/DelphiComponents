object Form1: TForm1
  Left = 53
  Top = 89
  Caption = 'OverByte ICS - MD5 checksum test - http://www.overbyte.be'
  ClientHeight = 358
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 61
    Width = 539
    Height = 297
    Align = alBottom
    TabOrder = 0
  end
  object RunButton: TButton
    Left = 16
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Run test'
    TabOrder = 1
    OnClick = RunButtonClick
  end
  object Button1: TButton
    Left = 164
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 260
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
end
