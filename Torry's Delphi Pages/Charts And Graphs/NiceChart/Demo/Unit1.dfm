object Form1: TForm1
  Left = 169
  Top = 68
  Width = 778
  Height = 596
  Caption = 'NiceChart Demo - priyatna.org'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 17
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 8
    Top = 9
    Width = 105
    Height = 25
    Caption = 'Monochrome'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 9
    Width = 105
    Height = 25
    Caption = 'Save to WMF'
    TabOrder = 1
    OnClick = Button2Click
  end
end
