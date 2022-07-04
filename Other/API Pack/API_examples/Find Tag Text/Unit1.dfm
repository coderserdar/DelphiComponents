object Form1: TForm1
  Left = 192
  Top = 114
  Width = 711
  Height = 207
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 128
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Edit1: TEdit
    Left = 24
    Top = 24
    Width = 649
    Height = 21
    TabOrder = 0
    Text = 
      '<html><body bgcolor=#ffffff>testing this shit<img src="test"></b' +
      'ody></html>'
  end
  object Button1: TButton
    Left = 24
    Top = 96
    Width = 225
    Height = 25
    Caption = 'Find Next Tagged Text'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 24
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '1'
  end
end
