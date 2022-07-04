object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'API_strings Example'
  ClientHeight = 203
  ClientWidth = 507
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
    Top = 160
    Width = 107
    Height = 13
    Caption = 'Result (Tag found)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 136
    Top = 56
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label3: TLabel
    Left = 24
    Top = 56
    Width = 60
    Height = 13
    Caption = 'Text Length:'
  end
  object Label4: TLabel
    Left = 24
    Top = 96
    Width = 90
    Height = 13
    Caption = 'Next Start Position:'
  end
  object Label5: TLabel
    Left = 24
    Top = 16
    Width = 169
    Height = 13
    Caption = 'Text to Search Tags From (Source):'
  end
  object Label6: TLabel
    Left = 256
    Top = 56
    Width = 85
    Height = 13
    Caption = 'Custom Start Tag:'
  end
  object Label7: TLabel
    Left = 256
    Top = 80
    Width = 82
    Height = 13
    Caption = 'Custom End Tag:'
  end
  object Label8: TLabel
    Left = 24
    Top = 75
    Width = 79
    Height = 13
    Caption = 'Tag Dimensions:'
  end
  object Label9: TLabel
    Left = 136
    Top = 75
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Edit1: TEdit
    Left = 24
    Top = 32
    Width = 457
    Height = 21
    TabOrder = 0
    Text = 
      '<html><head>b'#246#246'</head><><body bgcolor=#ffffff>testing <!-- <b>th' +
      'is</b> --> shit<img src="test"></body></html>'
  end
  object Button1: TButton
    Left = 24
    Top = 128
    Width = 225
    Height = 25
    Caption = 'Find Next Tagged Text'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 136
    Top = 96
    Width = 113
    Height = 21
    TabOrder = 2
    Text = '1'
  end
  object Button2: TButton
    Left = 255
    Top = 128
    Width = 226
    Height = 25
    Caption = 'Find Next Using Custom Tags'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit3: TEdit
    Left = 368
    Top = 56
    Width = 113
    Height = 21
    TabOrder = 4
    Text = '<b>'
  end
  object Edit4: TEdit
    Left = 368
    Top = 80
    Width = 113
    Height = 21
    TabOrder = 5
    Text = '</b>'
  end
  object Button3: TButton
    Left = 406
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 264
    Top = 159
    Width = 136
    Height = 25
    Caption = 'Find From End'
    TabOrder = 7
    OnClick = Button4Click
  end
  object Edit5: TEdit
    Left = 184
    Top = 160
    Width = 74
    Height = 21
    TabOrder = 8
    Text = 'b'#246#246
  end
end
