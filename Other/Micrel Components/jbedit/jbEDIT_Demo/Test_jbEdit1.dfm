object Form1: TForm1
  Left = 202
  Top = 117
  Width = 363
  Height = 306
  ActiveControl = Button1
  Caption = 'Test jbEdit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 108
    Height = 13
    Caption = 'Rodn'#233' '#269#237'slo s maskou:'
    FocusControl = PubEdit1
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 96
    Height = 13
    Caption = 'Po'#382'adov'#225'no vyplnit:'
    FocusControl = PubEdit2
  end
  object Label4: TLabel
    Left = 24
    Top = 128
    Width = 112
    Height = 13
    Caption = 'Povolen'#233' znaky "A..C":'
  end
  object Label5: TLabel
    Left = 24
    Top = 168
    Width = 98
    Height = 13
    Caption = 'Jm'#233'no s kapitalizac'#237':'
  end
  object Label6: TLabel
    Left = 160
    Top = 16
    Width = 169
    Height = 52
    Alignment = taCenter
    Caption = 
      'Na tla'#269#237'tku je fokus, aby prob'#283'hly'#13#10'operace na edita'#269'n'#237'ch pol'#237'ch' +
      ', je-li'#13#10'program ukon'#269'en kl'#225'vesou ENTER'#13#10'(Default = True).'
  end
  object Label3: TLabel
    Left = 24
    Top = 211
    Width = 143
    Height = 26
    Caption = 'Konverze ze 16-ov'#233' to 10-ov'#233#13#10'a zp'#283't (zad'#225'v'#225'no dekadicky):'
    FocusControl = PubEdit6
  end
  object Label7: TLabel
    Left = 160
    Top = 128
    Width = 169
    Height = 39
    Alignment = taCenter
    AutoSize = False
    Caption = 
      '(c) 1999-2004 Jaro Bene'#353#13#10'JBenes@micrel.cz'#13#10'http://www.micrel.cz' +
      '/delphi/'
  end
  object Label8: TLabel
    Left = 184
    Top = 192
    Width = 114
    Height = 13
    Caption = #268'asov'#253' '#250'daj (hh:mm:ss):'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 85
    Width = 105
    Height = 17
    Hint = 'Enabled/Disabled.'
    Caption = 'V'#253'po'#269'et v'#253'razu:'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object PubEdit1: TPubEdit
    Left = 24
    Top = 21
    Width = 121
    Height = 21
    Color = clWhite
    TabOrder = 0
    ValidChars = '0123456789'
    OnMask = PubEdit1Mask
    OnValidate = PubEdit1Validate
  end
  object Button1: TButton
    Left = 200
    Top = 72
    Width = 89
    Height = 33
    Caption = 'Konec'
    Default = True
    TabOrder = 8
    OnClick = Button1Click
  end
  object PubEdit2: TPubEdit
    Left = 24
    Top = 64
    Width = 121
    Height = 21
    Color = clWhite
    TabOrder = 1
    Required = True
  end
  object PubEdit3: TPubEdit
    Left = 24
    Top = 104
    Width = 121
    Height = 21
    Color = clBtnFace
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '1+2+3+4*4-3*4'
    EditType = teParser
  end
  object PubEdit4: TPubEdit
    Left = 24
    Top = 144
    Width = 121
    Height = 21
    Color = clWhite
    TabOrder = 4
    ValidChars = 'ABC'
  end
  object PubEdit5: TPubEdit
    Left = 24
    Top = 184
    Width = 121
    Height = 21
    Color = clWhite
    TabOrder = 5
    Capitalize = True
  end
  object PubEdit6: TPubEdit
    Left = 24
    Top = 240
    Width = 121
    Height = 21
    Color = clWhite
    TabOrder = 6
    EditType = teInteg
    OnConvert = PubEdit6Convert
  end
  object PubMaskEdit1: TPubMaskEdit
    Left = 184
    Top = 208
    Width = 121
    Height = 21
    EditMask = '!90:00:00;1;_'
    MaxLength = 8
    TabOrder = 7
    Text = '  :  :  '
  end
end
