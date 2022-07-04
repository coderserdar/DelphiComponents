object Form1: TForm1
  Left = 236
  Top = 125
  Width = 504
  Height = 303
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Input file'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 48
    Height = 13
    Caption = 'Output file'
  end
  object Label3: TLabel
    Left = 72
    Top = 56
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label4: TLabel
    Left = 72
    Top = 72
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label5: TLabel
    Left = 72
    Top = 88
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label6: TLabel
    Left = 8
    Top = 112
    Width = 57
    Height = 13
    Caption = 'Input String:'
  end
  object Label7: TLabel
    Left = 72
    Top = 144
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label8: TLabel
    Left = 72
    Top = 160
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Label9: TLabel
    Left = 72
    Top = 192
    Width = 329
    Height = 57
    AutoSize = False
    Caption = 
      'Note! Compressing strings is ok for complete stringlists, but th' +
      'en they also need to be saved to file as stream (string cannot h' +
      'old compressed data). AND also, there is no point trying to comp' +
      'ress short strings..'
    WordWrap = True
  end
  object Edit1: TEdit
    Left = 72
    Top = 8
    Width = 321
    Height = 17
    AutoSize = False
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 72
    Top = 32
    Width = 321
    Height = 17
    AutoSize = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 408
    Top = 8
    Width = 75
    Height = 17
    Caption = 'browse'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 408
    Top = 32
    Width = 75
    Height = 17
    Caption = 'compress'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit3: TEdit
    Left = 72
    Top = 112
    Width = 321
    Height = 17
    AutoSize = False
    TabOrder = 4
  end
  object Button3: TButton
    Left = 408
    Top = 112
    Width = 75
    Height = 17
    Caption = 'compress'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 408
    Top = 136
    Width = 75
    Height = 17
    Caption = 'decompress'
    TabOrder = 6
  end
  object Button5: TButton
    Left = 408
    Top = 56
    Width = 75
    Height = 17
    Caption = 'uncompress'
    TabOrder = 7
    OnClick = Button5Click
  end
  object OpenDialog1: TOpenDialog
    Left = 304
    Top = 56
  end
  object tAPI_compress1: TAPI_compress
    Left = 272
    Top = 56
  end
end
