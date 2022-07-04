object Form1: TForm1
  Left = 202
  Top = 126
  Width = 521
  Height = 253
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 128
    Width = 433
    Height = 81
  end
  object Label1: TLabel
    Left = 16
    Top = 160
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 80
    Top = 160
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label5: TLabel
    Left = 216
    Top = 160
    Width = 6
    Height = 13
    Caption = '--'
  end
  object Label6: TLabel
    Left = 144
    Top = 184
    Width = 6
    Height = 13
    Caption = '--'
  end
  object Label7: TLabel
    Left = 80
    Top = 184
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label8: TLabel
    Left = 16
    Top = 184
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label9: TLabel
    Left = 16
    Top = 136
    Width = 7
    Height = 13
    Caption = 'X'
  end
  object Label10: TLabel
    Left = 80
    Top = 136
    Width = 7
    Height = 13
    Caption = 'Y'
  end
  object Label11: TLabel
    Left = 144
    Top = 136
    Width = 30
    Height = 13
    Caption = 'String:'
  end
  object Label12: TLabel
    Left = 8
    Top = 0
    Width = 154
    Height = 13
    Caption = 'Highlight these words of the text:'
  end
  object Label3: TLabel
    Left = 144
    Top = 160
    Width = 6
    Height = 13
    Caption = '--'
  end
  object Label4: TLabel
    Left = 216
    Top = 136
    Width = 44
    Height = 13
    Caption = 'Highlight:'
  end
  object Label13: TLabel
    Left = 288
    Top = 160
    Width = 83
    Height = 13
    Caption = '<< cursor position'
  end
  object Label14: TLabel
    Left = 288
    Top = 184
    Width = 85
    Height = 13
    Caption = '<< mouse position'
  end
  object API_richedit1: TAPI_richedit
    Left = 8
    Top = 24
    Width = 433
    Height = 97
    Lines.Strings = (
      'now testing this thing out, all tests will be marked'
      'out with totally separate font and color...'
      'so while writing this new richedit controll will hightlight'
      'thos tests from the text. Also there is couple of other new'
      'features like positions and mouse word select - try with'
      'clicking words with mouse left button.. just testing..')
    TabOrder = 0
    OnMouseDown = API_richedit1MouseDown
    SearchType = [stWholeWord, stMatchCase]
    MaxLines = 0
  end
  object Edit1: TEdit
    Left = 168
    Top = 0
    Width = 273
    Height = 21
    TabOrder = 1
    Text = 'and'
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 400
    Top = 136
  end
end
