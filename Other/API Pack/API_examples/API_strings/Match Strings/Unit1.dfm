object Form1: TForm1
  Left = 192
  Top = 114
  Width = 449
  Height = 117
  Caption = 'Match Strings'
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
    Left = 16
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Look for:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 26
    Height = 13
    Caption = 'From:'
  end
  object Label3: TLabel
    Left = 352
    Top = 48
    Width = 3
    Height = 13
    Caption = '-'
  end
  object Edit1: TEdit
    Left = 80
    Top = 16
    Width = 257
    Height = 21
    TabOrder = 0
    Text = '*s?it*'
    OnKeyPress = Edit2KeyPress
  end
  object Edit2: TEdit
    Left = 80
    Top = 48
    Width = 257
    Height = 21
    TabOrder = 1
    Text = 'testing this shit out'
    OnKeyPress = Edit2KeyPress
  end
  object Button1: TButton
    Left = 352
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Match'
    TabOrder = 2
    OnClick = Button1Click
  end
end
