object Form1: TForm1
  Left = 192
  Top = 107
  Width = 371
  Height = 375
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 144
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Name :'
  end
  object Label2: TLabel
    Left = 144
    Top = 32
    Width = 60
    Height = 13
    Caption = 'MacroType :'
  end
  object Label3: TLabel
    Left = 144
    Top = 56
    Width = 53
    Height = 13
    Caption = 'DataType :'
  end
  object Label4: TLabel
    Left = 144
    Top = 81
    Width = 33
    Height = 13
    Caption = 'Value :'
  end
  object Label5: TLabel
    Left = 8
    Top = 120
    Width = 40
    Height = 13
    Caption = 'Source :'
  end
  object Label6: TLabel
    Left = 8
    Top = 232
    Width = 36
    Height = 13
    Caption = 'Result :'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 121
    Height = 105
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object ComboBox1: TComboBox
    Left = 208
    Top = 30
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnClick = ComboBox1Click
    Items.Strings = (
      'mtString'
      'mtSQL')
  end
  object Edit1: TEdit
    Left = 208
    Top = 5
    Width = 145
    Height = 21
    TabOrder = 2
    OnExit = Edit1Exit
  end
  object ComboBox2: TComboBox
    Left = 208
    Top = 55
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    OnClick = ComboBox2Click
    Items.Strings = (
      'mdUnknown'
      'mdString'
      'mdInteger'
      'mdBoolean'
      'mdFloat'
      'mdDate'
      'mdDateTime')
  end
  object Edit2: TEdit
    Left = 208
    Top = 80
    Width = 145
    Height = 21
    TabOrder = 4
    OnExit = Edit2Exit
  end
  object Memo1: TMemo
    Left = 8
    Top = 136
    Width = 345
    Height = 89
    Lines.Strings = (
      'select * from &TN')
    TabOrder = 5
    OnExit = Memo1Exit
  end
  object Memo2: TMemo
    Left = 8
    Top = 248
    Width = 345
    Height = 89
    TabOrder = 6
  end
  object OCIQuery1: TOCIQuery
    Left = 144
    Top = 96
  end
end
