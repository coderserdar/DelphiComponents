object Form1: TForm1
  Left = 318
  Top = 219
  Width = 404
  Height = 175
  Caption = '������ת�����ַ�������'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 60
    Height = 13
    Caption = '���븡����'
  end
  object Edit1: TEdit
    Left = 8
    Top = 27
    Width = 299
    Height = 21
    TabOrder = 0
    Text = '128.125'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 54
    Width = 121
    Height = 17
    Caption = 'ʮ����ָ������'
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 77
    Width = 97
    Height = 17
    Caption = 'һ��ʹ��ָ��'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 313
    Top = 25
    Width = 75
    Height = 25
    Caption = 'ת��'
    TabOrder = 3
    OnClick = Button1Click
  end
  object rdoBin: TRadioButton
    Left = 8
    Top = 112
    Width = 113
    Height = 17
    Caption = '������(Binary)'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object rdoOct: TRadioButton
    Left = 127
    Top = 112
    Width = 113
    Height = 17
    Caption = '�˽���(Octal)'
    TabOrder = 5
  end
  object rdoHex: TRadioButton
    Left = 246
    Top = 112
    Width = 139
    Height = 17
    Caption = 'ʮ������(Hexdecimal)'
    TabOrder = 6
  end
end
