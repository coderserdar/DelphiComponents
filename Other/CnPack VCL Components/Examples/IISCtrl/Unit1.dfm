object Form1: TForm1
  Left = 207
  Top = 142
  Width = 226
  Height = 162
  Caption = 'IIS Control'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 64
    Height = 13
    AutoSize = False
    Caption = '����Ŀ¼��'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 64
    Height = 13
    AutoSize = False
    Caption = 'ʵ��·��'
  end
  object Label3: TLabel
    Left = 16
    Top = 64
    Width = 64
    Height = 13
    AutoSize = False
    Caption = '������'
  end
  object Edit1: TEdit
    Left = 88
    Top = 16
    Width = 121
    Height = 20
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 88
    Top = 40
    Width = 121
    Height = 20
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 88
    Top = 64
    Width = 121
    Height = 20
    TabOrder = 2
  end
  object Button1: TButton
    Left = 24
    Top = 96
    Width = 75
    Height = 25
    Caption = '����'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 96
    Width = 75
    Height = 25
    Caption = 'ɾ��'
    TabOrder = 4
    OnClick = Button2Click
  end
  object CnIISCtrl1: TCnIISCtrl
    Left = 16
    Top = 8
  end
end
