object Form1: TForm1
  Left = 548
  Top = 259
  Width = 431
  Height = 334
  Caption = '����Ϸ�޸��������⼸����ֵ��'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 32
    Top = 80
    Width = 16
    Height = 13
    Caption = 'lbl1'
  end
  object lbl2: TLabel
    Left = 264
    Top = 80
    Width = 16
    Height = 13
    Caption = 'lbl1'
  end
  object Button1: TButton
    Left = 32
    Top = 32
    Width = 113
    Height = 25
    Caption = '�������Լ�һ'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 264
    Top = 32
    Width = 121
    Height = 25
    Caption = '�������Լ�һ'
    TabOrder = 1
    OnClick = Button2Click
  end
  object mmo1: TMemo
    Left = 32
    Top = 120
    Width = 353
    Height = 145
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '����'
    Font.Style = []
    Lines.Strings = (
      '˵������ֵ����TCnAntiCheater��ʵ���Լ���һ�������ʵ����'
      'ĳ���Ա��档TCnAntiCheater ���ڴ������������� published '
      '�Ĵ� Get �� Set ������ Integer ���Ե�ʱ���ҽ�����������'
      '���ڶ�д��Щ���ԵĹ����в���һ�Զ���任���̣��Ӷ��ﵽ��'
      '��������ֵ�Ͷ�����ʾ��һ�µ�Ч���Զ㿪��Ϸ�޸�����׷�١�'
      ''
      '�����CnAntiCheater��Ԫ��initialization���ֵ�'
      'FEnableProtect := True;'
      '��ΪFalse;���ر������У�����������������ֵ��')
    ParentFont = False
    TabOrder = 2
  end
end
