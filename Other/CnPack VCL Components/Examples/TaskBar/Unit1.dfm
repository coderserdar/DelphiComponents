object Form1: TForm1
  Left = 219
  Top = 183
  Width = 363
  Height = 486
  Caption = '������'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 355
    Height = 459
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = '����������'
      object Image2: TImage
        Left = 234
        Top = 5
        Width = 18
        Height = 18
      end
      object CheckListBox1: TCheckListBox
        Left = 0
        Top = 32
        Width = 343
        Height = 361
        OnClickCheck = CheckListBox1ClickCheck
        ImeName = '���� (����) - �ȸ�ƴ�����뷨'
        ItemHeight = 12
        TabOrder = 0
        OnClick = CheckListBox1Click
        OnDblClick = CheckListBox1DblClick
      end
      object RadioButton2: TRadioButton
        Left = 0
        Top = 400
        Width = 153
        Height = 17
        Caption = '����ʾĬ��ϵͳ����ͼ��'
        TabOrder = 1
        OnClick = RadioButton2Click
      end
      object RadioButton1: TRadioButton
        Left = 160
        Top = 399
        Width = 185
        Height = 17
        Caption = '��ʾȫ��������ϵͳ����ͼ�꣩'
        TabOrder = 2
        OnClick = RadioButton2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = '���������ó���������'
      ImageIndex = 1
      object CheckListBox2: TCheckListBox
        Left = 0
        Top = 36
        Width = 343
        Height = 361
        OnClickCheck = CheckListBox2ClickCheck
        ImeName = '���� (����) - �ȸ�ƴ�����뷨'
        ItemHeight = 12
        TabOrder = 0
        OnDblClick = CheckListBox2DblClick
      end
      object RadioButton3: TRadioButton
        Left = -2
        Top = 405
        Width = 153
        Height = 17
        Caption = '����ʾĬ��ϵͳ����ͼ��'
        TabOrder = 1
        OnClick = RadioButton3Click
      end
      object RadioButton4: TRadioButton
        Left = 158
        Top = 404
        Width = 185
        Height = 17
        Caption = '��ʾȫ��������ϵͳ����ͼ�꣩'
        TabOrder = 2
        OnClick = RadioButton3Click
      end
      object Button0: TButton
        Left = 192
        Top = 8
        Width = 137
        Height = 25
        Caption = '�����������ϵ�����'
        TabOrder = 3
        OnClick = Button0Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = '��ʼ��Ť����'
      ImageIndex = 2
      object Button1: TButton
        Left = 96
        Top = 48
        Width = 139
        Height = 25
        Caption = '�޸Ŀ�ʼ��Ť����'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 96
        Top = 88
        Width = 139
        Height = 25
        Caption = '���ؿ�ʼ��Ť'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 96
        Top = 128
        Width = 139
        Height = 25
        Caption = 'ʹ��ʼ��Ť������'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 96
        Top = 168
        Width = 139
        Height = 25
        Caption = '����������'
        TabOrder = 3
        OnClick = Button4Click
      end
    end
  end
end
