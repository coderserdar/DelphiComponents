object Form1: TForm1
  Left = 101
  Top = 72
  Width = 892
  Height = 682
  Caption = '������Գ��� - ��ǰ����ѡ�����ԣ�������ʾ���ǳ����ԭʼ�ַ���'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 24
    Top = 48
    Width = 66
    Height = 12
    Caption = 'Label1 ����'
  end
  object SpeedButton1: TSpeedButton
    Left = 496
    Top = 168
    Width = 23
    Height = 22
    Caption = '��'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 504
    Width = 841
    Height = 9
    Shape = bsTopLine
  end
  object lblLangs: TLabel
    Left = 248
    Top = 524
    Width = 72
    Height = 12
    Caption = '�Ѱ�װ���ԣ�'
  end
  object Edit1: TEdit
    Left = 144
    Top = 48
    Width = 121
    Height = 20
    TabOrder = 1
    Text = 'Edit ����'
  end
  object Memo1: TMemo
    Left = 24
    Top = 88
    Width = 241
    Height = 137
    Lines.Strings = (
      'Memo1 �ַ���'
      '�ַ����ڶ���')
    TabOrder = 5
  end
  object Button1: TButton
    Left = 288
    Top = 48
    Width = 137
    Height = 25
    Caption = 'Button ���ļ�'
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 288
    Top = 96
    Width = 137
    Height = 17
    Caption = 'CheckBox ��ѡ�����'
    TabOrder = 6
  end
  object RadioButton1: TRadioButton
    Left = 288
    Top = 136
    Width = 137
    Height = 17
    Caption = 'RadioButton1 ��ѡť'
    TabOrder = 7
  end
  object GroupBox1: TGroupBox
    Left = 464
    Top = 48
    Width = 185
    Height = 105
    Caption = 'GroupBox1 ����'
    TabOrder = 3
  end
  object RadioGroup1: TRadioGroup
    Left = 672
    Top = 48
    Width = 161
    Height = 105
    Caption = 'RadioGroup1 ����'
    Items.Strings = (
      'ѡ��һ'
      'ѡ���'
      'ѡ����')
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 288
    Top = 184
    Width = 185
    Height = 41
    Caption = 'Panel1 ������'
    TabOrder = 10
  end
  object BitBtn1: TBitBtn
    Left = 496
    Top = 200
    Width = 153
    Height = 25
    Caption = 'ͼƬ��ť'
    TabOrder = 11
    Kind = bkAbort
  end
  object StaticText1: TStaticText
    Left = 536
    Top = 174
    Width = 124
    Height = 16
    Caption = 'StaticText1 ��̬����'
    TabOrder = 9
  end
  object ComboBox1: TComboBox
    Left = 672
    Top = 168
    Width = 161
    Height = 20
    ItemHeight = 12
    TabOrder = 8
    Text = 'ComboBox1'
    Items.Strings = (
      '�������һ��'
      '������ڶ���'
      '�����������')
  end
  object ListBox1: TListBox
    Left = 672
    Top = 200
    Width = 161
    Height = 97
    ItemHeight = 12
    Items.Strings = (
      '�б���һ��'
      '�б��ڶ���'
      '�б�������')
    TabOrder = 12
  end
  object CheckListBox1: TCheckListBox
    Left = 496
    Top = 240
    Width = 153
    Height = 57
    ItemHeight = 12
    Items.Strings = (
      '��ѡ�б���һ��'
      '��ѡ�б��ڶ���')
    TabOrder = 13
  end
  object TabControl1: TTabControl
    Left = 24
    Top = 248
    Width = 289
    Height = 57
    TabOrder = 14
    Tabs.Strings = (
      '��һҳ'
      '�ڶ�ҳ'
      '����ҳ')
    TabIndex = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 617
    Width = 884
    Height = 19
    Panels = <
      item
        Text = '��һ�������'
        Width = 250
      end
      item
        Text = '�ڶ��������'
        Width = 150
      end
      item
        Text = '�����������'
        Width = 50
      end>
    SimplePanel = False
  end
  object PageControl1: TPageControl
    Left = 24
    Top = 328
    Width = 289
    Height = 145
    ActivePage = TabSheet1
    TabOrder = 15
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1 ��һҳ'
      object ListView1: TListView
        Left = 16
        Top = 16
        Width = 249
        Height = 81
        Columns = <
          item
            Caption = '��һ��'
            Width = 100
          end
          item
            Caption = '�ڶ���'
            Width = 120
          end>
        Items.Data = {
          7B0000000400000000000000FFFFFFFFFFFFFFFF000000000000000006B5DAD2
          BBB8F600000000FFFFFFFFFFFFFFFF010000000000000006B5DAB6FEB8F60AB5
          DAB6FEB5E3CEE5B8F600000000FFFFFFFFFFFFFFFF000000000000000006B5DA
          C8FDB8F600000000FFFFFFFFFFFFFFFF000000000000000000FFFF}
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2 �ڶ�ҳ'
      ImageIndex = 1
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 884
    Height = 29
    BorderWidth = 1
    ButtonHeight = 20
    ButtonWidth = 91
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    ShowCaptions = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = '��һ�����߰�ť'
      ImageIndex = 0
    end
    object ToolButton2: TToolButton
      Left = 91
      Top = 0
      Caption = '�ڶ������߰�ť'
      ImageIndex = 1
    end
    object ToolButton3: TToolButton
      Left = 182
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 190
      Top = 0
      Action = Action1
    end
    object ToolButton5: TToolButton
      Left = 281
      Top = 0
      Action = Action2
    end
  end
  object TreeView1: TTreeView
    Left = 336
    Top = 320
    Width = 241
    Height = 153
    AutoExpand = True
    Indent = 19
    TabOrder = 17
    Items.Data = {
      030000001F0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      06B5DAD2BBB8F61F0000000000000000000000FFFFFFFFFFFFFFFF0000000001
      00000006B5DAB6FEB8F6230000000000000000000000FFFFFFFFFFFFFFFF0000
      0000010000000AB5DAB6FEB5E3CEE5B8F6250000000000000000000000FFFFFF
      FFFFFFFFFF00000000000000000CB5DAB6FEB5E3B6FECEE5B8F61F0000000000
      000000000000FFFFFFFFFFFFFFFF000000000100000006B5DAC8FDB8F6230000
      000000000000000000FFFFFFFFFFFFFFFF00000000000000000AB5DAC8FDB5E3
      CEE5B8F6}
  end
  object Button2: TButton
    Left = 712
    Top = 520
    Width = 139
    Height = 25
    Caption = '������ AutoDetect ����'
    TabOrder = 18
    OnClick = Button2Click
  end
  object btn1: TButton
    Left = 16
    Top = 528
    Width = 201
    Height = 25
    Caption = '�л�����'
    TabOrder = 19
    OnClick = btn1Click
  end
  object chkStorageMode: TCheckBox
    Left = 16
    Top = 572
    Width = 161
    Height = 17
    Caption = '����洢ģʽ�������ļ�'
    TabOrder = 20
    OnClick = chkStorageModeClick
  end
  object mmoLangs: TMemo
    Left = 336
    Top = 520
    Width = 361
    Height = 73
    ScrollBars = ssVertical
    TabOrder = 21
  end
  object Button4: TButton
    Left = 616
    Top = 320
    Width = 217
    Height = 25
    Caption = '������һ������'
    TabOrder = 22
    OnClick = Button4Click
  end
  object Button3: TButton
    Left = 616
    Top = 360
    Width = 217
    Height = 25
    Caption = '��ʾ��һ���Ѵ����Ĵ���'
    TabOrder = 23
    OnClick = Button3Click
  end
  object lm1: TCnLangManager
    LanguageStorage = hfs1
    TranslationMode = tmByComponents
    AutoTransOptions = [atApplication, atForms, atDataModules]
    TranslateListItem = True
    TranslateTreeNode = True
    Left = 640
    Top = 528
  end
  object hfs1: TCnHashLangFileStorage
    StorageMode = smByDirectory
    Languages = <
      item
        Abbreviation = 'CHS'
        LanguageID = 2052
        LanguageName = '����(�й�)'
        LanguageFileName = 'CHS'
        LanguageDirName = '2052'
        DefaultFont.Charset = DEFAULT_CHARSET
        DefaultFont.Color = clWindowText
        DefaultFont.Height = -11
        DefaultFont.Name = 'MS Sans Serif'
        DefaultFont.Style = []
      end
      item
        Abbreviation = 'ENU'
        LanguageID = 1033
        LanguageName = 'Ӣ��(����)'
        LanguageFileName = 'ENU'
        LanguageDirName = '1033'
        DefaultFont.Charset = DEFAULT_CHARSET
        DefaultFont.Color = clWindowText
        DefaultFont.Height = -11
        DefaultFont.Name = 'MS Sans Serif'
        DefaultFont.Style = []
      end>
    ListLength = 1024
    IncSize = 2
    Left = 608
    Top = 528
  end
  object lt1: TCnLangTranslator
    Left = 672
    Top = 528
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 56
    Top = 64
    object N1: TMenuItem
      Caption = '�ļ�'
      object N3: TMenuItem
        Caption = '�˵�����һ'
      end
      object N4: TMenuItem
        Caption = '�˵����Զ�'
      end
    end
    object N2: TMenuItem
      Caption = '����'
      object N5: TMenuItem
        Caption = '�˵�������'
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 120
    Top = 64
    object N6: TMenuItem
      Caption = '�����˵�����һ'
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object N8: TMenuItem
      Caption = '�����˵����Զ�'
    end
  end
  object ActionList1: TActionList
    Left = 528
    Top = 88
    object Action1: TAction
      Caption = 'Action1 ����һ'
    end
    object Action2: TAction
      Caption = 'Action2 ���Զ�'
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '�ı��ļ�|*.txt|�����ļ�|*.*'
    Left = 416
    Top = 48
  end
end
