object FMain: TFMain
  Left = 192
  Top = 107
  Width = 570
  Height = 459
  Caption = 'Test Statically Linked property'
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
  object Label1: TLabel
    Left = 208
    Top = 16
    Width = 32
    Height = 13
    Caption = 'Tables'
  end
  object Label2: TLabel
    Left = 272
    Top = 16
    Width = 72
    Height = 13
    Caption = 'SQLite version:'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 193
    Height = 201
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object ListBox2: TListBox
    Left = 208
    Top = 40
    Width = 345
    Height = 169
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox2Click
  end
  object DBGrid1: TDBGrid
    Left = 40
    Top = 256
    Width = 497
    Height = 153
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 232
    Width = 97
    Height = 17
    Caption = 'Statically linked'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object ASQLite3DB1: TASQLite3DB
    TransactionType = 'DEFAULT'
    Database = 'test'
    DefaultExt = '.db'
    Version = '3.2.7'
    DriverDLL = 'SQLite3.dll'
    Connected = False
    MustExist = False
    ExecuteInlineSQL = False
    StaticallyLinked = False
    Left = 224
    Top = 48
  end
  object ASQLite3Table1: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = ASQLite3DB1
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    PrimaryAutoInc = False
    Left = 320
    Top = 48
  end
  object DataSource1: TDataSource
    DataSet = ASQLite3Table1
    Left = 280
    Top = 48
  end
end
