object Form1: TForm1
  Left = 192
  Top = 103
  Caption = 'Sample 1'
  ClientHeight = 261
  ClientWidth = 665
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
  object DBGrid1: TDBGrid
    Left = 8
    Top = 40
    Width = 649
    Height = 120
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 8
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object RGDate: TRadioGroup
    Left = 8
    Top = 168
    Width = 185
    Height = 81
    Caption = ' date format '
    ItemIndex = 0
    Items.Strings = (
      'dd-mm-yyyy'
      'yyyy-mm-dd'
      'mm-dd-yyyy')
    TabOrder = 2
    OnClick = RGDateClick
  end
  object TntDBEdit1: TTntDBEdit
    Left = 256
    Top = 184
    Width = 121
    Height = 21
    DataField = 'name'
    DataSource = DataSource1
    TabOrder = 3
  end
  object Button1: TButton
    Left = 464
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    OnClick = Button1Click
  end
  object ASQLite3DB1: TASQLite3DB
    CharacterEncoding = 'UTF8'
    Database = 'demo.sqb'
    DefaultExt = '.sqb'
    DefaultDir = '..\'
    Version = '3.4.0'
    DriverDLL = '..\SQLite3.dll'
    Connected = True
    MustExist = False
    ExecuteInlineSQL = False
    Left = 32
    Top = 104
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
    TableName = 'person'
    PrimaryAutoInc = False
    Left = 64
    Top = 104
  end
  object DataSource1: TDataSource
    DataSet = ASQLite3Table1
    Left = 96
    Top = 104
  end
end
