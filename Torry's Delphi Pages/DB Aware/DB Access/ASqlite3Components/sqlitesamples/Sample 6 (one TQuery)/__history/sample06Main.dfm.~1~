object Form1: TForm1
  Left = 192
  Top = 103
  Width = 673
  Height = 288
  Caption = 'Sample 6'
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
    Height = 209
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
  object ASQLite3DB1: TASQLite3DB
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
  object DataSource1: TDataSource
    DataSet = ASQLite3Query1
    Left = 96
    Top = 104
  end
  object ASQLite3Query1: TASQLite3Query
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = ASQLite3DB1
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    RawSQL = False
    SQL.Strings = (
      'select * from animal')
    UpdateSQL = ASQLite3UpdateSQL1
    Left = 64
    Top = 104
  end
  object ASQLite3UpdateSQL1: TASQLite3UpdateSQL
    InsertSQL.Strings = (
      'insert into animal *')
    UpdateSQL.Strings = (
      'update animal set desc=:desc where id=:id')
    DeleteSQL.Strings = (
      'delete from animal where id=:id'
      '')
    Left = 64
    Top = 136
  end
end
