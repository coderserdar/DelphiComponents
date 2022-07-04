object Form1: TForm1
  Left = 192
  Top = 103
  Caption = 'Sample 6'
  ClientHeight = 405
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
    OnClick = DBNavigator1Click
  end
  object Edit1: TEdit
    Left = 16
    Top = 264
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 16
    Top = 291
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
  end
  object Button1: TButton
    Left = 72
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button2Click
  end
  object ASQLite3DB1: TASQLite3DB
    Database = 'demo.sqb'
    DefaultExt = '.sqb'
    DefaultDir = '..\'
    Version = '3.4.0'
    DriverDLL = '..\SQLite3.dll'
    Connected = False
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
  object ASQLite3Query2: TASQLite3Query
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
    Left = 72
    Top = 184
  end
end
