object Form1: TForm1
  Left = 314
  Top = 118
  Width = 575
  Height = 463
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 465
    Height = 209
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 483
    Top = 8
    Width = 75
    Height = 25
    Caption = 'connect'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 483
    Top = 40
    Width = 75
    Height = 25
    Caption = 'disconnect'
    TabOrder = 2
    OnClick = Button2Click
  end
  object DBGrid2: TDBGrid
    Left = 8
    Top = 224
    Width = 465
    Height = 206
    DataSource = DataSource2
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object OCIBDEDatabase1: TOCIBDEDatabase
    BDEDatabase = Database1
    Connected = True
    Left = 8
    Top = 8
  end
  object Database1: TDatabase
    Connected = True
    DatabaseName = 'DB1'
    DriverName = 'ORACLE'
    Params.Strings = (
      'SERVER NAME=DAORA'
      'USER NAME=scott'
      'NET PROTOCOL='
      'OPEN MODE=READ/WRITE'
      'SCHEMA CACHE SIZE=128'
      'LANGDRIVER=DBWINUS0'
      'SQLQRYMODE='
      'SQLPASSTHRU MODE=SHARED AUTOCOMMIT'
      'SCHEMA CACHE TIME=-1'
      'MAX ROWS=-1'
      'BATCH COUNT=200'
      'ENABLE SCHEMA CACHE=FALSE'
      'SCHEMA CACHE DIR='
      'ENABLE BCD=FALSE'
      'ENABLE INTEGERS=FALSE'
      'LIST SYNONYMS=ALL'
      'ROWSET SIZE=0'
      'BLOBS TO CACHE=64'
      'BLOB SIZE=32'
      'OBJECT MODE=FALSE'
      'PASSWORD=tiger')
    SessionName = 'Default'
    Left = 40
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    Active = True
    Prepared = True
    SQL.Strings = (
      'select * from emp')
    Left = 8
    Top = 48
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 40
    Top = 48
  end
  object Query1: TQuery
    Active = True
    DatabaseName = 'DB1'
    SQL.Strings = (
      'select * from emp')
    Left = 8
    Top = 224
  end
  object DataSource2: TDataSource
    DataSet = Query1
    Left = 40
    Top = 224
  end
end
