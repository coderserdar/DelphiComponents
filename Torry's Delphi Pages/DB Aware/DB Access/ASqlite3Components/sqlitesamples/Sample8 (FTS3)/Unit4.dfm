object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 457
  ClientWidth = 584
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
    Left = 17
    Top = 383
    Width = 441
    Height = 73
    Margins.Bottom = 0
    Caption = 
      'Due to missing result declaration from the SQLite api on fts isu' +
      'es the current result length is limited to 255 chars. We are wor' +
      'king to overcome this limitation'
    WordWrap = True
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 8
    Width = 201
    Height = 25
    Caption = 'Create virtual table'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Memo1: TMemo
    Left = 17
    Top = 56
    Width = 481
    Height = 41
    Lines.Strings = (
      'select name, ingredients from recipe where name match '#39'pie'#39'; ')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 504
    Top = 56
    Width = 75
    Height = 25
    Caption = 'exec'
    TabOrder = 2
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 17
    Top = 199
    Width = 555
    Height = 170
    DataSource = DSQQ
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo2: TMemo
    Left = 17
    Top = 103
    Width = 481
    Height = 42
    Lines.Strings = (
      
        'select name, ingredients from recipe where ingredients match '#39'on' +
        'ions cheese'#39)
    TabOrder = 4
  end
  object Button2: TButton
    Left = 504
    Top = 103
    Width = 75
    Height = 25
    Caption = 'exec'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 240
    Top = 8
    Width = 75
    Height = 25
    Caption = 'select *'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Memo3: TMemo
    Left = 17
    Top = 151
    Width = 481
    Height = 42
    Lines.Strings = (
      
        'select name, ingredients from recipe where ingredients match '#39'on' +
        'ions OR cheese'#39'; ')
    TabOrder = 7
  end
  object Button4: TButton
    Left = 504
    Top = 151
    Width = 75
    Height = 25
    Caption = 'exec'
    TabOrder = 8
    OnClick = Button4Click
  end
  object DB: TASQLite3DB
    CharacterEncoding = 'UTF8'
    Database = 'fts.sqb'
    DefaultExt = '.sqb'
    DriverDLL = 'SQLite3.dll'
    Connected = False
    MustExist = False
    ExecuteInlineSQL = False
    Left = 192
    Top = 272
  end
  object Q: TASQLite3Query
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DB
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    RawSQL = False
    Left = 232
    Top = 272
  end
  object QQ: TASQLite3Query
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = DB
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    RawSQL = False
    Left = 264
    Top = 272
  end
  object DSQQ: TDataSource
    DataSet = QQ
    Left = 264
    Top = 312
  end
end
