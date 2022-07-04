object Form1: TForm1
  Left = 192
  Top = 103
  Caption = 'Sample 2'
  ClientHeight = 443
  ClientWidth = 695
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
    Left = 16
    Top = 352
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
  object DBGrid2: TDBGrid
    Left = 0
    Top = 216
    Width = 673
    Height = 120
    DataSource = DataSource2
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator2: TDBNavigator
    Left = 16
    Top = 184
    Width = 240
    Height = 25
    DataSource = DataSource2
    TabOrder = 4
  end
  object Button1: TButton
    Left = 288
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Append child'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 369
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Post child'
    TabOrder = 6
    OnClick = Button2Click
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
  object ASQLite3Table2: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = ASQLite3DB1
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    MasterFields = 'parent=number;'
    MasterSource = DataSource1
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    Filtered = True
    TableName = 'children'
    PrimaryAutoInc = False
    OrderBy = 'id'
    Left = 64
    Top = 272
    object ASQLite3Table2id: TIntegerField
      FieldName = 'id'
    end
    object ASQLite3Table2name: TStringField
      FieldName = 'name'
      Size = 80
    end
    object ASQLite3Table2gender: TStringField
      FieldName = 'gender'
      Size = 1
    end
    object ASQLite3Table2dob: TDateField
      FieldName = 'dob'
    end
    object ASQLite3Table2parent: TIntegerField
      FieldName = 'parent'
    end
  end
  object DataSource2: TDataSource
    DataSet = ASQLite3Table2
    Left = 96
    Top = 272
  end
  object ASQLite3Query1: TASQLite3Query
    AutoCommit = False
    SQLiteDateFormat = True
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    RawSQL = False
    Left = 568
    Top = 392
  end
end
