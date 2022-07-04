object Form1: TForm1
  Left = 192
  Top = 103
  Caption = 'Sample 1'
  ClientHeight = 500
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
    Width = 265
    Height = 120
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'bookid'
        Visible = True
      end>
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
  object Button1: TButton
    Left = 408
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object DBMemo1: TDBMemo
    Left = 279
    Top = 40
    Width = 378
    Height = 120
    DataField = 'description'
    DataSource = DataSource1
    TabOrder = 4
  end
  object Button2: TButton
    Left = 142
    Top = 431
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 32
    Top = 309
    Width = 185
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 32
    Top = 336
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 7
  end
  object ASQLite3DB1: TASQLite3DB
    Database = 'demo.sqb'
    DefaultExt = '.sqb'
    DefaultDir = 'c:\ASQLite3Null\sqlitesamples\'
    Version = '3.4.0'
    DriverDLL = 'c:\ASQLite3Null\sqlitesamples\sqlite3.dll'
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
    AutoCalcFields = False
    TableName = 'story'
    PrimaryAutoInc = False
    Left = 64
    Top = 104
  end
  object DataSource1: TDataSource
    DataSet = ASQLite3Table1
    Left = 96
    Top = 104
  end
  object ImageList1: TImageList
    Left = 280
    Top = 224
  end
end
