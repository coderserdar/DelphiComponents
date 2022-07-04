object Form1: TForm1
  Left = 192
  Top = 103
  Width = 673
  Height = 288
  Caption = 'Sample 4'
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
    Left = 216
    Top = 184
    Width = 79
    Height = 13
    Caption = 'Always 1 in delta'
  end
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
    Columns = <
      item
        Expanded = False
        FieldName = 'number'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'name'
        Width = 200
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'dob'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'gdesc'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'delta'
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
    OnCalcFields = ASQLite3Table1CalcFields
    TableName = 'person'
    PrimaryAutoInc = False
    Left = 64
    Top = 104
    object ASQLite3Table1number: TIntegerField
      FieldName = 'number'
    end
    object ASQLite3Table1name: TStringField
      FieldName = 'name'
      Size = 80
    end
    object ASQLite3Table1dob: TDateField
      FieldName = 'dob'
    end
    object ASQLite3Table1gender: TStringField
      FieldName = 'gender'
      Visible = False
      Size = 1
    end
    object ASQLite3Table1gdesc: TStringField
      FieldKind = fkLookup
      FieldName = 'gdesc'
      LookupDataSet = TGender
      LookupKeyFields = 'gid'
      LookupResultField = 'desc'
      KeyFields = 'gender'
      Lookup = True
    end
    object ASQLite3Table1delta: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'delta'
      Calculated = True
    end
  end
  object DataSource1: TDataSource
    DataSet = ASQLite3Table1
    Left = 96
    Top = 104
  end
  object TGender: TASQLite3Table
    AutoCommit = False
    SQLiteDateFormat = True
    Connection = ASQLite3DB1
    MaxResults = 0
    StartResult = 0
    TypeLess = False
    SQLCursor = True
    ReadOnly = False
    UniDirectional = False
    TableName = 'gender'
    PrimaryAutoInc = False
    Left = 128
    Top = 104
  end
end
