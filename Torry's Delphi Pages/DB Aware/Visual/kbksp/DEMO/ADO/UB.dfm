object Form1: TForm1
  Left = 192
  Top = 119
  Width = 523
  Height = 446
  Caption = 'Test Northwind'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SelectPanel1: TSelectPanel
    Left = 4
    Top = 4
    Width = 284
    Height = 193
    Caption = 'Suppliers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Indent = 8
    KeyField = 'SupplierID'
    ListField = 'CompanyName'
    NoteField = 'Region'
    ListSource = DataSource1
    SizeText = 40
  end
  object SelectPanel2: TSelectPanel
    Left = 316
    Top = 4
    Width = 194
    Height = 193
    Caption = 'Categories'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Indent = 8
    KeyField = 'CategoryID'
    ListField = 'CategoryName'
    ListSource = DataSource2
    SizeText = 25
  end
  object Memo1: TMemo
    Left = 4
    Top = 228
    Width = 505
    Height = 89
    Color = clActiveBorder
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button1: TButton
    Left = 4
    Top = 200
    Width = 109
    Height = 25
    Caption = 'Show Products'
    TabOrder = 3
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 4
    Top = 320
    Width = 505
    Height = 93
    DataSource = DataSource3
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Mi' +
      'crosoft Office\Office\Samples\Northwind.mdb;Persist Security Inf' +
      'o=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 48
    Top = 116
  end
  object DataSource1: TDataSource
    DataSet = ADODataSet1
    Left = 104
    Top = 116
  end
  object ADODataSet1: TADODataSet
    Active = True
    Connection = ADOConnection1
    CursorType = ctStatic
    CommandText = 'select * from Suppliers'#13#10'order by CompanyName'
    Parameters = <>
    Left = 76
    Top = 116
    object ADODataSet1SupplierID: TAutoIncField
      FieldName = 'SupplierID'
      ReadOnly = True
    end
    object ADODataSet1CompanyName: TWideStringField
      FieldName = 'CompanyName'
      OnGetText = ADODataSet1CompanyNameGetText
      Size = 40
    end
    object ADODataSet1ContactName: TWideStringField
      FieldName = 'ContactName'
      Size = 30
    end
    object ADODataSet1ContactTitle: TWideStringField
      FieldName = 'ContactTitle'
      Size = 30
    end
    object ADODataSet1Address: TWideStringField
      FieldName = 'Address'
      Size = 60
    end
    object ADODataSet1City: TWideStringField
      FieldName = 'City'
      Size = 15
    end
    object ADODataSet1Region: TWideStringField
      FieldName = 'Region'
      Size = 15
    end
    object ADODataSet1PostalCode: TWideStringField
      FieldName = 'PostalCode'
      Size = 10
    end
    object ADODataSet1Country: TWideStringField
      FieldName = 'Country'
      Size = 15
    end
    object ADODataSet1Phone: TWideStringField
      FieldName = 'Phone'
      Size = 24
    end
    object ADODataSet1Fax: TWideStringField
      FieldName = 'Fax'
      Size = 24
    end
    object ADODataSet1HomePage: TMemoField
      FieldName = 'HomePage'
      BlobType = ftMemo
    end
  end
  object ADODataSet2: TADODataSet
    Active = True
    Connection = ADOConnection1
    CursorType = ctStatic
    CommandText = 'select * from Categories'#13#10'order by CategoryName'
    Parameters = <>
    Left = 276
    Top = 116
  end
  object DataSource2: TDataSource
    DataSet = ADODataSet2
    Left = 304
    Top = 116
  end
  object DataSource3: TDataSource
    DataSet = ADOQuery1
    Left = 244
    Top = 344
  end
  object ADOQuery1: TADOQuery
    Active = True
    Connection = ADOConnection1
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select * from Products'
      'order by ProductName')
    Left = 216
    Top = 344
    object ADOQuery1ProductID: TAutoIncField
      FieldName = 'ProductID'
      ReadOnly = True
      Visible = False
    end
    object ADOQuery1ProductName: TWideStringField
      DisplayWidth = 28
      FieldName = 'ProductName'
      Size = 40
    end
    object ADOQuery1SupplierID: TIntegerField
      FieldName = 'SupplierID'
      Visible = False
    end
    object ADOQuery1CategoryID: TIntegerField
      FieldName = 'CategoryID'
      Visible = False
    end
    object ADOQuery1QuantityPerUnit: TWideStringField
      FieldName = 'QuantityPerUnit'
      Visible = False
    end
    object ADOQuery1UnitPrice: TBCDField
      FieldName = 'UnitPrice'
      Visible = False
      Precision = 19
    end
    object ADOQuery1UnitsInStock: TSmallintField
      FieldName = 'UnitsInStock'
      Visible = False
    end
    object ADOQuery1UnitsOnOrder: TSmallintField
      FieldName = 'UnitsOnOrder'
      Visible = False
    end
    object ADOQuery1ReorderLevel: TSmallintField
      FieldName = 'ReorderLevel'
      Visible = False
    end
    object ADOQuery1Discontinued: TBooleanField
      FieldName = 'Discontinued'
      Visible = False
    end
    object ADOQuery1Supplier: TStringField
      DisplayWidth = 28
      FieldKind = fkLookup
      FieldName = 'Supplier'
      LookupDataSet = ADODataSet1
      LookupKeyFields = 'SupplierID'
      LookupResultField = 'CompanyName'
      KeyFields = 'SupplierID'
      Size = 40
      Lookup = True
    end
    object ADOQuery1Category: TStringField
      DisplayWidth = 20
      FieldKind = fkLookup
      FieldName = 'Category'
      LookupDataSet = ADODataSet2
      LookupKeyFields = 'CategoryID'
      LookupResultField = 'CategoryName'
      KeyFields = 'CategoryID'
      Size = 25
      Lookup = True
    end
  end
end
