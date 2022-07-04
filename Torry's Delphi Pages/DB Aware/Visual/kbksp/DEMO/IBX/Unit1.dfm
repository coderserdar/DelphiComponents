object Form1: TForm1
  Left = 192
  Top = 119
  Width = 500
  Height = 542
  Caption = 'Test IBX'
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
    Top = 1
    Width = 260
    Height = 216
    Caption = 'Employees'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Indent = 5
    KeyField = 'EMP_NO'
    ListField = 'FULL_NAME'
    ListSource = DataSource2
    SizeText = 37
  end
  object SelectPanel2: TSelectPanel
    Left = 300
    Top = 4
    Width = 186
    Height = 217
    Caption = 'Customers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Indent = 4
    KeyField = 'CUST_NO'
    ListField = 'CUSTOMER'
    NoteField = 'CITY'
    ListSource = DataSource1
    SizeText = 25
  end
  object Button2: TButton
    Left = 4
    Top = 224
    Width = 161
    Height = 25
    Caption = 'Show Sales'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 4
    Top = 252
    Width = 481
    Height = 89
    Color = clInactiveBorder
    Lines.Strings = (
      'select * from sales')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object DBGrid1: TDBGrid
    Left = 4
    Top = 344
    Width = 481
    Height = 165
    DataSource = DataSource3
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DataSource1: TDataSource
    DataSet = Customer
    Left = 356
    Top = 116
  end
  object DataSource2: TDataSource
    DataSet = Employee
    Left = 96
    Top = 152
  end
  object IBDatabase1: TIBDatabase
    Connected = True
    DatabaseName = 'D:\Firebird\examples\EMPLOYEE.GDB'
    Params.Strings = (
      'user_name=SYSDBA'
      'lc_ctype=WIN1251')
    DefaultTransaction = IBTransaction1
    IdleTimer = 0
    SQLDialect = 1
    TraceFlags = []
    AllowStreamedConnected = False
    Left = 388
    Top = 320
  end
  object IBTransaction1: TIBTransaction
    Active = True
    DefaultDatabase = IBDatabase1
    Params.Strings = (
      'read_committed'
      'rec_version'
      'nowait')
    AutoStopAction = saNone
    Left = 424
    Top = 320
  end
  object Customer: TIBQuery
    Database = IBDatabase1
    Transaction = IBTransaction1
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    SQL.Strings = (
      'select * from CUSTOMER'
      'order by CUSTOMER')
    Left = 388
    Top = 116
  end
  object Employee: TIBQuery
    Database = IBDatabase1
    Transaction = IBTransaction1
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    SQL.Strings = (
      'select * from EMPLOYEE'
      'order by FULL_NAME')
    Left = 132
    Top = 152
  end
  object DataSource3: TDataSource
    DataSet = IBQuery3
    Left = 224
    Top = 388
  end
  object IBQuery3: TIBQuery
    Database = IBDatabase1
    Transaction = IBTransaction1
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    SQL.Strings = (
      'select * from sales')
    Left = 260
    Top = 388
    object IBQuery3PO_NUMBER: TIBStringField
      FieldName = 'PO_NUMBER'
      Origin = 'SALES.PO_NUMBER'
      Required = True
      FixedChar = True
      Size = 8
    end
    object IBQuery3CUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Origin = 'SALES.CUST_NO'
      Required = True
      Visible = False
    end
    object IBQuery3SALES_REP: TSmallintField
      FieldName = 'SALES_REP'
      Origin = 'SALES.SALES_REP'
      Visible = False
    end
    object IBQuery3ORDER_STATUS: TIBStringField
      FieldName = 'ORDER_STATUS'
      Origin = 'SALES.ORDER_STATUS'
      Required = True
      Visible = False
      Size = 7
    end
    object IBQuery3ORDER_DATE: TDateTimeField
      FieldName = 'ORDER_DATE'
      Origin = 'SALES.ORDER_DATE'
      Required = True
      Visible = False
    end
    object IBQuery3SHIP_DATE: TDateTimeField
      FieldName = 'SHIP_DATE'
      Origin = 'SALES.SHIP_DATE'
      Visible = False
    end
    object IBQuery3DATE_NEEDED: TDateTimeField
      FieldName = 'DATE_NEEDED'
      Origin = 'SALES.DATE_NEEDED'
      Visible = False
    end
    object IBQuery3PAID: TIBStringField
      FieldName = 'PAID'
      Origin = 'SALES.PAID'
      Visible = False
      FixedChar = True
      Size = 1
    end
    object IBQuery3QTY_ORDERED: TIntegerField
      FieldName = 'QTY_ORDERED'
      Origin = 'SALES.QTY_ORDERED'
      Required = True
      Visible = False
    end
    object IBQuery3TOTAL_VALUE: TIBBCDField
      FieldName = 'TOTAL_VALUE'
      Origin = 'SALES.TOTAL_VALUE'
      Required = True
      Visible = False
      Precision = 9
      Size = 2
    end
    object IBQuery3DISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = 'SALES.DISCOUNT'
      Required = True
      Visible = False
    end
    object IBQuery3ITEM_TYPE: TIBStringField
      FieldName = 'ITEM_TYPE'
      Origin = 'SALES.ITEM_TYPE'
      Visible = False
      Size = 12
    end
    object IBQuery3AGED: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'AGED'
      Origin = 'SALES.AGED'
      ReadOnly = True
      Visible = False
    end
    object IBQuery3FN: TStringField
      DisplayLabel = 'Full Name'
      DisplayWidth = 35
      FieldKind = fkLookup
      FieldName = 'FN'
      LookupDataSet = Employee
      LookupKeyFields = 'EMP_NO'
      LookupResultField = 'FULL_NAME'
      KeyFields = 'SALES_REP'
      Size = 37
      Lookup = True
    end
    object IBQuery3CUSTOMER: TStringField
      FieldKind = fkLookup
      FieldName = 'CUSTOMER'
      LookupDataSet = Customer
      LookupKeyFields = 'CUST_NO'
      LookupResultField = 'CUSTOMER'
      KeyFields = 'CUST_NO'
      Size = 25
      Lookup = True
    end
  end
end
