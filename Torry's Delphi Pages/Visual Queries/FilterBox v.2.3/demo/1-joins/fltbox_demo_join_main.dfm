object MainForm: TMainForm
  Left = 213
  Top = 133
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Joins Demo with FilterBox'
  ClientHeight = 481
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = '1. Select Tables:'
  end
  object Label2: TLabel
    Left = 296
    Top = 8
    Width = 75
    Height = 13
    Caption = '2. Select Fields:'
  end
  object Label3: TLabel
    Left = 8
    Top = 336
    Width = 24
    Height = 13
    Caption = 'SQL:'
  end
  object SpeedButton1: TButton
    Left = 456
    Top = 448
    Width = 121
    Height = 25
    Caption = 'Show Data Structure'
    TabOrder = 5
    OnClick = SpeedButton1Click
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 232
    Width = 569
    Height = 97
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object PSCFltBox1: TPSCFltBox
    Left = 8
    Top = 128
    Width = 569
    Height = 97
    BorderStyle = bsNone
    BevelInner = bvNone
    BevelOuter = bvRaised
    BevelKind = bkFlat
    Options = [fboSortFields, fboSortConditions, fboCanAdd, fboCanDelete, fboCanIndent, fboCanOpen, fboCanSave, fboCanSaveToMem, fboCanLoadFromMem, fboLookupDSOpen, fboHideFindRecord, fboAutoSizePopups]
    TabOrder = 1
    FilterSource = PSCFltBld1
  end
  object CheckListBox1: TCheckListBox
    Left = 8
    Top = 24
    Width = 281
    Height = 97
    OnClickCheck = CheckListBox1ClickCheck
    ItemHeight = 13
    TabOrder = 2
    OnClick = CheckListBox1Click
  end
  object CheckListBox2: TCheckListBox
    Left = 296
    Top = 24
    Width = 281
    Height = 97
    OnClickCheck = CheckListBox2ClickCheck
    ItemHeight = 13
    TabOrder = 3
  end
  object SQLMemo: TMemo
    Left = 8
    Top = 352
    Width = 569
    Height = 89
    ReadOnly = True
    TabOrder = 4
  end
  object CheckBox_ReOpen: TCheckBox
    Left = 8
    Top = 456
    Width = 169
    Height = 17
    Caption = 'Reopen when SQL changed'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object Button1: TButton
    Left = 344
    Top = 448
    Width = 106
    Height = 25
    Caption = 'Formatting...'
    TabOrder = 7
    OnClick = Button1Click
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;User ID=Admin;Data Source=C:\Us' +
      'ers\sergiy2\Downloads\documents-export-2015-06-01\EnricoRav-pute' +
      'rsoft.sdk-filterbox-5dd99289c459\demo\1-joins\join_demo.mdb;Mode' +
      '=Share Deny None;Persist Security Info=False;Jet OLEDB:System da' +
      'tabase="";Jet OLEDB:Registry Path="";Jet OLEDB:Database Password' +
      '="";Jet OLEDB:Engine Type=5;Jet OLEDB:Database Locking Mode=1;Je' +
      't OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Global Bulk Transact' +
      'ions=1;Jet OLEDB:New Database Password="";Jet OLEDB:Create Syste' +
      'm Database=False;Jet OLEDB:Encrypt Database=False;Jet OLEDB:Don'#39 +
      't Copy Locale on Compact=False;Jet OLEDB:Compact Without Replica' +
      ' Repair=False;Jet OLEDB:SFP=False'
    LoginPrompt = False
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 56
    Top = 400
  end
  object PSCFltBld1: TPSCFltBld
    FilterOptions = [fboDoubleQuoteInValue, fboUseUCASEioUPPER, fboUseADOFieldNameMask]
    AddTableNames = True
    TableNames.Strings = (
      'country'
      'customer'
      'orders')
    DataSet = ADOQuery1
    SQLHead.Strings = (
      
        'SELECT * FROM CUSTOMER INNER JOIN ORDERS ON ORDERS.CUSTNO = CUST' +
        'OMER.CUSTNO, COUNTRY WHERE COUNTRY.IDCOUNTRY = CUSTOMER.IDCOUNTR' +
        'Y')
    OnQuoteStr = PSCFltBld1QuoteStr
    SQLDateTimeFormat.ShortDateFormat = 'MM/dd/yyyy'
    OnChange = PSCFltBld1Change
    Left = 256
    Top = 368
  end
  object DataSource1: TDataSource
    DataSet = ADOQuery1
    Left = 208
    Top = 272
  end
  object ADOQuery1: TADOQuery
    Connection = ADOConnection1
    CursorType = ctStatic
    AfterOpen = ADOQuery1AfterOpen
    ParamCheck = False
    Parameters = <>
    Left = 112
    Top = 328
  end
  object ADOTableCountry: TADOTable
    Connection = ADOConnection1
    IndexName = 'ByName'
    TableName = 'country'
    Left = 168
    Top = 400
    object ADOTableCountryIDCountry: TAutoIncField
      FieldName = 'IDCountry'
      ReadOnly = True
    end
    object ADOTableCountryName: TWideStringField
      FieldName = 'Name'
      Size = 24
    end
  end
  object ADOTableCustomer: TADOTable
    Connection = ADOConnection1
    IndexName = 'ByCompany'
    TableName = 'customer'
    Left = 344
    Top = 336
    object ADOTableCustomerCustNo: TFloatField
      FieldName = 'CustNo'
    end
    object ADOTableCustomerCompany: TWideStringField
      FieldName = 'Company'
      Size = 30
    end
  end
  object PSCGridColors1: TPSCGridColors
    DataSet = ADOQuery1
    RecordsInfo = <
      item
        FilterSource = PSCFltBld1
        FontStyle = [FS_Bold]
        Active = True
        Name = 'asasas'
      end>
    Left = 408
    Top = 176
  end
end
