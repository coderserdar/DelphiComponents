object MainForm: TMainForm
  Left = 194
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Demo: Create Table'
  ClientHeight = 132
  ClientWidth = 290
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
  object Label1: TLabel
    Left = 9
    Top = 8
    Width = 53
    Height = 13
    Caption = '&Table type:'
    FocusControl = CB_TABLETYPE
  end
  object CB_TABLETYPE: TComboBox
    Left = 9
    Top = 27
    Width = 269
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'Clarion 2.1'
      'dBase III'
      'dBase IV'
      'dBase V'
      'dBase VII'
      'Degisy 1.0'
      'Paradox 3.5'
      'Paradox 4.0'
      'Paradox 5.0'
      'Paradox 7.0')
  end
  object Button1: TButton
    Left = 9
    Top = 64
    Width = 269
    Height = 25
    Caption = 'Create table [ Method #1 ]'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 9
    Top = 95
    Width = 269
    Height = 25
    Caption = 'Create table [ Method #2 ]'
    TabOrder = 2
    OnClick = Button2Click
  end
  object DemoDb: TDsDatabase
    DatabaseName = '.\Data\'
    Left = 100
    Top = 8
  end
  object TBL_CLR_1: TDsClrTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    TableName = 'Test1.dat'
    ReadOnly = False
    Left = 128
    Top = 8
    object TBL_CLR_1IDENT: TIntegerField
      FieldName = 'IDENT'
    end
    object TBL_CLR_1CODE: TStringField
      FieldName = 'CODE'
      Size = 10
    end
    object TBL_CLR_1PHONE: TStringField
      FieldName = 'PHONE'
    end
    object TBL_CLR_1COUNTRY: TStringField
      FieldName = 'COUNTRY'
      Size = 30
    end
    object TBL_CLR_1STATE: TStringField
      FieldName = 'STATE'
    end
  end
  object TBL_DDA_1: TDsDdaTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    ReadOnly = False
    TableName = 'Test1.dda'
    Left = 156
    Top = 8
    object TBL_DDA_1AUTO: TAutoIncField
      FieldName = 'AUTO'
    end
    object TBL_DDA_1IDENT: TIntegerField
      FieldName = 'IDENT'
    end
    object TBL_DDA_1CODE: TStringField
      FieldName = 'CODE'
      Size = 10
    end
    object TBL_DDA_1PHONE: TStringField
      FieldName = 'PHONE'
    end
    object TBL_DDA_1COUNTRY: TStringField
      FieldName = 'COUNTRY'
      Size = 30
    end
    object TBL_DDA_1STATE: TStringField
      FieldName = 'STATE'
    end
  end
  object TBL_DBF_1: TDsDbfTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    TableName = 'Test1.dbf'
    ReadOnly = False
    Left = 184
    Top = 8
    object TBL_DBF_1IDENT: TIntegerField
      FieldName = 'IDENT'
    end
    object TBL_DBF_1CODE: TStringField
      FieldName = 'CODE'
      Size = 10
    end
    object TBL_DBF_1PHONE: TStringField
      FieldName = 'PHONE'
    end
    object TBL_DBF_1COUNTRY: TStringField
      FieldName = 'COUNTRY'
      Size = 30
    end
    object TBL_DBF_1STATE: TStringField
      FieldName = 'STATE'
    end
  end
  object TBL_PDX_1: TDsPdxTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    TableName = 'Test1.db'
    ReadOnly = False
    Left = 212
    Top = 8
    object TBL_PDX_1AUTO: TAutoIncField
      FieldName = 'AUTO'
    end
    object TBL_PDX_1IDENT: TIntegerField
      FieldName = 'IDENT'
    end
    object TBL_PDX_1CODE: TStringField
      FieldName = 'CODE'
      Size = 10
    end
    object TBL_PDX_1PHONE: TStringField
      FieldName = 'PHONE'
    end
    object TBL_PDX_1COUNTRY: TStringField
      FieldName = 'COUNTRY'
      Size = 30
    end
    object TBL_PDX_1STATE: TStringField
      FieldName = 'STATE'
    end
  end
  object TBL_CLR_2: TDsClrTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    TableName = 'Test2.dat'
    ReadOnly = False
    Left = 128
    Top = 36
  end
  object TBL_DDA_2: TDsDdaTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    ReadOnly = False
    TableName = 'Test2.dda'
    Left = 156
    Top = 36
  end
  object TBL_DBF_2: TDsDbfTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    TableName = 'Test2.dbf'
    ReadOnly = False
    Left = 184
    Top = 36
  end
  object TBL_PDX_2: TDsPdxTable
    CodePage = ascii_ANSI
    Database = DemoDb
    Exclusive = False
    TableName = 'Test2.db'
    ReadOnly = False
    Left = 212
    Top = 36
  end
end
