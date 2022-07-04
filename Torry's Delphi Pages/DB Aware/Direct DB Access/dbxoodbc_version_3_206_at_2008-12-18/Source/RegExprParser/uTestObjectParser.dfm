object frmMain: TfrmMain
  Left = 175
  Top = 178
  ActiveControl = cmFullName
  Caption = 'OpenODBC dbExpress RegExp Object Name Parser'
  ClientHeight = 495
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 277
    Width = 36
    Height = 13
    Caption = 'Catalog'
  end
  object Label2: TLabel
    Left = 10
    Top = 299
    Width = 39
    Height = 13
    Caption = 'Schema'
  end
  object Label3: TLabel
    Left = 12
    Top = 322
    Width = 31
    Height = 13
    Caption = 'Object'
  end
  object Label4: TLabel
    Left = 12
    Top = 72
    Width = 60
    Height = 13
    Caption = 'Quote Char: '
  end
  object Shape1: TShape
    Left = 8
    Top = 216
    Width = 429
    Height = 5
  end
  object btnDecode: TButton
    Left = 8
    Top = 233
    Width = 102
    Height = 25
    Caption = 'Decode -> :'
    TabOrder = 6
    OnClick = btnDecodeClick
  end
  object edCatalog: TEdit
    Left = 54
    Top = 274
    Width = 383
    Height = 21
    TabOrder = 7
  end
  object edSchema: TEdit
    Left = 54
    Top = 298
    Width = 383
    Height = 21
    TabOrder = 8
  end
  object edObject: TEdit
    Left = 54
    Top = 322
    Width = 383
    Height = 21
    TabOrder = 9
  end
  object btnClearDecore: TButton
    Left = 335
    Top = 349
    Width = 102
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = btnClearDecoreClick
  end
  object cmFullName: TComboBox
    Left = 143
    Top = 6
    Width = 294
    Height = 21
    DropDownCount = 30
    ItemHeight = 13
    TabOrder = 0
    Text = '"catalog"."schema"."table"'
  end
  object rgParser: TRadioGroup
    Left = 8
    Top = 88
    Width = 117
    Height = 113
    Caption = '  Parser  '
    ItemIndex = 0
    Items.Strings = (
      'Default'
      'MSSQL'
      'FileName'
      'Informix'
      'Oracle')
    TabOrder = 4
  end
  object edQuote: TEdit
    Left = 80
    Top = 67
    Width = 45
    Height = 21
    TabOrder = 3
    Text = '"'
  end
  object btnNext: TButton
    Left = 143
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Next >'
    TabOrder = 1
    OnClick = btnNextClick
  end
  object mDefaultTestedNames: TMemo
    Left = 305
    Top = 33
    Width = 132
    Height = 58
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBtnShadow
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      '//  DEFAULT'
      '"catalog"."schema"."table"'
      'catalog."schema"."table"'
      'catalog.schema."table"'
      'catalog.schema.table'
      '"catalog".schema."table"'
      '"catalog"."schema".table'
      '"schema"."table"'
      'schema."table"'
      '"schema".table'
      'schema.table'
      '"table"'
      'table'
      '""'
      '.""'
      '""."".""'
      '..table name'
      '""..table name'
      '".".table name'
      '"."".table name'
      '"".".table name'
      '"catalog name"."".""'
      '"catalog"."schema name".""'
      '"catalog"."schema name"."table'
      'name"'
      '"catalog".""."table name"'
      '"schema"."""table name"'
      'catalog.'#39'schema name'#39'.'#39'table name'#39
      '"\\my object name\\"'
      '\\my object name\\'
      '//  INFORMIX'
      'catalog:schema.table'
      'catalog::schema.table'
      'schema.table'
      'catalog:table'
      'table'
      'catalog:'
      ':schema.table'
      ':table'
      '//  MSSQL'
      '"catalog"."schema"."table"'
      '"schema"."table"'
      '[schema].[table]'
      '[table]'
      '"table"'
      'table'
      'schema.table'
      '[schema].table')
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 16
    Visible = False
    WordWrap = False
  end
  object btnPrevious: TButton
    Left = 224
    Top = 33
    Width = 75
    Height = 25
    Caption = '< Previous'
    TabOrder = 2
    OnClick = btnPreviousClick
  end
  object btnEncode: TButton
    Left = 8
    Top = 372
    Width = 102
    Height = 25
    Caption = 'Encode -> :'
    TabOrder = 11
    OnClick = btnEncodeClick
  end
  object edEncode: TEdit
    Left = 54
    Top = 415
    Width = 383
    Height = 21
    TabOrder = 12
  end
  object btnClearEncode: TButton
    Left = 335
    Top = 442
    Width = 102
    Height = 25
    Caption = 'Clear'
    TabOrder = 13
    OnClick = Button6_Click
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 8
    Width = 126
    Height = 17
    Caption = 'Parsing Full Object Name:'
    TabOrder = 14
  end
  object StaticText2: TStaticText
    Left = 143
    Top = 74
    Width = 73
    Height = 17
    Caption = 'RegExpr New:'
    TabOrder = 15
  end
  object edRegExprNew: TMemo
    Left = 143
    Top = 93
    Width = 294
    Height = 108
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
  end
end
