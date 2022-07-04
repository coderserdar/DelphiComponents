object MainForm: TMainForm
  Left = 191
  Top = 107
  ActiveControl = CB_ACTIVETBL
  BorderStyle = bsDialog
  Caption = 'Degisy Data Demo'
  ClientHeight = 285
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 19
    Top = 17
    Width = 59
    Height = 13
    Caption = 'Active table:'
  end
  object Label2: TLabel
    Left = 19
    Top = 248
    Width = 25
    Height = 13
    Caption = 'Filter:'
  end
  object GRID: TDBGrid
    Left = 19
    Top = 47
    Width = 401
    Height = 181
    DataSource = DS
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object CB_ACTIVETBL: TComboBox
    Left = 83
    Top = 13
    Width = 337
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = CB_ACTIVETBLChange
    Items.Strings = (
      'Clarion - Product.dat'
      'DBase - Product.dbf'
      'Degisy - Product.dda'
      'Paradox - Product.db')
  end
  object CB_FILTER: TComboBox
    Left = 53
    Top = 246
    Width = 367
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    OnClick = CB_FILTERClick
    OnKeyDown = CB_FILTERKeyDown
    Items.Strings = (
      'pos('#39'Toner'#39',NAME)>0'
      '(SalePrice>3)and(SalePrice<=9.75)'
      'UOM='#39'CTG'#39)
  end
  object DDA_DB: TDsDatabase
    DatabaseName = '..\Data\'
    Left = 43
    Top = 121
  end
  object TBL_CLR: TDsClrTable
    Database = DDA_DB
    Exclusive = False
    Filtered = True
    TableName = 'product.dat'
    ReadOnly = False
    Left = 43
    Top = 149
  end
  object TBL_DDA: TDsDdaTable
    Database = DDA_DB
    Exclusive = False
    Filtered = True
    ReadOnly = False
    TableName = 'product.dda'
    Left = 71
    Top = 149
  end
  object TBL_DBF: TDsDbfTable
    Database = DDA_DB
    Exclusive = False
    Filtered = True
    TableName = 'product.dbf'
    ReadOnly = False
    Left = 43
    Top = 177
  end
  object TBL_PDX: TDsPdxTable
    Database = DDA_DB
    Exclusive = False
    Filtered = True
    TableName = 'product.db'
    ReadOnly = False
    Left = 71
    Top = 177
  end
  object DS: TDataSource
    Left = 71
    Top = 121
  end
end
