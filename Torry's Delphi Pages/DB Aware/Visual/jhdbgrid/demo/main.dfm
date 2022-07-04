object fmMain: TfmMain
  Left = 228
  Top = 139
  Width = 414
  Height = 346
  Caption = 'JanH DBGrid Demo'
  Color = 16296824
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    406
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object lbMove: TLabel
    Left = 16
    Top = 293
    Width = 40
    Height = 13
    Caption = 'move by'
  end
  object lbRows: TLabel
    Left = 112
    Top = 293
    Width = 22
    Height = 13
    Caption = 'rows'
  end
  object JanHDBGrid: TJanHDBGrid
    Left = 8
    Top = 8
    Width = 389
    Height = 274
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    FixedColor = 16638672
    Options = [dgTitles, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    MoveByRows = 1
    ActiveRowColor = clBlue
    ActiveRowFontColor = clYellow
    SelectRowColor = clNavy
    SelectRowFontColor = clWhite
    OnMouseWheelDown = JanHDBGridMouseWheel
    OnMouseWheelUp = JanHDBGridMouseWheel
    Columns = <
      item
        Expanded = False
        FieldName = 'field1'
        Title.Caption = 'field 1'
        Visible = True
      end
      item
        Alignment = taRightJustify
        Expanded = False
        FieldName = 'field2'
        Title.Alignment = taRightJustify
        Title.Caption = 'field 2'
        Visible = True
      end>
  end
  object btClose: TButton
    Left = 300
    Top = 288
    Width = 96
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btCloseClick
  end
  object spinWheel: TSpinEdit
    Left = 62
    Top = 288
    Width = 42
    Height = 22
    MaxValue = 20
    MinValue = 1
    TabOrder = 2
    Value = 1
  end
  object DataSource: TDataSource
    DataSet = ADOTable
    Left = 128
    Top = 80
  end
  object ADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;User ID=Admin;Data Source=K:\Ja' +
      'nH\JanH_DBGrid\demo\database.mdb;Mode=Share Deny None;Extended P' +
      'roperties="";Persist Security Info=False;Jet OLEDB:System databa' +
      'se="";Jet OLEDB:Registry Path="";Jet OLEDB:Database Password="";' +
      'Jet OLEDB:Engine Type=5;Jet OLEDB:Database Locking Mode=1;Jet OL' +
      'EDB:Global Partial Bulk Ops=2;Jet OLEDB:Global Bulk Transactions' +
      '=1;Jet OLEDB:New Database Password="";Jet OLEDB:Create System Da' +
      'tabase=False;Jet OLEDB:Encrypt Database=False;Jet OLEDB:Don'#39't Co' +
      'py Locale on Compact=False;Jet OLEDB:Compact Without Replica Rep' +
      'air=False;Jet OLEDB:SFP=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 216
    Top = 80
  end
  object ADOTable: TADOTable
    Connection = ADOConnection
    CursorType = ctStatic
    TableName = 'main'
    Left = 216
    Top = 144
  end
end
