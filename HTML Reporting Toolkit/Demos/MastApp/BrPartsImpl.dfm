object BrPartsFrm: TBrPartsFrm
  Left = 306
  Top = 252
  Width = 597
  Height = 400
  AxBorderStyle = afbNone
  Caption = 'BrPartsFrm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PartsGrid: TDBGrid
    Left = 0
    Top = 37
    Width = 589
    Height = 336
    Align = alClient
    BorderStyle = bsNone
    DataSource = PartsSource
    Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgRowSelect]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PartNo'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Width = 178
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OnHand'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OnOrder'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BackOrd'
        Width = 64
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 37
    HelpContext = 4
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object ActivateBtn: TSpeedButton
      Left = 284
      Top = 4
      Width = 85
      Height = 25
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Backorders'
      OnClick = ActivateBtnClick
    end
    object Bevel1: TBevel
      Left = 3
      Top = 32
      Width = 583
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object Navigator: TDBNavigator
      Left = 8
      Top = 4
      Width = 135
      Height = 25
      DataSource = PartsSource
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object EditBtn: TButton
      Left = 177
      Top = 4
      Width = 74
      Height = 25
      Caption = '&Edit'
      ModalResult = 1
      TabOrder = 1
      OnClick = EditBtnClick
    end
  end
  object Parts: TTable
    Active = True
    CachedUpdates = True
    OnCalcFields = PartsCalcFields
    DatabaseName = 'DBDEMOS'
    TableName = 'PARTS'
    Left = 156
    Top = 29
    object PartsPartNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 8
      FieldName = 'PartNo'
      Required = True
    end
    object PartsDescription: TStringField
      DisplayWidth = 21
      FieldName = 'Description'
      Required = True
      Size = 30
    end
    object PartsVendorNo: TFloatField
      DisplayWidth = 9
      FieldName = 'VendorNo'
    end
    object PartsOnHand: TFloatField
      DisplayWidth = 9
      FieldName = 'OnHand'
    end
    object PartsOnOrder: TFloatField
      DisplayWidth = 10
      FieldName = 'OnOrder'
    end
    object PartsBackOrd: TBooleanField
      DisplayWidth = 9
      FieldKind = fkCalculated
      FieldName = 'BackOrd'
      DisplayValues = 'Yes;No'
      Calculated = True
    end
    object PartsCost: TCurrencyField
      DisplayWidth = 12
      FieldName = 'Cost'
    end
    object PartsListPrice: TCurrencyField
      DisplayWidth = 12
      FieldName = 'ListPrice'
    end
  end
  object PartsSource: TDataSource
    DataSet = Parts
    Left = 192
    Top = 32
  end
  object PartsQuery: TQuery
    OnCalcFields = PartsQueryCalcFields
    DatabaseName = 'DBDEMOS'
    SQL.Strings = (
      'select * from parts'
      ' where (parts.OnOrder > parts.OnHand)'
      '')
    Left = 124
    Top = 29
    object PartsQueryPartNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 8
      FieldName = 'PartNo'
    end
    object PartsQueryDescription: TStringField
      DisplayWidth = 21
      FieldName = 'Description'
      Size = 30
    end
    object PartsQueryVendorNo: TFloatField
      FieldName = 'VendorNo'
    end
    object PartsQueryOnHand: TFloatField
      DisplayWidth = 9
      FieldName = 'OnHand'
    end
    object PartsQueryOnOrder: TFloatField
      DisplayWidth = 10
      FieldName = 'OnOrder'
    end
    object PartsQueryBackOrd: TBooleanField
      DisplayWidth = 9
      FieldKind = fkCalculated
      FieldName = 'BackOrd'
      DisplayValues = 'Yes;No'
      Calculated = True
    end
    object PartsQueryCost: TCurrencyField
      FieldName = 'Cost'
    end
    object PartsQueryListPrice: TCurrencyField
      FieldName = 'ListPrice'
    end
  end
end
