object Form1: TForm1
  Left = 487
  Top = 129
  Width = 407
  Height = 262
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 16
    Top = 40
    Width = 361
    Height = 177
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 16
    Top = 8
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    AutoCommit = False
    Left = 8
    Top = 8
  end
  object OCIStoredProc1: TOCIStoredProc
    Prepared = True
    Params = <
      item
        OName = ':C'
        ODataType = otCursor
        OParamType = odInOut
        ODataSize = 4
      end>
    OPackageName = 'TEST_REFCRS'
    OProcedureName = 'P'
    Left = 40
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = OCINestedDataSet1
    Left = 72
    Top = 8
  end
  object OCINestedDataSet1: TOCINestedDataSet
    Active = True
    Prepared = True
    Filters = <
      item
        Name = 'NCSysDefault'
        Active = True
      end>
    UpdateObject = OCIUpdateSQL1
    ParamDataSet = OCIStoredProc1
    ParamName = 'C'
    Left = 40
    Top = 40
    object OCINestedDataSet1PRODUCT_ID: TFloatField
      FieldName = 'PRODUCT_ID'
      Origin = 'PRODUCT_ID'
      Required = True
    end
    object OCINestedDataSet1DESCRIPTION: TStringField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      Size = 30
    end
  end
  object OCIUpdateSQL1: TOCIUpdateSQL
    AutoBuildKinds = [skUpdate, skInsert, skDelete, skRefresh]
    SQLUpdate.Strings = (
      'update product '
      'set description = :NEW_DESCRIPTION'
      'where product_id = :OLD_PRODUCT_ID')
    SQLInsert.Strings = (
      'insert into product (product_id, description)'
      'values (:NEW_PRODUCT_ID, :NEW_DESCRIPTION)')
    SQLDelete.Strings = (
      'delete from product where product_id = :OLD_PRODUCT_ID')
    SQLRefresh.Strings = (
      
        'select description from product where product_id = :OLD_PRODUCT_' +
        'ID')
    Left = 40
    Top = 72
  end
end
