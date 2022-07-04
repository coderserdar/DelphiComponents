object Form1: TForm1
  Left = 305
  Top = 110
  Width = 485
  Height = 308
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 457
    Height = 120
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 8
    Top = 136
    Width = 457
    Height = 137
    DataSource = DataSource2
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    SQLMonitor = NCSQLMonitorClient1
    Left = 16
    Top = 16
  end
  object OCIQuery1: TOCIQuery
    Active = True
    Prepared = True
    SQL.Strings = (
      
        'SELECT t.code, t.rowid, CURSOR(SELECT * FROM TABLE(t.content))  ' +
        'crs'
      'FROM ParentTable t')
    Left = 16
    Top = 48
    object OCIQuery1CODE: TFloatField
      FieldName = 'CODE'
      Origin = 'CODE'
      Required = True
    end
    object OCIQuery1ROWID: TStringField
      FieldName = 'ROWID'
      Origin = 'ROWID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
      Size = 18
    end
    object OCIQuery1CRS: TDataSetField
      FieldName = 'CRS'
      Origin = 'CRS'
    end
  end
  object OCINestedDataSet1: TOCINestedDataSet
    Active = True
    Prepared = True
    DataSetField = OCIQuery1CRS
    Left = 16
    Top = 80
    object OCINestedDataSet1NUM: TFloatField
      FieldName = 'NUM'
      Origin = 'NUM'
    end
    object OCINestedDataSet1STR: TStringField
      FieldName = 'STR'
      Origin = 'STR'
      Size = 30
    end
    object OCINestedDataSet1DAT: TDateTimeField
      FieldName = 'DAT'
      Origin = 'DAT'
    end
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 48
    Top = 48
  end
  object DataSource2: TDataSource
    DataSet = OCINestedDataSet1
    Left = 48
    Top = 80
  end
  object NCSQLMonitorClient1: TNCSQLMonitorClient
    Left = 48
    Top = 16
  end
end
