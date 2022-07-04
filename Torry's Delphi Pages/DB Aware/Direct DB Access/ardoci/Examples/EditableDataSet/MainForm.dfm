object FormMain: TFormMain
  Left = 281
  Top = 180
  Width = 588
  Height = 376
  Caption = 'Example of Editable DataSet'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid: TDBGrid
    Left = 0
    Top = 29
    Width = 580
    Height = 320
    Align = alClient
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 580
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavigator: TDBNavigator
      Left = 0
      Top = 0
      Width = 580
      Height = 29
      DataSource = DataSource
      Align = alClient
      TabOrder = 0
    end
  end
  object OraSQL: TOraSQL
    UpdateSQLs = AOraUpdateSQL
    Database = OraDB
    FetchCount = 100
    SQL.Strings = (
      'select * from test')
    Params = <>
    OnUpdateRecord = OraSQLUpdateRecord
    Left = 148
    Top = 196
  end
  object DataSource: TDataSource
    DataSet = OraSQL
    Left = 228
    Top = 196
  end
  object OraDB: TOraDB
    DBLogin = 'test'
    DBPassword = 'a'
    DBServer = 'RRR'
    OraTransIsolationLevel = tiDefault
    OraSessionIsolationLevel = siDefault
    RollbackOnDisconnect = False
    ConnectAs = caNormal
    SQLTrace = stDefault
    Left = 320
    Top = 196
  end
  object AOraUpdateSQL: TAOraUpdateSQL
    ModifySQL.Strings = (
      'update test'
      'set'
      '  F1 = :F1,'
      '  F2 = :F2,'
      '  F3 = :F3'
      'where'
      '  F1 = :OLD_F1 and'
      '  F2 = :OLD_F2 and'
      '  F3 = :OLD_F3')
    InsertSQL.Strings = (
      'insert into test'
      '  (F1, F2, F3)'
      'values'
      '  (:F1, :F2, :F3)')
    DeleteSQL.Strings = (
      'delete from test'
      'where'
      '  F1 = :OLD_F1 and'
      '  F2 = :OLD_F2 and'
      '  F3 = :OLD_F3')
    Left = 400
    Top = 196
  end
end
