object Form1: TForm1
  Left = 372
  Top = 195
  Width = 511
  Height = 261
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
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 200
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 457
    Height = 177
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 256
    Top = 200
    Width = 89
    Height = 25
    Caption = 'Load BLOB SP'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 352
    Top = 200
    Width = 89
    Height = 25
    Caption = 'Load BLOB Fld'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Database: TOCIDatabase
    CachedUpdates = False
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    SQLMonitor = NCSQLMonitorClient1
    Left = 16
    Top = 8
  end
  object Query: TOCIQuery
    Active = True
    Prepared = True
    UpdateObject = OCIUpdateSQL1
    SQL.Strings = (
      'select t.*, rowid from test_blob2 t')
    Left = 16
    Top = 40
  end
  object DataSource1: TDataSource
    DataSet = Query
    Left = 16
    Top = 104
  end
  object StoredProc: TOCIStoredProc
    Prepared = True
    Params = <
      item
        OName = ':AF1'
        ODataType = otFloat
        OParamType = odIn
        ODataSize = 8
      end
      item
        OName = ':AF2'
        ODataType = otBLOB
        OParamType = odOut
        ODataSize = 4
      end>
    OProcedureName = 'FETCH_BLOB'
    Left = 48
    Top = 40
  end
  object OCIUpdateSQL1: TOCIUpdateSQL
    LockMode = lmPessimistic
    LockPoint = lpImmediate
    Left = 16
    Top = 72
  end
  object NCSQLMonitorClient1: TNCSQLMonitorClient
    Left = 48
    Top = 8
  end
end
