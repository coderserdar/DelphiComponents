object Form1: TForm1
  Left = 318
  Top = 121
  Width = 696
  Height = 480
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
    Left = 40
    Top = 96
    Width = 320
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
    Left = 48
    Top = 288
    Width = 320
    Height = 120
    DataSource = DataSource2
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object BitBtn1: TBitBtn
    Left = 232
    Top = 240
    Width = 75
    Height = 25
    Caption = 'BitBtn1'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 288
    Top = 64
    Width = 75
    Height = 25
    Caption = 'BitBtn2'
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object DataSource1: TDataSource
    DataSet = AstaClientDataSet1
    Left = 80
    Top = 40
  end
  object DataSource2: TDataSource
    DataSet = AstaClientDataSet2
    Left = 88
    Top = 248
  end
  object Timer1: TTimer
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 240
    Top = 88
  end
  object AstaClientSocket2: TAstaClientSocket
    Active = False
    Address = '127.0.0.1'
    ClientType = ctNonBlocking
    Port = 9000
    SQLTransactionStart = 'BEGIN TRANSACTION'
    SQLTransactionEnd = 'COMMIT'
    UpdateSQLSyntax = usOracle
    SQLDialect = sqlOracle
    Left = 8
    Top = 8
  end
  object AstaClientDataSet1: TAstaClientDataSet
    Active = True
    Constraints = <>
    EditMode = 'Read Only'
    AstaClientSocket = AstaClientSocket2
    SQL.Strings = (
      'SELECT * From SCOTT.EMP')
    Params = <>
    Left = 48
    Top = 40
    DetailFilter = ''
    FastFields = (
      'EMPNO,6,0'
      'ENAME,1,10'
      'JOB,1,9'
      'MGR,6,0'
      'HIREDATE,11,0'
      'SAL,6,0'
      'COMM,6,0'
      'DEPTNO,6,0')
    FMultiTable = ()
    OracleSequence = ''
    UpdateMethod = umManual
  end
  object AstaClientDataSet2: TAstaClientDataSet
    Active = True
    Constraints = <>
    EditMode = 'Read Only'
    AstaClientSocket = AstaClientSocket2
    SQL.Strings = (
      'SELECT * From SCOTT.EMP')
    Params = <>
    Left = 56
    Top = 248
    DetailFilter = ''
    FastFields = (
      'EMPNO,6,0'
      'ENAME,1,10'
      'JOB,1,9'
      'MGR,6,0'
      'HIREDATE,11,0'
      'SAL,6,0'
      'COMM,6,0'
      'DEPTNO,6,0')
    FMultiTable = ()
    OracleSequence = ''
    UpdateMethod = umManual
  end
end
