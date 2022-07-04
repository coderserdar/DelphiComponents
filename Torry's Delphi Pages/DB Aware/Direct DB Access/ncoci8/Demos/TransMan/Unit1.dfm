object Form1: TForm1
  Left = 285
  Top = 132
  Width = 556
  Height = 337
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
  object SpeedButton1: TSpeedButton
    Left = 8
    Top = 232
    Width = 121
    Height = 22
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'Close/Open query'
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 144
    Top = 232
    Width = 161
    Height = 22
    GroupIndex = 2
    Caption = 'Select First querys && TM set'
    OnClick = SpeedButton2Click
  end
  object SpeedButton3: TSpeedButton
    Left = 144
    Top = 256
    Width = 161
    Height = 22
    GroupIndex = 2
    Caption = 'Select Second querys && TM set'
    OnClick = SpeedButton3Click
  end
  object SpeedButton4: TSpeedButton
    Left = 8
    Top = 256
    Width = 121
    Height = 22
    Caption = 'ExecSQL query'
    OnClick = SpeedButton4Click
  end
  object SpeedButton5: TSpeedButton
    Left = 320
    Top = 256
    Width = 65
    Height = 22
    Caption = 'Commit TX'
    OnClick = SpeedButton5Click
  end
  object SpeedButton6: TSpeedButton
    Left = 384
    Top = 232
    Width = 73
    Height = 22
    Caption = 'Rollback TX'
    OnClick = SpeedButton6Click
  end
  object SpeedButton7: TSpeedButton
    Left = 320
    Top = 232
    Width = 65
    Height = 22
    Caption = 'Start TX'
    OnClick = SpeedButton7Click
  end
  object SpeedButton8: TSpeedButton
    Left = 385
    Top = 256
    Width = 73
    Height = 22
    Caption = 'Suspend TX'
    OnClick = SpeedButton8Click
  end
  object SpeedButton9: TSpeedButton
    Left = 457
    Top = 232
    Width = 73
    Height = 22
    Caption = 'Resume TX'
    OnClick = SpeedButton9Click
  end
  object SpeedButton10: TSpeedButton
    Left = 458
    Top = 256
    Width = 73
    Height = 22
    Caption = 'Forget TX'
    OnClick = SpeedButton10Click
  end
  object Label1: TLabel
    Left = 8
    Top = 288
    Width = 100
    Height = 13
    Caption = 'Update emp with No:'
  end
  object Label2: TLabel
    Left = 240
    Top = 288
    Width = 47
    Height = 13
    Caption = 'set salary:'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 16
    Width = 529
    Height = 209
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Edit1: TEdit
    Left = 112
    Top = 283
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 293
    Top = 283
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object NCSQLMonitorClient1: TNCSQLMonitorClient
    Active = True
    ClientObjName = '1'
    Left = 8
    Top = 40
  end
  object OCIDatabase1: TOCIDatabase
    DatabaseName = '1'
    InitModes = [dmDistributed]
    UserName = 'scott'
    Password = 'tiger'
    Connected = True
    AutoCommit = False
    SQLMonitor = NCSQLMonitorClient1
    Left = 8
    Top = 8
  end
  object qrExec1: TOCIQuery
    Params = <
      item
        OName = ':SAL'
        ODataType = otFloat
        OParamType = odIn
        ODataSize = 8
      end
      item
        OName = ':EMPNO'
        ODataType = otFloat
        OParamType = odIn
        ODataSize = 8
      end>
    DatabaseName = '1'
    TransactionManager = OCITransactionManager1
    SQL.Strings = (
      'update emp set sal = :SAL where empno = :EMPNO')
    Left = 72
    Top = 8
  end
  object OCITransactionManager1: TOCITransactionManager
    DatabaseName = '1'
    AutoCommit = False
    XID.GTRID = 'First'
    XID.BQUAL = '1'
    Left = 40
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = qrSel1
    Left = 184
    Top = 8
  end
  object qrExec2: TOCIQuery
    Params = <
      item
        OName = ':SAL'
        ODataType = otFloat
        OParamType = odIn
        ODataSize = 8
      end
      item
        OName = ':EMPNO'
        ODataType = otFloat
        OParamType = odIn
        ODataSize = 8
      end>
    DatabaseName = '1'
    TransactionManager = OCITransactionManager2
    SQL.Strings = (
      'update emp set sal = :SAL where empno = :EMPNO')
    Left = 72
    Top = 40
  end
  object OCITransactionManager2: TOCITransactionManager
    DatabaseName = '1'
    AutoCommit = False
    XID.GTRID = 'Two'
    XID.BQUAL = '1'
    Left = 40
    Top = 40
  end
  object qrSel1: TOCIQuery
    DatabaseName = '1'
    TransactionManager = OCITransactionManager1
    SQL.Strings = (
      'select * from emp')
    Left = 104
    Top = 8
  end
  object qrSel2: TOCIQuery
    DatabaseName = '1'
    TransactionManager = OCITransactionManager2
    SQL.Strings = (
      'select * from emp')
    Left = 104
    Top = 40
  end
end
