object Form1: TForm1
  Left = 314
  Top = 96
  Width = 395
  Height = 398
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 344
    Width = 5
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 369
    Height = 120
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBImage1: TDBImage
    Left = 8
    Top = 136
    Width = 177
    Height = 129
    DataField = 'F2'
    DataSource = DataSource1
    TabOrder = 1
  end
  object DBMemo1: TDBMemo
    Left = 192
    Top = 136
    Width = 185
    Height = 129
    DataField = 'F3'
    DataSource = DataSource1
    TabOrder = 2
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 304
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 3
  end
  object Button1: TButton
    Left = 8
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 192
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 256
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Reopen'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Start TX'
    TabOrder = 7
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 88
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Commit TX'
    TabOrder = 8
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 168
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Rollback TX'
    TabOrder = 9
    OnClick = Button6Click
  end
  object OCIDatabase1: TOCIDatabase
    CachedUpdates = False
    UserName = 'demo'
    Password = 'demo'
    TransactionManager = OCITransactionManager1
    SQLMonitor = NCSQLMonitorClient1
    Left = 8
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    UpdateObject = OCIUpdateSQL1
    SQL.Strings = (
      'select t.*, t.rowid from test_blob t for update nowait')
    Left = 40
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 72
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'BMP'
    Filter = 'Bitmap files|*.bmp|All files|*.*'
    FilterIndex = 0
    Left = 88
    Top = 272
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'TXT'
    Filter = 'Text files|*.txt|All files|*.*'
    Left = 272
    Top = 272
  end
  object OCIUpdateSQL1: TOCIUpdateSQL
    LockMode = lmNone
    LockPoint = lpImmediate
    TableName = 'TEST_BLOB'
    Left = 40
    Top = 40
  end
  object OCITransactionManager1: TOCITransactionManager
    TransIsolation = tmSerializable
    ConnectAction = tcNone
    OnStateChanged = OCITransactionManager1StateChanged
    Left = 8
    Top = 40
  end
  object NCSQLMonitorClient1: TNCSQLMonitorClient
    Active = True
    ClientObjName = 'Default'
    Left = 72
    Top = 40
  end
end
