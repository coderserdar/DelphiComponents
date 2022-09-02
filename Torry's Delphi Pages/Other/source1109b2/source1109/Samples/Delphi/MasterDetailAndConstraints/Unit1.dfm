object Form1: TForm1
  Left = 213
  Top = 82
  Width = 755
  Height = 525
  Caption = 'Form1'
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 171
    Width = 747
    Height = 121
    Align = alTop
    BorderStyle = bsNone
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 146
    Width = 747
    Height = 25
    DataSource = DataSource1
    Align = alTop
    Flat = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 747
    Height = 146
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 200
      Top = 15
      Width = 26
      Height = 13
      Caption = 'Login'
    end
    object Label2: TLabel
      Left = 191
      Top = 42
      Width = 36
      Height = 13
      Caption = 'passwd'
    end
    object Label4: TLabel
      Left = 313
      Top = 27
      Width = 58
      Height = 13
      Caption = 'Alias or path'
    end
    object Label7: TLabel
      Left = 514
      Top = 46
      Width = 113
      Height = 13
      Caption = 'Check record - TimeOut'
    end
    object Label8: TLabel
      Left = 568
      Top = 86
      Width = 40
      Height = 13
      Caption = 'TimeOut'
    end
    object Label3: TLabel
      Left = 112
      Top = 45
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label5: TLabel
      Left = 112
      Top = 29
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Button1: TButton
      Left = 7
      Top = 8
      Width = 50
      Height = 22
      Caption = 'Open'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 227
      Top = 8
      Width = 77
      Height = 21
      TabOrder = 1
      Text = 'systemadmin'
    end
    object Edit2: TEdit
      Left = 227
      Top = 39
      Width = 78
      Height = 21
      TabOrder = 2
      Text = 'masteradmin'
    end
    object ServerProtocol: TRadioGroup
      Left = 478
      Top = 3
      Width = 153
      Height = 39
      Caption = 'ServerProtocol'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Single'
        'TCP'
        'IPX')
      TabOrder = 3
    end
    object Edit3: TEdit
      Left = 313
      Top = 8
      Width = 161
      Height = 21
      TabOrder = 4
      Text = 'data@localhost'
    end
    object Button2: TButton
      Left = 7
      Top = 35
      Width = 50
      Height = 22
      Caption = 'Close'
      TabOrder = 5
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 59
      Top = 35
      Width = 50
      Height = 22
      Caption = 'Port'
      TabOrder = 6
      OnClick = Button3Click
    end
    object SpinEdit1: TSpinEdit
      Left = 569
      Top = 60
      Width = 67
      Height = 22
      Increment = 100
      MaxValue = 10000000
      MinValue = -1
      TabOrder = 7
      Value = 500
      OnChange = SpinEdit1Change
    end
    object SpinEdit2: TSpinEdit
      Left = 570
      Top = 100
      Width = 65
      Height = 22
      Increment = 100
      MaxValue = 10000000
      MinValue = -1
      TabOrder = 8
      Value = 10000
      OnChange = SpinEdit2Change
    end
    object Button6: TButton
      Left = 505
      Top = 61
      Width = 60
      Height = 18
      Caption = 'StartTrans'
      TabOrder = 9
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 505
      Top = 82
      Width = 60
      Height = 18
      Caption = 'Commit'
      TabOrder = 10
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 505
      Top = 105
      Width = 60
      Height = 18
      Caption = 'Rollback'
      TabOrder = 11
      OnClick = Button8Click
    end
    object RadioGroup2: TRadioGroup
      Left = 266
      Top = 60
      Width = 234
      Height = 62
      Caption = 'Database record LockType (for all table etc..)'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'tlOptimisticNoWait'
        'tlOptimisticWait'
        'tlPessimisticNoWait'
        'tlPessimisticWait')
      TabOrder = 12
      OnClick = RadioGroup2Click
    end
    object CheckBox1: TCheckBox
      Left = 350
      Top = 124
      Width = 112
      Height = 19
      Caption = 'Locked before edit'
      TabOrder = 13
      OnClick = CheckBox1Click
    end
    object Memo1: TMemo
      Left = 633
      Top = 1
      Width = 113
      Height = 144
      Align = alRight
      Lines.Strings = (
        'Passwords here 1'
        'Passwords here 2'
        '...')
      TabOrder = 14
    end
    object Edit4: TEdit
      Left = 313
      Top = 39
      Width = 163
      Height = 21
      TabOrder = 15
    end
    object CheckBox2: TCheckBox
      Left = 60
      Top = 11
      Width = 109
      Height = 17
      Caption = 'Embeded server'
      Checked = True
      State = cbChecked
      TabOrder = 16
    end
    object CheckBox3: TCheckBox
      Left = 463
      Top = 124
      Width = 62
      Height = 17
      Caption = 'Flip order'
      TabOrder = 17
      OnClick = CheckBox3Click
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 59
      Width = 250
      Height = 62
      Caption = 'Table record LockType (for this table)'
      Columns = 2
      ItemIndex = 3
      Items.Strings = (
        'tluDataBase'
        'tluOptimisticNoWait'
        'tluOptimisticWait'
        'tluPessimisticNoWait'
        'tluPessimisticWait')
      TabOrder = 18
      OnClick = RadioGroup1Click
    end
    object Button4: TButton
      Left = 9
      Top = 124
      Width = 76
      Height = 18
      Hint = 'Individual lock record'
      Caption = 'Lock record'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 86
      Top = 124
      Width = 76
      Height = 18
      Hint = 'Individual lock record'
      Caption = 'unlock record'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = Button5Click
    end
    object Button9: TButton
      Left = 258
      Top = 124
      Width = 90
      Height = 18
      Hint = 'Individual lock record'
      Caption = 'Is locked record'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      OnClick = Button9Click
    end
    object CheckBox4: TCheckBox
      Left = 530
      Top = 124
      Width = 97
      Height = 17
      Caption = 'Support recno'
      TabOrder = 22
      OnClick = CheckBox4Click
    end
    object Button10: TButton
      Left = 163
      Top = 124
      Width = 93
      Height = 18
      Hint = 'Individual lock record'
      Caption = 'unlock record all'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 23
      OnClick = Button10Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 292
    Width = 747
    Height = 206
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 3
    object Splitter1: TSplitter
      Left = 441
      Top = 1
      Width = 3
      Height = 204
      Cursor = crHSplit
      AutoSnap = False
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 440
      Height = 204
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object DBGrid2: TDBGrid
        Left = 0
        Top = 25
        Width = 440
        Height = 90
        Align = alClient
        BorderStyle = bsNone
        DataSource = DataSource2
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBNavigator2: TDBNavigator
        Left = 0
        Top = 0
        Width = 440
        Height = 25
        DataSource = DataSource2
        Align = alTop
        Flat = True
        TabOrder = 1
      end
      object DBMemo1: TDBMemo
        Left = 0
        Top = 115
        Width = 440
        Height = 89
        Align = alBottom
        DataField = 'Notatka'
        DataSource = DataSource2
        TabOrder = 2
      end
    end
    object Panel4: TPanel
      Left = 444
      Top = 1
      Width = 302
      Height = 204
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel4'
      TabOrder = 1
      object DBGrid3: TDBGrid
        Left = 0
        Top = 25
        Width = 302
        Height = 90
        Align = alClient
        BorderStyle = bsNone
        DataSource = DataSource3
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object DBNavigator3: TDBNavigator
        Left = 0
        Top = 0
        Width = 302
        Height = 25
        DataSource = DataSource3
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
        Align = alTop
        Flat = True
        TabOrder = 1
      end
      object DBMemo2: TDBMemo
        Left = 0
        Top = 115
        Width = 302
        Height = 89
        Align = alBottom
        DataField = 'Notatka'
        DataSource = DataSource3
        TabOrder = 2
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = FSTable1
    OnDataChange = DataSource1DataChange
    Left = 87
    Top = 184
  end
  object DataSource2: TDataSource
    DataSet = FSTable2
    Left = 45
    Top = 372
  end
  object FSSession1: TFSSession
    ClientName = 'cln'
    SessionName = 'sn1'
    Left = 192
    Top = 186
  end
  object FSDatabase1: TFSDatabase
    AliasName = 'tablexx'
    DataBaseName = 'testdb'
    SessionName = 'sn1'
    RecLocking = tlOptimisticNoWait
    Left = 240
    Top = 187
  end
  object FSTable1: TFSTable
    BlobAutoStartTransaction = True
    BlobModifiedError = False
    BlobMode = bmCache
    CheckTimeout = 0
    DeleteTimeout = 0
    DataBaseName = 'testdb'
    RecLockedBeforeEdit = False
    RecLockedType = tluDatabase
    FieldDefs = <
      item
        Name = 'IdKlienta'
        DataType = ftAutoInc
      end
      item
        Name = 'ParentIdKlienta'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IDActKlienta'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdWaznosc'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdTypKlienta'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdDokumentu'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdPRegion'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'Version'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'VersionTime'
        Attributes = [faRequired]
        DataType = ftDateTime
      end
      item
        Name = 'IdStanKlienta'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'TypRekordu'
        Attributes = [faRequired]
        DataType = ftWord
      end
      item
        Name = 'IdFK'
        Attributes = [faRequired]
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Litera'
        Attributes = [faRequired]
        DataType = ftString
        Size = 1
      end
      item
        Name = 'Skrot'
        Attributes = [faRequired]
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Nazwa'
        Attributes = [faRequired]
        DataType = ftString
        Size = 150
      end
      item
        Name = 'Nazwa2'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Nazwa3'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'IdHierarchia'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'IdKraju'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdRegion'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdMiasto'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'Ulica'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'KodPocztowy'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'NrPosesji'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'NIP'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Regon'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Pesel'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'InnyNr'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Telefon'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'TelefonKom'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Fax'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Email'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Www'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'DataPowstania'
        DataType = ftDate
      end
      item
        Name = 'PoczWsp'
        Attributes = [faRequired]
        DataType = ftDate
      end
      item
        Name = 'KonWsp'
        DataType = ftDate
      end
      item
        Name = 'IdSposobZap'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'DefIloscDniZaplaty'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'Rabat'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'Opis'
        DataType = ftMemo
      end
      item
        Name = 'ZgodaPrzet'
        DataType = ftMemo
      end
      item
        Name = 'X'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'Y'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'Reserved'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Reserved2'
        DataType = ftFloat
      end
      item
        Name = 'Reserved3'
        DataType = ftInteger
      end
      item
        Name = 'Reserved4'
        DataType = ftInteger
      end
      item
        Name = 'UserCreate'
        Attributes = [faRequired]
        DataType = ftString
        Size = 31
      end
      item
        Name = 'DateCreate'
        Attributes = [faRequired]
        DataType = ftDateTime
      end
      item
        Name = 'Sys'
        Attributes = [faRequired]
        DataType = ftInteger
      end>
    FilterEval = fseLocal
    FilterTimeout = 3000
    FlipOrder = False
    BlobChunkSize = 524288
    SessionName = 'sn1'
    SupportRecNo = False
    TableName = 'DtKlienci'
    Left = 136
    Top = 183
  end
  object FSTable2: TFSTable
    BlobAutoStartTransaction = True
    BlobModifiedError = False
    BlobMode = bmCache
    CheckTimeout = 0
    DeleteTimeout = 0
    DataBaseName = 'testdb'
    RecLockedBeforeEdit = False
    RecLockedType = tluDatabase
    FieldDefs = <
      item
        Name = 'IdKontakt'
        DataType = ftAutoInc
      end
      item
        Name = 'ParentIdKontakt'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'Version'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'VersionTime'
        Attributes = [faRequired]
        DataType = ftDateTime
      end
      item
        Name = 'IdKlienta'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdPracownika'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IdTypKontaktu'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'Tydzien'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'DataWizyty'
        Attributes = [faRequired]
        DataType = ftDate
      end
      item
        Name = 'Nazwa'
        Attributes = [faRequired]
        DataType = ftString
        Size = 150
      end
      item
        Name = 'Notatka'
        DataType = ftMemo
      end
      item
        Name = 'IdOcenaKontaktu'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'NastepnyKrok'
        DataType = ftMemo
      end
      item
        Name = 'TerminNastSpotkania'
        DataType = ftDate
      end
      item
        Name = 'InfoDoTerminu'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'Uwagi'
        DataType = ftMemo
      end
      item
        Name = 'UserCreate'
        Attributes = [faRequired]
        DataType = ftString
        Size = 31
      end
      item
        Name = 'DateCreate'
        Attributes = [faRequired]
        DataType = ftDateTime
      end
      item
        Name = 'Sys'
        Attributes = [faRequired]
        DataType = ftInteger
      end>
    FilterEval = fseLocal
    FilterTimeout = 3000
    FlipOrder = False
    IndexDefs = <
      item
        Name = 'Sequential Access Index'
        Options = [ixUnique, ixCaseInsensitive, ixExpression]
      end
      item
        Name = 'Id'
        Fields = 'IdKontakt'
        Options = [ixUnique, ixCaseInsensitive]
      end
      item
        Name = 'IdKlienta'
        Fields = 'IdKlienta'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'IdParent'
        Fields = 'ParentIdKontakt'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'DataWizyty'
        Fields = 'DataWizyty'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'IdPracownika'
        Fields = 'IdPracownika'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'IdTypKontaktu'
        Fields = 'IdTypKontaktu'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'IdOcenaKontaktu'
        Fields = 'IdOcenaKontaktu'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'TerminNastSpot'
        Fields = 'TerminNastSpotkania'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'Version'
        Fields = 'Version'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'VersionTime'
        Fields = 'VersionTime'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'Sys'
        Fields = 'Sys'
        Options = [ixCaseInsensitive]
      end>
    IndexName = 'IdKlienta'
    MasterFields = 'IdKlienta'
    MasterSource = DataSource1
    BlobChunkSize = 524288
    SessionName = 'sn1'
    SupportRecNo = False
    TableName = 'DtKlienciKontakt'
    Left = 100
    Top = 380
  end
  object FSClient1: TFSClient
    Active = True
    ClientName = 'cln'
    OnConnectionLost = FSClient1ConnectionLost
    ServerEngine = FSRemoteServer1
    Left = 300
    Top = 185
  end
  object FSRemoteServer1: TFSRemoteServer
    Transport = FSParamConnect1
    Left = 351
    Top = 186
  end
  object FSParamConnect1: TFSParamConnect
    Enabled = True
    ServerName = 'Local'
    Protocol = ptSingleUser
    Left = 421
    Top = 186
  end
  object FSQuery1: TFSQuery
    BlobAutoStartTransaction = True
    BlobChunkSize = 524288
    BlobModifiedError = False
    BlobMode = bmCache
    DataBaseName = 'testdb'
    DataSource = DataSource1
    FilterEval = fseLocal
    FilterTimeout = 3000
    FlipOrder = False
    SessionName = 'sn1'
    SQL.Strings = (
      'select * from DtKlienciKontakt '
      'where idklienta = :Idklienta')
    SupportRecNo = False
    Left = 516
    Top = 372
    ParamData = <
      item
        DataType = ftAutoInc
        Name = 'IdKlienta'
        ParamType = ptUnknown
      end>
  end
  object DataSource3: TDataSource
    DataSet = FSQuery1
    Left = 462
    Top = 370
  end
  object FSServer1: TFSServer
    CollectGarbageEnabled = False
    CollectCloseInactiveTables = False
    CollectClearCache = False
    SecurityEnabled = False
    ClearCachePerCount = 0
    CloseInactiveTablesAfterCommitOrRoolback = False
    Left = 563
    Top = 189
  end
end
