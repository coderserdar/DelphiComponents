object Form1: TForm1
  Left = 293
  Top = 152
  Width = 567
  Height = 375
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 126
    Top = 73
    Width = 4
    Height = 268
    Cursor = crHSplit
    AutoSnap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 559
    Height = 73
    Align = alTop
    TabOrder = 0
    object Label4: TLabel
      Left = 61
      Top = 19
      Width = 22
      Height = 13
      Caption = 'Path'
    end
    object Button1: TButton
      Left = 7
      Top = 8
      Width = 47
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 7
      Top = 35
      Width = 47
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Edit4: TEdit
      Left = 88
      Top = 11
      Width = 326
      Height = 21
      TabOrder = 2
    end
    object Memo1: TMemo
      Left = 418
      Top = 1
      Width = 140
      Height = 71
      Align = alRight
      Lines.Strings = (
        'Passwords here 1'
        'Passwords here 2'
        '...')
      TabOrder = 3
    end
    object spdundel: TButton
      Left = 88
      Top = 35
      Width = 106
      Height = 25
      Caption = 'Undelete'
      TabOrder = 4
      OnClick = spdundelClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 73
    Width = 126
    Height = 268
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object ListBox1: TListBox
      Left = 0
      Top = 0
      Width = 126
      Height = 268
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
  end
  object Panel3: TPanel
    Left = 130
    Top = 73
    Width = 429
    Height = 268
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object DBNavigator1: TDBNavigator
      Left = 0
      Top = 0
      Width = 429
      Height = 25
      DataSource = DataSource1
      Align = alTop
      Flat = True
      TabOrder = 0
    end
    object DBGrid1: TDBGrid
      Left = 0
      Top = 25
      Width = 429
      Height = 243
      Align = alClient
      BorderStyle = bsNone
      DataSource = DataSource1
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object DataSource1: TDataSource
    DataSet = FSTable1
    Left = 87
    Top = 184
  end
  object FSSession1: TFSSession
    ClientName = 'cln'
    SessionName = 'tt'
    Left = 191
    Top = 186
  end
  object FSDatabase1: TFSDatabase
    AliasName = 'E:\Delphi5\Component\KWSource\DataBase\FSSQLI\Samples\Embeded'
    DataBaseName = 'testdb'
    SessionName = 'tt'
    RecLocking = tlOptimisticNoWait
    Left = 241
    Top = 187
  end
  object FSTable1: TFSTable
    BlobAutoStartTransaction = False
    BlobModifiedError = False
    BlobMode = bmAuto
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
    SessionName = 'tt'
    SupportRecNo = False
    TableName = 'DtKlienci'
    Left = 136
    Top = 183
  end
  object FSClient1: TFSClient
    ClientName = 'cln'
    OnConnectionLost = FSClient1ConnectionLost
    ServerEngine = FSServer1
    TimeOut = 1000
    Left = 300
    Top = 186
  end
  object FSServer1: TFSServer
    NoAutoSaveCfg = True
    CollectGarbageEnabled = False
    CollectCloseInactiveTables = False
    CollectClearCache = False
    SecurityEnabled = False
    ClearCachePerCount = 0
    CloseInactiveTablesAfterCommitOrRoolback = False
    Left = 352
    Top = 186
  end
end
