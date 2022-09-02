object Form1: TForm1
  Left = 369
  Top = 153
  Caption = 'Form1'
  ClientHeight = 304
  ClientWidth = 405
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
    Left = 16
    Top = 56
    Width = 377
    Height = 225
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object FSRemoteServer1: TFSRemoteServer
    Transport = FSParamConnect1
    Left = 8
    Top = 16
  end
  object FSClient1: TFSClient
    Active = True
    ClientName = 'Client1'
    ServerEngine = FSRemoteServer1
    Left = 80
    Top = 16
  end
  object FSSession1: TFSSession
    Active = True
    ClientName = 'Client1'
    SessionName = 'Ses1'
    Left = 128
    Top = 16
  end
  object FSDatabase1: TFSDatabase
    AliasName = 'Data'
    Connected = True
    DataBaseName = 'DBs1'
    SessionName = 'Ses1'
    RecLocking = tlOptimisticNoWait
    Left = 176
    Top = 16
  end
  object FSTable1: TFSTable
    Active = True
    BlobAutoStartTransaction = False
    BlobModifiedError = False
    BlobMode = bmAuto
    CheckTimeout = 0
    DeleteTimeout = 0
    DataBaseName = 'DBs1'
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
        DataType = ftSmallint
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
    FilterEval = fseServer
    FilterTimeout = 3000
    FlipOrder = False
    BlobChunkSize = 524288
    SessionName = 'Ses1'
    SupportRecNo = False
    TableName = 'DtKlienci'
    Left = 208
    Top = 16
  end
  object FSParamConnect1: TFSParamConnect
    Enabled = True
    ServerName = 'Local'
    Protocol = ptSingleUser
    Left = 40
    Top = 16
  end
  object DataSource1: TDataSource
    DataSet = FSTable1
    Left = 248
    Top = 16
  end
end
