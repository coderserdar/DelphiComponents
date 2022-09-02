object Form1: TForm1
  Left = 369
  Top = 153
  Width = 413
  Height = 331
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
    ClientName = 'Client1'
    ServerEngine = FSRemoteServer1
    Left = 112
    Top = 16
  end
  object FSSession1: TFSSession
    ClientName = 'Client1'
    SessionName = 'Ses1'
    Left = 144
    Top = 16
  end
  object FSDatabase1: TFSDatabase
    AliasName = 'CrmData'
    DataBaseName = 'DBs1'
    SessionName = 'Ses1'
    RecLocking = tlOptimisticNoWait
    Left = 192
    Top = 16
  end
  object FSTable1: TFSTable
    BlobAutoStartTransaction = False
    BlobModifiedError = False
    BlobMode = bmAuto
    CheckTimeout = 0
    DeleteTimeout = 0
    DataBaseName = 'DBs1'
    RecLockedBeforeEdit = False
    FieldDefs = <
      item
        Name = 'IDKLIENTA'
        DataType = ftAutoInc
      end
      item
        Name = 'IDPARENT'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'VERSION'
        DataType = ftLargeint
      end
      item
        Name = 'SKROT'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'NAZWA'
        DataType = ftString
        Size = 250
      end
      item
        Name = 'IDSTANKL'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'DATASTANU'
        Attributes = [faRequired]
        DataType = ftDate
      end
      item
        Name = 'IDREGION'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'IDKRAJ'
        DataType = ftInteger
      end
      item
        Name = 'IDWOJ'
        DataType = ftInteger
      end
      item
        Name = 'IDMIASTO'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'ADRES'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'KOD'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'TELEFON'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'TELEFON2'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'FAX'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'E_MAIL'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'WWW'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'KONTAKT'
        DataType = ftString
        Size = 80
      end
      item
        Name = 'TELKONTAKT'
        DataType = ftString
        Size = 60
      end
      item
        Name = 'UWAGA'
        DataType = ftMemo
      end
      item
        Name = 'FLAGA'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'STM'
        DataType = ftDateTime
      end
      item
        Name = 'SUSER'
        DataType = ftString
        Size = 30
      end>
    FilterEval = fseServer
    FilterTimeout = 3000
    FlipOrder = False
    BlobChunkSize = 524288
    SessionName = 'Ses1'
    SupportRecNo = False
    TableName = 'DtKlienci'
    Left = 232
    Top = 16
  end
  object FSParamConnect1: TFSParamConnect
    ServerName = 'Local'
    Protocol = ptSingleUser
    Left = 40
    Top = 16
  end
  object DataSource1: TDataSource
    DataSet = FSTable1
    Left = 264
    Top = 16
  end
end
