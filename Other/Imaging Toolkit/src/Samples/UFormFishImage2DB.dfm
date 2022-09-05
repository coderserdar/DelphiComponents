object FormImageDB: TFormImageDB
  Left = 212
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Paradox Fish'
  ClientHeight = 578
  ClientWidth = 266
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990000000000000000000000000000999
    9000000000000000000000000000099990000000000000000000000000000999
    9000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990000000000000000000000999
    9009999000000000000000000000099990099990000000000000000000000999
    9009999000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990099990000000000000000999
    9009999009999000000000000000099990099990099990000000000000000999
    9009999009999000000000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990099990099990000000000999
    9009999009999009999000000000099990099990099990099990000000000999
    9009999009999009999000000000000000000000000000000000000000000000
    0000000000000000000000000000099990099990099990099990099990000999
    9009999009999009999009999000099990099990099990099990099990000999
    900999900999900999900999900000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFF87FFFFFF87FFFFFF87FFFFFF87FFFFFFFFFFFFFFFFFF
    FFFF861FFFFF861FFFFF861FFFFF861FFFFFFFFFFFFFFFFFFFFF86187FFF8618
    7FFF86187FFF86187FFFFFFFFFFFFFFFFFFF861861FF861861FF861861FF8618
    61FFFFFFFFFFFFFFFFFF86186187861861878618618786186187FFFFFFFF}
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mcmImageDB1: TmcmImageDB
    Left = 8
    Top = 8
    Width = 249
    Height = 241
    BorderStyle = BS_SINGLE
    Center = True
    Color = clBtnFace
    ParentShowHint = False
    Scale = 1.000000000000000000
    ScaleToFit = False
    OnChange = mcmImageDB1Change
    AddGraphicHeader = True
    AutoEdit = True
    Compression = 0
    DataField = 'Graphic'
    DataSource = DataSource1
    Quality = 100
    ShowHint = True
  end
  object lName: TLabel
    Left = 8
    Top = 256
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lFileFormat: TLabel
    Left = 8
    Top = 416
    Width = 54
    Height = 13
    Caption = 'File Format:'
  end
  object lCompression: TLabel
    Left = 8
    Top = 464
    Width = 63
    Height = 13
    Caption = 'Compression:'
  end
  object lAboutFish: TLabel
    Left = 8
    Top = 304
    Width = 31
    Height = 13
    Caption = 'About:'
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 512
    Width = 250
    Height = 25
    DataSource = DataSource1
    Flat = True
    Hints.Strings = (
      'First record'
      'Previous record'
      'Next record'
      'Last record'
      'Add record'
      'Delete record'
      'Edit record'
      'Apply changes'
      'Cancel changes'
      'Refresh view')
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object dbeName: TDBEdit
    Left = 8
    Top = 272
    Width = 249
    Height = 21
    DataField = 'Common_Name'
    DataSource = DataSource1
    TabOrder = 1
  end
  object btnReadImage: TButton
    Left = 8
    Top = 544
    Width = 75
    Height = 25
    Caption = 'Read Image'
    TabOrder = 2
    OnClick = btnReadImageClick
  end
  object cbFilename: TComboBox
    Left = 8
    Top = 432
    Width = 249
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbFilenameChange
  end
  object cbCompression: TComboBox
    Left = 8
    Top = 480
    Width = 249
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = cbCompressionChange
  end
  object DBMemoAbout: TDBMemo
    Left = 8
    Top = 320
    Width = 249
    Height = 89
    DataField = 'Notes'
    DataSource = DataSource1
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 16
    Top = 16
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'biolife.db'
    Left = 48
    Top = 16
    object Table1SpeciesNo: TFloatField
      FieldName = 'Species No'
    end
    object Table1Category: TStringField
      FieldName = 'Category'
      Size = 15
    end
    object Table1Common_Name: TStringField
      FieldName = 'Common_Name'
      Size = 30
    end
    object Table1SpeciesName: TStringField
      FieldName = 'Species Name'
      Size = 40
    end
    object Table1Lengthcm: TFloatField
      FieldName = 'Length (cm)'
    end
    object Table1Length_In: TFloatField
      FieldName = 'Length_In'
    end
    object Table1Notes: TMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
      Size = 50
    end
    object Table1Graphic: TGraphicField
      FieldName = 'Graphic'
      BlobType = ftGraphic
    end
  end
  object OpenDialog: TOpenDialog
    Left = 88
    Top = 544
  end
end
