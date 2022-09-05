object FormImageDB: TFormImageDB
  Left = 269
  Top = 220
  BorderStyle = bsDialog
  Caption = 'TmcmImageDB Example'
  ClientHeight = 461
  ClientWidth = 257
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
    Width = 241
    Height = 241
    BorderStyle = BS_SINGLE
    Center = True
    Color = clBtnFace
    ParentShowHint = False
    Scale = 1.000000000000000000
    ScaleToFit = True
    OnChange = mcmImageDB1Change
    AutoEdit = True
    AutoFormat = False
    Compression = 0
    DataField = 'Image'
    DataSource = DataSource1
    Quality = 100
    ShowHint = True
  end
  object Label1: TLabel
    Left = 8
    Top = 256
    Width = 24
    Height = 13
    Caption = 'Text:'
  end
  object lFileFormat: TLabel
    Left = 8
    Top = 304
    Width = 54
    Height = 13
    Caption = 'File Format:'
  end
  object lCompression: TLabel
    Left = 8
    Top = 352
    Width = 63
    Height = 13
    Caption = 'Compression:'
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 400
    Width = 240
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
  object DBEdit1: TDBEdit
    Left = 8
    Top = 272
    Width = 241
    Height = 21
    DataField = 'Text'
    DataSource = DataSource1
    TabOrder = 1
  end
  object btnReadImage: TButton
    Left = 8
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Read Image'
    TabOrder = 2
    OnClick = btnReadImageClick
  end
  object cbFilename: TComboBox
    Left = 8
    Top = 320
    Width = 241
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbFilenameChange
  end
  object cbCompression: TComboBox
    Left = 8
    Top = 368
    Width = 241
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = cbCompressionChange
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 16
    Top = 16
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'mcmImageDB.db'
    Left = 48
    Top = 16
  end
  object OpenDialog: TOpenDialog
    Left = 88
    Top = 432
  end
end
