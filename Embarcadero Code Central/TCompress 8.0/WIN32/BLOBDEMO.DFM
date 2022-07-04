object Form1: TForm1
  Left = 264
  Top = 143
  Width = 330
  Height = 274
  Caption = 'Compressed Blob I/O Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 240
    Height = 16
    Caption = '(See source before trying to run this!)'
  end
  object DBGrid1: TDBGrid
    Left = 9
    Top = 62
    Width = 236
    Height = 120
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'System'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        Visible = True
      end>
  end
  object SaveArray: TButton
    Left = 8
    Top = 26
    Width = 89
    Height = 33
    Hint = 'Compresses a 4000-integer array into a new record'
    Caption = 'Save Array'
    TabOrder = 1
    OnClick = SaveArrayClick
  end
  object LoadArray: TButton
    Left = 158
    Top = 26
    Width = 89
    Height = 33
    Hint = 'Reads the integer array back from the record'
    Caption = 'Load Array'
    TabOrder = 2
    OnClick = LoadArrayClick
  end
  object Table1: TTable
    DatabaseName = 'DBDEMOS'
    TableName = 'BLOB.DB'
    Left = 246
    Top = 94
    object Table1Name: TStringField
      FieldName = 'Name'
      Size = 10
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 246
    Top = 126
  end
  object Compress1: TCompress
    RegNumber = 0
    MakeDirectories = False
    ExceptionOnFileError = False
    Key = 0
    CheckSpaceBeforeExpand = False
    Left = 245
    Top = 63
  end
end
