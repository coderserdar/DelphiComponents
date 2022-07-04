object Form1: TForm1
  Left = 289
  Top = 128
  Width = 696
  Height = 290
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 673
    Height = 217
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 232
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    Left = 16
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    Active = True
    DataFormat.EnableInteger = True
    DataFormat.EnableBCD = True
    DataFormat.EnableNumber = True
    DataFormat.EnableLongString = True
    DataFormat.EnableFixedString = True
    Prepared = True
    SQL.Strings = (
      'select t.*, rowid from test_num t')
    Left = 48
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 80
    Top = 8
  end
end
