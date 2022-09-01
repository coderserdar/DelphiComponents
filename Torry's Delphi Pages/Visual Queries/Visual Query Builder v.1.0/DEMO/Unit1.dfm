object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Visual Query Builder BDE DEMO'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 289
    Width = 688
    Height = 8
    Cursor = crVSplit
    Align = alTop
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 688
    Height = 289
    Align = alTop
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 297
    Width = 688
    Height = 137
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 688
      Height = 137
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object SQLDialog1: TSQLDialog
    DBEngine = DBEngineBDE1
    OnLoadModel = SQLDialog1LoadModel
    Left = 32
    Top = 88
  end
  object DBEngineBDE1: TDBEngineBDE
    DataBase = Database1
    ShowSystemTables = False
    Left = 64
    Top = 88
  end
  object Database1: TDatabase
    AliasName = 'DBDEMOS'
    Connected = True
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    Left = 96
    Top = 88
  end
  object Query1: TQuery
    DatabaseName = 'DBDEMOS'
    Left = 128
    Top = 88
  end
  object DataSource1: TDataSource
    DataSet = Query1
    Left = 160
    Top = 88
  end
  object MainMenu1: TMainMenu
    Left = 224
    Top = 88
    object Builder1: TMenuItem
      Caption = 'Builder'
      object Execute1: TMenuItem
        Caption = 'Execute'
        OnClick = Execute1Click
      end
    end
  end
end
