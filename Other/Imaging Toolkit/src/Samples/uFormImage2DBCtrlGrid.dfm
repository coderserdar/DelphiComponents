object Form1: TForm1
  Left = 280
  Top = 145
  BorderStyle = bsDialog
  Caption = 'DBCtrlGrid and TmcmImageDB'
  ClientHeight = 497
  ClientWidth = 286
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object DBCtrlGrid1: TDBCtrlGrid
    Left = 0
    Top = 0
    Width = 286
    Height = 497
    Align = alClient
    AllowDelete = False
    AllowInsert = False
    ColCount = 1
    DataSource = DataSource1
    PanelHeight = 165
    PanelWidth = 269
    TabOrder = 0
    RowCount = 3
    object mcmImageDB1: TmcmImageDB
      Left = 8
      Top = 32
      Width = 121
      Height = 121
      BorderStyle = BS_SINGLE
      Center = True
      Color = clBtnFace
      Scale = 0.468000000000000000
      ScaleToFit = True
      AddGraphicHeader = True
      Compression = 0
      DataField = 'Graphic'
      DataSource = DataSource1
      Quality = 100
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 73
      Height = 13
      Caption = 'TmcmImageDB'
    end
    object Label2: TLabel
      Left = 136
      Top = 8
      Width = 51
      Height = 13
      Caption = 'TDBImage'
    end
    object DBImage1: TDBImage
      Left = 136
      Top = 32
      Width = 121
      Height = 121
      DataField = 'Graphic'
      DataSource = DataSource1
      Stretch = True
      TabOrder = 0
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 16
    Top = 368
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    TableName = 'biolife.db'
    Left = 48
    Top = 368
  end
end
