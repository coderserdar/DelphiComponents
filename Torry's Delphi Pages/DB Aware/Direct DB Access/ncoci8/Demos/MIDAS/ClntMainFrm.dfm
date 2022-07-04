object Form1: TForm1
  Left = 321
  Top = 195
  Width = 569
  Height = 317
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
    Top = 8
    Width = 529
    Height = 241
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 16
    Top = 256
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object Button1: TButton
    Left = 264
    Top = 256
    Width = 89
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 360
    Top = 256
    Width = 89
    Height = 25
    Caption = 'CancelUpdates'
    TabOrder = 3
    OnClick = Button2Click
  end
  object DCOMConnection1: TDCOMConnection
    Connected = True
    ServerGUID = '{4881E394-6F8A-11D4-94F8-000000000000}'
    ServerName = 'Srv.NCOCI8MidasTest'
    Left = 16
    Top = 8
  end
  object ClientDataSet1: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    RemoteServer = DCOMConnection1
    Left = 16
    Top = 40
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 16
    Top = 72
  end
end
