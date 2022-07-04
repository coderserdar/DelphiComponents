object Form1: TForm1
  Left = 397
  Top = 152
  Width = 532
  Height = 245
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
    Width = 497
    Height = 169
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
    Top = 184
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object OCIDatabase1: TOCIDatabase
    UserName = 'demo'
    Password = 'demo'
    Connected = True
    SQLMonitor = NCSQLMonitorClient1
    Left = 8
    Top = 8
  end
  object OCIQuery1: TOCIQuery
    Active = True
    Prepared = True
    UpdateObject = OCIUpdateSQL1
    SQL.Strings = (
      'select * from mytab')
    Left = 48
    Top = 8
  end
  object OCIUpdateSQL1: TOCIUpdateSQL
    SQLUpdate.Strings = (
      'begin'
      'UPD_(:OLD_ID, :TXT);'
      'end;')
    SQLInsert.Strings = (
      'begin'
      'INS_(:ID, :TXT);'
      'end;')
    SQLDelete.Strings = (
      'begin'
      'del_(:OLD_ID);'
      'end;')
    Left = 88
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = OCIQuery1
    Left = 48
    Top = 40
  end
  object NCSQLMonitorClient1: TNCSQLMonitorClient
    Active = True
    ClientObjName = 'Default'
    Left = 8
    Top = 40
  end
end
