object ExportExcel: TExportExcel
  Left = 189
  Top = 121
  Width = 253
  Height = 640
  Caption = 'Export To Excel'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 48
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Export'
    TabOrder = 0
    OnClick = Button1Click
  end
  object PB: TProgressBar
    Left = 8
    Top = 16
    Width = 25
    Height = 577
    Orientation = pbVertical
    Smooth = True
    Step = 1
    TabOrder = 1
  end
  object SmExportToExcel1: TSmExportToExcel
    FileName = 'C:\Example.xls'
    DataSet = ADOTable1
    Header.Strings = (
      'dgh'
      'dfgh'
      'dfgh'
      'dfghdfdf')
    Footer.Strings = (
      'This is SoftMaster Example')
    Captions = smFieldNames
    CaptionBgColor = clYellow
    OnExportProgress = SmExportToExcel1ExportProgress
    Left = 120
    Top = 80
  end
  object ADOTable1: TADOTable
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Password="";Data Source=..\SMCo' +
      'mponentDemo.mdb;Persist Security Info=True'
    TableName = 'Objects'
    Left = 120
    Top = 48
  end
end
