object Form1: TForm1
  Left = 280
  Top = 180
  Width = 532
  Height = 339
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 29
    Align = alTop
    TabOrder = 0
    object sbConvert: TSpeedButton
      Left = 4
      Top = 4
      Width = 75
      Height = 22
      Caption = 'Convert'
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777770000007777717777747777770000007777111777444777770000007771
        7177777474777700000077177777777777477700000071777000000077747700
        000071770788888807747700000017770F77777807774700000017770F777778
        07774700000017770F77777807774700000017770F7777780777470000001777
        0F77777807774700000071770FFFFFF707747700000071777000000077747700
        0000771777777777774777000000777171777774747777000000777711177744
        477777000000777771777774777777000000}
      OnClick = sbConvertClick
    end
    object sbPreview: TSpeedButton
      Left = 84
      Top = 4
      Width = 75
      Height = 22
      Caption = 'Preview'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777700000000000077000FFFFFFFFFF070000FFFFFFF000080070FFFFFF08778
        08770FFFFF0877E880770FFFFF07777870770FFFFF07E77870770FFFFF08EE78
        80770FFFFFF0877807770FFFFFFF000077770FFFFFFFFFF077770FFFFFFF0000
        77770FFFFFFF070777770FFFFFFF007777770000000007777777}
      OnClick = sbPreviewClick
    end
    object cbCreateBAK: TCheckBox
      Left = 168
      Top = 8
      Width = 105
      Height = 17
      Caption = 'Create BAK files'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 29
    Width = 173
    Height = 264
    Align = alLeft
    Caption = 'Panel2'
    TabOrder = 1
    object lbDirectory: TDirectoryListBox
      Left = 1
      Top = 29
      Width = 171
      Height = 234
      Align = alClient
      ItemHeight = 16
      TabOrder = 0
      OnChange = lbDirectoryChange
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 171
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object cbDrive: TDriveComboBox
        Left = 4
        Top = 4
        Width = 161
        Height = 19
        TabOrder = 0
        OnChange = cbDriveChange
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 293
    Width = 524
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object lbFiles: TFileListBox
    Left = 173
    Top = 29
    Width = 351
    Height = 264
    Align = alClient
    ItemHeight = 13
    Mask = '*.frf'
    TabOrder = 3
    OnChange = lbFilesChange
  end
  object frReport1: TfrReport
    InitialZoom = pzDefault
    PreviewButtons = [pbZoom, pbLoad, pbSave, pbPrint, pbFind, pbHelp, pbExit]
    Left = 8
    Top = 245
    ReportForm = {17000000}
  end
  object frRichObject1: TfrRichObject
    Left = 40
    Top = 244
  end
  object frCheckBoxObject1: TfrCheckBoxObject
    Left = 72
    Top = 244
  end
  object frShapeObject1: TfrShapeObject
    Left = 104
    Top = 244
  end
  object frBarCodeObject1: TfrBarCodeObject
    Left = 136
    Top = 244
  end
  object frChartObject1: TfrChartObject
    Left = 168
    Top = 244
  end
  object frRoundRectObject1: TfrRoundRectObject
    Left = 200
    Top = 244
  end
  object frDesigner1: TfrDesigner
    Left = 232
    Top = 244
  end
end
