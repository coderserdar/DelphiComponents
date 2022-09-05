object FormStreaming: TFormStreaming
  Left = 243
  Top = 128
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'TImage2Stream'
  ClientHeight = 467
  ClientWidth = 287
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 101
    Height = 13
    Caption = 'Data saved to stream'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 160
    Top = 8
    Width = 115
    Height = 13
    Caption = 'Data loaded from stream'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 137
    Top = 8
    Width = 7
    Height = 241
    Shape = bsRightLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 256
    Width = 270
    Height = 185
  end
  object Label3: TLabel
    Left = 16
    Top = 264
    Width = 48
    Height = 13
    Caption = 'File name:'
  end
  object Label4: TLabel
    Left = 16
    Top = 312
    Width = 77
    Height = 13
    Caption = 'Image file format'
  end
  object Label5: TLabel
    Left = 16
    Top = 360
    Width = 91
    Height = 13
    Caption = 'Image compression'
  end
  object Label6: TLabel
    Left = 16
    Top = 416
    Width = 62
    Height = 13
    Caption = 'Quality/Ratio'
  end
  object mcmImageCtrl1: TmcmImageCtrl
    Left = 8
    Top = 64
    Width = 121
    Height = 153
    Center = True
    Flat = True
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = True
    OnChange = mcmImageCtrl1Change
  end
  object mcmImageCtrl2: TmcmImageCtrl
    Left = 160
    Top = 64
    Width = 121
    Height = 153
    Center = True
    Flat = True
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = True
  end
  object Edit1: TEdit
    Left = 8
    Top = 32
    Width = 116
    Height = 21
    MaxLength = 255
    TabOrder = 0
    Text = 'First text'
  end
  object Edit2: TEdit
    Left = 8
    Top = 224
    Width = 121
    Height = 21
    MaxLength = 255
    TabOrder = 1
    Text = 'Second text'
  end
  object Edit3: TEdit
    Left = 160
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object Edit4: TEdit
    Left = 160
    Top = 224
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object eFilename: TEdit
    Left = 16
    Top = 280
    Width = 249
    Height = 21
    TabOrder = 4
    Text = '.\MCMSTREAM.1ST'
  end
  object cbFileFormat: TComboBox
    Left = 16
    Top = 328
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = cbFileFormatChange
  end
  object cbCompression: TComboBox
    Left = 16
    Top = 376
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    OnChange = cbCompressionChange
  end
  object btnSaveAndLoad: TButton
    Left = 192
    Top = 328
    Width = 73
    Height = 105
    Caption = 'Save-Load'
    TabOrder = 7
    OnClick = btnSaveAndLoadClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 448
    Width = 287
    Height = 19
    Panels = <>
    SimplePanel = True
    SizeGrip = False
  end
  object cbQuality: TComboBox
    Left = 96
    Top = 408
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Items.Strings = (
      'Low'
      'Medium'
      'High'
      'Maximum')
    TabOrder = 9
    OnChange = cbQualityChange
  end
  object OpenDialog: TmcmOpenDialog
    HelpContext = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofFileMustExist, ofNoNetworkButton]
    Title = 'Open Image'
    ViewStyle = vsReport
    Left = 128
    Top = 32
  end
end
