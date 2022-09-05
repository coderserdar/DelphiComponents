object Form1: TForm1
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'FirstScan'
  ClientHeight = 377
  ClientWidth = 344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 329
    Height = 329
    Stretch = True
  end
  object mcmTWAIN: TmcmTWAIN
    Left = 184
    Top = 344
    Width = 28
    Height = 28
    Country = UNITEDKINGDOM
    LogFilename = '.\APPTWN.LOG'
    LogToFile = True
    Manufacturer = 'MCM DESIGN'
    MessageLevel = ML_INFO
    ProductFamily = 'Image Capture'
    ProductName = 'TWAIN Application'
    OnDisableMenus = mcmTWAINDisableMenus
    OnEnableMenus = mcmTWAINEnableMenus
    OnImageReady = mcmTWAINImageReady
  end
  object btnAcquire: TButton
    Left = 8
    Top = 344
    Width = 75
    Height = 25
    Caption = '&Acquire'
    Default = True
    TabOrder = 1
    OnClick = btnAcquireClick
  end
  object btnSelect: TButton
    Left = 96
    Top = 344
    Width = 75
    Height = 25
    Caption = '&Select'
    TabOrder = 2
    OnClick = btnSelectClick
  end
end
