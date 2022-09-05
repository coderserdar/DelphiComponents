object FormTWAIN: TFormTWAIN
  Left = 273
  Top = 107
  Width = 591
  Height = 428
  Caption = 'TWAIN Toolkit for Delphi, Large Image example'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 41
    Width = 583
    Height = 314
    Align = alClient
    Center = True
    IncrementalDisplay = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 583
    Height = 41
    Align = alTop
    TabOrder = 0
    object sbAcquire: TSpeedButton
      Left = 40
      Top = 8
      Width = 25
      Height = 25
      Hint = 'Acquire'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
        033333FFFF77777773F330000077777770333777773FFFFFF733077777000000
        03337F3F3F777777733F0797A770003333007F737337773F3377077777778803
        30807F333333337FF73707888887880007707F3FFFF333777F37070000878807
        07807F777733337F7F3707888887880808807F333333337F7F37077777778800
        08807F333FFF337773F7088800088803308073FF777FFF733737300008000033
        33003777737777333377333080333333333333F7373333333333300803333333
        33333773733333333333088033333333333373F7F33333333333308033333333
        3333373733333333333333033333333333333373333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = AcquireClick
    end
    object sbSource: TSpeedButton
      Left = 8
      Top = 8
      Width = 25
      Height = 25
      Hint = 'Select Source'
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        555555555555555555555555555555555555555FFFFFFFFFF555550000000000
        55555588888888885F55500B7B7B7B7B05555885F555555585F550F0B7B7B7B7
        B05558F85F555555585F50BF0B7B7B7B7B0558F585FFFFFFFF8F50FBF0000000
        000558F558888888888550BFBFBFBFB0555558F555555558F55550F0000000F0
        555558588888885FF5F55007888887005005588F55F5F588F88555078A898700
        0705558F5858558885855508888887007705558F5FFFF588FF85550700008700
        0705558F88885588858555077777870550055585FFFFFF855885555000000055
        5555555888888855555555555555555555555555555555555555}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = SourceClick
    end
  end
  object mcmTWAIN: TmcmTWAIN
    Left = 48
    Top = 56
    Width = 28
    Height = 28
    Country = UNITEDKINGDOM
    Filename = '.\abcd.bmp'
    LogFilename = '.\APPTWN.LOG'
    LogToFile = True
    Manufacturer = 'MCM DESIGN'
    MessageLevel = ML_INFO
    ProductFamily = 'My Product Family'
    ProductName = 'My Product Name'
    ReturnHandle = False
    XferMech = TWFX_MEMORY
    OnDisableMenus = mcmTWAINDisableMenus
    OnEnableMenus = mcmTWAINEnableMenus
    OnMemXferBuffer = mcmTWAINMemXferBuffer
    OnMemXferSize = mcmTWAINMemXferSize
    OnNegotiation = mcmTWAINNegotiation
    OnXferNext = mcmTWAINXferNext
    OnXferReady = mcmTWAINXferReady
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 355
    Width = 583
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 56
    object FileMenu: TMenuItem
      Caption = '&File'
      OnClick = FileMenuClick
      object Acquire1: TMenuItem
        Caption = '&Acquire'
        OnClick = AcquireClick
      end
      object Source1: TMenuItem
        Caption = '&Source ...'
        OnClick = SourceClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ErrorLevel1: TMenuItem
        Caption = 'Error &Level'
        OnClick = ErrorLevel1Click
        object None1: TMenuItem
          Caption = '&None'
          Checked = True
          OnClick = ErrorLevelClick
        end
        object Error1: TMenuItem
          Tag = 2
          Caption = '&Error'
          OnClick = ErrorLevelClick
        end
        object Information1: TMenuItem
          Tag = 1
          Caption = '&Information'
          OnClick = ErrorLevelClick
        end
        object Full1: TMenuItem
          Tag = 3
          Caption = '&Full'
          OnClick = ErrorLevelClick
        end
      end
      object ShowUIItem: TMenuItem
        Caption = 'Show User Interface'
        Checked = True
        OnClick = ShowUIItemClick
      end
      object DisableAfterItem: TMenuItem
        Caption = 'Disable After Acquire'
        Checked = True
        OnClick = DisableAfterItemClick
      end
      object NegotiateItem: TMenuItem
        Caption = 'Negotiate'
        OnClick = NegotiateItemClick
      end
      object ADFItem: TMenuItem
        Caption = 'ADF'
        object EnableADFItem: TMenuItem
          Caption = 'Enable ADF'
          OnClick = EnableADFItemClick
        end
        object EnableAutoFeedItem: TMenuItem
          Caption = 'Enable Auto Feed'
          OnClick = EnableAutoFeedItemClick
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object StretchImageItem: TMenuItem
        Caption = 'Stretch Image'
        Checked = True
        OnClick = StretchImageItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object AboutItem: TMenuItem
      Caption = '&About'
      OnClick = AboutItemClick
    end
  end
end
