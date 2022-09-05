object FormTWAIN: TFormTWAIN
  Left = 308
  Top = 140
  Width = 444
  Height = 385
  Caption = 'TWAIN Toolkit for Delphi'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 436
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
    object SourceList: TComboBox
      Left = 88
      Top = 8
      Width = 233
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 312
    Width = 436
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 41
    Width = 436
    Height = 271
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 3
    object Image1: TImage
      Left = 24
      Top = 88
      Width = 321
      Height = 137
      Center = True
      IncrementalDisplay = True
    end
  end
  object mcmTWAIN: TmcmTWAIN
    Left = 40
    Top = 55
    Width = 28
    Height = 28
    Country = UNITEDKINGDOM
    DeviceEventTypes = [TWDE_DEVICEOFFLINE, TWDE_DEVICEREADY, TWDE_DEVICEREMOVED]
    FileFormats = [TWFF_BMP, TWFF_JFIF]
    Filename = '.\image1.bmp'
    LenientOnCaps = True
    LogFilename = '.\APPTWN.LOG'
    LogToFile = True
    Manufacturer = 'MCM DESIGN'
    MessageLevel = ML_INFO
    ProductFamily = 'MCM DESIGN, Imaging & TWAIN Tool'
    ProductName = 'TWAIN Toolkit for Delphi'
    ReturnHandle = False
    OnCloseSource = mcmTWAINCloseSource
    OnDeviceEvent = mcmTWAINDeviceEvent
    OnDeviceNotReady = mcmTWAINDeviceNotReady
    OnDisableMenus = mcmTWAINDisableMenus
    OnEnableMenus = mcmTWAINEnableMenus
    OnFailure = mcmTWAINFailure
    OnImageReady = mcmTWAINImageReady
    OnMemXferSize = mcmTWAINMemXferSize
    OnNegotiation = mcmTWAINNegotiation
    OnXferNext = mcmTWAINXferNext
    OnXferReady = mcmTWAINXferReady
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
      object SourceIconItem: TMenuItem
        Caption = 'Source Icon ...'
        Visible = False
        OnClick = SourceIconItemClick
      end
      object PreferenceItem: TMenuItem
        Caption = '&Preference ...'
        OnClick = PreferenceItemClick
      end
      object ConfigureSourceItem: TMenuItem
        Caption = 'Configure Source'
        OnClick = ConfigureSourceItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ColorFormatMenu: TMenuItem
        Caption = 'Color Format'
        object BWItem: TMenuItem
          Caption = 'BW'
          GroupIndex = 10
          OnClick = ColorFormatItemClick
        end
        object GrayItem: TMenuItem
          Tag = 1
          Caption = 'Gray'
          GroupIndex = 10
          OnClick = ColorFormatItemClick
        end
        object PaletteItem: TMenuItem
          Tag = 3
          Caption = 'Palette'
          GroupIndex = 10
          OnClick = ColorFormatItemClick
        end
        object RGBItem: TMenuItem
          Tag = 2
          Caption = 'RGB'
          GroupIndex = 10
          OnClick = ColorFormatItemClick
        end
      end
      object TransferMode1: TMenuItem
        Caption = 'Transfer Mode'
        OnClick = TransferMode1Click
        object Native1: TMenuItem
          Caption = '&Native'
          Checked = True
          GroupIndex = 20
          OnClick = XFerClick
        end
        object File2: TMenuItem
          Tag = 1
          Caption = '&File'
          GroupIndex = 20
          OnClick = XFerClick
        end
        object Memory1: TMenuItem
          Tag = 2
          Caption = '&Memory'
          GroupIndex = 20
          OnClick = XFerClick
        end
      end
      object ErrorLevel1: TMenuItem
        Caption = 'Error &Level'
        OnClick = ErrorLevel1Click
        object None1: TMenuItem
          Caption = '&None'
          Checked = True
          GroupIndex = 30
          OnClick = ErrorLevelClick
        end
        object Error1: TMenuItem
          Tag = 2
          Caption = '&Error'
          GroupIndex = 30
          OnClick = ErrorLevelClick
        end
        object Information1: TMenuItem
          Tag = 1
          Caption = '&Information'
          GroupIndex = 30
          OnClick = ErrorLevelClick
        end
        object Full1: TMenuItem
          Tag = 3
          Caption = '&Full'
          GroupIndex = 30
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
      object EnableNegotiatItem: TMenuItem
        Caption = 'Enable Negotiation'
        OnClick = EnableNegotiatItemClick
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
    object SpecialMenu: TMenuItem
      Caption = '&Special'
      object OpenSMItem: TMenuItem
        Caption = '&1 Open Source Manager'
        OnClick = OpenSMItemClick
      end
      object OpenSItem: TMenuItem
        Caption = '&2 Open Source'
        Enabled = False
        OnClick = OpenSItemClick
      end
      object SendItem: TMenuItem
        Caption = '&3 Send ...'
        Enabled = False
        OnClick = SendItemClick
      end
      object EnableItem: TMenuItem
        Caption = '&4 Enable'
        Enabled = False
        OnClick = EnableItemClick
      end
      object EnableTransferItem: TMenuItem
        Caption = '&5 Enable / Transfer'
        Enabled = False
        OnClick = EnableTransferItemClick
      end
      object DisableItem: TMenuItem
        Caption = '&6 Disable'
        Enabled = False
        OnClick = DisableItemClick
      end
      object CloseSItem: TMenuItem
        Caption = '&7 Close Source'
        Enabled = False
        OnClick = CloseSItemClick
      end
      object CloseSMItem: TMenuItem
        Caption = '&8 Close Source Manager'
        Enabled = False
        OnClick = CloseSMItemClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object SourceInfoItem: TMenuItem
        Caption = 'Source Info'
        OnClick = SourceInfoItemClick
      end
    end
    object AboutItem: TMenuItem
      Caption = '&About'
      OnClick = AboutItemClick
    end
  end
  object mcmSTI: TmcmSTI
    Left = 72
    Top = 56
  end
end
