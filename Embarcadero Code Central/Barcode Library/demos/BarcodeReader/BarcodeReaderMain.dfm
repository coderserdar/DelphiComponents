object Form1: TForm1
  Left = 121
  Top = 105
  Width = 545
  Height = 431
  Caption = 'Barcode reader component demo (http://www.psoft.sk)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 312
    Top = 8
    Width = 185
    Height = 57
    AutoSize = False
    Caption = 
      'Timeout-time between last char read and OnBarcodeReady event (ms' +
      ')'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 289
    Height = 65
    AutoSize = False
    Caption = 
      'Please connect your barcode reader and try read some printed bar' +
      'code.'#13#10'For latest version please visit http://www.psoft.sk'
    Color = clScrollBar
    ParentColor = False
    WordWrap = True
  end
  object Ean: TEan
    Left = 8
    Top = 80
    Width = 297
    Height = 153
    AutoInc = False
    AutoIncFrom = 0
    AutoIncTo = 0
    BackgroundColor = clWhite
    Transparent = False
    ShowLabels = True
    StartStopLines = True
    TypBarCode = bcEan13
    LinesColor = clBlack
    Ean13AddUp = True
    FontAutoSize = True
    Security = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    BarCode = '9771210107001'
    DemoVersion = False
    Caption.Visible = True
    Caption.Font.Charset = DEFAULT_CHARSET
    Caption.Font.Color = clWindowText
    Caption.Font.Height = -13
    Caption.Font.Name = 'Arial'
    Caption.Font.Style = []
    Caption.Alignment = taLeftJustify
    CaptionBottom.Visible = True
    CaptionBottom.Font.Charset = DEFAULT_CHARSET
    CaptionBottom.Font.Color = clWindowText
    CaptionBottom.Font.Height = -13
    CaptionBottom.Font.Name = 'Arial'
    CaptionBottom.Font.Style = []
    CaptionBottom.Alignment = taLeftJustify
    HorzLines.LinesCount = 0
    AutoCheckDigit = True
  end
  object Barcodes: TMemo
    Left = 8
    Top = 240
    Width = 297
    Height = 161
    TabOrder = 1
  end
  object E_PORT: TRadioGroup
    Left = 432
    Top = 80
    Width = 97
    Height = 105
    Caption = 'Port'
    ItemIndex = 1
    Items.Strings = (
      'Com1'
      'Com2'
      'Com3'
      'Com4')
    TabOrder = 2
    OnClick = E_PORTClick
  end
  object E_SPEED: TRadioGroup
    Left = 432
    Top = 184
    Width = 97
    Height = 217
    Caption = 'Speed'
    ItemIndex = 5
    Items.Strings = (
      '300'
      '600'
      '1200'
      '2400'
      '4800'
      '9600'
      '14400'
      '19200'
      '38400'
      '57600'
      '115200')
    TabOrder = 3
    OnClick = E_SPEEDClick
  end
  object E_PARITY: TRadioGroup
    Left = 312
    Top = 152
    Width = 105
    Height = 105
    Caption = 'Parity'
    ItemIndex = 3
    Items.Strings = (
      'None'
      'Odd'
      'Space'
      'Even'
      'Mark')
    TabOrder = 4
    OnClick = E_PARITYClick
  end
  object E_DATABITS: TRadioGroup
    Left = 312
    Top = 264
    Width = 105
    Height = 65
    Caption = 'Data bits'
    ItemIndex = 1
    Items.Strings = (
      '7'
      '8')
    TabOrder = 5
    OnClick = E_DATABITSClick
  end
  object E_STOPBITS: TRadioGroup
    Left = 312
    Top = 336
    Width = 105
    Height = 65
    Caption = 'Stop bits'
    ItemIndex = 0
    Items.Strings = (
      '1'
      '1.5'
      '2')
    TabOrder = 6
    OnClick = E_STOPBITSClick
  end
  object E_TIMEOUT: TEdit
    Left = 496
    Top = 8
    Width = 33
    Height = 24
    TabOrder = 7
    Text = '100'
    OnChange = E_TIMEOUTChange
  end
  object E_REMOVECR: TCheckBox
    Left = 312
    Top = 112
    Width = 97
    Height = 17
    Caption = 'Remove CR'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = E_REMOVECRClick
  end
  object E_REMOVELF: TCheckBox
    Left = 312
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Remove LF'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = E_REMOVELFClick
  end
  object BarcodeReader: TBarcodeReader
    Active = False
    Port = psCom2
    BitRate = br9600
    Parity = parityEven
    DataBits = db8Bits
    StopBits = sbStop1
    OnBarcodeReady = BarcodeReaderBarcodeReady
    Options = [boRemoveCR, boRemoveLF]
    Left = 112
    Top = 288
  end
end
