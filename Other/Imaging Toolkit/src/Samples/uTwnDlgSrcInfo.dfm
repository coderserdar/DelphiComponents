object FormSrcInfo: TFormSrcInfo
  Left = 393
  Top = 199
  BorderStyle = bsDialog
  Caption = 'Data Source Info'
  ClientHeight = 311
  ClientWidth = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object gbInformation: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 265
    Caption = 'Data Source Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lTWAINID: TLabel
      Left = 16
      Top = 24
      Width = 53
      Height = 15
      Caption = 'TWAIN Id:'
    end
    object lAppVersion: TLabel
      Left = 16
      Top = 48
      Width = 45
      Height = 15
      Caption = 'Version:'
    end
    object lAppLanguage: TLabel
      Left = 16
      Top = 72
      Width = 92
      Height = 15
      Caption = 'Language Code:'
    end
    object lAppCountry: TLabel
      Left = 16
      Top = 96
      Width = 78
      Height = 15
      Caption = 'Country Code:'
    end
    object lAppInfo: TLabel
      Left = 16
      Top = 120
      Width = 65
      Height = 15
      Caption = 'Information:'
    end
    object lAppProtocol: TLabel
      Left = 16
      Top = 144
      Width = 48
      Height = 15
      Caption = 'Protocol:'
    end
    object lAppGroups: TLabel
      Left = 16
      Top = 168
      Width = 44
      Height = 15
      Caption = 'Groups:'
    end
    object lAppManufacturer: TLabel
      Left = 16
      Top = 192
      Width = 74
      Height = 15
      Caption = 'Manufacturer:'
    end
    object lAppProductFamily: TLabel
      Left = 16
      Top = 216
      Width = 84
      Height = 15
      Caption = 'Product Family:'
    end
    object lAppProductName: TLabel
      Left = 16
      Top = 240
      Width = 82
      Height = 15
      Caption = 'Product Name:'
    end
    object lValTWAINID: TLabel
      Left = 120
      Top = 24
      Width = 69
      Height = 15
      Caption = 'lValTWAINID'
    end
    object lValVersion: TLabel
      Left = 120
      Top = 48
      Width = 62
      Height = 15
      Caption = 'lValVersion'
    end
    object lValLanguage: TLabel
      Left = 120
      Top = 72
      Width = 76
      Height = 15
      Caption = 'lValLanguage'
    end
    object lValCountry: TLabel
      Left = 120
      Top = 96
      Width = 62
      Height = 15
      Caption = 'lValCountry'
    end
    object lValInfo: TLabel
      Left = 120
      Top = 120
      Width = 40
      Height = 15
      Caption = 'lValInfo'
    end
    object lValProtocol: TLabel
      Left = 120
      Top = 144
      Width = 65
      Height = 15
      Caption = 'lValProtocol'
    end
    object lValGroups: TLabel
      Left = 120
      Top = 168
      Width = 61
      Height = 15
      Caption = 'lValGroups'
    end
    object lValManufacturer: TLabel
      Left = 120
      Top = 192
      Width = 91
      Height = 15
      Caption = 'lValManufacturer'
    end
    object lValProductFamily: TLabel
      Left = 120
      Top = 216
      Width = 98
      Height = 15
      Caption = 'lValProductFamily'
    end
    object lValProductName: TLabel
      Left = 120
      Top = 240
      Width = 96
      Height = 15
      Caption = 'lValProductName'
    end
  end
  object btnOK: TButton
    Left = 143
    Top = 280
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
