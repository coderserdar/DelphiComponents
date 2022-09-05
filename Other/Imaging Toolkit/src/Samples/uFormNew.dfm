object FormNewImage: TFormNewImage
  Left = 200
  Top = 116
  BorderStyle = bsDialog
  Caption = 'New Image'
  ClientHeight = 274
  ClientWidth = 282
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbImageDimension: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 113
    Caption = 'Image Dimensions'
    TabOrder = 0
    object lWidth: TLabel
      Left = 16
      Top = 32
      Width = 31
      Height = 13
      Caption = '&Width:'
      FocusControl = rsWidth
    end
    object lHeight: TLabel
      Left = 16
      Top = 60
      Width = 34
      Height = 13
      Caption = '&Height:'
      FocusControl = rsHeight
    end
    object lResolution: TLabel
      Left = 16
      Top = 88
      Width = 53
      Height = 13
      Caption = '&Resolution:'
      FocusControl = rsResolution
    end
    object rsWidth: TmcmRealSpin
      Left = 88
      Top = 24
      Width = 65
      Height = 22
      TabOrder = 0
      Value = 256.000000000000000000
      MaxValue = 65535.000000000000000000
      Decimals = 0
      Increment = 1.000000000000000000
    end
    object rsHeight: TmcmRealSpin
      Left = 88
      Top = 52
      Width = 65
      Height = 22
      TabOrder = 1
      Value = 256.000000000000000000
      MaxValue = 65535.000000000000000000
      Decimals = 0
      Increment = 1.000000000000000000
    end
    object rsResolution: TmcmRealSpin
      Left = 88
      Top = 80
      Width = 65
      Height = 22
      TabOrder = 3
      Value = 72.000000000000000000
      MaxValue = 9999.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object cbResPerUnit: TComboBox
      Left = 168
      Top = 80
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Pixels/Inch'
        'Pixels/cm')
      TabOrder = 4
      OnChange = cbResPerUnitChange
    end
    object cbUnit: TComboBox
      Left = 168
      Top = 24
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Pixels'
        'Inch'
        'cm')
      TabOrder = 2
      OnChange = cbUnitChange
    end
  end
  object gbImageCharacteristics: TGroupBox
    Left = 8
    Top = 128
    Width = 265
    Height = 105
    Caption = 'Image Characteristics'
    TabOrder = 1
    object lColorDepth: TLabel
      Left = 16
      Top = 32
      Width = 57
      Height = 13
      Caption = 'Color &depth:'
      FocusControl = cbColorDepth
    end
    object icColorBk: TmcmImageCtrl
      Left = 88
      Top = 56
      Width = 64
      Height = 25
      Cursor = 128
      BorderStyle = BS_SUNKEN
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleToFit = False
      OnClick = icColorBkClick
    end
    object lColorBk: TLabel
      Left = 16
      Top = 64
      Width = 27
      Height = 13
      Caption = '&Color:'
    end
    object cbColorDepth: TComboBox
      Left = 88
      Top = 24
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        '2 Colour, 1 bit'
        '16 Colours, 4 bit'
        '16 Greys, 4 bit'
        '256 Colours, 8 bit'
        '256 Greys, 8 bit'
        '16.7 Million Colours, 24 bit'
        '16.7 Million Colours, 32 bit')
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 8
    Top = 242
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 88
    Top = 242
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnHelp: TButton
    Left = 200
    Top = 242
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 4
    OnClick = btnHelpClick
  end
end
