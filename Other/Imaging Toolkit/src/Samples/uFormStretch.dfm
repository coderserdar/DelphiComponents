object FormResize: TFormResize
  Left = 341
  Top = 113
  BorderStyle = bsDialog
  Caption = 'Scale / Stretch'
  ClientHeight = 390
  ClientWidth = 264
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbPixelSize: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 57
    Caption = 'Pixel size'
    TabOrder = 2
    object lPixelWidth: TLabel
      Left = 16
      Top = 29
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lPixelHeight: TLabel
      Left = 136
      Top = 29
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object sePixelWidth: TmcmIntSpin
      Left = 64
      Top = 24
      Width = 57
      Height = 22
      TabOrder = 0
      OnChange = sePixelWidthChange
      Value = 1
      MaxValue = 65536
      MinValue = 1
    end
    object sePixelHeight: TmcmIntSpin
      Left = 176
      Top = 24
      Width = 57
      Height = 22
      TabOrder = 1
      OnChange = sePixelHeightChange
      Value = 1
      MaxValue = 65536
      MinValue = 1
    end
  end
  object gbPercentage: TGroupBox
    Left = 8
    Top = 72
    Width = 249
    Height = 57
    Caption = 'Percentage of original'
    TabOrder = 3
    object lPercentWidth: TLabel
      Left = 16
      Top = 29
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lPercentHeight: TLabel
      Left = 136
      Top = 29
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object rsPercentWidth: TmcmRealSpin
      Left = 64
      Top = 24
      Width = 57
      Height = 22
      TabOrder = 0
      OnChange = rsPercentWidthChange
      Value = 1.000000000000000000
      MaxValue = 10000.000000000000000000
      MinValue = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object rsPercentHeight: TmcmRealSpin
      Left = 176
      Top = 24
      Width = 57
      Height = 22
      TabOrder = 1
      OnChange = rsPercentHeightChange
      Value = 1.000000000000000000
      MaxValue = 10000.000000000000000000
      MinValue = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
  end
  object btnOK: TButton
    Left = 8
    Top = 360
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 88
    Top = 360
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbActualSize: TGroupBox
    Left = 8
    Top = 136
    Width = 249
    Height = 121
    Caption = 'Actual size'
    TabOrder = 4
    object lActualWidth: TLabel
      Left = 16
      Top = 29
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lActualHeight: TLabel
      Left = 16
      Top = 61
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object lActualRes: TLabel
      Left = 16
      Top = 93
      Width = 53
      Height = 13
      Caption = 'Resolution:'
    end
    object rsActualWidth: TmcmRealSpin
      Left = 80
      Top = 24
      Width = 65
      Height = 22
      MaxLength = 6
      TabOrder = 0
      OnChange = rsActualWidthChange
      Value = 1.000000000000000000
      MaxValue = 10000.000000000000000000
      Decimals = 2
      Increment = 0.100000000000000000
    end
    object rsActualHeight: TmcmRealSpin
      Left = 80
      Top = 56
      Width = 65
      Height = 22
      MaxLength = 6
      TabOrder = 1
      OnChange = rsActualHeightChange
      Value = 1.000000000000000000
      MaxValue = 10000.000000000000000000
      Decimals = 2
      Increment = 0.100000000000000000
    end
    object rsActualRes: TmcmRealSpin
      Left = 80
      Top = 88
      Width = 65
      Height = 22
      TabOrder = 3
      OnChange = rsActualResChange
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 0.010000000000000000
    end
    object cbActualRes: TComboBox
      Left = 160
      Top = 88
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'pixel / cm'
        'pixel / inch')
      TabOrder = 4
      OnChange = cbActualResChange
    end
    object cbMeasure: TComboBox
      Left = 160
      Top = 40
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'cm'
        'inch')
      TabOrder = 2
      OnChange = cbMeasureChange
    end
  end
  object gbMethod: TGroupBox
    Left = 8
    Top = 264
    Width = 249
    Height = 89
    Caption = 'Resize'
    TabOrder = 5
    object lResizeMethod: TLabel
      Left = 16
      Top = 28
      Width = 39
      Height = 13
      Caption = 'Method:'
    end
    object lto1: TLabel
      Left = 216
      Top = 61
      Width = 18
      Height = 13
      Caption = 'to 1'
    end
    object cbAspectRatio: TCheckBox
      Left = 16
      Top = 59
      Width = 129
      Height = 17
      Caption = 'Maintain aspect ratio'
      State = cbChecked
      TabOrder = 1
      OnClick = cbAspectRatioClick
    end
    object cbMethod: TComboBox
      Left = 64
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Nearest Neighbour'
        'Bilinear'
        'Biquadratic'
        'Bicubic'
        'Hermite')
      TabOrder = 0
      OnChange = cbMethodChange
    end
    object rsAspectRatio: TmcmRealSpin
      Left = 144
      Top = 56
      Width = 65
      Height = 22
      TabOrder = 2
      OnChange = rsAspectRatioChange
      Value = 1.330000000000000000
      MaxValue = 10000.000000000000000000
      Decimals = 4
      Increment = 0.001000000000000000
    end
  end
end
