object FormRotate: TFormRotate
  Left = 367
  Top = 141
  BorderStyle = bsDialog
  Caption = 'Rotate'
  ClientHeight = 184
  ClientWidth = 279
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bntOK: TButton
    Left = 8
    Top = 152
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object bntCancel: TButton
    Left = 88
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object rgDirection: TRadioGroup
    Left = 8
    Top = 8
    Width = 81
    Height = 81
    Caption = 'Direction'
    Items.Strings = (
      '&Right'
      '&Left')
    TabOrder = 2
  end
  object gbDegree: TGroupBox
    Left = 96
    Top = 8
    Width = 177
    Height = 81
    Caption = 'Degrees'
    TabOrder = 3
    object rb90: TRadioButton
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Caption = '&90'
      TabOrder = 0
      OnClick = rb90Click
    end
    object rb180: TRadioButton
      Left = 64
      Top = 24
      Width = 49
      Height = 17
      Caption = '&180'
      TabOrder = 1
      OnClick = rb180Click
    end
    object rb270: TRadioButton
      Left = 120
      Top = 24
      Width = 49
      Height = 17
      Caption = '&270'
      TabOrder = 2
      OnClick = rb270Click
    end
    object rsDegree: TmcmRealSpin
      Left = 64
      Top = 48
      Width = 73
      Height = 22
      TabOrder = 4
      OnMouseDown = rsDegreeMouseDown
      Value = 90.000000000000000000
      MaxValue = 355.990000000000000000
      MinValue = 0.010000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object rbFree: TRadioButton
      Left = 8
      Top = 53
      Width = 49
      Height = 17
      Caption = '&Free'
      TabOrder = 3
      OnClick = rbFreeClick
    end
  end
  object gbMethod: TGroupBox
    Left = 8
    Top = 89
    Width = 265
    Height = 56
    Caption = 'Interpolate'
    TabOrder = 4
    object lResizeMethod: TLabel
      Left = 16
      Top = 28
      Width = 39
      Height = 13
      Caption = '&Method:'
      FocusControl = cbMethod
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
    end
  end
end
