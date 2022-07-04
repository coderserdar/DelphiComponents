object frmFilterEffects: TfrmFilterEffects
  Left = 318
  Top = 111
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Filter Effects'
  ClientHeight = 454
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BrightnessBox: TGroupBox
    Left = 8
    Top = 8
    Width = 313
    Height = 65
    TabOrder = 0
    object BrightnessLabel: TLabel
      Left = 286
      Top = 29
      Width = 6
      Height = 13
      Caption = '0'
    end
    object BrightnessTrackBar: TTrackBar
      Left = 8
      Top = 24
      Width = 273
      Height = 33
      Max = 255
      Min = -255
      TabOrder = 1
      TickStyle = tsManual
      OnChange = BrightnessTrackBarChange
    end
    object UseBrightnessEffect: TCheckBox
      Left = 8
      Top = 0
      Width = 123
      Height = 17
      Caption = 'Use brightness effect'
      TabOrder = 0
      OnClick = UseBrightnessEffectClick
    end
  end
  object ContrastBox: TGroupBox
    Left = 8
    Top = 80
    Width = 313
    Height = 65
    TabOrder = 1
    object ContrastLabel: TLabel
      Left = 286
      Top = 29
      Width = 6
      Height = 13
      Caption = '0'
    end
    object ContrastTrackBar: TTrackBar
      Left = 8
      Top = 24
      Width = 273
      Height = 33
      Max = 255
      Min = -255
      TabOrder = 1
      TickStyle = tsManual
      OnChange = ContrastTrackBarChange
    end
    object UseContrastEffect: TCheckBox
      Left = 8
      Top = 0
      Width = 113
      Height = 17
      Caption = 'Use contrast effect'
      TabOrder = 0
      OnClick = UseContrastEffectClick
    end
  end
  object ColorAdjustingBox: TGroupBox
    Left = 8
    Top = 152
    Width = 313
    Height = 149
    TabOrder = 2
    object RedLabel: TLabel
      Left = 286
      Top = 28
      Width = 6
      Height = 13
      Caption = '0'
    end
    object GreenLabel: TLabel
      Left = 286
      Top = 68
      Width = 6
      Height = 13
      Caption = '0'
    end
    object BlueLabel: TLabel
      Left = 286
      Top = 108
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label4: TLabel
      Left = 8
      Top = 28
      Width = 32
      Height = 13
      Caption = 'Red   :'
    end
    object Label5: TLabel
      Left = 8
      Top = 68
      Width = 35
      Height = 13
      Caption = 'Green :'
    end
    object Label6: TLabel
      Left = 8
      Top = 108
      Width = 33
      Height = 13
      Caption = 'Blue   :'
    end
    object RedTrackBar: TTrackBar
      Left = 43
      Top = 24
      Width = 238
      Height = 33
      Max = 255
      Min = -255
      TabOrder = 1
      TickStyle = tsManual
      OnChange = RedTrackBarChange
    end
    object GreenTrackBar: TTrackBar
      Left = 43
      Top = 64
      Width = 238
      Height = 33
      Max = 255
      Min = -255
      TabOrder = 2
      TickStyle = tsManual
      OnChange = GreenTrackBarChange
    end
    object BlueTrackBar: TTrackBar
      Left = 43
      Top = 104
      Width = 238
      Height = 33
      Max = 255
      Min = -255
      TabOrder = 3
      TickStyle = tsManual
      OnChange = BlueTrackBarChange
    end
    object UseColorAdjustingEffect: TCheckBox
      Left = 8
      Top = 0
      Width = 145
      Height = 17
      Caption = 'Use color adjusting effect'
      TabOrder = 0
      OnClick = UseColorAdjustingEffectClick
    end
  end
  object SaturationBox: TGroupBox
    Left = 8
    Top = 304
    Width = 313
    Height = 65
    TabOrder = 3
    object SaturationLabel: TLabel
      Left = 285
      Top = 29
      Width = 6
      Height = 13
      Caption = '0'
    end
    object SaturationTrackBar: TTrackBar
      Left = 8
      Top = 24
      Width = 273
      Height = 33
      Max = 255
      TabOrder = 1
      TickStyle = tsManual
      OnChange = SaturationTrackBarChange
    end
    object UseSaturationEffect: TCheckBox
      Left = 8
      Top = 0
      Width = 121
      Height = 17
      Caption = 'Use saturation effect'
      TabOrder = 0
      OnClick = UseSaturationEffectClick
    end
  end
  object SolarizeBox: TGroupBox
    Left = 8
    Top = 376
    Width = 313
    Height = 65
    TabOrder = 4
    object NoiseLabel: TLabel
      Left = 286
      Top = 29
      Width = 6
      Height = 13
      Caption = '0'
    end
    object NoiseTrackBar: TTrackBar
      Left = 8
      Top = 24
      Width = 273
      Height = 33
      Max = 255
      TabOrder = 1
      TickStyle = tsManual
      OnChange = NoiseTrackBarChange
    end
    object UseNoiseEffect: TCheckBox
      Left = 8
      Top = 0
      Width = 97
      Height = 17
      Caption = 'Use noise effect'
      TabOrder = 0
      OnClick = UseNoiseEffectClick
    end
  end
  object CheckGrayScale: TCheckBox
    Left = 343
    Top = 173
    Width = 110
    Height = 17
    Caption = 'Grayscale drawing'
    TabOrder = 7
    OnClick = CheckGrayScaleClick
  end
  object ScreenRotation: TRadioGroup
    Left = 328
    Top = 8
    Width = 137
    Height = 151
    Items.Strings = (
      '90 Degree'
      '180 Degree'
      '270 Degree'
      'Mirror 0 Degree'
      'Mirror 180 Degree')
    TabOrder = 6
    OnClick = ScreenRotationClick
  end
  object OkBtn: TButton
    Left = 352
    Top = 397
    Width = 89
    Height = 25
    Caption = 'Ok'
    TabOrder = 9
    OnClick = OkBtnClick
  end
  object UseScreenRotation: TCheckBox
    Left = 336
    Top = 8
    Width = 121
    Height = 17
    Caption = 'Use screen rotation'
    TabOrder = 5
    OnClick = UseScreenRotationClick
  end
  object DefaultValueBtn: TButton
    Left = 352
    Top = 208
    Width = 89
    Height = 25
    Caption = 'Default Values'
    TabOrder = 8
    OnClick = DefaultValueBtnClick
  end
end
