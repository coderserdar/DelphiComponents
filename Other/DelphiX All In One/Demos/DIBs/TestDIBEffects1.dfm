object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DIB effects'
  ClientHeight = 554
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object FlowPanel1: TPanel
    Left = 0
    Top = 309
    Width = 553
    Height = 245
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object btnSpray: TButton
      Tag = 1
      Left = 5
      Top = 5
      Width = 90
      Height = 25
      Caption = 'Spray'
      TabOrder = 0
      OnClick = btnAutoClick
    end
    object btnEmboss: TButton
      Tag = 2
      Left = 95
      Top = 5
      Width = 90
      Height = 25
      Caption = 'Emboss'
      TabOrder = 1
      OnClick = btnAutoClick
    end
    object btnMonoNoise: TButton
      Tag = 3
      Left = 185
      Top = 5
      Width = 90
      Height = 25
      Caption = 'MonoNoise'
      TabOrder = 2
      OnClick = btnAutoClick
    end
    object btnGradientNoise: TButton
      Tag = 4
      Left = 275
      Top = 5
      Width = 90
      Height = 25
      Caption = 'GradientNoise'
      TabOrder = 3
      OnClick = btnAutoClick
    end
    object btnTwist: TButton
      Tag = 5
      Left = 365
      Top = 5
      Width = 90
      Height = 25
      Caption = 'Twist'
      TabOrder = 4
      OnClick = btnAutoClick
    end
    object btnFishEye: TButton
      Tag = 6
      Left = 455
      Top = 5
      Width = 90
      Height = 25
      Caption = 'FishEye'
      TabOrder = 5
      OnClick = btnAutoClick
    end
    object btnSmoothRotWr: TButton
      Tag = 7
      Left = 5
      Top = 30
      Width = 90
      Height = 25
      Caption = 'SmoothRotWr'
      TabOrder = 6
      OnClick = btnAutoClick
    end
    object btnLightness: TButton
      Tag = 8
      Left = 95
      Top = 30
      Width = 90
      Height = 25
      Caption = 'Lightness'
      TabOrder = 7
      OnClick = btnAutoClick
    end
    object btnSaturation: TButton
      Tag = 9
      Left = 185
      Top = 30
      Width = 90
      Height = 25
      Caption = 'Saturation'
      TabOrder = 8
      OnClick = btnAutoClick
    end
    object btnContrast: TButton
      Tag = 10
      Left = 275
      Top = 30
      Width = 90
      Height = 25
      Caption = 'Contrast'
      TabOrder = 9
      OnClick = btnAutoClick
    end
    object btnAddRGB: TButton
      Tag = 11
      Left = 365
      Top = 30
      Width = 90
      Height = 25
      Caption = 'AddRGB'
      TabOrder = 10
      OnClick = btnAutoClick
    end
    object btnFilter: TButton
      Tag = 12
      Left = 455
      Top = 30
      Width = 90
      Height = 25
      Caption = 'Filter'
      TabOrder = 11
      OnClick = btnAutoClick
    end
    object btnSharpen: TButton
      Tag = 13
      Left = 5
      Top = 55
      Width = 90
      Height = 25
      Caption = 'Sharpen'
      TabOrder = 12
      OnClick = btnAutoClick
    end
    object btnRotate: TButton
      Tag = 14
      Left = 95
      Top = 55
      Width = 90
      Height = 25
      Caption = 'Rotate'
      TabOrder = 13
      OnClick = btnAutoClick
    end
    object btnSplitBlur: TButton
      Tag = 15
      Left = 185
      Top = 55
      Width = 90
      Height = 25
      Caption = 'SplitBlur'
      TabOrder = 14
      OnClick = btnAutoClick
    end
    object btnGaussianBlur: TButton
      Tag = 16
      Left = 275
      Top = 55
      Width = 90
      Height = 25
      Caption = 'GaussianBlur'
      TabOrder = 15
      OnClick = btnAutoClick
    end
    object btnDoInvert: TButton
      Tag = 17
      Left = 365
      Top = 55
      Width = 90
      Height = 25
      Caption = 'DoInvert'
      TabOrder = 16
      OnClick = btnAutoClick
    end
    object btnDoAddColorNoise: TButton
      Tag = 18
      Left = 455
      Top = 55
      Width = 90
      Height = 25
      Caption = 'DoAddColorNoise'
      TabOrder = 17
      OnClick = btnAutoClick
    end
    object btnDoAddMonoNoise: TButton
      Tag = 19
      Left = 5
      Top = 80
      Width = 90
      Height = 25
      Caption = 'DoAddMonoNoise'
      TabOrder = 18
      OnClick = btnAutoClick
    end
    object btnDoAntiAlias: TButton
      Tag = 20
      Left = 95
      Top = 80
      Width = 90
      Height = 25
      Caption = 'DoAntiAlias'
      TabOrder = 19
      OnClick = btnAutoClick
    end
    object btnDoContrast: TButton
      Tag = 21
      Left = 185
      Top = 80
      Width = 90
      Height = 25
      Caption = 'DoContrast'
      TabOrder = 20
      OnClick = btnAutoClick
    end
    object btnDoFishEye: TButton
      Tag = 22
      Left = 275
      Top = 80
      Width = 90
      Height = 25
      Caption = 'DoFishEye'
      TabOrder = 21
      OnClick = btnAutoClick
    end
    object btnDoGrayScale: TButton
      Tag = 23
      Left = 365
      Top = 80
      Width = 90
      Height = 25
      Caption = 'DoGrayScale'
      TabOrder = 22
      OnClick = btnAutoClick
    end
    object btnDoLightness: TButton
      Tag = 24
      Left = 455
      Top = 80
      Width = 90
      Height = 25
      Caption = 'DoLightness'
      TabOrder = 23
      OnClick = btnAutoClick
    end
    object btnDoDarkness: TButton
      Tag = 25
      Left = 5
      Top = 105
      Width = 90
      Height = 25
      Caption = 'DoDarkness'
      TabOrder = 24
      OnClick = btnAutoClick
    end
    object btnDoSaturation: TButton
      Tag = 26
      Left = 95
      Top = 105
      Width = 90
      Height = 25
      Caption = 'DoSaturation'
      TabOrder = 25
      OnClick = btnAutoClick
    end
    object btnDoSplitBlur: TButton
      Tag = 27
      Left = 185
      Top = 105
      Width = 90
      Height = 25
      Caption = 'DoSplitBlur'
      TabOrder = 26
      OnClick = btnAutoClick
    end
    object btnDoGaussianBlur: TButton
      Tag = 28
      Left = 275
      Top = 105
      Width = 90
      Height = 25
      Caption = 'DoGaussianBlur'
      TabOrder = 27
      OnClick = btnAutoClick
    end
    object btnDoMosaic: TButton
      Tag = 29
      Left = 365
      Top = 105
      Width = 90
      Height = 25
      Caption = 'DoMosaic'
      TabOrder = 28
      OnClick = btnAutoClick
    end
    object btnDoTwist: TButton
      Tag = 30
      Left = 455
      Top = 105
      Width = 90
      Height = 25
      Caption = 'DoTwist'
      TabOrder = 29
      OnClick = btnAutoClick
    end
    object btnDoSplitlight: TButton
      Tag = 31
      Left = 5
      Top = 130
      Width = 90
      Height = 25
      Caption = 'DoSplitlight'
      TabOrder = 30
      OnClick = btnAutoClick
    end
    object btnDoTile: TButton
      Tag = 32
      Left = 95
      Top = 130
      Width = 90
      Height = 25
      Caption = 'DoTile'
      TabOrder = 31
      OnClick = btnAutoClick
    end
    object btnDoSpotLight: TButton
      Tag = 33
      Left = 185
      Top = 130
      Width = 90
      Height = 25
      Caption = 'DoSpotLight'
      TabOrder = 32
      OnClick = btnAutoClick
    end
    object btnDoTrace: TButton
      Tag = 34
      Left = 275
      Top = 130
      Width = 90
      Height = 25
      Caption = 'DoTrace'
      TabOrder = 33
      OnClick = btnAutoClick
    end
    object btnDoEmboss: TButton
      Tag = 35
      Left = 365
      Top = 130
      Width = 90
      Height = 25
      Caption = 'DoEmboss'
      TabOrder = 34
      OnClick = btnAutoClick
    end
    object btnDoSolorize: TButton
      Tag = 36
      Left = 455
      Top = 130
      Width = 90
      Height = 25
      Caption = 'DoSolorize'
      TabOrder = 35
      OnClick = btnAutoClick
    end
    object btnDoPosterize: TButton
      Tag = 37
      Left = 5
      Top = 155
      Width = 90
      Height = 25
      Caption = 'DoPosterize'
      TabOrder = 36
      OnClick = btnAutoClick
    end
    object btnDoColorize: TButton
      Tag = 38
      Left = 95
      Top = 155
      Width = 90
      Height = 25
      Caption = 'DoColorize'
      TabOrder = 37
      OnClick = btnAutoClick
    end
    object btnDoBrightness: TButton
      Tag = 39
      Left = 185
      Top = 155
      Width = 90
      Height = 25
      Caption = 'DoBrightness'
      TabOrder = 38
      OnClick = btnAutoClick
    end
    object btnDoResample: TButton
      Tag = 40
      Left = 275
      Top = 155
      Width = 90
      Height = 25
      Caption = 'DoResample'
      TabOrder = 39
      OnClick = btnAutoClick
    end
    object btnDoSmoothRotate: TButton
      Tag = 41
      Left = 365
      Top = 155
      Width = 90
      Height = 25
      Caption = 'DoSmoothRotate'
      TabOrder = 40
      OnClick = btnAutoClick
    end
    object btnNova: TButton
      Tag = 42
      Left = 455
      Top = 155
      Width = 90
      Height = 25
      Caption = 'Nova'
      TabOrder = 41
      OnClick = btnAutoClick
    end
    object btnMandel: TButton
      Tag = 43
      Left = 5
      Top = 180
      Width = 90
      Height = 25
      Caption = 'MandelSet'
      TabOrder = 42
      OnClick = btnAutoClick
    end
    object btnOldPhotos: TButton
      Tag = 44
      Left = 95
      Top = 180
      Width = 90
      Height = 25
      Caption = 'OldPhotos'
      TabOrder = 43
      OnClick = btnAutoClick
    end
    object btnBlendPixel: TButton
      Tag = 45
      Left = 185
      Top = 180
      Width = 90
      Height = 25
      Caption = 'BlendPixel'
      TabOrder = 44
      OnClick = btnAutoClick
    end
    object btnLinePolar: TButton
      Tag = 46
      Left = 275
      Top = 180
      Width = 90
      Height = 25
      Caption = 'LinePolar'
      TabOrder = 45
      OnClick = btnAutoClick
    end
    object btnDarker: TButton
      Tag = 47
      Left = 365
      Top = 180
      Width = 90
      Height = 25
      Caption = 'Darker'
      TabOrder = 46
      OnClick = btnAutoClick
    end
    object btnLighter: TButton
      Tag = 48
      Left = 455
      Top = 180
      Width = 90
      Height = 25
      Caption = 'Lighter'
      TabOrder = 47
      OnClick = btnAutoClick
    end
    object btnEncrypt: TButton
      Tag = 49
      Left = 5
      Top = 205
      Width = 90
      Height = 25
      Caption = 'Encrypt'
      TabOrder = 48
      OnClick = btnAutoClick
    end
    object btnDecrypt: TButton
      Tag = 50
      Left = 95
      Top = 205
      Width = 90
      Height = 25
      Caption = 'Decrypt'
      TabOrder = 49
      OnClick = btnAutoClick
    end
    object btnDoZoom: TButton
      Tag = 51
      Left = 185
      Top = 205
      Width = 90
      Height = 25
      Caption = 'DoZoom'
      Enabled = False
      TabOrder = 50
      OnClick = btnAutoClick
    end
    object btnInk: TButton
      Tag = 52
      Left = 275
      Top = 205
      Width = 90
      Height = 25
      Caption = 'Ink'
      TabOrder = 51
      OnClick = btnAutoClick
    end
    object btnDoRotate: TButton
      Tag = 53
      Left = 365
      Top = 205
      Width = 90
      Height = 25
      Caption = 'DoRotate'
      TabOrder = 52
      OnClick = btnAutoClick
    end
    object btnDistort: TButton
      Tag = 54
      Left = 455
      Top = 205
      Width = 90
      Height = 25
      Caption = 'Distort'
      TabOrder = 53
      OnClick = btnAutoClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 553
    Height = 309
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 62
      Height = 13
      Caption = 'Own picture:'
    end
    object ScrollBox2: TScrollBox
      Left = 285
      Top = 47
      Width = 256
      Height = 256
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 4
      object Image2: TImage
        Left = 0
        Top = 0
        Width = 256
        Height = 256
      end
    end
    object ScrollBox1: TScrollBox
      Left = 12
      Top = 47
      Width = 256
      Height = 256
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 3
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 256
        Height = 256
        AutoSize = True
      end
    end
    object FilenameEdit1: TFilenameEdit
      Left = 76
      Top = 5
      Width = 465
      Height = 21
      OnAfterDialog = FilenameEdit1AfterDialog
      DefaultExt = 'bmp'
      Filter = 'Bitmap image (*.bmp)|*.bmp'
      DialogTitle = 'Open bitmap'
      NumGlyphs = 1
      TabOrder = 0
    end
    object StaticText2: TStaticText
      Left = 285
      Top = 29
      Width = 29
      Height = 17
      Caption = 'After'
      TabOrder = 2
    end
    object StaticText1: TStaticText
      Left = 12
      Top = 29
      Width = 36
      Height = 17
      Caption = 'Before'
      TabOrder = 1
    end
  end
  object DXDIB1: TDXDIB
    Left = 48
    Top = 24
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 80
    Top = 24
  end
end
