object FormImageCtrl: TFormImageCtrl
  Left = 293
  Top = 80
  BorderStyle = bsDialog
  Caption = 'mcmImageCtrl demo'
  ClientHeight = 475
  ClientWidth = 825
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object mcmImageCtrl1: TmcmImageCtrl
    Left = 9
    Top = 50
    Width = 376
    Height = 375
    Cursor = crCross
    Color = clWhite
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = False
    OnChange = mcmImageCtrl1Change
    OnMouseMove = mcmImageCtrl1MouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 825
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lScale: TLabel
      Left = 16
      Top = 13
      Width = 30
      Height = 13
      Caption = 'Scale:'
    end
    object cbCenter: TCheckBox
      Left = 136
      Top = 19
      Width = 89
      Height = 17
      Caption = 'Center Image'
      TabOrder = 1
      OnClick = cbCenterClick
    end
    object cbFitToWindow: TCheckBox
      Left = 136
      Top = 3
      Width = 89
      Height = 17
      Caption = 'Fit to window'
      TabOrder = 2
      OnClick = cbFitToWindowClick
    end
    object cbTransparentWindow: TCheckBox
      Left = 232
      Top = 3
      Width = 121
      Height = 17
      Caption = 'Transparent Window'
      TabOrder = 3
      OnClick = cbTransparentWindowClick
    end
    object cbBorder: TCheckBox
      Left = 232
      Top = 19
      Width = 97
      Height = 17
      Caption = 'Show Border'
      TabOrder = 4
      OnClick = cbBorderClick
    end
    object rsScale: TmcmRealSpin
      Left = 56
      Top = 8
      Width = 73
      Height = 22
      TabOrder = 0
      OnChange = rsScaleChange
      Value = 1.000000000000000000
      MaxValue = 1000.000000000000000000
      MinValue = 0.020000000000000000
      Decimals = 2
      Increment = 0.020000000000000000
    end
  end
  object HorzScrollBar: TScrollBar
    Left = 8
    Top = 432
    Width = 377
    Height = 16
    LargeChange = 16
    SmallChange = 8
    TabOrder = 2
    OnChange = ScrollBarChange
  end
  object VertScrollBar: TScrollBar
    Left = 392
    Top = 48
    Width = 16
    Height = 377
    Kind = sbVertical
    LargeChange = 8
    SmallChange = 4
    TabOrder = 1
    OnChange = ScrollBarChange
  end
  object ScrollBox1: TScrollBox
    Left = 416
    Top = 48
    Width = 401
    Height = 401
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    object mcmImageCtrl2: TmcmImageCtrl
      Left = 0
      Top = 0
      Width = 397
      Height = 397
      Align = alClient
      Color = clWhite
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleToFit = False
      OnChange = mcmImageCtrl2Change
      OnMouseMove = mcmImageCtrl2MouseMove
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 456
    Width = 825
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object OpenDialog1: TOpenDialog
    Left = 16
    Top = 40
  end
end
