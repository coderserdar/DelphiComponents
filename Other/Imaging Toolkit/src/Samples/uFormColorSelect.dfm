object FormColorSelect: TFormColorSelect
  Left = 269
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Color'
  ClientHeight = 489
  ClientWidth = 281
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcColor: TPageControl
    Left = 0
    Top = 0
    Width = 281
    Height = 449
    ActivePage = tsHSV
    Align = alTop
    TabOrder = 0
    OnMouseDown = mcmImageHSVMouseDown
    OnMouseMove = mcmImageHSVMouseMove
    object tsHSV: TTabSheet
      Caption = 'HSV'
      TabVisible = False
      object mcmImageHSV: TmcmImageCtrl
        Left = 8
        Top = 4
        Width = 257
        Height = 257
        Cursor = crCross
        ParentColor = False
        Scale = 1.000000000000000000
        ScaleToFit = False
        OnMouseDown = mcmImageHSVMouseDown
        OnMouseMove = mcmImageHSVMouseMove
      end
      object miOldColor: TmcmImageCtrl
        Left = 8
        Top = 408
        Width = 121
        Height = 25
        BorderStyle = BS_ETCHED
        ParentColor = False
        Scale = 1.000000000000000000
        ScaleToFit = False
      end
      object miNewColor: TmcmImageCtrl
        Left = 136
        Top = 408
        Width = 129
        Height = 25
        BorderStyle = BS_ETCHED
        ParentColor = False
        Scale = 1.000000000000000000
        ScaleToFit = False
      end
      object mcmShape1: TmcmShape
        Left = 128
        Top = 128
        Width = 9
        Height = 9
        Cursor = crCross
        Angle = 90
        Brush.Style = bsClear
        Length = 65
        Pen.Mode = pmNotXor
        Shape = ST_CIRCLE
        OnMouseDown = mcmImageHSVMouseDown
        OnMouseMove = mcmImageHSVMouseMove
      end
      object mcmImageValue: TmcmImageCtrl
        Left = 8
        Top = 268
        Width = 256
        Height = 17
        Cursor = crCross
        ParentColor = False
        ParentShowHint = False
        Scale = 1.000000000000000000
        ScaleToFit = False
        OnMouseDown = mcmImageValueMouseDown
        OnMouseMove = mcmImageValueMouseMove
      end
      object msValue: TmcmShape
        Left = 260
        Top = 268
        Width = 9
        Height = 17
        Cursor = crSizeWE
        Angle = 90
        Brush.Style = bsClear
        Length = 65
        Pen.Mode = pmNotXor
        OnMouseDown = mcmImageValueMouseDown
        OnMouseMove = mcmImageValueMouseMove
      end
      object lPrevColor: TLabel
        Left = 44
        Top = 414
        Width = 41
        Height = 13
        Caption = 'Previous'
        Transparent = True
      end
      object lCurrColor: TLabel
        Left = 183
        Top = 414
        Width = 34
        Height = 13
        Caption = 'Current'
        Transparent = True
      end
      object gbRGB: TGroupBox
        Left = 8
        Top = 288
        Width = 121
        Height = 113
        Caption = 'RGB'
        TabOrder = 0
        object lRed: TLabel
          Left = 8
          Top = 32
          Width = 20
          Height = 13
          Caption = '&Red'
          FocusControl = isRed
        end
        object lGreen: TLabel
          Left = 8
          Top = 60
          Width = 29
          Height = 13
          Caption = '&Green'
          FocusControl = isGreen
        end
        object lBlue: TLabel
          Left = 8
          Top = 88
          Width = 21
          Height = 13
          Caption = '&Blue'
          FocusControl = isBlue
        end
        object isRed: TmcmIntSpin
          Left = 56
          Top = 24
          Width = 54
          Height = 22
          TabOrder = 0
          OnChange = tbRedChange
          Value = 255
          MaxValue = 255
          MinValue = 0
        end
        object isGreen: TmcmIntSpin
          Left = 56
          Top = 52
          Width = 54
          Height = 22
          TabOrder = 1
          OnChange = tbGreenChange
          Value = 255
          MaxValue = 255
          MinValue = 0
        end
        object isBlue: TmcmIntSpin
          Left = 56
          Top = 80
          Width = 54
          Height = 22
          TabOrder = 2
          OnChange = tbBlueChange
          Value = 255
          MaxValue = 255
          MinValue = 0
        end
      end
      object gbHSV: TGroupBox
        Left = 136
        Top = 288
        Width = 129
        Height = 113
        Caption = 'HSV'
        TabOrder = 1
        object lHue: TLabel
          Left = 8
          Top = 32
          Width = 20
          Height = 13
          Caption = '&Hue'
          FocusControl = isHue
        end
        object lSaturation: TLabel
          Left = 8
          Top = 60
          Width = 48
          Height = 13
          Caption = '&Saturation'
          FocusControl = isSaturation
        end
        object lValue: TLabel
          Left = 8
          Top = 88
          Width = 27
          Height = 13
          Caption = '&Value'
          FocusControl = isValue
        end
        object isHue: TmcmIntSpin
          Left = 64
          Top = 24
          Width = 54
          Height = 22
          TabOrder = 0
          OnChange = tbHueChange
          Value = 0
          MaxValue = 359
          MinValue = 0
        end
        object isSaturation: TmcmIntSpin
          Left = 64
          Top = 52
          Width = 54
          Height = 22
          TabOrder = 1
          OnChange = tbSaturationChange
          Value = 0
          MaxValue = 255
          MinValue = 0
        end
        object isValue: TmcmIntSpin
          Left = 64
          Top = 80
          Width = 54
          Height = 22
          TabOrder = 2
          OnChange = tbValueChange
          Value = 255
          MaxValue = 255
          MinValue = 0
        end
      end
    end
  end
  object btnOK: TButton
    Left = 8
    Top = 456
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 88
    Top = 456
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnHelp: TButton
    Left = 200
    Top = 456
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
  object pmColorSelect: TPopupMenu
    Left = 236
    Top = 32
    object HSVCircular1: TMenuItem
      Caption = 'HSV Circular'
      OnClick = HSVCircular1Click
    end
    object HSVRectangular1: TMenuItem
      Caption = 'HSV Rectangular'
      OnClick = HSVRectangular1Click
    end
  end
end
