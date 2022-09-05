object FormViewProfile: TFormViewProfile
  Left = 334
  Top = 121
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Intensity Profile'
  ClientHeight = 299
  ClientWidth = 545
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 464
    Top = 264
    Width = 75
    Height = 25
    Caption = '&Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object PageControl: TPageControl
    Left = 344
    Top = 8
    Width = 193
    Height = 249
    ActivePage = tsScanProp
    TabOrder = 1
    object tsScanProp: TTabSheet
      Caption = 'Scan properties'
      object gbScanProp: TGroupBox
        Left = 0
        Top = 0
        Width = 185
        Height = 221
        Align = alClient
        TabOrder = 0
        object lInterpolate: TLabel
          Left = 8
          Top = 96
          Width = 53
          Height = 13
          Caption = '&Interpolate:'
          FocusControl = cbInterpolate
        end
        object lAverage: TLabel
          Left = 8
          Top = 64
          Width = 40
          Height = 13
          Caption = 'A&verage'
        end
        object lScanWidth: TLabel
          Left = 8
          Top = 32
          Width = 56
          Height = 13
          Caption = '&Scan width:'
        end
        object lAngle: TLabel
          Left = 8
          Top = 136
          Width = 30
          Height = 13
          Caption = '&Angle:'
        end
        object lLength: TLabel
          Left = 8
          Top = 168
          Width = 36
          Height = 13
          Caption = '&Length:'
        end
        object cbInterpolate: TComboBox
          Left = 96
          Top = 88
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'None'
            'Bilinear')
          TabOrder = 0
          OnChange = cbInterpolateChange
        end
        object rsAngle: TmcmRealSpin
          Left = 96
          Top = 128
          Width = 73
          Height = 22
          TabOrder = 1
          OnChange = rsAngleChange
          Value = 1.000000000000000000
          MaxValue = 359.990000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
        object rsLength: TmcmRealSpin
          Left = 96
          Top = 160
          Width = 73
          Height = 22
          TabOrder = 2
          OnChange = rsLengthChange
          Value = 100.000000000000000000
          MaxValue = 1000000.000000000000000000
          MinValue = 1.000000000000000000
          Decimals = 2
          Increment = 1.000000000000000000
        end
        object seScanWidth: TmcmIntSpin
          Left = 96
          Top = 24
          Width = 73
          Height = 22
          MaxLength = 199
          TabOrder = 3
          OnChange = seScanWidthChange
          Value = 1
          MaxValue = 199
          MinValue = 1
        end
        object seAverage: TmcmIntSpin
          Left = 96
          Top = 56
          Width = 73
          Height = 22
          TabOrder = 4
          OnChange = seAverageChange
          Value = 1
          MaxValue = 49
          MinValue = 1
        end
      end
    end
    object tsDetect: TTabSheet
      Caption = 'Detect'
      object gbDetect: TGroupBox
        Left = 0
        Top = 0
        Width = 185
        Height = 97
        Align = alTop
        Caption = 'Sensitivity'
        TabOrder = 0
        object lDevirative: TLabel
          Left = 8
          Top = 32
          Width = 51
          Height = 13
          Caption = '&Devirative:'
        end
        object lHysteresis: TLabel
          Left = 8
          Top = 64
          Width = 51
          Height = 13
          Caption = '&Hysteresis:'
        end
        object seDevirative: TmcmIntSpin
          Left = 96
          Top = 24
          Width = 73
          Height = 22
          TabOrder = 0
          OnChange = seDevirativeChange
          Value = 0
          MaxValue = 255
          MinValue = 0
        end
        object seHysteresis: TmcmIntSpin
          Left = 96
          Top = 56
          Width = 73
          Height = 22
          TabOrder = 1
          OnChange = seHysteresisChange
          Value = 0
          MaxValue = 255
          MinValue = 0
        end
      end
      object gbTransitions: TGroupBox
        Left = 0
        Top = 104
        Width = 185
        Height = 117
        Align = alBottom
        Caption = 'Transitions'
        TabOrder = 1
        object cbPeak: TCheckBox
          Left = 96
          Top = 32
          Width = 73
          Height = 17
          Caption = 'Pea&k'
          TabOrder = 0
          OnClick = cbPeakClick
        end
        object cbPositive: TCheckBox
          Left = 8
          Top = 32
          Width = 73
          Height = 17
          Caption = '&Positive'
          TabOrder = 1
          OnClick = cbPositiveClick
        end
        object cbNegative: TCheckBox
          Left = 8
          Top = 52
          Width = 73
          Height = 17
          Caption = '&Negative'
          TabOrder = 2
          OnClick = cbNegativeClick
        end
        object cbValley: TCheckBox
          Left = 96
          Top = 52
          Width = 73
          Height = 17
          Caption = '&Valley'
          TabOrder = 3
          OnClick = cbValleyClick
        end
        object cbCenterPeaks: TCheckBox
          Left = 8
          Top = 84
          Width = 145
          Height = 17
          Caption = 'Center peaks && valleys'
          TabOrder = 4
          OnClick = cbCenterPeaksClick
        end
      end
    end
    object tsResults: TTabSheet
      Caption = 'Results'
      object sgResults: TStringGrid
        Left = 0
        Top = 0
        Width = 185
        Height = 221
        Align = alClient
        ColCount = 4
        DefaultColWidth = 44
        DefaultRowHeight = 18
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        TabOrder = 0
        ColWidths = (
          44
          44
          44
          44)
      end
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 329
    Height = 281
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clWhite
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    object lValues: TLabel
      Left = 8
      Top = 4
      Width = 42
      Height = 13
      Caption = 'Intensity:'
    end
    object sbProfile: TScrollBox
      Left = 2
      Top = 22
      Width = 325
      Height = 257
      HorzScrollBar.Tracking = True
      VertScrollBar.Visible = False
      Align = alBottom
      BorderStyle = bsNone
      Color = clWhite
      ParentColor = False
      TabOrder = 0
      OnMouseMove = sbProfileMouseMove
      object mcmShape1: TmcmShape
        Left = 0
        Top = 0
        Width = 241
        Height = 253
        Cursor = crCross
        Angle = 90
        Length = 65
        Shape = ST_POLYGON
        OnMouseMove = sbProfileMouseMove
      end
      object mcmShape2: TmcmShape
        Left = 0
        Top = 0
        Width = 129
        Height = 253
        Cursor = crCross
        Angle = 90
        Length = 63
        Pen.Color = clRed
        Shape = ST_POLYGON
        OnMouseMove = sbProfileMouseMove
      end
    end
  end
end
