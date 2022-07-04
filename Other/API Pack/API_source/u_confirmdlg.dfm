object f_confirmdlg: Tf_confirmdlg
  Left = 192
  Top = 107
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderStyle = bsDialog
  ClientHeight = 143
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    346
    143)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 329
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Memo1: TMemo
    Left = 16
    Top = 16
    Width = 313
    Height = 81
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    TabOrder = 0
  end
  object API_grbutton1: TAPI_grbutton
    Left = 8
    Top = 111
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    BorderWidth = 1
    Caption = 'Yes'
    Color = clBtnFace
    Enabled = True
    TabOrder = 1
    VerticalAlignment = taVerticalCenter
    OnClick = BitBtn1Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = 8454016
    ColorDown = clGreen
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clWhite
    Elliptic = True
    CornerRadius = 10
    ShowCaption = True
    WordWrap = False
  end
  object API_grbutton2: TAPI_grbutton
    Left = 258
    Top = 111
    Width = 80
    Height = 25
    Anchors = [akRight, akBottom]
    BorderWidth = 1
    Caption = 'No'
    Color = clBtnFace
    Enabled = True
    TabOrder = 2
    VerticalAlignment = taVerticalCenter
    OnClick = BitBtn2Click
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedStyle = lsEllipse
    LedPosition = lpRight
    LedColorOn = clGreen
    LedColorOff = clRed
    LedHeight = 8
    ColorOver = 8421631
    ColorDown = clRed
    ColorDisabled = clGray
    GradientEnd = clWhite
    GradientStyle = gsVertical
    GradinetFlip = False
    ColorBorder = clWhite
    Elliptic = True
    CornerRadius = 10
    ShowCaption = True
    WordWrap = False
  end
  object API_abform1: TAPI_abform
    Active = True
    AlphaStart = 0
    AlphaStop = 255
    TimeDelay = 1
    StepSize = 5
    Left = 16
    Top = 16
  end
end
