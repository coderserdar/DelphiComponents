object Form2: TForm2
  Left = 572
  Top = 191
  Width = 498
  Height = 476
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object API_grbutton5: TAPI_grbutton
    Left = 136
    Top = 8
    Width = 233
    Height = 57
    BorderWidth = 1
    Caption = 'Close'
    Color = clBtnFace
    Enabled = True
    TabOrder = 0
    OnClick = API_grbutton5Click
    LedExists = False
    LedState = False
    LedChangeOnClick = False
    LedColorOn = clGreen
    LedStyle = lsellipse
    LedPosition = lpright
    LedColorOff = clRed
    ColorBorder = clBlack
    GradientEnd = clWhite
    FontMouseOver.Charset = DEFAULT_CHARSET
    FontMouseOver.Color = clWindowText
    FontMouseOver.Height = -11
    FontMouseOver.Name = 'MS Sans Serif'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
    VerticalAlignment = taVerticalCenter
  end
  object API_skin1: TAPI_skin
    BlurRadius = 2
    BlurColor = clWhite
    Left = 256
    Top = 16
  end
end
