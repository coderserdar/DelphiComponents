object Form1: TForm1
  Left = 192
  Top = 107
  Width = 314
  Height = 94
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object API_grbutton1: TAPI_grbutton
    Left = 96
    Top = 24
    Width = 113
    Height = 22
    BorderWidth = 1
    Caption = 'Draw On Screen!'
    Color = clBtnFace
    Enabled = True
    TabOrder = 0
    OnClick = API_grbutton1Click
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
    BlurRadius = 3
    BlurColor = clWhite
    Active = False
    Left = 232
    Top = 8
  end
end
