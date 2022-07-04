object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Form1'
  ClientHeight = 268
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    446
    268)
  PixelsPerInch = 96
  TextHeight = 13
  object API_glpanel1: TAPI_glpanel
    Left = 0
    Top = 0
    Width = 446
    Height = 241
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'API_GLPANEL'
    TabOrder = 0
    Background = 4194304
    WireFrame = False
    AutoAspect = True
    AxisLength = 5
    AxisColorZ = clOlive
    AxisColorX = clMaroon
    AxisColorY = clGreen
    ThreadActive = False
    ThreadPriority = tpIdle
    ThreadDelay = 0
  end
  object API_grbutton2: TAPI_grbutton
    Left = 0
    Top = 248
    Width = 80
    Height = 22
    Anchors = [akLeft, akBottom]
    BorderWidth = 1
    Caption = 'exit'
    Color = clBtnFace
    Enabled = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    VerticalAlignment = taVerticalCenter
    OnClick = API_grbutton2Click
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
    FontMouseOver.Name = 'Arial'
    FontMouseOver.Style = []
    ColorOver = clSilver
    ColorDown = clGray
    ShowCaption = True
    WordWrap = False
  end
  object CheckBox1: TCheckBox
    Left = 88
    Top = 248
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Wireframe'
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 168
    Top = 248
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'draw axis'
    TabOrder = 3
  end
  object CheckBox3: TCheckBox
    Left = 256
    Top = 248
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Lighting'
    TabOrder = 4
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 336
    Top = 248
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Fog'
    TabOrder = 5
    OnClick = CheckBox4Click
  end
  object Timer1: TTimer
    Interval = 25
    OnTimer = Timer1Timer
    Left = 24
    Top = 16
  end
end
