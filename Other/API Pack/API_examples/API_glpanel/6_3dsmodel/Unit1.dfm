object Form1: TForm1
  Left = 200
  Top = 121
  Width = 435
  Height = 255
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
  object API_glpanel1: TAPI_glpanel
    Left = 0
    Top = 0
    Width = 427
    Height = 228
    Align = alClient
    Caption = 'API_GLPANEL'
    TabOrder = 0
    OnMouseDown = API_glpanel1MouseDown
    OnMouseMove = API_glpanel1MouseMove
    OnMouseUp = API_glpanel1MouseUp
    Background = clNavy
    WireFrame = False
    AutoAspect = True
    AxisLength = 5
    AxisColorZ = clOlive
    AxisColorX = clMaroon
    AxisColorY = clGreen
    ThreadActive = False
    ThreadPriority = tpIdle
    ThreadDelay = 0
    DesignSize = (
      427
      228)
    object API_grbutton1: TAPI_grbutton
      Left = 336
      Top = 8
      Width = 80
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Open 3DS'
      Color = clWhite
      Enabled = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
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
      ColorOverCaption = clBlack
      ColorOver = clSilver
      ColorDown = clGray
      ShowCaption = True
    end
  end
  object Timer1: TTimer
    Interval = 25
    OnTimer = Timer1Timer
    Left = 48
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Left = 80
    Top = 16
  end
  object API_msgdlg1: TAPI_msgdlg
    Caption = 'Message'
    Fade = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 112
    Top = 16
  end
end
