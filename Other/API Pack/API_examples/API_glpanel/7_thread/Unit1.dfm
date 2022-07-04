object Form1: TForm1
  Left = 192
  Top = 107
  Width = 425
  Height = 296
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
    Width = 417
    Height = 269
    Align = alClient
    Color = clBlack
    TabOrder = 0
    Background = clBlack
    WireFrame = False
    AutoAspect = True
    AxisLength = 0
    AxisColorZ = clBlack
    AxisColorX = clBlack
    AxisColorY = clBlack
    ThreadActive = False
    ThreadEvent = API_glpanel1ThreadEvent
    ThreadPriority = tpIdle
    ThreadDelay = 0
  end
end
