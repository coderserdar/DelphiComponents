object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
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
    Width = 688
    Height = 453
    Align = alClient
    Caption = 'API_GLPANEL'
    TabOrder = 0
    Background = 8388672
    WireFrame = False
    AutoAspect = True
    AxisLength = 0
    AxisColorZ = clBlack
    AxisColorX = clBlack
    AxisColorY = clBlack
    ThreadActive = False
    ThreadEvent = API_glpanel1ThreadEvent
    ThreadPriority = tpIdle
    ThreadDelay = 25
  end
end
