object MainForm: TMainForm
  Left = 200
  Top = 114
  Caption = 'Direct3D7 IM Sample - Video Texture'
  ClientHeight = 471
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object DXDraw: TDXDraw
    Left = 0
    Top = 0
    Width = 632
    Height = 471
    AutoInitialize = True
    AutoSize = True
    Color = clBtnFace
    Display.FixedBitCount = False
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doWaitVBlank, doCenter, do3D, doDirectX7Mode, doHardware, doSelectDriver]
    SurfaceHeight = 471
    SurfaceWidth = 632
    OnFinalize = DXDrawFinalize
    OnInitialize = DXDrawInitialize
    OnInitializeSurface = DXDrawInitializeSurface
    Align = alClient
    TabOrder = 0
    Traces = <>
  end
  object DXTimer: TDXTimer
    ActiveOnly = True
    Enabled = True
    Interval = 1
    OnTimer = DXTimerTimer
    Left = 16
    Top = 16
  end
end
