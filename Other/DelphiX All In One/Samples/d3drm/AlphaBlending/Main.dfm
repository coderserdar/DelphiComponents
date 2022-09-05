object MainForm: TMainForm
  Left = 200
  Top = 114
  Caption = 'Direct3D Sample'
  ClientHeight = 231
  ClientWidth = 312
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
    Width = 312
    Height = 231
    AutoInitialize = True
    AutoSize = True
    Color = clBtnFace
    Display.FixedBitCount = False
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doWaitVBlank, doCenter, do3D, doRetainedMode, doHardware, doSelectDriver]
    SurfaceHeight = 231
    SurfaceWidth = 312
    OnFinalize = DXDrawFinalize
    OnInitialize = DXDrawInitialize
    OnInitializeSurface = DXDrawInitializeSurface
    Align = alClient
    TabOrder = 0
    Traces = <>
    OnClick = DXDrawClick
  end
  object DXTimer: TDXTimer
    ActiveOnly = True
    Enabled = False
    Interval = 0
    OnTimer = DXTimerTimer
    Left = 16
    Top = 16
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'x'
    Filter = 'Direct3D file(*.x)|*.x'
    Options = [ofPathMustExist, ofFileMustExist]
    Left = 48
    Top = 16
  end
end
