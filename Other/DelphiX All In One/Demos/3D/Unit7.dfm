object Form1: TForm1
  Left = 192
  Top = 116
  Width = 696
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object DXDraw1: TDXDraw
    Left = 0
    Top = 0
    Width = 688
    Height = 446
    AutoInitialize = True
    AutoSize = True
    Color = clBtnFace
    Display.FixedBitCount = True
    Display.FixedRatio = True
    Display.FixedSize = False
    Display.Height = 600
    Display.Width = 800
    Options = [doAllowReboot, doWaitVBlank, doCenter, doFlip, do3D, doDirectX7Mode, doHardware, doSelectDriver, doZBuffer]
    SurfaceHeight = 446
    SurfaceWidth = 688
    OnFinalize = DXDraw1Finalize
    OnInitialize = DXDraw1Initialize
    OnInitializeSurface = DXDraw1InitializeSurface
    Align = alClient
    TabOrder = 0
  end
  object DXTimer1: TDXTimer
    ActiveOnly = True
    Enabled = True
    Interval = 0
    OnTimer = DXTimer1Timer
    Left = 16
    Top = 16
  end
  object DXImageList1: TDXImageList
    DXDraw = DXDraw1
    Items = <>
    Left = 48
    Top = 16
  end
end
