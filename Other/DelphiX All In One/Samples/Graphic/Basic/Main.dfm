object MainForm: TMainForm
  Left = 201
  Top = 113
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DelphiX Sample'
  ClientHeight = 480
  ClientWidth = 640
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
    Width = 640
    Height = 480
    AutoInitialize = True
    AutoSize = False
    Color = clBtnFace
    Display.FixedBitCount = True
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doWaitVBlank, doCenter, doFlip, do3D, doDirectX7Mode, doHardware]
    OnFinalize = DXDrawFinalize
    OnInitialize = DXDrawInitialize
    Align = alClient
    TabOrder = 0
    Traces = <>
  end
  object DXTimer: TDXTimer
    ActiveOnly = True
    Enabled = False
    Interval = 0
    OnTimer = DXTimerTimer
    Left = 24
    Top = 16
  end
end
