object MainForm: TMainForm
  Left = 200
  Top = 114
  Width = 648
  Height = 507
  Caption = 'Direct3D Sample'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '‚l‚r ‚oƒSƒVƒbƒN'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefaultPosOnly
  Scaled = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 16
  object DXDraw: TDXDraw
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    AutoInitialize = True
    AutoSize = True
    Color = clBtnFace
    Display.FixedBitCount = False
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doCenter, do3D, doDirectX7Mode, doRetainedMode, doHardware, doZBuffer]
    OnFinalize = DXDrawFinalize
    OnFinalizeSurface = DXDrawFinalizeSurface
    OnInitialize = DXDrawInitialize
    OnInitializeSurface = DXDrawInitializeSurface
    OnInitializing = DXDrawInitializing
    Align = alClient
    TabOrder = 0
  end
  object DXTimer: TDXTimer
    ActiveOnly = True
    Enabled = True
    Interval = 0
    OnTimer = DXTimerTimer
    Left = 16
    Top = 16
  end
  object DXImageList1: TDXImageList
    DXDraw = DXDraw
    Items = <
      item
        PatternHeight = 0
        PatternWidth = 0
        SystemMemory = False
        Transparent = True
        TransparentColor = clBlack
      end>
    Left = 72
    Top = 16
  end
end
