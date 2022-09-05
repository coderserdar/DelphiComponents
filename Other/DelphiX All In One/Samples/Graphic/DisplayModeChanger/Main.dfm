object Form1: TForm1
  Left = 194
  Top = 114
  Caption = 'Display mode change'
  ClientHeight = 241
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DXDraw: TDXDraw
    Left = 48
    Top = 64
    Width = 100
    Height = 100
    AutoInitialize = False
    AutoSize = False
    Color = clBtnFace
    Display.FixedBitCount = True
    Display.FixedRatio = True
    Display.FixedSize = False
    Options = [doAllowReboot, doWaitVBlank, doSystemMemory, do3D, doDirectX7Mode, doHardware]
    SurfaceHeight = 1
    SurfaceWidth = 1
    OnInitialize = DXDrawInitialize
    TabOrder = 0
    Traces = <>
    Visible = False
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 88
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = FileExitClick
      end
    end
    object ModesMenu: TMenuItem
      Caption = '&Mode'
    end
  end
end
