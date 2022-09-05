object MainForm: TMainForm
  Left = 198
  Top = 117
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SoundCapture Sample'
  ClientHeight = 91
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanLabel: TLabel
    Left = 208
    Top = 16
    Width = 19
    Height = 13
    Caption = 'Pan'
  end
  object VolumeLabel: TLabel
    Left = 208
    Top = 56
    Width = 35
    Height = 13
    Caption = 'Volume'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 332
    Height = 2
    Align = alTop
  end
  object PanTrackBar: TTrackBar
    Left = 8
    Top = 8
    Width = 193
    Height = 33
    Ctl3D = True
    Enabled = False
    LineSize = 0
    Max = 40
    Min = -40
    ParentCtl3D = False
    TabOrder = 0
    TickStyle = tsManual
    OnChange = PanTrackBarChange
  end
  object VolumeTrackBar: TTrackBar
    Left = 8
    Top = 48
    Width = 193
    Height = 33
    Enabled = False
    LineSize = 0
    Max = 0
    Min = -40
    TabOrder = 1
    TickStyle = tsManual
    OnChange = VolumeTrackBarChange
  end
  object DXSound: TDXSound
    AutoInitialize = False
    Options = [soGlobalFocus]
    OnFinalize = DXSoundFinalize
    OnInitialize = DXSoundInitialize
    Left = 272
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 240
    Top = 16
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileEnd: TMenuItem
        Caption = 'E&xit'
        OnClick = FileEndClick
      end
    end
    object O1: TMenuItem
      Caption = '&Option'
      object GlobalFocusItem: TMenuItem
        Caption = '&GlobalFocus'
        Checked = True
        OnClick = GlobalFocusItemClick
      end
    end
  end
end
