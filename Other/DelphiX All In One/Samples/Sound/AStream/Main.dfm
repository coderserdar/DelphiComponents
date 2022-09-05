object MainForm: TMainForm
  Left = 198
  Top = 112
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Audio Stream Sample'
  ClientHeight = 194
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Scaled = False
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
  object FrequencyLabel: TLabel
    Left = 208
    Top = 96
    Width = 50
    Height = 13
    Caption = 'Frequency'
  end
  object ProgressLabel: TLabel
    Left = 208
    Top = 136
    Width = 23
    Height = 13
    Caption = 'Time'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 443
    Height = 2
    Align = alTop
  end
  object BytesLabel: TLabel
    Left = 208
    Top = 168
    Width = 26
    Height = 13
    Caption = 'Bytes'
  end
  object PlayButton: TButton
    Left = 344
    Top = 72
    Width = 91
    Height = 33
    Caption = '&Play'
    Enabled = False
    TabOrder = 0
    OnClick = PlayButtonClick
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
    TabOrder = 1
    TickStyle = tsManual
    OnChange = PanTrackBarChange
  end
  object LoopCheckBox: TCheckBox
    Left = 352
    Top = 48
    Width = 57
    Height = 17
    Caption = '&Looped'
    Enabled = False
    TabOrder = 2
    OnClick = LoopCheckBoxClick
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
    TabOrder = 3
    TickStyle = tsManual
    OnChange = VolumeTrackBarChange
  end
  object FrequencyTrackBar: TTrackBar
    Left = 8
    Top = 88
    Width = 193
    Height = 33
    Enabled = False
    LineSize = 0
    Max = 44100
    Min = 4410
    PageSize = 1100
    Position = 4410
    TabOrder = 4
    TickStyle = tsManual
    OnChange = FrequencyTrackBarChange
  end
  object ProgressTrackBar: TTrackBar
    Left = 8
    Top = 128
    Width = 193
    Height = 33
    Enabled = False
    LineSize = 0
    Max = 100
    PageSize = 100
    TabOrder = 5
    TickStyle = tsManual
    OnChange = ProgressTrackBarChange
  end
  object StopButton: TButton
    Left = 344
    Top = 152
    Width = 91
    Height = 33
    Caption = '&Stop'
    Enabled = False
    TabOrder = 6
    OnClick = StopButtonClick
  end
  object PauseButton: TButton
    Left = 344
    Top = 112
    Width = 91
    Height = 33
    Caption = 'Pa&use'
    Enabled = False
    TabOrder = 7
    OnClick = PauseButtonClick
  end
  object DXSound: TDXSound
    AutoInitialize = False
    Options = [soGlobalFocus, soExclusive]
    OnFinalize = DXSoundFinalize
    Left = 280
    Top = 16
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 312
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 248
    Top = 16
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileOpen: TMenuItem
        Caption = '&Open'
        OnClick = FileOpenClick
      end
      object FileClose: TMenuItem
        Caption = '&Close'
        OnClick = FileCloseClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FileEnd: TMenuItem
        Caption = 'E&xit'
        OnClick = FileEndClick
      end
    end
    object O1: TMenuItem
      Caption = '&Option'
      object GlobalFocusItem: TMenuItem
        Tag = 1
        Caption = '&GlobalFocus'
        Checked = True
        OnClick = GlobalFocusItemClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave (*.wav)|*.wav|All files (*.*)|*.*'
    Options = [ofFileMustExist]
    Left = 344
    Top = 16
  end
end
