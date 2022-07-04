object Form1: TForm1
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'BASS CD Test by XMinioNX'
  ClientHeight = 345
  ClientWidth = 315
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
  object cmbDrives: TComboBox
    Left = 8
    Top = 8
    Width = 297
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbDrivesChange
  end
  object lvlLeft: TProgressBar
    Left = 8
    Top = 40
    Width = 297
    Height = 9
    Max = 30000
    TabOrder = 1
  end
  object lvlRight: TProgressBar
    Left = 8
    Top = 49
    Width = 297
    Height = 9
    Max = 30000
    TabOrder = 2
  end
  object lstTracks: TListBox
    Left = 8
    Top = 64
    Width = 145
    Height = 273
    ItemHeight = 13
    TabOrder = 3
    OnClick = lstTracksClick
  end
  object GroupBox1: TGroupBox
    Left = 160
    Top = 64
    Width = 145
    Height = 60
    Caption = 'Position'
    TabOrder = 4
    object Panel1: TPanel
      Left = 8
      Top = 16
      Width = 129
      Height = 19
      BevelOuter = bvLowered
      Caption = '-'
      TabOrder = 0
    end
    object trkPos: TScrollBar
      Left = 8
      Top = 41
      Width = 129
      Height = 11
      LargeChange = 30
      PageSize = 10
      TabOrder = 1
      OnScroll = trkPosScroll
    end
  end
  object btnPlay: TButton
    Left = 160
    Top = 128
    Width = 145
    Height = 20
    Caption = 'Play / Pause'
    TabOrder = 5
    OnClick = btnPlayClick
  end
  object GroupBox2: TGroupBox
    Left = 160
    Top = 152
    Width = 97
    Height = 41
    Caption = 'Speed'
    TabOrder = 6
    object trkSpeed: TTrackBar
      Left = 4
      Top = 16
      Width = 88
      Height = 17
      Max = 150
      Min = 50
      PageSize = 10
      Position = 50
      TabOrder = 0
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = trkSpeedChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 264
    Top = 152
    Width = 41
    Height = 81
    Caption = 'Vol'
    TabOrder = 7
    object trkVol: TTrackBar
      Left = 10
      Top = 12
      Width = 20
      Height = 65
      Max = 100
      Orientation = trVertical
      TabOrder = 0
      ThumbLength = 14
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = trkVolChange
    end
  end
  object chkAdvance: TCheckBox
    Left = 160
    Top = 208
    Width = 97
    Height = 17
    Caption = 'Auto advance'
    TabOrder = 8
  end
  object GroupBox4: TGroupBox
    Left = 160
    Top = 240
    Width = 145
    Height = 97
    Caption = 'Door'
    TabOrder = 9
    object lblStatus: TLabel
      Left = 8
      Top = 16
      Width = 58
      Height = 13
      Caption = 'Drive Status'
      ShowAccelChar = False
    end
    object btnOpen: TButton
      Left = 8
      Top = 32
      Width = 129
      Height = 25
      Caption = 'Open/Close'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnLock: TButton
      Left = 8
      Top = 64
      Width = 129
      Height = 25
      Caption = 'Lock/Unlock'
      TabOrder = 1
      OnClick = btnLockClick
    end
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
  end
end
