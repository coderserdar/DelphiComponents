object frmOption: TfrmOption
  Left = 358
  Top = 110
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Audio/Video Options'
  ClientHeight = 433
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Compressor: TGroupBox
    Left = 8
    Top = 8
    Width = 425
    Height = 177
    Caption = 'Video / Audio Settings'
    TabOrder = 0
    object Label1: TLabel
      Left = 17
      Top = 59
      Width = 99
      Height = 13
      Caption = 'Video output quality :'
    end
    object LabelQuality: TLabel
      Left = 299
      Top = 60
      Width = 3
      Height = 13
    end
    object Label8: TLabel
      Left = 17
      Top = 16
      Width = 71
      Height = 13
      Caption = 'Video codecs :'
    end
    object Label9: TLabel
      Left = 16
      Top = 105
      Width = 65
      Height = 13
      Caption = 'Audio format :'
      Enabled = False
    end
    object Label10: TLabel
      Left = 17
      Top = 146
      Width = 96
      Height = 13
      Caption = 'Audio input volume :'
      Enabled = False
    end
    object Label11: TLabel
      Left = 312
      Top = 105
      Width = 59
      Height = 13
      Caption = 'Audio input :'
      Enabled = False
    end
    object LabelVolume: TLabel
      Left = 299
      Top = 147
      Width = 3
      Height = 13
      Enabled = False
    end
    object VideoCompressor: TComboBox
      Left = 16
      Top = 31
      Width = 281
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnClick = VideoCompressorClick
    end
    object AboutB: TButton
      Left = 336
      Top = 14
      Width = 75
      Height = 25
      Caption = 'About'
      TabOrder = 5
      OnClick = AboutBClick
    end
    object VideoQuality: TTrackBar
      Left = 121
      Top = 56
      Width = 177
      Height = 33
      Position = 10
      TabOrder = 1
      OnChange = VideoQualityChange
    end
    object ConfigB: TButton
      Left = 336
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Configure'
      TabOrder = 6
      OnClick = ConfigBClick
    end
    object AudioFormat: TComboBox
      Left = 16
      Top = 120
      Width = 281
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
    end
    object AudioVolume: TTrackBar
      Left = 121
      Top = 143
      Width = 178
      Height = 23
      Enabled = False
      Max = 100
      TabOrder = 4
      TickStyle = tsNone
      OnChange = AudioVolumeChange
    end
    object AudioInput: TComboBox
      Left = 312
      Top = 120
      Width = 105
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 7
      OnClick = AudioInputClick
    end
    object AudioRecord: TCheckBox
      Left = 16
      Top = 85
      Width = 89
      Height = 17
      Caption = 'Audio record'
      TabOrder = 2
      OnClick = AudioRecordClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 185
    Width = 425
    Height = 208
    Caption = 'Video frame parameters :'
    TabOrder = 1
    object Label3: TLabel
      Left = 188
      Top = 106
      Width = 31
      Height = 13
      Caption = 'frames'
    end
    object Label2: TLabel
      Left = 16
      Top = 106
      Width = 99
      Height = 13
      Caption = 'Set key frames every'
    end
    object Label4: TLabel
      Left = 362
      Top = 62
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = '1 Second'
    end
    object Label6: TLabel
      Left = 22
      Top = 62
      Width = 61
      Height = 13
      Caption = '1 Millisecond'
    end
    object labelmspFrecord: TLabel
      Left = 86
      Top = 62
      Width = 275
      Height = 13
      Alignment = taCenter
      AutoSize = False
    end
    object Label12: TLabel
      Left = 9
      Top = 43
      Width = 96
      Height = 13
      Caption = 'Delay every frames :'
    end
    object Label7: TLabel
      Left = 366
      Top = 155
      Width = 41
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = '1000 fps'
      ParentBiDiMode = False
    end
    object labelFPSPlayback: TLabel
      Left = 48
      Top = 155
      Width = 316
      Height = 13
      Alignment = taCenter
      AutoSize = False
    end
    object Label5: TLabel
      Left = 22
      Top = 155
      Width = 23
      Height = 13
      Caption = '1 fps'
    end
    object Label13: TLabel
      Left = 9
      Top = 135
      Width = 129
      Height = 13
      Caption = 'Playback frame rate : (FPS)'
    end
    object EditKeyFrames: TEdit
      Left = 120
      Top = 103
      Width = 65
      Height = 21
      TabOrder = 2
    end
    object TrackBarRecord: TTrackBar
      Left = 16
      Top = 76
      Width = 401
      Height = 23
      Max = 1000
      Min = 1
      Position = 2
      TabOrder = 1
      TickStyle = tsNone
      OnChange = TrackBarRecordChange
    end
    object TrackBarPlayback: TTrackBar
      Left = 16
      Top = 169
      Width = 401
      Height = 23
      Max = 1000
      Min = 1
      Position = 1000
      TabOrder = 3
      TickStyle = tsNone
      OnChange = TrackBarPlaybackChange
    end
    object AutoMode: TCheckBox
      Left = 10
      Top = 21
      Width = 199
      Height = 17
      Caption = 'Full auto mode on (Synchronize  f/s)'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = AutoModeClick
    end
  end
  object Button2: TButton
    Left = 126
    Top = 401
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 238
    Top = 401
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = Button3Click
  end
end
