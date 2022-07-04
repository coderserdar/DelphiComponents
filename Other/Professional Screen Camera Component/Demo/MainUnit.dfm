object frmMain: TfrmMain
  Left = 194
  Top = 110
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Professional Screen Camera Demo'
  ClientHeight = 538
  ClientWidth = 531
  Color = clBtnFace
  TransparentColorValue = clLime
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 421
    Top = 12
    Width = 37
    Height = 13
    Caption = 'Top     :'
    Transparent = True
  end
  object Label3: TLabel
    Left = 421
    Top = 36
    Width = 36
    Height = 13
    Caption = 'Left     :'
    Transparent = True
  end
  object Label4: TLabel
    Left = 421
    Top = 60
    Width = 37
    Height = 13
    Caption = 'Width  :'
    Transparent = True
  end
  object Label5: TLabel
    Left = 421
    Top = 84
    Width = 37
    Height = 13
    Caption = 'Height :'
    Transparent = True
  end
  object Label9: TLabel
    Left = 221
    Top = 177
    Width = 12
    Height = 13
    Caption = '----'
    Transparent = True
  end
  object Label10: TLabel
    Left = 194
    Top = 107
    Width = 66
    Height = 13
    Caption = 'Video priority :'
  end
  object Label11: TLabel
    Left = 363
    Top = 107
    Width = 57
    Height = 13
    Caption = 'Filter color  :'
    Transparent = True
  end
  object Label12: TLabel
    Left = 13
    Top = 88
    Width = 59
    Height = 13
    Caption = 'Save to file :'
    Transparent = True
  end
  object Label13: TLabel
    Left = 363
    Top = 131
    Width = 59
    Height = 13
    Caption = 'Color mode :'
  end
  object Label14: TLabel
    Left = 211
    Top = 150
    Width = 77
    Height = 13
    Caption = 'Current monitor :'
  end
  object RecBut: TButton
    Left = 13
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start Record'
    TabOrder = 0
    OnClick = RecButClick
  end
  object StopBut: TButton
    Left = 13
    Top = 34
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = StopButClick
  end
  object AudVidOptBut: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'A/V Options'
    TabOrder = 3
    OnClick = AudVidOptButClick
  end
  object CheckBox1: TCheckBox
    Left = 12
    Top = 128
    Width = 161
    Height = 17
    Caption = 'Record cursor in area capture'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object CheckBox3: TCheckBox
    Left = 12
    Top = 149
    Width = 189
    Height = 17
    Caption = 'Draw rectangle around area capture'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = CheckBox3Click
  end
  object Edit1: TEdit
    Left = 461
    Top = 8
    Width = 44
    Height = 21
    Enabled = False
    TabOrder = 23
    Text = '0'
    OnChange = Edit1Change
    OnKeyPress = Edit1KeyPress
  end
  object Edit2: TEdit
    Left = 461
    Top = 32
    Width = 44
    Height = 21
    Enabled = False
    TabOrder = 25
    Text = '0'
    OnChange = Edit2Change
    OnKeyPress = Edit1KeyPress
  end
  object Edit3: TEdit
    Left = 461
    Top = 56
    Width = 44
    Height = 21
    TabOrder = 27
    Text = '0'
    OnChange = Edit3Change
    OnKeyPress = Edit1KeyPress
  end
  object Edit4: TEdit
    Left = 461
    Top = 80
    Width = 44
    Height = 21
    TabOrder = 29
    Text = '0'
    OnChange = Edit4Change
    OnKeyPress = Edit1KeyPress
  end
  object CheckBox4: TCheckBox
    Left = 12
    Top = 170
    Width = 141
    Height = 17
    Caption = 'Draw cross rectangle line'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 12
    Top = 191
    Width = 187
    Height = 17
    Caption = 'Minimize application before capture'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 12
    Top = 212
    Width = 173
    Height = 17
    Caption = 'Restore application after capture'
    Checked = True
    State = cbChecked
    TabOrder = 12
    OnClick = CheckBox6Click
  end
  object CheckBox8: TCheckBox
    Left = 12
    Top = 233
    Width = 103
    Height = 17
    Caption = 'Preview (on / off)'
    TabOrder = 13
    OnClick = CheckBox8Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 296
    Width = 206
    Height = 72
    TabOrder = 16
    object Label6: TLabel
      Left = 34
      Top = 28
      Width = 23
      Height = 13
      Caption = 'Hour'
      Enabled = False
    end
    object Label7: TLabel
      Left = 85
      Top = 28
      Width = 17
      Height = 13
      Caption = 'Min'
      Enabled = False
    end
    object Label8: TLabel
      Left = 134
      Top = 28
      Width = 19
      Height = 13
      Caption = 'Sec'
      Enabled = False
    end
    object Edit5: TEdit
      Left = 32
      Top = 43
      Width = 25
      Height = 21
      Enabled = False
      TabOrder = 1
      Text = '0'
      OnChange = Edit5Change
      OnKeyPress = Edit7KeyPress
    end
    object UpDown1: TUpDown
      Left = 57
      Top = 43
      Width = 16
      Height = 21
      Associate = Edit5
      Enabled = False
      Max = 99
      TabOrder = 2
    end
    object UpDown2: TUpDown
      Left = 107
      Top = 43
      Width = 16
      Height = 21
      Associate = Edit6
      Enabled = False
      Max = 59
      TabOrder = 4
    end
    object Edit6: TEdit
      Left = 82
      Top = 43
      Width = 25
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = '0'
      OnChange = Edit5Change
      OnKeyPress = Edit7KeyPress
    end
    object UpDown3: TUpDown
      Left = 157
      Top = 43
      Width = 16
      Height = 21
      Associate = Edit7
      Enabled = False
      Max = 59
      TabOrder = 6
    end
    object Edit7: TEdit
      Left = 132
      Top = 43
      Width = 25
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = '0'
      OnChange = Edit5Change
      OnKeyPress = Edit7KeyPress
    end
    object CheckBox9: TCheckBox
      Left = 8
      Top = 9
      Width = 102
      Height = 16
      Caption = 'Timer capture on'
      TabOrder = 0
      OnClick = CheckBox9Click
    end
  end
  object RegSelect: TRadioGroup
    Left = 176
    Top = 5
    Width = 233
    Height = 96
    Caption = 'Select Region Type'
    ItemIndex = 2
    Items.Strings = (
      'Region stable - region from windows'
      'Region stable - free hand mode'
      'Region moving by mouse'
      'Region stable - set values by user'
      'Region stable - full screen')
    TabOrder = 18
    OnClick = RegSelectClick
  end
  object ExitBut: TButton
    Left = 13
    Top = 60
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 2
    OnClick = ExitButClick
  end
  object Panel1: TPanel
    Left = 220
    Top = 196
    Width = 302
    Height = 302
    BevelInner = bvLowered
    BevelOuter = bvNone
    Caption = 'Preview part...'
    TabOrder = 22
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 300
      Height = 300
      Align = alClient
      Stretch = True
    end
  end
  object ComboBox1: TComboBox
    Left = 264
    Top = 104
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 19
    Text = 'Normal'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Idle'
      'Lowest'
      'Lower'
      'Normal'
      'Higher'
      'Highest'
      'TimeCritical')
  end
  object ComboBox2: TComboBox
    Left = 426
    Top = 104
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 31
    Text = 'Original colors'
    OnChange = ComboBox2Change
    Items.Strings = (
      'Original colors'
      'Reverse colors')
  end
  object Edit8: TEdit
    Left = 12
    Top = 102
    Width = 133
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object Panel2: TPanel
    Left = 0
    Top = 506
    Width = 531
    Height = 32
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 33
    object LStatus: TLabel
      Left = 4
      Top = 2
      Width = 47
      Height = 13
      Caption = 'No Status'
      Enabled = False
      Transparent = True
    end
    object Progress: TProgressBar
      Left = 108
      Top = 2
      Width = 421
      Height = 28
      Align = alRight
      Smooth = True
      TabOrder = 1
    end
    object BCancel: TButton
      Left = 3
      Top = 15
      Width = 103
      Height = 15
      Caption = 'Cancel'
      Enabled = False
      TabOrder = 0
      OnClick = BCancelClick
    end
  end
  object UpDownTop: TUpDown
    Left = 505
    Top = 8
    Width = 17
    Height = 21
    Associate = Edit1
    TabOrder = 24
    Thousands = False
  end
  object UpDownLeft: TUpDown
    Left = 505
    Top = 32
    Width = 17
    Height = 21
    Associate = Edit2
    TabOrder = 26
    Thousands = False
  end
  object UpDownWidth: TUpDown
    Left = 505
    Top = 56
    Width = 17
    Height = 21
    Associate = Edit3
    Max = 640
    TabOrder = 28
    Thousands = False
  end
  object UpDownHeight: TUpDown
    Left = 505
    Top = 80
    Width = 17
    Height = 21
    Associate = Edit4
    Max = 480
    TabOrder = 30
    Thousands = False
  end
  object ComboBox3: TComboBox
    Left = 426
    Top = 128
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 32
    Text = '32 bit'
    OnChange = ComboBox3Change
    Items.Strings = (
      '8 bit'
      '16 bit'
      '24 bit'
      '32 bit')
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 369
    Width = 206
    Height = 129
    Caption = 'Information'
    TabOrder = 17
    object VideoCodec: TLabel
      Left = 11
      Top = 39
      Width = 67
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = 'Video Codec :'
      ParentBiDiMode = False
      Transparent = True
    end
    object ElapsedTime: TLabel
      Left = 11
      Top = 53
      Width = 70
      Height = 13
      Caption = 'Elapsed Time :'
      Transparent = True
    end
    object RealCapturing: TLabel
      Left = 11
      Top = 67
      Width = 97
      Height = 13
      Caption = 'Real Capturing F/S :'
      Transparent = True
    end
    object CurrentCaptured: TLabel
      Left = 11
      Top = 81
      Width = 107
      Height = 13
      Caption = 'Current Captured F/S :'
      Transparent = True
    end
    object FramesCaptured: TLabel
      Left = 11
      Top = 95
      Width = 86
      Height = 13
      Caption = 'Captured Frames :'
      Transparent = True
    end
    object DropedFrames: TLabel
      Left = 11
      Top = 109
      Width = 78
      Height = 13
      Caption = 'Droped Frames :'
      Transparent = True
    end
    object Label1: TLabel
      Left = 11
      Top = 17
      Width = 97
      Height = 13
      Caption = 'Update refresh rate :'
    end
    object VideoCodecValue: TLabel
      Left = 83
      Top = 39
      Width = 3
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = '-'
      ParentBiDiMode = False
      Transparent = True
    end
    object ElapsedTimeValue: TLabel
      Left = 85
      Top = 53
      Width = 3
      Height = 13
      Caption = '-'
      Transparent = True
    end
    object RealCapturingValue: TLabel
      Left = 112
      Top = 67
      Width = 3
      Height = 13
      Caption = '-'
      Transparent = True
    end
    object CurrentCapturedValue: TLabel
      Left = 123
      Top = 81
      Width = 3
      Height = 13
      Caption = '-'
      Transparent = True
    end
    object FramesCapturedValue: TLabel
      Left = 102
      Top = 95
      Width = 3
      Height = 13
      Caption = '-'
      Transparent = True
    end
    object DropedFramesValue: TLabel
      Left = 93
      Top = 109
      Width = 3
      Height = 13
      Caption = '-'
      Transparent = True
    end
    object UpDown4: TUpDown
      Left = 147
      Top = 13
      Width = 16
      Height = 21
      Associate = Edit9
      Max = 500
      Position = 250
      TabOrder = 1
    end
    object Edit9: TEdit
      Left = 111
      Top = 13
      Width = 36
      Height = 21
      TabOrder = 0
      Text = '250'
      OnChange = Edit9Change
      OnKeyPress = Edit7KeyPress
    end
  end
  object AboutBut: TButton
    Left = 89
    Top = 60
    Width = 75
    Height = 25
    Caption = 'About'
    TabOrder = 5
    OnClick = AboutButClick
  end
  object CheckBox2: TCheckBox
    Left = 12
    Top = 254
    Width = 198
    Height = 17
    Caption = 'Overlay drawing (preview and record)'
    TabOrder = 14
    OnClick = CheckBox2Click
  end
  object BitBtn1: TBitBtn
    Left = 147
    Top = 100
    Width = 24
    Height = 24
    Hint = 'Save To File'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = BitBtn1Click
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000430B0000430B00000000000000000000FF00FFFF00FF
      6D422F6D422F6D422F6D422F6D422F6D422F6D422F6D422F6D422F6D422F6D42
      2F6D422F6D422F6D422FFF00FFCE726FD19792CB8E8A715A48C0B2ABC4BCB8CA
      C1BCCAC1BCCAC1BCCAC1BC8C76679443409443409443406D422FFF00FFCE726F
      D79F9BD19792715A482E1F185F5046FFFFFFFFFFFFFFFFFFF0E4DE8C76679747
      449443409443406D422FFF00FFCE726FDDA8A3D79F9B715A4800000036241BFF
      FFFFF7F1EEF0E4DEE1CABD8C76679E504D9747449443406D422FFF00FFCE726F
      E3B0ABDDA8A3715A48715A48715A48715A48715A48715A48715A48715A48A459
      569E504D9747446D422FFF00FFCE726FEDBDB8E7B5B0E0ACA7DAA49FD49B97CB
      8E8AC58682BF7E79B97571B26B68AB625FA459569E504D6D422FFF00FFCE726F
      F0C1BCEDBDB8E7B5B0E0ACA7DAA49FD49B97CE938EC88A86C2827EBC7975B26B
      68AB625FA459566D422FFF00FFCE726FF0C1BCF0C1BCC8635CC8635CC8635CC8
      635CC8635CC8635CC8635CC8635CC8635CB5706CAE67636D422FFF00FFCE726F
      F0C1BCC8635CFAF5F3F6EEEAF2E7E2EEE2DAEBDCD3E7D6CBE4D0C4E1CABDE1CA
      BDC8635CB5706C6D422FFF00FFCE726FF0C1BCC8635CFFFFFFFCF8F7F8F1EEF4
      EAE6F0E4DEECDFD6E9D9CFE6D3C8E2CDC0C8635CBC79756D422FFF00FFCE726F
      F0C1BCC8635CFFFFFFFFFFFFFCF8F7F8F1EEF4EAE6F0E4DEECDFD6E9D9CFE6D3
      C8C8635CC2827E6D422FFF00FFCE726FF0C1BCC8635CFFFFFFFFFFFFFFFFFFFC
      F8F7F8F1EEF4EAE6F0E4DEECDFD6E9D9CFC8635CC88A866D422FFF00FFCE726F
      F0C1BCC8635CFFFFFFFFFFFFFFFFFFFFFFFFFCF8F7F8F1EEF4EAE6F0E4DEECDF
      D6C8635CD197926D422FFF00FFCE726FF0C1BCC8635CFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFCF8F7F8F1EEF4EAE6F0E4DEC8635C0000006D422FFF00FFCE726F
      F0C1BCC8635CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCF8F7F8F1EEF4EA
      E6C8635CC88A866D422FFF00FFCE726FCE726FCE726FCE726FCE726FCE726FCE
      726FCE726FCE726FCE726FCE726FCE726FCE726FCE726FCE726F}
    Margin = 1
  end
  object FilterEffectsBut: TButton
    Left = 89
    Top = 34
    Width = 75
    Height = 25
    Caption = 'Filter Effects'
    TabOrder = 4
    OnClick = FilterEffectsButClick
  end
  object CheckBox7: TCheckBox
    Left = 212
    Top = 128
    Width = 133
    Height = 17
    Caption = 'Record from all monitors'
    TabOrder = 20
    OnClick = CheckBox7Click
  end
  object ComboBox4: TComboBox
    Left = 294
    Top = 147
    Width = 51
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 21
    OnChange = ComboBox4Change
  end
  object CheckBox10: TCheckBox
    Left = 12
    Top = 275
    Width = 187
    Height = 17
    Caption = 'Add filter effects to overlay drawing'
    TabOrder = 15
    OnClick = CheckBox10Click
  end
  object XPManifest1: TXPManifest
    Left = 492
    Top = 160
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'avi'
    Filter = 'Avi Files (*.avi)|*.avi|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save To File'
    Left = 462
    Top = 160
  end
  object ScrCamera: TScreenCamera
    PSC_Version = '4.4.1.1'
    CurrentMonitor = 1
    RecordAllMonitors = False
    UpdateRate = 100
    PlayBack_FPS = 200
    Record_MSPF = 5
    KeyFramesEvery = 20
    CompressionQuality = 10000
    Colors = pf32bit
    SelectedCompressor = -1
    ScreenRegion = FixedMoving
    ScreenLeft = 0
    ScreenTop = 0
    ScreenWidth = 300
    ScreenHeight = 300
    UseAudioRecord = False
    RecordCursor = True
    DrawAreaCapture = True
    LineRectClear = True
    MinimizeAppOnStart = True
    RestoreAppOnStop = True
    VideoPriority = tpNormal
    FilterColor = 0
    OvelayDrawing = False
    EffectToOvelayDraw = False
    GrayScale = False
    EffectScreenRotate = R90D
    UseRotateImage = False
    EffectNoise = 0
    UseNoise = False
    EffectSaturation = 0
    UseSaturation = False
    EffectRedValue = 0
    EffectGreenValue = 0
    EffectBlueValue = 0
    UseColorAdjusting = False
    EffectBrightness = 0
    UseBrightness = False
    EffectContrast = 0
    UseContrast = False
    OnError = ScrCameraError
    OnUpdate = ScrCameraUpdate
    OnStart = ScrCameraStart
    OnStop = ScrCameraStop
    OnPreview = ScrCameraPreview
    OnSaving = ScrCameraSaving
    OnDeleting = ScrCameraDeleting
    OnOverlay = ScrCameraOverlay
    Left = 432
    Top = 160
  end
end
