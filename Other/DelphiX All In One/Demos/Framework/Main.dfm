object LaunchForm: TLaunchForm
  Left = 284
  Top = 186
  ActiveControl = LaunchButton
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'UnDelphiX Framework Launcher'
  ClientHeight = 361
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object SetupPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 440
    Height = 328
    ActivePage = GameTabSheet
    Align = alTop
    Images = IconImageList
    TabOrder = 1
    object GameTabSheet: TTabSheet
      Caption = 'Game'
      ImageIndex = 7
      object GameGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 410
        Height = 257
        Caption = ' Game Launcher '
        TabOrder = 0
        object LaunchButton: TButton
          Left = 136
          Top = 224
          Width = 121
          Height = 25
          Caption = 'Play Game'
          TabOrder = 0
          OnClick = LaunchButtonClick
        end
        object GameMemo: TMemo
          Left = 16
          Top = 24
          Width = 377
          Height = 193
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object PlayNetworkButton: TButton
          Left = 264
          Top = 224
          Width = 121
          Height = 25
          Caption = 'Play Networked Game'
          TabOrder = 2
          OnClick = PlayNetworkButtonClick
        end
      end
      object ShowAgainCheckBox: TCheckBox
        Left = 218
        Top = 277
        Width = 200
        Height = 17
        Caption = 'Show this setup dialog next time I play'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
    end
    object VideoTabSheet: TTabSheet
      Caption = 'Video'
      object ScreenGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 417
        Height = 48
        Caption = ' Screen settings '
        TabOrder = 0
        object VideoLabel: TLabel
          Left = 13
          Top = 20
          Width = 57
          Height = 13
          Caption = 'Video Mode'
        end
        object VideoModeCombo: TComboBox
          Left = 78
          Top = 16
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            '320 x 240 x 8'
            '512 x 384 x 8'
            '640 x 480 x 8'
            '800 x 600 x 8'
            '1024 x 768 x 8'
            '320 x 240 x 16'
            '512 x 384 x 16'
            '640 x 480 x 16'
            '800 x 600 x 16'
            '1024 x 768 x 16'
            '320 x 240 x 24'
            '512 x 384 x 24'
            '640 x 480 x 24'
            '800 x 600 x 24'
            '1024 x 768 x 24')
        end
        object FullScreenCheckBox: TCheckBox
          Left = 249
          Top = 18
          Width = 83
          Height = 17
          Caption = 'Full Screen'
          TabOrder = 1
        end
      end
      object PerformGroupBox: TGroupBox
        Left = 244
        Top = 65
        Width = 181
        Height = 100
        Caption = ' Video Performance '
        TabOrder = 1
        object SysMemCheckBox: TCheckBox
          Left = 9
          Top = 39
          Width = 125
          Height = 17
          Caption = 'Use System Memory'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object WaitVSyncCheckBox: TCheckBox
          Left = 9
          Top = 19
          Width = 101
          Height = 17
          Caption = 'Wait for v-sync'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object FlipCheckBox: TCheckBox
          Left = 9
          Top = 60
          Width = 160
          Height = 17
          Caption = 'Use Hardware Page Flipping'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
      object FrameRateGroupBox: TGroupBox
        Left = 8
        Top = 65
        Width = 227
        Height = 101
        Caption = '   '
        TabOrder = 2
        object SetFrameCheckBox: TCheckBox
          Left = 10
          Top = -1
          Width = 132
          Height = 17
          Caption = 'Set target frame rate'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object FrameSkipCheckBox: TCheckBox
          Left = 11
          Top = 75
          Width = 116
          Height = 17
          Caption = 'Frame skip if behind'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object FrameTrackPanel: TPanel
          Left = 10
          Top = 20
          Width = 209
          Height = 46
          TabOrder = 2
          object LowFrameLabel: TLabel
            Left = 8
            Top = 27
            Width = 26
            Height = 13
            Caption = '15fps'
          end
          object MedFrameLabel: TLabel
            Left = 120
            Top = 27
            Width = 26
            Height = 13
            Caption = '60fps'
          end
          object HiFrameLabel: TLabel
            Left = 173
            Top = 27
            Width = 26
            Height = 13
            Caption = '85fps'
          end
          object FrameTrackBar: TTrackBar
            Left = 3
            Top = 6
            Width = 201
            Height = 21
            Max = 85
            Min = 15
            Orientation = trHorizontal
            Frequency = 5
            Position = 60
            SelEnd = 0
            SelStart = 0
            TabOrder = 0
            ThumbLength = 12
            TickMarks = tmBottomRight
            TickStyle = tsAuto
          end
        end
      end
      object VideoInfoGroupBox: TGroupBox
        Left = 8
        Top = 168
        Width = 417
        Height = 97
        Caption = ' Video Info '
        Enabled = False
        TabOrder = 3
      end
    end
    object AudioTabSheet: TTabSheet
      Caption = 'Audio'
      ImageIndex = 4
      object AudioBitRadioGroup: TRadioGroup
        Left = 296
        Top = 152
        Width = 129
        Height = 65
        Caption = ' Audio bit depth'
        ItemIndex = 0
        Items.Strings = (
          '8 bit audio'
          '16 bit audio')
        TabOrder = 0
      end
      object AudiokHzRadioGroup: TRadioGroup
        Left = 296
        Top = 64
        Width = 129
        Height = 81
        Caption = ' Audio kHz rate'
        ItemIndex = 1
        Items.Strings = (
          '11kHz'
          '22kHz'
          '44kHz')
        TabOrder = 1
      end
      object VolumeGroupBox: TGroupBox
        Left = 8
        Top = 64
        Width = 281
        Height = 153
        Caption = ' Audio volume '
        TabOrder = 2
        object SoundVolPanel: TPanel
          Left = 16
          Top = 36
          Width = 249
          Height = 43
          TabOrder = 0
          object MinLabel1: TLabel
            Left = 7
            Top = 27
            Width = 17
            Height = 13
            Caption = 'Min'
            Enabled = False
          end
          object MaxLabel1: TLabel
            Left = 216
            Top = 26
            Width = 20
            Height = 13
            Caption = 'Max'
            Enabled = False
          end
          object SoundVolTrackBar: TTrackBar
            Left = 3
            Top = 6
            Width = 238
            Height = 22
            Enabled = False
            Max = 0
            Min = -10000
            Orientation = trHorizontal
            Frequency = 500
            Position = -2000
            SelEnd = 0
            SelStart = 0
            TabOrder = 0
            ThumbLength = 12
            TickMarks = tmBottomRight
            TickStyle = tsAuto
          end
        end
        object MusicVolPanel: TPanel
          Left = 16
          Top = 100
          Width = 249
          Height = 43
          TabOrder = 1
          object MinLabel2: TLabel
            Left = 7
            Top = 27
            Width = 17
            Height = 13
            Caption = 'Min'
            Enabled = False
          end
          object MaxLabel2: TLabel
            Left = 216
            Top = 26
            Width = 20
            Height = 13
            Caption = 'Max'
            Enabled = False
          end
          object MusicVolTrackBar: TTrackBar
            Left = 3
            Top = 6
            Width = 238
            Height = 22
            Enabled = False
            Max = 0
            Min = -10000
            Orientation = trHorizontal
            Frequency = 500
            Position = -3000
            SelEnd = 0
            SelStart = 0
            TabOrder = 0
            ThumbLength = 12
            TickMarks = tmBottomRight
            TickStyle = tsAuto
          end
        end
        object SoundCheckBox: TCheckBox
          Left = 15
          Top = 17
          Width = 97
          Height = 17
          Caption = 'Sound effects'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 2
        end
        object MusicCheckBox: TCheckBox
          Left = 15
          Top = 81
          Width = 97
          Height = 17
          Caption = 'Music'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 3
        end
      end
      object AudioSettingsGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 417
        Height = 48
        Caption = ' Audio settings '
        TabOrder = 3
        object ExclusiveAudioCheckBox: TCheckBox
          Left = 115
          Top = 18
          Width = 100
          Height = 17
          Caption = 'Exclusive Audio'
          TabOrder = 0
        end
        object AudioCheckBox: TCheckBox
          Left = 12
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Audio Enabled'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object StereoCheckBox: TCheckBox
          Left = 219
          Top = 18
          Width = 89
          Height = 17
          Caption = 'Stereo Audio'
          TabOrder = 2
        end
      end
    end
    object InputTabSheet: TTabSheet
      Caption = 'Input'
      ImageIndex = 5
      object InputGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 409
        Height = 48
        Caption = ' Input settings'
        TabOrder = 0
        object DXInputCheckBox: TCheckBox
          Left = 12
          Top = 18
          Width = 105
          Height = 17
          Caption = 'Use Direct Input'
          TabOrder = 0
        end
      end
      object PageControl: TPageControl
        Left = 8
        Top = 64
        Width = 409
        Height = 201
        ActivePage = TesrTabSheet
        Images = IconImageList
        TabOrder = 1
        object JoyTabSheet: TTabSheet
          Caption = 'Joystick'
          ImageIndex = 1
          object JoyGroupBox: TGroupBox
            Left = 8
            Top = 8
            Width = 345
            Height = 121
            Caption = ' '
            TabOrder = 0
            object SenseLabel: TLabel
              Left = 15
              Top = 20
              Width = 90
              Height = 13
              Caption = 'Joystick sensativity'
              Enabled = False
            end
            object AutoCenter: TCheckBox
              Left = 16
              Top = 92
              Width = 121
              Height = 17
              Caption = 'Auto Center Joystick'
              Enabled = False
              TabOrder = 0
            end
            object JoyCheckBox: TCheckBox
              Left = 11
              Top = -1
              Width = 104
              Height = 17
              Caption = 'Joystick enabled'
              Enabled = False
              TabOrder = 1
            end
            object JoySensePanel: TPanel
              Left = 14
              Top = 37
              Width = 249
              Height = 44
              TabOrder = 2
              object LessLabel: TLabel
                Left = 8
                Top = 28
                Width = 22
                Height = 13
                Caption = 'Less'
                Enabled = False
              end
              object MoreLabel: TLabel
                Left = 214
                Top = 27
                Width = 24
                Height = 13
                Caption = 'More'
                Enabled = False
              end
              object JoySenseTrackBar: TTrackBar
                Left = 3
                Top = 6
                Width = 238
                Height = 22
                Enabled = False
                Max = 100
                Orientation = trHorizontal
                Frequency = 5
                Position = 50
                SelEnd = 0
                SelStart = 0
                TabOrder = 0
                ThumbLength = 12
                TickMarks = tmBottomRight
                TickStyle = tsAuto
              end
            end
          end
        end
        object KeyTabSheet: TTabSheet
          Caption = 'Keyboard'
          ImageIndex = 2
          object KeyGroupBox: TGroupBox
            Left = 8
            Top = 8
            Width = 377
            Height = 157
            Caption = ' Keyboard assign '
            TabOrder = 0
            object OrLabel: TLabel
              Left = 297
              Top = 88
              Width = 9
              Height = 13
              Caption = 'or'
              Enabled = False
            end
            object SecondaryLabel: TLabel
              Left = 166
              Top = 67
              Width = 72
              Height = 13
              Caption = 'Secondary Key'
              Enabled = False
            end
            object PrimaryLabel: TLabel
              Left = 166
              Top = 36
              Width = 55
              Height = 13
              Caption = 'Primary Key'
              Enabled = False
            end
            object ActionLabel: TLabel
              Left = 13
              Top = 19
              Width = 30
              Height = 13
              Caption = 'Action'
              Enabled = False
            end
            object StateListBox: TListBox
              Left = 11
              Top = 35
              Width = 148
              Height = 113
              Enabled = False
              ItemHeight = 13
              TabOrder = 0
            end
            object KeyComboBox1: TComboBox
              Left = 248
              Top = 34
              Width = 113
              Height = 21
              Style = csDropDownList
              Enabled = False
              ItemHeight = 0
              TabOrder = 1
            end
            object KeyComboBox2: TComboBox
              Left = 248
              Top = 63
              Width = 113
              Height = 21
              Style = csDropDownList
              Enabled = False
              ItemHeight = 0
              TabOrder = 2
            end
            object KeyComboBox3: TComboBox
              Left = 248
              Top = 107
              Width = 113
              Height = 21
              Style = csDropDownList
              Enabled = False
              ItemHeight = 0
              TabOrder = 3
            end
          end
        end
        object TesrTabSheet: TTabSheet
          Caption = 'Test'
          ImageIndex = 7
          object InputTestGroupBox: TGroupBox
            Left = 8
            Top = 8
            Width = 361
            Height = 105
            Caption = ' Input test '
            TabOrder = 0
            object SpeedButton1: TSpeedButton
              Left = 96
              Top = 45
              Width = 23
              Height = 22
              Caption = '1'
              Enabled = False
            end
            object SpeedButton2: TSpeedButton
              Left = 128
              Top = 45
              Width = 23
              Height = 22
              Caption = '2'
              Enabled = False
            end
            object SpeedButton3: TSpeedButton
              Left = 160
              Top = 45
              Width = 23
              Height = 22
              Caption = '3'
              Enabled = False
            end
            object SpeedButton4: TSpeedButton
              Left = 192
              Top = 45
              Width = 23
              Height = 22
              Caption = '4'
              Enabled = False
            end
            object SpeedButton5: TSpeedButton
              Left = 224
              Top = 45
              Width = 23
              Height = 22
              Caption = '5'
              Enabled = False
            end
            object SpeedButton6: TSpeedButton
              Left = 256
              Top = 45
              Width = 23
              Height = 22
              Caption = '6'
              Enabled = False
            end
            object SpeedButton7: TSpeedButton
              Left = 288
              Top = 45
              Width = 23
              Height = 22
              Caption = '7'
              Enabled = False
            end
            object SpeedButton8: TSpeedButton
              Left = 320
              Top = 45
              Width = 23
              Height = 22
              Caption = '8'
              Enabled = False
            end
            object JoyTestPanel: TPanel
              Left = 16
              Top = 24
              Width = 64
              Height = 64
              TabOrder = 0
              object JoyShape: TShape
                Left = 24
                Top = 24
                Width = 16
                Height = 16
                Enabled = False
                Pen.Mode = pmBlack
                Shape = stCircle
              end
            end
          end
        end
      end
    end
    object NetworkTabSheet: TTabSheet
      Caption = 'Network'
      ImageIndex = 6
      object ConnectionGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 417
        Height = 48
        Caption = ' Connection settings '
        TabOrder = 0
        object ConnectionLabel: TLabel
          Left = 15
          Top = 20
          Width = 77
          Height = 13
          Caption = 'Connection type'
        end
        object ConnectionComboBox: TComboBox
          Left = 100
          Top = 16
          Width = 221
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = ConnectionComboBoxChange
        end
        object ConnectButton: TButton
          Left = 328
          Top = 14
          Width = 81
          Height = 25
          Caption = 'Connect'
          TabOrder = 1
          OnClick = ConnectButtonClick
        end
      end
      object InternetGroupBox: TGroupBox
        Left = 8
        Top = 64
        Width = 417
        Height = 49
        Caption = '   '
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 21
          Width = 60
          Height = 13
          Caption = 'Game server'
          Enabled = False
        end
        object ServersComboBox: TComboBox
          Left = 86
          Top = 17
          Width = 163
          Height = 21
          Enabled = False
          ItemHeight = 13
          TabOrder = 0
          Text = 'directplay.server.org'
          Items.Strings = (
            'directplayserver.no2games.com')
        end
        object PortCheckBox: TCheckBox
          Left = 258
          Top = 18
          Width = 69
          Height = 17
          Caption = 'Fixed port'
          Enabled = False
          TabOrder = 1
        end
        object PortSpinEdit: TSpinEdit
          Left = 328
          Top = 16
          Width = 65
          Height = 22
          Enabled = False
          MaxValue = 65535
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object SearchInternetCheckBox: TCheckBox
          Left = 11
          Top = -1
          Width = 143
          Height = 17
          Caption = 'Search internet for games'
          Enabled = False
          TabOrder = 3
        end
      end
      object JoinHostPageControl: TPageControl
        Left = 8
        Top = 120
        Width = 417
        Height = 178
        ActivePage = Join
        Images = IconImageList
        TabOrder = 2
        Visible = False
        object Join: TTabSheet
          Caption = 'Join'
          ImageIndex = 6
          object NameLabel: TLabel
            Left = 105
            Top = 128
            Width = 60
            Height = 13
            Caption = 'Player Name'
          end
          object JoinButton: TButton
            Left = 323
            Top = 122
            Width = 75
            Height = 25
            Caption = 'Join Game'
            TabOrder = 0
            OnClick = JoinButtonClick
          end
          object PlayerComboBox: TComboBox
            Left = 170
            Top = 125
            Width = 145
            Height = 21
            ItemHeight = 13
            TabOrder = 1
            Text = 'New Player'
          end
          object GameStringGrid: TStringGrid
            Left = 0
            Top = 0
            Width = 409
            Height = 93
            Align = alTop
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 10
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect, goThumbTracking]
            ScrollBars = ssVertical
            TabOrder = 2
            OnMouseUp = GameStringGridMouseUp
            ColWidths = (
              407)
          end
          object SessionEdit: TEdit
            Left = 1
            Top = 96
            Width = 407
            Height = 21
            TabOrder = 3
          end
        end
        object HostSheet: TTabSheet
          Caption = 'Host'
          ImageIndex = 8
          object HostGroupBox: TGroupBox
            Left = 8
            Top = 8
            Width = 393
            Height = 73
            Caption = ' Hosting options '
            TabOrder = 0
            object PlayerHostLabel: TLabel
              Left = 17
              Top = 44
              Width = 60
              Height = 13
              Caption = 'Player Name'
            end
            object GameNameLabel: TLabel
              Left = 17
              Top = 20
              Width = 59
              Height = 13
              Caption = 'Game Name'
            end
            object PasswordLabel: TLabel
              Left = 241
              Top = 20
              Width = 46
              Height = 13
              Caption = 'Password'
            end
            object Label2: TLabel
              Left = 241
              Top = 44
              Width = 80
              Height = 13
              Caption = 'Maximum players'
            end
            object PlayerHostComboBox: TComboBox
              Left = 85
              Top = 41
              Width = 145
              Height = 21
              ItemHeight = 0
              TabOrder = 0
              Text = 'New Player'
            end
            object HostNameComboBox: TComboBox
              Left = 85
              Top = 17
              Width = 145
              Height = 21
              ItemHeight = 0
              TabOrder = 1
              Text = 'New Game'
            end
            object PasswordEdit: TEdit
              Left = 296
              Top = 16
              Width = 81
              Height = 21
              PasswordChar = '*'
              TabOrder = 2
            end
            object MaxPlayersSpinEdit: TSpinEdit
              Left = 328
              Top = 40
              Width = 49
              Height = 22
              MaxValue = 8
              MinValue = 0
              TabOrder = 3
              Value = 8
            end
          end
          object HostButton: TButton
            Left = 315
            Top = 90
            Width = 75
            Height = 25
            Caption = 'Host Game'
            TabOrder = 1
            OnClick = HostButtonClick
          end
        end
      end
    end
  end
  object DXDraw: TDXDraw
    Left = 2
    Top = 331
    Width = 28
    Height = 28
    AutoInitialize = False
    AutoSize = True
    Color = clBlack
    Display.BitCount = 16
    Display.FixedBitCount = False
    Display.FixedRatio = True
    Display.FixedSize = False
    Display.Height = 384
    Display.Width = 512
    Options = [doFullScreen, doAllowReboot, doAllowPalette256, doCenter, doRetainedMode, doSelectDriver]
    SurfaceHeight = 28
    SurfaceWidth = 28
    OnFinalize = DXDrawFinalize
    OnInitialize = DXDrawInitialize
    OnInitializing = DXDrawInitializing
    TabOrder = 0
    Visible = False
    OnMouseMove = DXDrawMouseMove
  end
  object EngineDXTimer: TDXTimer
    ActiveOnly = True
    Enabled = False
    Interval = 0
    OnTimer = EngineDXTimerTimer
    Left = 68
    Top = 331
  end
  object DXImageList: TDXImageList
    DXDraw = DXDraw
    Items = <
      item
        Name = 'mouse'
        PatternHeight = 0
        PatternWidth = 0
        Picture.Data = {
          04544449422800000020000000200000000100180000000000000C0000000000
          00000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFF00
          0000000000000000025353013F3F000000000000000000FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFF00000000
          0000000000039D9D039292025555013F3F000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF000000038F8F00000000000000000000000000
          000003A6A604C6C604E1E104C0C0026262013F3F000000000000000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000004CFCF013F3F00000000000000000003
          A6A604C6C605EBEB05FAFA05F3F304C4C4027070014646000000000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF0000001FFDFD038C8C013F3F000000038B8B04
          C6C605EBEB05FAFA05FDFD08FDFD08FDFD04D9D903AFAF000000000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF0000003BFDFD05EBEB038686038C8C04C0C005
          EBEB05FAFA05FDFD08FDFD11FDFD18FDFD0BFDFD000000000000000000FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000041FDFD1FFDFD05E4E405E4E405F1F105
          FAFA05FDFD08FDFD11FDFD18FDFD12FDFD000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000041FDFD26FDFD05FDFD05FDFD05FDFD05
          FDFD08FDFD11FDFD18FDFD12FDFD000000000000000000FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000041FDFD26FDFD05FDFD05FDFD05FDFD05
          FDFD06FDFD05F7F705E8E8000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000041FDFD26FDFD05FDFD05FDFD05FDFD05
          FDFD05EEEE04B8B8000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000041FDFD26FDFD05FDFD05FDFD05FDFD05
          FDFD05E5E5039292013F3F000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000064FEFE5DFEFE44FEFE44FEFE44FEFE44
          FEFE3DFDFD11FDFD03A9A9025656000000000000000000FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000098FEFE96FEFE87FEFE87FEFE87FEFE87
          FEFE87FEFE84FEFE6AFEFE26FDFD04D5D5000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFF}
        SystemMemory = False
        Transparent = True
        TransparentColor = clWhite
      end
      item
        Name = 'font'
        PatternHeight = 6
        PatternWidth = 5
        Picture.Data = {
          0454444942280000009B000000120000000100080000000000F80A0000000000
          0000000000000000000000000000000000010101000202020003030300040404
          0005050500060606000707070008080800090909000A0A0A000B0B0B000C0C0C
          000D0D0D000E0E0E000F0F0F0010101000111111001212120013131300141414
          0015151500161616001717170018181800191919001A1A1A001B1B1B001C1C1C
          001D1D1D001E1E1E001F1F1F0020202000212121002222220023232300242424
          0025252500262626002727270028282800292929002A2A2A002B2B2B002C2C2C
          002D2D2D002E2E2E002F2F2F0030303000313131003232320033333300343434
          0035353500363636003737370038383800393939003A3A3A003B3B3B003C3C3C
          003D3D3D003E3E3E003F3F3F0040404000414141004242420043434300444444
          0045454500464646004747470048484800494949004A4A4A004B4B4B004C4C4C
          004D4D4D004E4E4E004F4F4F0050505000515151005252520053535300545454
          0055555500565656005757570058585800595959005A5A5A005B5B5B005C5C5C
          005D5D5D005E5E5E005F5F5F0060606000616161006262620063636300646464
          0065656500666666006767670068686800696969006A6A6A006B6B6B006C6C6C
          006D6D6D006E6E6E006F6F6F0070707000717171007272720073737300747474
          0075757500767676007777770078787800797979007A7A7A007B7B7B007C7C7C
          007D7D7D007E7E7E007F7F7F0080808000818181008282820083838300848484
          0085858500868686008787870088888800898989008A8A8A008B8B8B008C8C8C
          008D8D8D008E8E8E008F8F8F0090909000919191009292920093939300949494
          0095959500969696009797970098989800999999009A9A9A009B9B9B009C9C9C
          009D9D9D009E9E9E009F9F9F00A0A0A000A1A1A100A2A2A200A3A3A300A4A4A4
          00A5A5A500A6A6A600A7A7A700A8A8A800A9A9A900AAAAAA00ABABAB00ACACAC
          00ADADAD00AEAEAE00AFAFAF00B0B0B000B1B1B100B2B2B200B3B3B300B4B4B4
          00B5B5B500B6B6B600B7B7B700B8B8B800B9B9B900BABABA00BBBBBB00BCBCBC
          00BDBDBD00BEBEBE00BFBFBF00C0C0C000C1C1C100C2C2C200C3C3C300C4C4C4
          00C5C5C500C6C6C600C7C7C700C8C8C800C9C9C900CACACA00CBCBCB00CCCCCC
          00CDCDCD00CECECE00CFCFCF00D0D0D000D1D1D100D2D2D200D3D3D300D4D4D4
          00D5D5D500D6D6D600D7D7D700D8D8D800D9D9D900DADADA00DBDBDB00DCDCDC
          00DDDDDD00DEDEDE00DFDFDF00E0E0E000E1E1E100E2E2E200E3E3E300E4E4E4
          00E5E5E500E6E6E600E7E7E700E8E8E800E9E9E900EAEAEA00EBEBEB00ECECEC
          00EDEDED00EEEEEE00EFEFEF00F0F0F000F1F1F100F2F2F200F3F3F300F4F4F4
          00F5F5F500F6F6F600F7F7F700F8F8F800F9F9F900FAFAFA00FBFBFB00FCFCFC
          00FDFDFD00FEFEFE00FFFFFF00FF0000FF006FFFFF6F00FF0000FF00006FFF00
          0000000000000000000000000000FBFB00000000000000000000000000000000
          00000000000000000000FF000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000FFFFFFFF00FF0000FF00FFFFFFFF000000000000006F10
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000FF00000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000FF0000FF00FF0000FF00FF0000FF00006FFF000010FFFF6F000000
          00000000000000000000000000000000000000000000000000000000000000FF
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          006FFFFF6F00FF0000FF006FFFFF6F00000000FF006FFFFF1000000000000000
          000000000000000000000000000000000000000000000000000000FF1000FF00
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000FF0000
          FF006FFFFF6F000000000000FF0000FF0000106F000000000000000000000000
          000000000000000000000000000000000000000000000000FF10FF0000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000006FFFFF6F00FF00
          00FF00FF0000FF006FFFFF6F0000000000000000000000000000000000000000
          000000000000000000000000000000000000000000FFFF000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000006FFFFF6F00000000FF00FF
          FFFFFF006FFFFF6F00000000FF006FFFFF6F006FFFFF6F0000FF0000006FFFFF
          6F0000FFFF6F00FF00FF00FFFFFF000000000000000000006FFF00FF6F000000
          0000000000000000000000FF000000FFFFFFFFFF000000000000000000FFFF00
          00000000FFFF00000000FFFF000000000000006FFF6FFF000000FFFF0000FF00
          0000000000000000FF0000000000000000FF0010FF00000000FF0010FF000000
          FF0000FF00000000FF00FF0000FF00FF0000FF0000FF000000FF0000FF000000
          00FF000000000000000000000000FFFF00000000FF6F006FFF00000000000000
          000000000000000000000000000000000000FF0000000000FF0000FF00000000
          FF000000000000FF00000000000000FF00FF0000FF00FFFF0000FF0000000000
          000000FFFF6F0000FFFF000000FF00FFFF00000000FF000010FF0000000000FF
          00FFFFFFFF00000000FF00FF0000FF0000FF6F0000FF0000FF006FFFFFFF0000
          00000000000000000000000000000000FF000000FF000000FFFFFFFF00000000
          000000FF00000000000000000000FF00000000FF00000000FF000000FF000000
          000000FF00000000000000FF6FFFFF0000FF0000000000000000FFFFFF000000
          FFFF00FFFFFFFF0000FFFF00FF00000000FF00000010FF000000FF6F00FF1000
          FF00FFFFFF6F00FFFFFF00000000FF00006FFFFF6F00FF0000FF000000000000
          000000000000000000000000FF000000FF0000000000000000000000000000FF
          0000000000000000FFFFFFFFFF00FF000000000000FF0000FF000000000000FF
          00FF000000FF006FFF0000000000FF000000000000000000000000FFFF000000
          FFFF000000FF1000FF000000FFFF00FF0000FF00FF0000FF0000FF10FF00FF00
          000000FF0000FF0000006FFF00FF0000FF00FF0000FF00000000000000000000
          0000FFFF00000000FF6F006FFF00000000000000000000FF000000FF00000000
          000000000000FF0000FF0000000000000000FF00FF000000000000FF0000FF00
          FF0000FF00FF0000FFFF00FF000000000000FFFFFF00006FFFFF00FFFFFFFF00
          006FFFFF6F0000006FFF006FFFFF6F006FFFFF6F000000FFFF00FFFFFFFF006F
          FFFF6F00FFFFFFFF006FFFFF6F006FFFFF6F0000000000000000000000000000
          000000006FFF00FF6F00000000000000000000FF000000FF0000000000000000
          0000FF00000000000000000000000000FFFF00000000FFFF000000FF0000006F
          FF6F0000FFFF0000000000000000000000000000FF000000FFFF000000FF0000
          FF00FFFFFF6F006FFFFF6F00FFFFFF6F00FFFFFFFF00FF000000006FFFFFFF00
          FF0000FF0000FFFFFF006FFFFF6F00FF0010FF00FFFFFFFF00FF0000FF00FF00
          00FF006FFFFF6F00FF000000006FFF6FFF00FF0000FF006FFFFF6F000000FF00
          006FFFFF6F0000EBEB0000FF0000FF00FF0000FF0000FFFF6F00FFFFFFFF0000
          0000000000006FFFFF6F000000000000000000000000000000FF0000FF00FF00
          00FF00FF1000FF00FF0010FF00FF00000000FF00000000FF0000FF00FF0000FF
          000000FF0000FF1000FF00FF006FFF00FF00000000FF0000FF00FF0010FF00FF
          0000FF00FF00000000FF00FF6F00FF0010FF00FF0000FF000000FF0000FF0000
          FF0008FFFF0800FFFFFFFF00FF0000FF00000000FF00FF100000000000000000
          006FFF0000FF000000000000000000000000000000FFFFFFFF00FF0000FF00FF
          00000000FF0000FF00FF00000000FF00000000FF00FFFF00FF0000FF000000FF
          0000000000FF00FF00FF6F00FF00000000FF1010FF00FF00FFFF00FF0000FF00
          FFFFFF6F00FF6F00FF00FFFFFF10000010FF6F000000FF0000FF0000FF009191
          919100FF6F6FFF00FF0000FF006FFFFFFF0010FF100000000000000000FF00FF
          FFFFFF0000000000000000000000000000FF0000FF00FFFFFF6F00FF00000000
          FF0000FF00FFFFFF0000FFFFFF0000FF00000000FFFFFFFF000000FF00000000
          00FF00FFFF6F0000FF00000000FFFFFFFF00FF6F6FFF00FF0000FF00FF0000FF
          00FF0000FF00FF0010FF006FFF1000000000FF0000FF0000FF00FD0808FD00FF
          0000FF006FFFFF6F00FF0000FF000010FF1000000000000000FF00FFFF00FF00
          00000000000000000000000000FF1010FF00FF0000FF00FF1000FF00FF0010FF
          00FF00000000FF00000000FF0000FF00FF0000FF000000FF0000000000FF00FF
          00FF0000FF00000000FF6F6FFF00FFFF00FF00FF0000FF00FF0000FF00FF0000
          FF00FF0000FF00FF0000FF000000FF0000FF0000FF00FF0000FF00FF0000FF00
          FF0000FF00FF0000FF00000010FF0000FF00FF00006FFF0000FF6F0000000000
          0000000000000000006FFFFF6F00FFFFFF6F006FFFFF6F00FFFFFF6F00FFFFFF
          FF00FFFFFFFF006FFFFF6F00FF0000FF0000FFFFFF000010FFFF00FF0010FF00
          FF00000000FF0000FF00FF0000FF006FFFFF6F00FFFFFF6F006FFFFF6F00FFFF
          FF6F006FFFFF6F0000FFFFFF00FF0000FF00FF0000FF00FF0000FF00FF0000FF
          00FF0000FF00FFFFFFFF0000FF00FF0000006FFFFF6F00000000000000000000
          0000000000}
        SystemMemory = True
        Transparent = False
        TransparentColor = clBlack
      end>
    Left = 266
    Top = 331
  end
  object IconImageList: TImageList
    Left = 233
    Top = 331
    Bitmap = {
      494C010109000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      000000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000DFE0E000DFE0
      E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B0CFC000DFE0E000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0E000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00B0CFC000DFE0
      E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CF
      C000DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0
      E000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DFE0E000DFE0E000DFE0E000DFE0
      E000DFE0E000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B0CFC000DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000DFE0E000DFE0E000DFE0E000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000DFE0E000DFE0E000DFE0E000DFE0E000DFE0E000DFE0
      E000DFE0E000DFE0E000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00B0CFC000DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC0006F908F00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B0CFC000DFE0E000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0E000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000DFE0E000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B0CFC000DFE0E000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0E000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000DFE0E000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0
      E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F90
      8F00DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F
      7F007F7F7F007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000DFE0E000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000000
      000000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F
      7F00FFFFFF00FFFFFF00FFFFFF007F7F7F000000000000000000000000000000
      000000000000000000000F0F0F000080BF00006090000080C0000F0F0F000000
      0000000000000000000000000000000000007F7F7F00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CF
      C000DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000F0F0F000080C000006090000F8FD0000F0F0F000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000F0F0F00008FCF00007FB0000F90DF000F0F0F000000
      000000000000000000000000000000000000FFFFFF007F7F00007F7F00007F7F
      00007F7F00007F7F00007F7F00007F7F00007F7F00007F7F00007F7F0000FFFF
      FF00FFFFFF007F7F7F00FFFFFF00FFFFFF00FFFFFF007F7F7F00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B0CFC000DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      0000000000000F0F0F000F0F0F000F8FD0000F90DF001090DF000F0F0F000F0F
      0F0000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      0000000000000F0F0F000F8FD0000F90DF001090E000109FE000109FE0000F0F
      0F0000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F00007F7F
      00007F7F00007F7F0000FFFFFF007F7F00007F7F00007F7F00007F7F00007F7F
      0000FFFFFF00FFFFFF00FFFFFF007F7F7F007F7F7F007F7F7F00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00B0CFC000DFE0E000B0CFC0000000000000000000000000000000
      0000000000000F0F0F001090DF001090E000109FE0001FA0EF001FA0F0000F0F
      0F0000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0E000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      0000000000000F0F0F00109FE000109FE0001FA0EF001FA0FF001FA0FF000F0F
      0F0000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F0000000000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000FFFFFF00B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007F7F
      00007F7F00007F7F00007F7F0000FFFFFF007F7F00007F7F00007F7F00007F7F
      00007F7F0000FFFFFF00FFFFFF007F7F7F000000000000000000000000000000
      0000000000000F0F0F001FA0EF001FAFEF0020B0F00020BFF0002FC0FF000F0F
      0F00000000000000000000000000000000007F7F7F00FFFFFF00FFFFFF00FFFF
      FF007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007F7F7F007F7F7F00B0CFC000DFE0E000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0E000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000F0F0F00707F70006F6F6F00404F40000F0F0F000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007F7F00007F7F00007F7F00007F7F00007F7F0000FFFFFF007F7F
      00007F7F00007F7F00007F7F0000FFFFFF00FFFFFF007F7F7F00FFFFFF00FFFF
      FF00FFFFFF007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007F7F7F00F0F0F000F0F0F0005F606000DFE0E000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0
      E000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      000000000000000000000F0F0F00B0B0B0008F908F006F6F6F000F0F0F000000
      000000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC0000000000000000000000000000000
      000000000000000000000F0F0F00B0B0B000B0B0B000707F70000F0F0F000000
      000000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007F7F00007F7F00007F7F00007F7F00007F7F0000FFFF
      FF007F7F00007F7F00007F7F00007F7F0000FFFFFF00FFFFFF00FFFFFF007F7F
      7F007F7F7F007F7F7F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007F7F7F00F0F0F0000000000000000000000000000000
      00000000000000000000000000000F0F0F000F0F0F000F0F0F00000000000000
      000000000000000000000000000000000000F0F0F0007F7F7F00B0CFC000DFE0
      E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C0006F908F00DFE0E000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B0CFC0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC0006F908F00DFE0E000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000B0CF
      C000B0CFC000B0CFC000B0CFC000B0CFC000B0CFC000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007F7F00007F7F00007F7F00007F7F00007F7F
      00007F7F0000FFFFFF007F7F00007F7F00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0F0F000F0F
      0F000F0F0F000F0F0F0000000000000000000000000000000000000000000000
      000000000000000000000F0F0F000080BF00006090000080C0000F0F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000F5FAF00004F8F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000F0F0F00000000009F9FA0008F8F
      900080808000707070000F0F0F000000000000000000000000000F0F0F000F0F
      0F000F0F0F000F0F0F000F0F0F000080C000006090000F8FD0000F0F0F000F0F
      0F000F0F0F000F0F0F000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F5FBF000F6FD0000F5FAF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000F0F0F00AFAFAF00B0B0B000B0B0B000A0AF
      AF009F9FA00090909F0060606F000F0F0F0000000000000000000F0F0F00BF6F
      0000904F0000C06F00000F0F0F00008FCF00007FB0000F90DF000F0F0F00409F
      200030701F004FA02F000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F60C0000F70E0000F7FEF000F60C00000000000202020002020
      2000202020002020200000000000000000000000000000000000000000000000
      000000000000000000000F0F0F00B0B0BF00BFBFC000C0CFCF00C0CFCF00BFBF
      C000B0B0BF00AFAFB000808080000F0F0F0000000000000000000F0F0F00C06F
      0000904F00000F0F0F000F0F0F000F8FD0000F90DF001090DF000F0F0F000F0F
      0F0030701F004FAF2F000F0F0F0000000000000000000F7F800010909F001090
      9F0010909F0010909F0010909F0010909F0010909F0010909F0010909F001090
      9F0010909F0010909F0010909F00000000000000000000000000000000000000
      00000F70DF001F80EF002F8FEF002F8FEF000F6FD00000000000F07F40002020
      2000F07F40002020200000000000000000000000000000000000000000000000
      0000000000000F0F0F00BFBFBF00C0CFCF00CFD0D000CFCFCF00CFCFCF00C0C0
      C000BFBFBF00AFB0B000808080000F0F0F0000000000000000000F0F0F00CF70
      0000B06000000F0F0F000F8FD0000F90DF001090E000109FE000109FE0000F0F
      0F00408F200050B02F000F0F0F0000000000000000001FC0CF00505050004040
      3F0040403F0040404000404040004040400040403F004F403F004F403F004040
      3F00404040005050500010909F0000000000000000000F60C0001F80EF002F8F
      EF004F9FF0005FA0F0005FA0F0005FA0F0001F80EF0000000000DF8F5F002020
      2000DF8F5F002020200000000000000000000000000000000000000000000000
      00000F0F0F00907F7F002F302F00BFBFC000C0CFCF00CFD0D000CFD0D000C0CF
      CF00BFBFC000B0B0B000707070000F0F0F00000000000F0F0F000F0F0F00D07F
      0F00DF800F000F0F0F001090DF001090E000109FE0001FA0EF001FA0F0000F0F
      0F0050BF2F005FC030000F0F0F000F0F0F00000000001FC0CF003F302F00BFA0
      80007F6F6000606060006060600060606000705F5000D0B08000DFBF8F00705F
      5000606060003030300010909F0000000000000000003F90F00080BFF0008FC0
      F0008FC0F0007FB0F00070B0F00070B0F0003F90F00000000000BF9F7F002020
      2000BF9F7F002020200000000000000000000000000000000000000000000F0F
      0F00A08F8F00CFC0C000A09090003F3F3F00BFBFC000C0C0C000C0C0C000C0CF
      CF00BFBFC000B0B0BF000000000000000000000000000F0F0F00D07F0F00DF80
      0F00E08F10000F0F0F00109FE000109FE0001FA0EF001FA0FF001FA0FF000F0F
      0F005FC0300060CF3F0060CF3F000F0F0F00000000001FC0CF003F302F00A080
      6F00E0C08F007F6F60008F7F6F00A0806F00E0C08F009F806F009F806F00BFA0
      7F007F706F003030300010909F000000000000000000409FF0009FC0F000A0CF
      F000A0CFF0008FC0F0007FB0F00070B0F000409FF00000000000202020002020
      2000202020002020200020202000202020000000000000000000000000000F0F
      0F00C0AFAF00D0CFCF00DFD0CF00BFAFA00050505000BFBFBF00BFBFBF00C0C0
      C000A0A0A000808080000F0F0F0000000000000000000F0F0F00DF801000E08F
      1000EF8F10000F0F0F001FA0EF001FAFEF0020B0F00020BFF0002FC0FF000F0F
      0F0060CF3F0060CF40006FCF40000F0F0F00000000001FC0CF00303030007F6F
      6000A0806F00B08F6F00E0C08F00CFAF7F008F705F00807060007F605F00BFA0
      7F00CFAF7F003F302F0010909F0000000000000000003F90F00080BFF0008FC0
      F0008FC0F0007FB0F00070B0F00070B0F0003F90F000000000007FCFBF002020
      20007FCFBF00202020007FCFBF00202020000000000000000000000000000F0F
      0F00C0AFAF00DFD0CF00D0CFCF002F302F00BFAFA0003F3F3F00AFB0B000B0B0
      B00090909F000F0F0F000000000000000000000000000F0F0F00EF8F1000EF8F
      1000F09F1F00F09F1F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F0060CF
      3F0060CF40006FCF40006FCF4F000F0F0F00000000001FC0CF00303030006060
      60008F7060007F6F60008F705F006F5F50006F605F0060606000606060008070
      60008F705F003F302F0010909F0000000000000000000F60C0001F80EF002F8F
      EF004F9FF0005FA0F0005FA0F0005FA0F0001F80EF00000000005FE0DF002020
      20005FE0DF00202020005FE0DF00202020000000000000000000000000000F0F
      0F00C0AFAF00D0CFCF002F302F00CFBFB000CFBFBF00A09090002F302F00A0A0
      A0000F0F0F00000000000000000000000000000000000F0F0F00EF8F1000F08F
      2000F08F2000F08F20000F0F0F00707F70006F6F6F00404F40000F0F0F0060CF
      40006FCF4F0070CF4F0070D050000F0F0F00000000001FC0CF00505050004040
      400040403F0040403F0040403F00404040004040400040404000404040004040
      3F0040403F005050500010909F00000000000000000000000000000000000000
      00000F70DF001F80EF002F8FEF002F8FEF000F6FD0000000000040F0F0002020
      200040F0F0002020200040F0F000202020000000000000000000000000000000
      00000F0F0F0050505000CFC0C000D0CFCF00CFBFBF00BFB0AF00907F7F000F0F
      0F000000000000000000000000000000000000000000000000000F0F0F000F0F
      0F000F0F0F000F0F0F000F0F0F00B0B0B0008F908F006F6F6F000F0F0F000F0F
      0F000F0F0F000F0F0F000F0F0F0000000000000000001FC0CF001FC0CF001FC0
      CF001FC0CF001FC0CF001FC0CF001FC0CF001FC0CF001FC0CF001FC0CF001FC0
      CF001FC0CF001FC0CF000F7F8000000000000000000000000000000000000000
      0000000000000F60C0000F70E0000F7FEF000F60C00000000000202020002020
      2000202020002020200020202000202020000000000000000000000000000000
      0000505050000F0F0F00CFBFBF00C0AFAF00B0A09F00A08F8F000F0F0F000000
      00000000000000000000000000000000000000000000000000000F0F0F00707F
      70006F6F6F00404F40000F0F0F00B0B0B000B0B0B000707F70000F0F0F00707F
      70006F6F6F00404F40000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F5FBF000F6FD0000F5FAF0000000000000000000000
      000000000000000000000000000000000000000000000F0F0F00000000000000
      00000F0F0F00000000000F0F0F000F0F0F000F0F0F000F0F0F00000000000000
      00000000000000000000000000000000000000000000000000000F0F0F00B0B0
      B0008F908F006F6F6F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F00B0B0
      B0008F908F006F6F6F000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000F5FAF00004F8F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0F0F00B0B0
      B000B0B0B000707F70000F0F0F000000000000000000000000000F0F0F00B0B0
      B000B0B0B000707F70000F0F0F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000F0F
      0F000F0F0F000F0F0F0000000000000000000000000000000000000000000F0F
      0F000F0F0F000F0F0F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000101F1000101F1000101F1000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007F7F7F0000000000000000007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000101F1000101F1000A0AFA0005F605F00707F7000101F1000101F
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000007F7F7F007F7F7F00BFBFBF007F7F7F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F0F0F000F0F0F00DFE0E000DFE0E000D0DFDF00D0DFDF00D0DFDF000F0F
      0F000F0F0F00000000000000000000000000000000000000000000000000101F
      1000101F1000A0AFA000A0AFA0003F3F3F00BFC0BF00707F7000707F7000707F
      7000101F10000000000000000000000000000000000000000000000000007F7F
      7F007F7F7F007F7F7F00BFBFBF00BFBFBF007F7F7F007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BFC0BF00BFC0BF008080
      80003F3F3F000000000000000000000000000000000000000000000000000F0F
      0F00E0EFEF00DFE0E000FFD08F00FFC06000FFC06000FFC06000FFD08F00D0DF
      DF00D0DFDF000F0F0F00000000000000000000000000101F1000101F1000A0AF
      A000A0AFA000505050005050500020202000BFC0BF00707F7000808080008080
      8000101F100000000000000000000000000000000000000000007F7F7F007F7F
      7F00BFBFBF00BFBFBF00FFFFFF00FFFFFF00FFFFFF00BFBFBF007F7F7F007F7F
      7F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BFC0BF00BFC0BF00808080008F908F008F90
      8F00808080003F3F3F00000000000000000000000000000000000F0F0F00E0EF
      EF00FFD08F00F0B03F00EFAF3000EFAF2F00EFA01F00EFA01000EF9F0F00FFC0
      6000FFD08F00D0DFDF000F0F0F0000000000101F100080808000A0AFA0005050
      5000505050003FB000003FB0000020202000BFC0BF00707F70008F908F008F90
      8F003F3F3F00101F10000000000000000000000000007F7F7F007F7F7F00BFBF
      BF00FFFFFF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF007F7F
      7F007F7F7F000000000000000000000000000000000000000000000000000000
      000000000000BFC0BF00BFC0BF00808080008F908F003F3F3F00202020008F90
      8F008F908F0080808000505050000000000000000000000000000F0F0F00EFEF
      EF00F0BF4F00F0B04000F0B03F00B07F000090600000B07F0000EFA01000EF9F
      0F00FFC06000D0DFDF000F0F0F0000000000101F1000A0AFA000505050003FB0
      00003FB000004F4F00004F4F000020202000BFC0BF00707F70008F908F008F90
      8F003F3F3F00707F7000101F100000000000000000007F7F7F00BFBFBF00FFFF
      FF00BFBFBF00BFBFBF00BFBFBF0000000000000000007F7F7F007F7F7F007F7F
      7F007F7F7F007F7F7F000000000000000000000000000000000000000000BFC0
      BF00BFC0BF00808080008F908F0050505000202020008F908F008F908F005050
      5000202020008F908F006F6F6F0030303000000000000F0F0F00F0F0F000FFD0
      8F00F0BF5F00F0BF4F00F0B04000FFFFFF00C0C0CF0090600000EFA01F00EFA0
      1000EF9F0F00FFD08F00D0DFDF000F0F0F00101F1000A0AFA000505050004F4F
      00004F4F00003FB000003FB0000020202000BFC0BF008F908F008F908F008F90
      8F003F3F3F005F605F00707F7000101F100000000000000000007F7F7F007F7F
      7F00BFBFBF00BFBFBF007F7F7F007F7F7F007F7F7F000000000000007F000000
      7F00BFBFBF00BFBFBF00000000000000000000000000BFC0BF00BFC0BF008080
      80008F908F0050505000202020008F908F008F908F0050505000202020007F7F
      7F008F908F006F6F6F000000000000000000000000000F0F0F00F0F0F000FFC0
      6000F0C06000F0BF5F00F0BF4F00FFFFFF00D0DFDF0090600000EFAF2F00EFA0
      1F00EFA01000FFC06000D0DFDF000F0F0F00101F1000A0AFA000505050003FB0
      00003FB000004F4F00004F4F000020202000BFC0BF008F908F008F908F008F90
      8F003F3F3F00707F70005F605F00101F10000000000000000000000000000000
      00007F7F7F007F7F7F007F7F7F007F7F7F007F7F7F00BFBFBF00BFBFBF007F7F
      7F000000000000000000000000000000000000000000808080008F908F005F60
      5F00202020008F908F008F908F0050505000202020007F7F7F008F908F005F5F
      5F0000000000000000000000000000000000000000000F0F0F00FFFFFF00FFC0
      6000FFC06F00F0C06000F0BF5F00FFFFFF00FFFFFF0090600000EFAF3000EFAF
      2F00EFA01F00FFC06000DFE0E0000F0F0F00101F1000A0AFA000505050004F4F
      00004F4F00003FB000003FB0000020202000BFC0BF00BFC0BF00A0AFA0008F90
      8F003F3F3F005F605F00707F7000101F10000000000000000000000000000000
      000000000000000000007F7F7F007F7F7F007F7F7F007F7F7F00000000000000
      00000000000000000000000000000000000000000000000000008F908F008F90
      8F008F908F005F605F0020202000808080008F908F005F5F5F00000000000000
      000000000000000000000000000000000000000000000F0F0F00FFFFFF00FFC0
      6000FFCF7000FFC06F00F0C06000F0BF5F00F0BF4F00F0B04000F0B03F00EFAF
      3000EFAF2F00FFC06000DFE0E0000F0F0F00101F1000A0AFA000505050003FB0
      00003FB000002020200020202000B0B0B000B0B0B000CFCFCF00BFC0BF009F9F
      9F003F3F3F00BFC0BF0080808000101F10000000000000000000000000000000
      000000000000000000007F7F7F00FFFFFF007F7F7F007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000008F90
      8F006F6F6F008F908F008F908F005F5F5F000000000000000000000000000000
      000000000000000000000000000000000000000000000F0F0F00FFFFFF00FFD0
      8F00FFCF7000FFCF7000FFC06F00B07F000090600000B07F0000F0B04000F0B0
      3F00EFAF3000FFD08F00DFE0E0000F0F0F00101F1000A0AFA000505050002020
      200020202000B0B0B000B0B0B000808080008F908F009F9F9F00A0AFA0003F3F
      3F005F605F00808080009F9F9F00101F10000000000000000000000000000000
      00000000000000000000BFBFBF00FFFFFF007F7F7F007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008F908F005F5F5F0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0F0F00FFFF
      FF00FFC06000FFCF7000FFCF7000FFFFFF00FFFFFF0090600000F0BF4F00F0B0
      4000FFC06000E0EFEF000F0F0F0000000000101F1000B0B0B0003F3F3F00B0B0
      B000B0B0B000808080008F908F009F9F9F009F9F9F003F3F3F003F3F3F005F60
      5F008F908F00101F1000101F1000000000000000000000000000000000000000
      00000000000000000000BFBFBF00FFFFFF007F7F7F007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000001F1F000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0F0F00FFFF
      FF00FFD08F00FFC06000FFCF7000FFCF7000FFC06F00F0C06000F0BF5F00FFC0
      6000FFD08F00E0EFEF000F0F0F0000000000101F10009F9F9F00A0AFA0008080
      80008F908F009F9F9F009F9F9F00101F10003F3F3F005F605F008F908F00101F
      1000101F10000000000000000000000000000000000000000000000000000000
      00000000000000000000BFBFBF00FFFFFF007F7F7F007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000F0F
      0F00FFFFFF00FFFFFF00FFD08F00FFC06000FFC06000FFC06000FFD08F00F0F0
      F000F0F0F0000F0F0F000000000000000000101F10006F6F6F00808080008F90
      8F008F908F00101F1000101F100000000000101F1000101F1000101F10000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BFBFBF00FFFFFF007F7F7F007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F0F0F000F0F0F00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000F0F
      0F000F0F0F0000000000000000000000000000000000101F1000101F1000101F
      1000101F10000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007F7F7F007F7F7F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFF00000000FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFF
      FC1FFFFFFFFFFFFFFC1FFFFFFFFFFFFFFC1F0000FE220000F80F000026230000
      F80F000001000000F80FFFFFFFFFFFFFF80F000011010000F80F000029000000
      FC1F000004000000FC1F73823CF07382FC1F0000296E0EA0FC1F000000000000
      FE3F000000000000FFFF000000000000FFFFFFFFFC1FFFFFFE3FFFC3FC1FFFFF
      FC3FFF01C001FFFFF83FFE00C0018001F003FC00C00100008003F800C0010000
      0003F000800000000003E001800000000000E001800000000000E00380000000
      0000E007800000008000F00FC0010000F000F01FC0018001F83FB43FC001FFFF
      FC3F4FFFC1C1FFFFFE3FFFFFE3E3FFFFFE3FFC3FFFFFFC1FF80FF81FFF87F007
      E007E00FFE03E0038007C003F801C00100038001E000C0010001800180008000
      0000C003000080000000F00F000380000000FC3F800F80000000FC3FC03F8000
      0000FC3FE07FC0010001FC3FF3BFC0010007FC3FFF7FE003011FFC3FFF7FF007
      87FFFE7FFFFFFC1FFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object DXSound: TDXSound
    AutoInitialize = False
    Options = []
    OnInitialize = DXSoundInitialize
    Left = 35
    Top = 331
  end
  object DXInput: TDXInput
    ActiveOnly = True
    Joystick.BindInputStates = True
    Joystick.Effects.Effects = {
      FF0A0044454C50484958464F524345464545444241434B454646454354003010
      7D000000545046301D54466F726365466565646261636B456666656374436F6D
      706F6E656E740007456666656374730E01044E616D650607456666656374730A
      45666665637454797065070665744E6F6E6506506572696F64023205506F7765
      720310270454696D6503E8030E537461727444656C617954696D650200000000
      00}
    Joystick.Enabled = True
    Joystick.ForceFeedback = False
    Joystick.AutoCenter = True
    Joystick.DeadZoneX = 50
    Joystick.DeadZoneY = 50
    Joystick.DeadZoneZ = 50
    Joystick.ID = 0
    Joystick.RangeX = 1000
    Joystick.RangeY = 1000
    Joystick.RangeZ = 1000
    Keyboard.BindInputStates = True
    Keyboard.Effects.Effects = {
      FF0A0044454C50484958464F524345464545444241434B454646454354003010
      7D000000545046301D54466F726365466565646261636B456666656374436F6D
      706F6E656E740007456666656374730E01044E616D650607456666656374730A
      45666665637454797065070665744E6F6E6506506572696F64023205506F7765
      720310270454696D6503E8030E537461727444656C617954696D650200000000
      00}
    Keyboard.Enabled = True
    Keyboard.ForceFeedback = False
    Keyboard.Assigns = {
      4B00000026000000680000004A00000028000000620000004800000025000000
      640000004C00000027000000660000002000000060000000000000000D000000
      6500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000071000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000}
    Mouse.BindInputStates = False
    Mouse.Effects.Effects = {
      FF0A0044454C50484958464F524345464545444241434B454646454354003010
      7D000000545046301D54466F726365466565646261636B456666656374436F6D
      706F6E656E740007456666656374730E01044E616D650607456666656374730A
      45666665637454797065070665744E6F6E6506506572696F64023205506F7765
      720310270454696D6503E8030E537461727444656C617954696D650200000000
      00}
    Mouse.Enabled = False
    Mouse.ForceFeedback = False
    UseDirectInput = True
    Left = 134
    Top = 331
  end
  object InputTestTimer: TTimer
    Interval = 0
    Left = 167
    Top = 331
  end
  object DXPlay: TDXPlay
    Async = False
    GUID = '{BDB76A61-D08C-11D4-BB05-00105A229A26}'
    MaxPlayers = 8
    ModemSetting.Enabled = False
    TCPIPSetting.Enabled = False
    TCPIPSetting.Port = 0
    OnMessage = DXPlayMessage
    Left = 101
    Top = 331
  end
  object DXFont: TDXFont
    Font = 'font'
    FontIndex = 1
    DXImageList = DXImageList
    Left = 200
    Top = 331
  end
end
