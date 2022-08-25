object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Magenta GPS and Location Test Tools - 2nd February 2022'
  ClientHeight = 616
  ClientWidth = 1120
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Log: TMemo
    Left = 0
    Top = 0
    Width = 689
    Height = 401
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object PaneRight: TPanel
    Left = 689
    Top = 0
    Width = 431
    Height = 616
    TabOrder = 1
    object LabelPosition: TLabel
      Left = 15
      Top = 432
      Width = 396
      Height = 37
      AutoSize = False
      Caption = 'LabelPosition'
      Color = clYellow
      ParentColor = False
      Transparent = False
      WordWrap = True
    end
    object BoxLocation: TGroupBox
      Left = 142
      Top = 247
      Width = 131
      Height = 176
      Caption = 'Win Location Sensor'
      TabOrder = 0
      object doLocation: TButton
        Left = 10
        Top = 77
        Width = 101
        Height = 25
        Caption = 'Get Location'
        TabOrder = 2
        OnClick = doLocationClick
      end
      object doLocStart: TButton
        Left = 10
        Top = 46
        Width = 101
        Height = 25
        Caption = 'Location Start'
        TabOrder = 1
        OnClick = doLocStartClick
      end
      object doLocEnd: TButton
        Left = 10
        Top = 108
        Width = 101
        Height = 25
        Caption = 'Location End'
        TabOrder = 3
        OnClick = doLocEndClick
      end
      object SetLocEvents: TCheckBox
        Left = 10
        Top = 22
        Width = 118
        Height = 17
        Caption = 'Set Location Events'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object doLatLong: TButton
        Left = 10
        Top = 140
        Width = 101
        Height = 25
        Caption = 'Get LatLong'
        TabOrder = 4
        OnClick = doLatLongClick
      end
    end
    object BoxNmea: TGroupBox
      Left = 5
      Top = 5
      Width = 131
      Height = 296
      Caption = 'NMEA Sensor'
      TabOrder = 1
      object LabelNmea: TLabel
        Left = 10
        Top = 260
        Width = 111
        Height = 28
        AutoSize = False
        Caption = 'Nmea Sentances:'
        WordWrap = True
      end
      object doNMEACom: TButton
        Left = 10
        Top = 23
        Width = 101
        Height = 25
        Caption = 'NMEA Com Port'
        TabOrder = 0
        OnClick = doNMEAComClick
      end
      object doNmeaTCP: TButton
        Left = 10
        Top = 81
        Width = 101
        Height = 25
        Caption = 'NMEA TCP Client'
        TabOrder = 2
        OnClick = doNmeaTCPClick
      end
      object doNmeaStop: TButton
        Left = 10
        Top = 229
        Width = 101
        Height = 25
        Caption = 'NMEA Stop'
        TabOrder = 7
        OnClick = doNmeaStopClick
      end
      object NmeaComPort: TEdit
        Left = 10
        Top = 54
        Width = 76
        Height = 21
        TabOrder = 1
        Text = 'COM1'
      end
      object NmeaIpAddr: TEdit
        Left = 10
        Top = 174
        Width = 111
        Height = 21
        TabOrder = 5
        Text = '192.168.1.213'
      end
      object NmeaIpPort: TSpinEdit
        Left = 10
        Top = 201
        Width = 71
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 7777
      end
      object doNmeaTCPSrv: TButton
        Left = 10
        Top = 112
        Width = 101
        Height = 25
        Caption = 'NMEA TCP Server'
        TabOrder = 3
        OnClick = doNmeaTCPSrvClick
      end
      object doNmeaUDP: TButton
        Left = 10
        Top = 143
        Width = 101
        Height = 25
        Caption = 'NMEA UDP Server'
        TabOrder = 4
        OnClick = doNmeaUDPClick
      end
    end
    object BoxGT02: TGroupBox
      Left = 142
      Top = 5
      Width = 136
      Height = 236
      Caption = 'GT02 Sensor'
      TabOrder = 2
      object LabelGt02Packets: TLabel
        Left = 10
        Top = 200
        Width = 116
        Height = 33
        AutoSize = False
        Caption = 'GT02 Packets:'
        WordWrap = True
      end
      object doGT02ServStart: TButton
        Left = 10
        Top = 21
        Width = 101
        Height = 25
        Caption = 'GT02 Server Start'
        TabOrder = 0
        OnClick = doGT02ServStartClick
      end
      object Gt02ServIP: TEdit
        Left = 10
        Top = 52
        Width = 111
        Height = 21
        TabOrder = 1
        Text = '192.168.1.73'
      end
      object Gt02ServPort: TSpinEdit
        Left = 10
        Top = 79
        Width = 71
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 2222
      end
      object doGt02ServStop: TButton
        Left = 11
        Top = 111
        Width = 101
        Height = 25
        Caption = 'GT02 Server Stop'
        TabOrder = 3
        OnClick = doGt02ServStopClick
      end
      object Gt02Cmd: TEdit
        Left = 10
        Top = 142
        Width = 111
        Height = 21
        TabOrder = 4
        Text = 'WHERE,666666#'
      end
      object doGT02ServCmd: TButton
        Left = 11
        Top = 166
        Width = 101
        Height = 25
        Caption = 'GT02 Command'
        TabOrder = 5
        OnClick = doGT02ServCmdClick
      end
    end
    object ButtonBox: TPanel
      Left = 7
      Top = 475
      Width = 276
      Height = 136
      TabOrder = 3
      object Label1: TLabel
        Left = 10
        Top = 35
        Width = 48
        Height = 13
        Caption = 'Min (secs)'
      end
      object Label2: TLabel
        Left = 140
        Top = 35
        Width = 52
        Height = 13
        Caption = 'Max (secs)'
      end
      object doClose: TButton
        Left = 145
        Top = 105
        Width = 101
        Height = 25
        Caption = 'Close'
        TabOrder = 3
        OnClick = doCloseClick
      end
      object doGetOS: TButton
        Left = 10
        Top = 105
        Width = 101
        Height = 25
        Caption = 'Get OS'
        TabOrder = 2
        OnClick = doGetOSClick
      end
      object doMap: TButton
        Left = 10
        Top = 70
        Width = 101
        Height = 25
        Caption = 'Map'
        TabOrder = 1
        OnClick = doMapClick
      end
      object RawData: TCheckBox
        Left = 10
        Top = 10
        Width = 97
        Height = 17
        Caption = 'Show Raw Data'
        TabOrder = 0
      end
      object doSensorInfo: TButton
        Left = 145
        Top = 70
        Width = 101
        Height = 25
        Caption = 'Sensor Infomation'
        TabOrder = 4
        OnClick = doSensorInfoClick
      end
      object MinInterval: TSpinEdit
        Left = 75
        Top = 30
        Width = 51
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
      object MaxInterval: TSpinEdit
        Left = 205
        Top = 30
        Width = 51
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 0
      end
    end
    object BoxTk102: TGroupBox
      Left = 285
      Top = 5
      Width = 136
      Height = 236
      Caption = 'TK102/103 Sensor'
      TabOrder = 4
      object LabelTk102: TLabel
        Left = 10
        Top = 200
        Width = 116
        Height = 33
        AutoSize = False
        Caption = 'TK102 Packets:'
        WordWrap = True
      end
      object doTK102Start: TButton
        Left = 10
        Top = 21
        Width = 101
        Height = 25
        Caption = 'TK102 Server Start'
        TabOrder = 0
        OnClick = doTK102StartClick
      end
      object Tk102IpAddr: TEdit
        Left = 10
        Top = 52
        Width = 111
        Height = 21
        TabOrder = 1
        Text = '192.168.1.124'
      end
      object Tk102IpPort: TSpinEdit
        Left = 10
        Top = 79
        Width = 71
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 1770
      end
      object doTK102Stop: TButton
        Left = 11
        Top = 107
        Width = 101
        Height = 25
        Caption = 'TK102 Server Stop'
        TabOrder = 3
        OnClick = doTK102StopClick
      end
    end
    object BoxWondex: TGroupBox
      Left = 285
      Top = 247
      Width = 136
      Height = 176
      Caption = 'WondeX Sensor'
      TabOrder = 5
      object LabelWondeX: TLabel
        Left = 10
        Top = 142
        Width = 123
        Height = 28
        AutoSize = False
        Caption = 'WondeX Packets:'
        WordWrap = True
      end
      object doWondeXStart: TButton
        Left = 10
        Top = 21
        Width = 111
        Height = 25
        Caption = 'WondeX Server Start'
        TabOrder = 0
        OnClick = doWondeXStartClick
      end
      object WondeXIpAddr: TEdit
        Left = 10
        Top = 52
        Width = 111
        Height = 21
        TabOrder = 1
        Text = '192.168.1.124'
      end
      object WondeXIpPort: TSpinEdit
        Left = 10
        Top = 79
        Width = 71
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 1770
      end
      object doWondeXStop: TButton
        Left = 11
        Top = 107
        Width = 110
        Height = 25
        Caption = 'WondeX Server Stop'
        TabOrder = 3
        OnClick = doWondeXStopClick
      end
    end
  end
  object CaptureData: TMemo
    Left = 0
    Top = 407
    Width = 688
    Height = 209
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object ApdComPort: TApdComPort
    ComNumber = 999
    Baud = 4800
    PromptForPort = False
    AutoOpen = False
    DTR = False
    TraceName = 'APRO.TRC'
    LogName = 'APRO.LOG'
    Left = 265
    Top = 290
  end
  object ApdDataPacket: TApdDataPacket
    Enabled = True
    EndCond = [ecString, ecPacketSize]
    StartString = '$'
    EndString = #13
    ComPort = ApdComPort
    PacketSize = 255
    OnStringPacket = ApdDataPacketStringPacket
    Left = 220
    Top = 290
  end
  object MagIpLog: TIcsIpStrmLog
    MaxSockets = 1
    RemoteIpPort = '514'
    SocFamily = sfIPv4
    LocalIpAddr = '0.0.0.0'
    LocalIpPort = '0'
    SrvIcsHosts = <
      item
        HostNames.Strings = (
          '*')
        HostEnabled = True
        BindIpAddr = '0.0.0.0'
        BindSslPort = 0
        BindNonPort = 7777
        HostTag = 'HostGPS'
        Descr = 'GPS Server'
        ForwardProxy = False
        WebLogIdx = 0
        SslSrvSecurity = sslSrvSecBack
        AuthSslCmd = False
        AuthForceSsl = False
        WebRedirectStat = 0
        CertSupplierProto = SuppProtoNone
        CertChallenge = ChallNone
        CertPKeyType = PrivKeyRsa1024
        CertSignDigest = Digest_md5
      end>
    ForceSsl = False
    PingWaitSecs = 5
    CheckPing = False
    RetryAttempts = 0
    RetryWaitSecs = 10
    RetryNoImm = False
    AutoReconnect = True
    LogProtocol = logprotUdpClient
    KeepAliveSecs = 120
    UdpNoCRLF = False
    AddCRLF = True
    LineEndType = lineendLF
    CustomLineEnd = '$03'
    MaxLineLen = 132
    StripControls = True
    RawData = False
    MaxSendBuffer = 65536
    SndBufSize = 65536
    RcvBufSize = 65536
    SrvTimeoutSecs = 0
    LogSslCliSecurity = sslCliSecTls11
    SslSessCache = True
    LogSslVerMethod = logSslVerNone
    LogSslRevocation = False
    LogSslReportChain = False
    LogSslRootFile = 'RootCaCertsBundle.pem'
    SrvCertAutoOrder = False
    CertExpireDays = 30
    onLogRecvEvent = MagIpLogRecvEvent
    onLogProgEvent = MagIpLogLogProgEvent
    Left = 170
    Top = 290
  end
end
