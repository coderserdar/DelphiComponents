object SimpleSslServerForm: TSimpleSslServerForm
  Left = 339
  Top = 282
  Caption = 'Simple Ssl Server - http://www.overbyte.be'
  ClientHeight = 296
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 456
    Height = 157
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 44
      Top = 84
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label3: TLabel
      Left = 29
      Top = 11
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label7: TLabel
      Left = 250
      Top = 12
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label6: TLabel
      Left = 13
      Top = 35
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label4: TLabel
      Left = 8
      Top = 59
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label2: TLabel
      Left = 244
      Top = 36
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label9: TLabel
      Left = 40
      Top = 112
      Width = 25
      Height = 13
      Caption = 'hosts'
    end
    object Label8: TLabel
      Left = 12
      Top = 100
      Width = 54
      Height = 13
      Caption = 'Acceptable'
    end
    object StartButton: TButton
      Left = 4
      Top = 128
      Width = 57
      Height = 21
      Caption = '&Start'
      Default = True
      TabOrder = 4
      OnClick = StartButtonClick
    end
    object PortEdit: TEdit
      Left = 68
      Top = 80
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'PortEdit'
    end
    object CertFileEdit: TEdit
      Left = 68
      Top = 8
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 288
      Top = 8
      Width = 153
      Height = 21
      TabOrder = 5
      Text = 'CAFileEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 68
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 68
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'PassPhraseEdit'
    end
    object CAPathEdit: TEdit
      Left = 288
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 6
      Text = 'CAPathEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 230
      Top = 60
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 7
    end
    object StopButton: TButton
      Left = 68
      Top = 128
      Width = 57
      Height = 21
      Caption = 'S&top'
      TabOrder = 8
      OnClick = StopButtonClick
    end
    object AcceptableHostsEdit: TEdit
      Left = 68
      Top = 104
      Width = 373
      Height = 21
      TabOrder = 9
      Text = 'AcceptableHostsEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 157
    Width = 456
    Height = 139
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object SslWSocketServer1: TSslWSocketServer
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Addr = '0.0.0.0'
    Port = '443'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 1
    ReqVerHigh = 1
    Banner = 'Welcome to TcpSrv'
    BannerTooBusy = 'Sorry, too many clients'
    MaxClients = 0
    OnClientConnect = SslWSocketServer1ClientConnect
    SslContext = SslContext1
    SslEnable = False
    OnSslVerifyPeer = ClientVerifyPeer
    Left = 20
    Top = 184
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_SERVER]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslSessionTimeout = 300
    SslSessionCacheSize = 20480
    SslDefaultSessionIDContext = 'dfhgdfg'
    Left = 50
    Top = 184
  end
end
