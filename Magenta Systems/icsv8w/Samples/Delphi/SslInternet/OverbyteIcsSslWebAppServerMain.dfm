object WebAppSrvForm: TWebAppSrvForm
  Left = 66
  Top = 425
  Caption = 'Overbyte ICS SSL Web Application Server Demo'
  ClientHeight = 387
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 539
    Height = 173
    Align = alTop
    TabOrder = 0
    object Label4: TLabel
      Left = 39
      Top = 55
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label11: TLabel
      Left = 264
      Top = 55
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label10: TLabel
      Left = 256
      Top = 79
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label7: TLabel
      Left = 20
      Top = 103
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label6: TLabel
      Left = 23
      Top = 79
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label8: TLabel
      Left = 24
      Top = 120
      Width = 54
      Height = 13
      Caption = 'Acceptable'
    end
    object Label9: TLabel
      Left = 52
      Top = 132
      Width = 25
      Height = 13
      Caption = 'hosts'
    end
    object Label19: TLabel
      Left = 256
      Top = 103
      Width = 35
      Height = 13
      Caption = 'DH File'
    end
    object CertFileEdit: TEdit
      Left = 80
      Top = 52
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 300
      Top = 52
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 300
      Top = 76
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 80
      Top = 76
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 80
      Top = 100
      Width = 153
      Height = 21
      TabOrder = 4
      Text = 'PassPhraseEdit'
    end
    object AcceptableHostsEdit: TEdit
      Left = 80
      Top = 124
      Width = 373
      Height = 21
      TabOrder = 5
      Text = 'AcceptableHostsEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 22
      Top = 149
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 6
    end
    object DhParamFileEdit: TEdit
      Left = 300
      Top = 100
      Width = 153
      Height = 21
      Hint = 'Enter DH Parameter File (can be empty).'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'DhParamFileEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 173
    Width = 539
    Height = 214
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
  object HousekeepingTimer: TTimer
    Enabled = False
    OnTimer = HousekeepingTimerTimer
    Left = 220
    Top = 228
  end
  object HttpAppSrv1: TSslHttpAppSrv
    ListenBacklog = 5
    MultiListenSockets = <>
    Port = '443'
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    MaxClients = 0
    DocDir = 'c:\wwwroot'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 0
    Options = []
    KeepAliveTimeSec = 10
    KeepAliveTimeXferSec = 300
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 5000
    SizeCompressMax = 5000000
    MaxBlkSize = 8192
    BandwidthLimit = 0
    BandwidthSampling = 1000
    ServerHeader = 'Server: ICS-HttpServer-8.08'
    OnServerStarted = HttpAppSrv1ServerStarted
    OnServerStopped = HttpAppSrv1ServerStopped
    OnClientConnect = HttpAppSrv1ClientConnect
    OnGetDocument = HttpAppSrv1GetDocument
    OnBeforeProcessRequest = HttpAppSrv1BeforeProcessRequest
    OnAfterAnswer = HttpAppSrv1AfterAnswer
    AuthTypes = []
    AuthRealm = 'ics'
    OnBgException = HttpAppSrv1BgException
    SocketErrs = wsErrTech
    ExclusiveAddr = True
    SessionTimeout = 300
    MaxSessions = 100
    OnDeleteSession = HttpAppSrv1DeleteSession
    OnVirtualException = HttpAppSrv1VirtualException
    SslEnable = True
    SslContext = SslContext1
    IcsHosts = <>
    SslCliCertMethod = sslCliCertNone
    SslCertAutoOrder = False
    CertExpireDays = 30
    OnSslHandshakeDone = HttpAppSrv1SslHandshakeDone
    Left = 44
    Top = 228
  end
  object SslContext1: TSslContext
    SslDHParamLines.Strings = (
      '-----BEGIN DH PARAMETERS-----'
      'MIICCAKCAgEA45KZVdTCptcakXZb7jJvSuuOdMlUbl1tpncHbQcYbFhRbcFmmefp'
      'bOmZsTowlWHQpoYRRTe6NEvYox8J+44i/X5cJkMTlIgMb0ZBty7t76U9f6qAId/O'
      '6elE0gnk2ThER9nmBcUA0ZKgSXn0XCBu6j5lzZ0FS+bx9OVNhlzvIFBclRPXbI58'
      '71dRoTjOjfO1SIzV69T3FoKJcqur58l8b+no/TOQzekMzz4XJTRDefqvePhj7ULP'
      'Z/Zg7vtEh11h8gHR0/rlF378S05nRMq5hbbJeLxIbj9kxQunETSbwwy9qx0SyQgH'
      'g+90+iUCrKCJ9Fb7WKqtQLkQuzJIkkXkXUyuxUuyBOeeP9XBUAOQu+eYnRPYSmTH'
      'GkhyRbIRTPCDiBWDFOskdyGYYDrxiK7LYJQanqHlEFtjDv9t1XmyzDm0k7W9oP/J'
      'p0ox1+WIpFgkfv6nvihqCPHtAP5wevqXNIQADhDk5EyrR3XWRFaySeKcmREM9tbc'
      'bOvmsEp5MWCC81ZsnaPAcVpO66aOPojNiYQZUbmm70fJsr8BDzXGpcQ44+wmL4Ds'
      'k3+ldVWAXEXs9s1vfl4nLNXefYl74cV8E5Mtki9hCjUrUQ4dzbmNA5fg1CyQM/v7'
      'JuP6PBYFK7baFDjG1F5YJiO0uHo8sQx+SWdJnGsq8piI3w0ON9JhUvMCAQI='
      '-----END DH PARAMETERS-----')
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslCheckHostFlags = []
    SslSecLevel = sslSecLevel80bits
    SslOptions = [sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_MICROSOFT_BIG_SSLV3_BUFFER, sslOpt_SSLEAY_080_CLIENT_DH_BUG, sslOpt_TLS_D5_BUG, sslOpt_TLS_BLOCK_PADDING_BUG, sslOpt_TLS_ROLLBACK_BUG, sslOpt_SINGLE_DH_USE, sslOpt_NO_SSLv2, sslOpt_NO_SSLv3, sslOpt_NETSCAPE_CA_DN_BUG, sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG]
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_SERVER, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslBestVer_SERVER
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDH_P256
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslSessionTimeout = 300
    SslSessionCacheSize = 20480
    SslDefaultSessionIDContext = 'Webservertest'
    AutoEnableBuiltinEngines = False
    Left = 134
    Top = 229
  end
end
