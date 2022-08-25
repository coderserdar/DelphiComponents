object SmtpSslSrvForm: TSmtpSslSrvForm
  Left = 192
  Top = 107
  Caption = 'Test SSL SMTP Server - http://www.overbyte.be'
  ClientHeight = 621
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPanel: TPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 161
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 35
      Width = 57
      Height = 13
      Caption = 'DNS Server'
    end
    object Label2: TLabel
      Left = 233
      Top = 38
      Width = 125
      Height = 13
      Caption = 'Email Accounts Accepted:'
    end
    object Label3: TLabel
      Left = 10
      Top = 10
      Width = 72
      Height = 13
      Caption = 'Spool Directory'
    end
    object Label4: TLabel
      Left = 369
      Top = 38
      Width = 334
      Height = 13
      Caption = 
        'Alias Email Accounts (alias=account, *@domain=account for catch-' +
        'all):'
    end
    object PrefDnsServer: TEdit
      Left = 98
      Top = 30
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '8.8.8.8'
    end
    object PrefEmailAccs: TMemo
      Left = 173
      Top = 57
      Width = 203
      Height = 94
      Lines.Strings = (
        'PrefEmailAccs')
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
    object PrefSpoolDir: TEdit
      Left = 98
      Top = 5
      Width = 248
      Height = 21
      TabOrder = 2
      Text = 'c:\mailspool\'
    end
    object PrefAddRecvHdrs: TCheckBox
      Left = 10
      Top = 60
      Width = 131
      Height = 17
      Caption = 'Add Received Header'
      TabOrder = 3
    end
    object PrefAddEnvHdrs: TCheckBox
      Left = 10
      Top = 80
      Width = 141
      Height = 17
      Caption = 'Add Envelope Headers'
      TabOrder = 4
    end
    object doStart: TButton
      Left = 369
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Start Server'
      TabOrder = 5
      OnClick = doStartClick
    end
    object doStop: TButton
      Left = 460
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Stop Server'
      Enabled = False
      TabOrder = 6
      OnClick = doStopClick
    end
    object doExit: TButton
      Left = 550
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 7
      OnClick = doExitClick
    end
    object PrefAllowRelay: TCheckBox
      Left = 10
      Top = 120
      Width = 131
      Height = 17
      Caption = 'Allow Relaying'
      TabOrder = 8
    end
    object PrefAliasAccs: TMemo
      Left = 388
      Top = 57
      Width = 298
      Height = 94
      Lines.Strings = (
        'PrefAliasAccs')
      ScrollBars = ssVertical
      TabOrder = 9
      WordWrap = False
    end
    object PrefAddReplayHdrs: TCheckBox
      Left = 10
      Top = 100
      Width = 141
      Height = 17
      Caption = 'Add Replay Headers'
      TabOrder = 10
    end
    object PrefAuthTls: TCheckBox
      Left = 10
      Top = 140
      Width = 146
      Height = 17
      Caption = 'Auth Requires TLS/SSL'
      TabOrder = 11
    end
  end
  object Log: TMemo
    Left = 0
    Top = 241
    Width = 707
    Height = 380
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Log')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 161
    Width = 707
    Height = 80
    Align = alTop
    TabOrder = 2
    object Label5: TLabel
      Left = 39
      Top = 10
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label11: TLabel
      Left = 264
      Top = 10
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label10: TLabel
      Left = 256
      Top = 35
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label7: TLabel
      Left = 20
      Top = 60
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label6: TLabel
      Left = 23
      Top = 35
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label8: TLabel
      Left = 466
      Top = 10
      Width = 35
      Height = 13
      Caption = 'IP Addr'
    end
    object CertFileEdit: TEdit
      Left = 80
      Top = 5
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 300
      Top = 5
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 300
      Top = 30
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 80
      Top = 30
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 80
      Top = 55
      Width = 153
      Height = 21
      TabOrder = 4
      Text = 'PassPhraseEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 300
      Top = 57
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 6
    end
    object ServIpAddr: TEdit
      Left = 524
      Top = 6
      Width = 153
      Height = 21
      TabOrder = 5
      Text = '0.0.0.0'
    end
  end
  object SmtpServer1: TSslSmtpServer
    MultiListenSockets = <
      item
        Addr = '0.0.0.0'
        ListenBacklog = 15
        Port = '587'
        SslEnable = False
      end>
    Addr = '0.0.0.0'
    Port = 'smtp'
    SocketFamily = sfAnyIPv4
    ServerHost = 'pc19-web'
    ServerDesc = 'SMTP Server (c) 1997-2016 Francois Piette V8.37'
    MaxClients = 0
    MultiThreaded = False
    MaxMessageSize = 0
    ClientTimeout = 60
    GreyDelaySecs = 0
    Options = [smtpsAddIpAddrHdr]
    SocketErrs = wsErrFriendly
    ExclusiveAddr = True
    OnException = SmtpServer1Exception
    OnServerStarted = SmtpServer1ServerStarted
    OnServerStopped = SmtpServer1ServerStopped
    OnConnect = SmtpServer1Connect
    OnDisconnect = SmtpServer1Disconnect
    OnCommand = SmtpServer1Command
    OnResponse = SmtpServer1Response
    OnMailFrom = SmtpServer1MailFrom
    OnRcptTo = SmtpServer1RcptTo
    OnAuth = SmtpServer1Auth
    OnAuthPW = SmtpServer1AuthPW
    OnDataStart = SmtpServer1DataStart
    OnDataEnd = SmtpServer1DataEnd
    SslContext = SslContext1
    OnSslVerifyPeer = SmtpServer1SslVerifyPeer
    OnSslHandshakeDone = SmtpServer1SslHandshakeDone
    Left = 35
    Top = 325
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
    SslOptions = [sslOpt_NO_SSLv2, sslOpt_NO_SSLv3]
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 85
    Top = 325
  end
end
