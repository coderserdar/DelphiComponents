object HttpsTstForm: THttpsTstForm
  Left = 244
  Top = 170
  Caption = 'HTTPS TEST'
  ClientHeight = 387
  ClientWidth = 649
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
  object Splitter1: TSplitter
    Left = 0
    Top = 295
    Width = 649
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 288
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 199
    Width = 649
    Height = 96
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
    TabOrder = 0
  end
  object DocumentMemo: TMemo
    Left = 0
    Top = 298
    Width = 649
    Height = 89
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DocumentMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 649
    Height = 199
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 40
      Top = 7
      Width = 13
      Height = 13
      Caption = 'Url'
    end
    object Label8: TLabel
      Left = 449
      Top = 8
      Width = 62
      Height = 13
      Caption = 'Socks server'
    end
    object Label9: TLabel
      Left = 459
      Top = 32
      Width = 52
      Height = 13
      Caption = 'Socks Port'
    end
    object Label2: TLabel
      Left = 13
      Top = 31
      Width = 49
      Height = 13
      Caption = 'Document'
    end
    object Label3: TLabel
      Left = 27
      Top = 55
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label7: TLabel
      Left = 238
      Top = 56
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label10: TLabel
      Left = 452
      Top = 56
      Width = 59
      Height = 13
      Caption = 'Socks Level'
    end
    object Label6: TLabel
      Left = 11
      Top = 79
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label4: TLabel
      Left = 6
      Top = 103
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label12: TLabel
      Left = 12
      Top = 120
      Width = 54
      Height = 13
      Caption = 'Acceptable'
    end
    object Label13: TLabel
      Left = 40
      Top = 132
      Width = 25
      Height = 13
      Caption = 'hosts'
    end
    object Label5: TLabel
      Left = 484
      Top = 103
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object Label11: TLabel
      Left = 484
      Top = 122
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object Label14: TLabel
      Left = 484
      Top = 134
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label15: TLabel
      Left = 476
      Top = 74
      Width = 20
      Height = 13
      Caption = 'Http'
    end
    object Label16: TLabel
      Left = 476
      Top = 86
      Width = 35
      Height = 13
      Caption = 'Version'
    end
    object Label17: TLabel
      Left = 236
      Top = 80
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label18: TLabel
      Left = 7
      Top = 151
      Width = 124
      Height = 13
      Caption = 'Modified Since Date/Time'
    end
    object UrlEdit: TEdit
      Left = 68
      Top = 4
      Width = 373
      Height = 21
      Hint = 'Enter the hostname or IP address of the host to connect to'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'UrlEdit'
    end
    object SocksServerEdit: TEdit
      Left = 516
      Top = 4
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'SocksServerEdit'
    end
    object SocksPortEdit: TEdit
      Left = 516
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'SocksPortEdit'
    end
    object DocEdit: TEdit
      Left = 68
      Top = 28
      Width = 373
      Height = 21
      Hint = 'Enter the document name to send to connected host.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'DocEdit'
    end
    object CertFileEdit: TEdit
      Left = 68
      Top = 52
      Width = 165
      Height = 21
      Hint = 'Enter the certificate file name. PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 276
      Top = 52
      Width = 165
      Height = 21
      Hint = 'Enter the CA certificate file name.  PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'CAFileEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 238
      Top = 104
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 6
    end
    object CAPathEdit: TEdit
      Left = 276
      Top = 76
      Width = 165
      Height = 21
      Hint = 'Enter CA certicate directory (can be empty).'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 68
      Top = 76
      Width = 165
      Height = 21
      Hint = 
        'Enter the private file name. Could be the same as CertFile if th' +
        'is file contains both certificate and private key.  PEM file for' +
        'mat.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 68
      Top = 100
      Width = 165
      Height = 21
      Hint = 'Enter pass phrase protecting private key file.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      Text = 'PassPhraseEdit'
    end
    object AcceptableHostsEdit: TEdit
      Left = 68
      Top = 124
      Width = 373
      Height = 21
      TabOrder = 10
      Text = 'AcceptableHostsEdit'
    end
    object SocksLevelComboBox: TComboBox
      Left = 516
      Top = 52
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
      Items.Strings = (
        '5'
        '4A'
        '4')
    end
    object GetButton: TButton
      Left = 20
      Top = 172
      Width = 69
      Height = 21
      Hint = 'Connect to the host using the port.'
      Caption = '&Get'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = GetButtonClick
    end
    object ClearButton: TButton
      Left = 164
      Top = 172
      Width = 69
      Height = 21
      Hint = 'Clear display.'
      Caption = 'C&lear'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = ClearButtonClick
    end
    object CloseButton: TButton
      Left = 308
      Top = 172
      Width = 69
      Height = 21
      Hint = 'Close the connected or listening socket.'
      Caption = 'Cl&ose'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = CloseButtonClick
    end
    object ProxyHostEdit: TEdit
      Left = 516
      Top = 100
      Width = 121
      Height = 21
      TabOrder = 15
      Text = 'ProxyHostEdit'
    end
    object ProxyPortEdit: TEdit
      Left = 516
      Top = 124
      Width = 121
      Height = 21
      TabOrder = 16
      Text = 'ProxyPortEdit'
    end
    object HttpVersionComboBox: TComboBox
      Left = 516
      Top = 76
      Width = 85
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 17
      Items.Strings = (
        'HTTP/1.0'
        'HTTP/1.1')
    end
    object SessCacheCheckBox: TCheckBox
      Left = 320
      Top = 104
      Width = 121
      Height = 17
      Alignment = taLeftJustify
      Caption = 'SSL Session Caching'
      TabOrder = 18
    end
    object ButtonOSSLVersion: TButton
      Left = 236
      Top = 172
      Width = 69
      Height = 21
      Caption = 'OpenSSL?'
      TabOrder = 19
      OnClick = ButtonOSSLVersionClick
    end
    object DebugEventCheckBox: TCheckBox
      Left = 516
      Top = 178
      Width = 85
      Height = 17
      Caption = 'loDestEvent'
      TabOrder = 20
    end
    object DebugOutputCheckBox: TCheckBox
      Left = 516
      Top = 162
      Width = 105
      Height = 17
      Caption = 'loDestOutDebug'
      TabOrder = 21
    end
    object DebugFileCheckBox: TCheckBox
      Left = 516
      Top = 146
      Width = 73
      Height = 17
      Caption = 'loDestFile'
      TabOrder = 22
    end
    object DateTimeEdit: TEdit
      Left = 136
      Top = 148
      Width = 116
      Height = 21
      TabOrder = 23
      Text = 'DateTimeEdit'
    end
    object HeadButton: TButton
      Left = 92
      Top = 172
      Width = 69
      Height = 21
      Caption = '&Head'
      TabOrder = 24
      OnClick = HeadButtonClick
    end
    object AbortButton: TButton
      Left = 380
      Top = 172
      Width = 69
      Height = 21
      Caption = '&Abort'
      Enabled = False
      TabOrder = 25
      OnClick = AbortButtonClick
    end
  end
  object SslHttpCli1: TSslHttpCli
    LocalAddr = '0.0.0.0'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0 (compatible; ICS; MSIE 4.0)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    ProxyConnection = 'Keep-Alive'
    NoCache = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    MultiThreaded = False
    RequestVer = '1.0'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = []
    IcsLogger = IcsLogger1
    OnHeaderData = SslHttpCli1HeaderData
    OnCommand = SslHttpCli1Command
    OnDocBegin = SslHttpCli1DocBegin
    OnDocData = SslHttpCli1DocData
    OnDocEnd = SslHttpCli1DocEnd
    OnRequestDone = SslHttpCli1RequestDone
    OnLocationChange = SslHttpCli1LocationChange
    OnCookie = SslHttpCli1Cookie
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    SslContext = SslContext1
    OnSslVerifyPeer = SslHttpCli1SslVerifyPeer
    OnSslCliGetSession = SslHttpCli1SslCliGetSession
    OnSslCliNewSession = SslHttpCli1SslCliNewSession
    OnSslHandshakeDone = SslHttpCli1SslHandshakeDone
    OnSslCliCertRequest = SslHttpCli1SslCliCertRequest
    Left = 30
    Top = 220
  end
  object SslContext1: TSslContext
    IcsLogger = IcsLogger1
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = [sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_SSLREF2_REUSE_CERT_TYPE_BUG, sslOpt_MICROSOFT_BIG_SSLV3_BUFFER, sslOpt_MSIE_SSLV2_RSA_PADDING, sslOpt_SSLEAY_080_CLIENT_DH_BUG, sslOpt_TLS_D5_BUG, sslOpt_TLS_BLOCK_PADDING_BUG, sslOpt_TLS_ROLLBACK_BUG, sslOpt_NO_SSLv2, sslOpt_NETSCAPE_CA_DN_BUG, sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_CLIENT
    SslSessionTimeout = 300
    SslSessionCacheSize = 20480
    Left = 62
    Top = 220
  end
  object IcsLogger1: TIcsLogger
    LogFileOption = lfoOverwrite
    LogFileEncoding = lfeUtf8
    LogFileName = 'Debug_Out_HttpsTst.txt'
    LogOptions = [loDestFile, loProtSpecErr, loProtSpecInfo, loProtSpecDump]
    OnIcsLogEvent = IcsLogger1IcsLogEvent
    Left = 92
    Top = 220
  end
  object SslAvlSessionCache1: TSslAvlSessionCache
    IcsLogger = IcsLogger1
    IdleTimeout = 30
    FlushInterval = 10000
    MaxCacheSize = 1000
    Left = 140
    Top = 220
  end
end
