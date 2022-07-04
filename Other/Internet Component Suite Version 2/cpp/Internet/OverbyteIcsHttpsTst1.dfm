object HttpTestForm: THttpTestForm
  Left = 56
  Top = 114
  Caption = 'Https Test - http://www.overbyte.be'
  ClientHeight = 429
  ClientWidth = 660
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 660
    Height = 213
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 36
      Top = 11
      Width = 22
      Height = 13
      Caption = 'URL'
    end
    object Label2: TLabel
      Left = 484
      Top = 105
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object Label3: TLabel
      Left = 13
      Top = 35
      Width = 49
      Height = 13
      Caption = 'Document'
    end
    object Label4: TLabel
      Left = 27
      Top = 155
      Width = 124
      Height = 13
      Caption = 'Modified Since Date/Time'
    end
    object Label5: TLabel
      Left = 484
      Top = 138
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label6: TLabel
      Left = 528
      Top = 83
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object Label7: TLabel
      Left = 27
      Top = 59
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label8: TLabel
      Left = 238
      Top = 60
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label17: TLabel
      Left = 236
      Top = 84
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label9: TLabel
      Left = 11
      Top = 83
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label10: TLabel
      Left = 6
      Top = 107
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label12: TLabel
      Left = 12
      Top = 124
      Width = 54
      Height = 13
      Caption = 'Acceptable'
    end
    object Label13: TLabel
      Left = 40
      Top = 136
      Width = 25
      Height = 13
      Caption = 'hosts'
    end
    object Label11: TLabel
      Left = 452
      Top = 56
      Width = 59
      Height = 13
      Caption = 'Socks Level'
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
    object Label14: TLabel
      Left = 459
      Top = 32
      Width = 52
      Height = 13
      Caption = 'Socks Port'
    end
    object Label18: TLabel
      Left = 449
      Top = 8
      Width = 62
      Height = 13
      Caption = 'Socks server'
    end
    object GetButton: TButton
      Left = 24
      Top = 179
      Width = 69
      Height = 21
      Caption = '&Get'
      Default = True
      TabOrder = 4
      OnClick = GetButtonClick
    end
    object URLEdit: TEdit
      Left = 68
      Top = 8
      Width = 375
      Height = 21
      Hint = 'protocol://[user[:password]@]server[:port]/path'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'http://www.overbyte.be'
    end
    object ProxyHostEdit: TEdit
      Left = 516
      Top = 102
      Width = 89
      Height = 21
      TabOrder = 2
      Text = 'ProxyHostEdit'
    end
    object ProxyPortEdit: TEdit
      Left = 516
      Top = 128
      Width = 65
      Height = 21
      TabOrder = 3
      Text = 'ProxyPortEdit'
    end
    object DocEdit: TEdit
      Left = 68
      Top = 32
      Width = 373
      Height = 21
      TabOrder = 1
      Text = 'DocEdit'
    end
    object DateTimeEdit: TEdit
      Left = 155
      Top = 152
      Width = 97
      Height = 21
      TabOrder = 5
      Text = 'DateTimeEdit'
    end
    object HeadButton: TButton
      Left = 99
      Top = 179
      Width = 69
      Height = 21
      Caption = '&Head'
      TabOrder = 6
      OnClick = HeadButtonClick
    end
    object AbortButton: TButton
      Left = 399
      Top = 179
      Width = 69
      Height = 21
      Caption = '&Abort'
      Enabled = False
      TabOrder = 7
      OnClick = AbortButtonClick
    end
    object CertFileEdit: TEdit
      Left = 68
      Top = 56
      Width = 165
      Height = 21
      Hint = 'Enter the certificate file name. PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 276
      Top = 56
      Width = 165
      Height = 21
      Hint = 'Enter the CA certificate file name.  PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 276
      Top = 80
      Width = 165
      Height = 21
      Hint = 'Enter CA certicate directory (can be empty).'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 68
      Top = 80
      Width = 165
      Height = 21
      Hint = 
        'Enter the private file name. Could be the same as CertFile if th' +
        'is file contains both certificate and private key.  PEM file for' +
        'mat.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 68
      Top = 104
      Width = 165
      Height = 21
      Hint = 'Enter pass phrase protecting private key file.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      Text = 'PassPhraseEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 238
      Top = 108
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 13
    end
    object SessCacheCheckBox: TCheckBox
      Left = 320
      Top = 108
      Width = 121
      Height = 17
      Alignment = taLeftJustify
      Caption = 'SSL Session Caching'
      TabOrder = 14
    end
    object AcceptableHostsEdit: TEdit
      Left = 68
      Top = 128
      Width = 373
      Height = 21
      TabOrder = 15
      Text = 'AcceptableHostsEdit'
    end
    object ButtonOSSLVersion: TButton
      Left = 249
      Top = 179
      Width = 69
      Height = 21
      Caption = 'OpenSSL?'
      TabOrder = 16
      OnClick = ButtonOSSLVersionClick
    end
    object ClearButton: TButton
      Left = 174
      Top = 179
      Width = 69
      Height = 21
      Hint = 'Clear display.'
      Caption = 'C&lear'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = ClearButtonClick
    end
    object HttpVersionComboBox: TComboBox
      Left = 516
      Top = 80
      Width = 85
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 18
      Items.Strings = (
        'HTTP/1.0'
        'HTTP/1.1')
    end
    object SocksLevelComboBox: TComboBox
      Left = 516
      Top = 53
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 19
      Items.Strings = (
        '5'
        '4A'
        '4')
    end
    object SocksPortEdit: TEdit
      Left = 516
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 20
      Text = 'SocksPortEdit'
    end
    object SocksServerEdit: TEdit
      Left = 516
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 21
      Text = 'SocksServerEdit'
    end
    object DebugFileCheckBox: TCheckBox
      Left = 516
      Top = 150
      Width = 73
      Height = 17
      Caption = 'loDestFile'
      TabOrder = 22
    end
    object DebugOutputCheckBox: TCheckBox
      Left = 516
      Top = 166
      Width = 105
      Height = 17
      Caption = 'loDestOutDebug'
      TabOrder = 23
    end
    object DebugEventCheckBox: TCheckBox
      Left = 516
      Top = 182
      Width = 85
      Height = 17
      Caption = 'loDestEvent'
      TabOrder = 24
    end
    object CloseButton: TButton
      Left = 324
      Top = 179
      Width = 69
      Height = 21
      Hint = 'Close the connected or listening socket.'
      Caption = 'Cl&ose'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 25
      OnClick = CloseButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 213
    Width = 660
    Height = 136
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
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
  object DocumentMemo: TMemo
    Left = 0
    Top = 349
    Width = 660
    Height = 80
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DocumentMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object SslHttpCli1: TSslHttpCli
    LocalAddr = '0.0.0.0'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0 (compatible; ICS)'
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
    Options = [httpoEnableContentCoding]
    IcsLogger = IcsLogger1
    OnHeaderData = SslHttpCli1HeaderData
    OnCommand = SslHttpCli1Command
    OnDocBegin = SslHttpCli1DocBegin
    OnDocEnd = SslHttpCli1DocEnd
    OnRequestDone = SslHttpCli1RequestDone
    OnLocationChange = SslHttpCli1LocationChange
    OnCookie = SslHttpCli1Cookie
    SocksAuthentication = socksNoAuthentication
    SslContext = SslContext1
    OnSslVerifyPeer = SslHttpCli1SslVerifyPeer
    OnSslCliGetSession = SslHttpCli1SslCliGetSession
    OnSslCliNewSession = SslHttpCli1SslCliNewSession
    OnSslHandshakeDone = SslHttpCli1SslHandshakeDone
    OnSslCliCertRequest = SslHttpCli1SslCliCertRequest
    Left = 16
    Top = 232
  end
  object IcsLogger1: TIcsLogger
    LogFileOption = lfoOverwrite
    LogFileName = 'Debug_Out_HttpsTst.txt'
    LogOptions = [loDestFile, loProtSpecErr, loProtSpecInfo, loProtSpecDump]
    OnIcsLogEvent = IcsLogger1IcsLogEvent
    Left = 84
    Top = 232
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = [sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_SSLREF2_REUSE_CERT_TYPE_BUG, sslOpt_MICROSOFT_BIG_SSLV3_BUFFER, sslOpt_MSIE_SSLV2_RSA_PADDING, sslOpt_SSLEAY_080_CLIENT_DH_BUG, sslOpt_TLS_D5_BUG, sslOpt_TLS_BLOCK_PADDING_BUG, sslOpt_TLS_ROLLBACK_BUG, sslOpt_NO_SSLv2, sslOpt_NETSCAPE_CA_DN_BUG, sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    Left = 48
    Top = 232
  end
end
