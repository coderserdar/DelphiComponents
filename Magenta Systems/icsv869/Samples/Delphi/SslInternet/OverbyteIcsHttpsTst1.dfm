object HttpsTstForm: THttpsTstForm
  Left = 244
  Top = 170
  Caption = 
    'ICS HTTPS SSL Web Client Demo - http://www.overbyte.be - 17 May ' +
    '2022'
  ClientHeight = 732
  ClientWidth = 909
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
    Top = 588
    Width = 909
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 295
    ExplicitWidth = 649
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 281
    Width = 909
    Height = 307
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
    Top = 591
    Width = 909
    Height = 141
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
    Width = 909
    Height = 281
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
      Top = 7
      Width = 62
      Height = 13
      Caption = 'Socks server'
    end
    object Label9: TLabel
      Left = 667
      Top = 7
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
      Left = 2
      Top = 55
      Width = 64
      Height = 13
      Caption = 'Client CertFile'
    end
    object Label7: TLabel
      Left = 236
      Top = 55
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label10: TLabel
      Left = 452
      Top = 31
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
      Left = 460
      Top = 56
      Width = 51
      Height = 13
      Caption = 'Proxy Host'
    end
    object Label11: TLabel
      Left = 668
      Top = 55
      Width = 48
      Height = 13
      Caption = 'Proxy Port'
    end
    object Label15: TLabel
      Left = 511
      Top = 202
      Width = 58
      Height = 13
      Caption = 'Http Version'
    end
    object Label17: TLabel
      Left = 236
      Top = 79
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label18: TLabel
      Left = 317
      Top = 202
      Width = 45
      Height = 13
      Caption = 'Last Etag'
    end
    object Label20: TLabel
      Left = 6
      Top = 151
      Width = 109
      Height = 13
      Caption = 'SSL Protocol: Minimum'
    end
    object Label21: TLabel
      Left = 6
      Top = 175
      Width = 53
      Height = 13
      Caption = 'SSL Cipher'
    end
    object Label14: TLabel
      Left = 236
      Top = 151
      Width = 44
      Height = 13
      Caption = 'Maximum'
    end
    object Label22: TLabel
      Left = 6
      Top = 202
      Width = 67
      Height = 13
      Caption = 'Security Level'
    end
    object Label16: TLabel
      Left = 452
      Top = 81
      Width = 93
      Height = 13
      Caption = 'Proxy/Socks: Login'
    end
    object Label23: TLabel
      Left = 643
      Top = 81
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label19: TLabel
      Left = 386
      Top = 101
      Width = 51
      Height = 13
      Caption = 'Proxy URL'
    end
    object Label24: TLabel
      Left = 458
      Top = 167
      Width = 73
      Height = 26
      Caption = 'Modified Since Date/Time'
      WordWrap = True
    end
    object Label25: TLabel
      Left = 409
      Top = 228
      Width = 47
      Height = 13
      Caption = 'Post Data'
    end
    object Label26: TLabel
      Left = 6
      Top = 228
      Width = 61
      Height = 13
      Caption = 'Post/Put File'
    end
    object SocksServerEdit: TEdit
      Left = 517
      Top = 2
      Width = 128
      Height = 21
      TabOrder = 15
      Text = 'SocksServerEdit'
    end
    object SocksPortEdit: TEdit
      Left = 725
      Top = 2
      Width = 62
      Height = 21
      TabOrder = 16
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
      TabOrder = 1
      Text = 'DocEdit'
    end
    object CertFileEdit: TEdit
      Left = 68
      Top = 52
      Width = 165
      Height = 21
      Hint = 
        'Only enter if the server requires an SSL client certificate,  PE' +
        'M file format, quite rare '
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 275
      Top = 52
      Width = 165
      Height = 21
      Hint = 
        'Enter the  Root Certificate Authority Bundle file name.  PEM fil' +
        'e format, needed to Verify Peer certificates for sites we connec' +
        't to'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'CAFileEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 307
      Top = 126
      Width = 71
      Height = 17
      Hint = 
        'Tick if you want the SSL certificate chain to be checked against' +
        ' the Root CA File'
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
    end
    object CAPathEdit: TEdit
      Left = 275
      Top = 76
      Width = 165
      Height = 21
      Hint = 
        'Alternate to  CA File, a directory with lots of root certicate f' +
        'iles (can be empty).'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 68
      Top = 76
      Width = 165
      Height = 21
      Hint = 
        'Only if a client certificate is required, enter the private file' +
        ' name. Could be the same as CertFile if this file contains both ' +
        'certificate and private key.  PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
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
      TabOrder = 4
      Text = 'PassPhraseEdit'
    end
    object AcceptableHostsEdit: TEdit
      Left = 71
      Top = 124
      Width = 230
      Height = 21
      Hint = 'Rarely used, only if SSL certificates have the wrong Common Name'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      Text = 'AcceptableHostsEdit'
    end
    object SocksLevelComboBox: TComboBox
      Left = 517
      Top = 28
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 17
      Items.Strings = (
        '5'
        '4A'
        '4')
    end
    object GetButton: TButton
      Left = 11
      Top = 252
      Width = 69
      Height = 21
      Hint = 'Connect to the host using the port.'
      Caption = '&Get'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 35
      OnClick = GetButtonClick
    end
    object ClearButton: TButton
      Left = 161
      Top = 252
      Width = 69
      Height = 21
      Hint = 'Clear display.'
      Caption = 'C&lear'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 37
      OnClick = ClearButtonClick
    end
    object CloseButton: TButton
      Left = 311
      Top = 252
      Width = 69
      Height = 21
      Hint = 'Close the connected or listening socket.'
      Caption = 'Cl&ose'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 39
      OnClick = CloseButtonClick
    end
    object ProxyHostEdit: TEdit
      Left = 516
      Top = 52
      Width = 121
      Height = 21
      TabOrder = 19
      Text = 'ProxyHostEdit'
    end
    object ProxyPortEdit: TEdit
      Left = 725
      Top = 52
      Width = 52
      Height = 21
      TabOrder = 20
      Text = 'ProxyPortEdit'
    end
    object HttpVersionComboBox: TComboBox
      Left = 575
      Top = 198
      Width = 85
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 30
      Items.Strings = (
        'HTTP/1.0'
        'HTTP/1.1')
    end
    object SessCacheCheckBox: TCheckBox
      Left = 239
      Top = 99
      Width = 129
      Height = 17
      Alignment = taLeftJustify
      Caption = 'SSL Session Caching'
      TabOrder = 7
    end
    object ButtonOSSLVersion: TButton
      Left = 236
      Top = 252
      Width = 69
      Height = 21
      Caption = 'OpenSSL?'
      TabOrder = 38
      OnClick = ButtonOSSLVersionClick
    end
    object DebugEventCheckBox: TCheckBox
      Left = 406
      Top = 126
      Width = 85
      Height = 17
      Caption = 'loDestEvent'
      TabOrder = 25
    end
    object DebugOutputCheckBox: TCheckBox
      Left = 406
      Top = 150
      Width = 105
      Height = 17
      Caption = 'loDestOutDebug'
      TabOrder = 27
    end
    object DebugFileCheckBox: TCheckBox
      Left = 510
      Top = 150
      Width = 73
      Height = 17
      Caption = 'loDestFile'
      TabOrder = 28
    end
    object DateTimeEdit: TEdit
      Left = 544
      Top = 171
      Width = 116
      Height = 21
      TabOrder = 29
      Text = 'DateTimeEdit'
    end
    object HeadButton: TButton
      Left = 86
      Top = 252
      Width = 69
      Height = 21
      Caption = '&Head'
      TabOrder = 36
      OnClick = HeadButtonClick
    end
    object AbortButton: TButton
      Left = 386
      Top = 252
      Width = 69
      Height = 21
      Caption = '&Abort'
      Enabled = False
      TabOrder = 40
      OnClick = AbortButtonClick
    end
    object SslMaxVersion: TComboBox
      Left = 295
      Top = 148
      Width = 108
      Height = 21
      Hint = 'Only used if Security Level set to Ignore '
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnChange = ResetSsl
      Items.Strings = (
        'SSLv3'
        'TLSv1'
        'TLSv1.1'
        'TLSv1.2'
        'TLSv1.3'
        'Best Version')
    end
    object SslCipherEdit: TEdit
      Left = 65
      Top = 172
      Width = 360
      Height = 21
      Hint = 'Only used if Security Level set to Ignore '
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      Text = 'SslCipherEdit'
      OnChange = ResetSsl
    end
    object UrlEdit: TComboBox
      Left = 68
      Top = 4
      Width = 373
      Height = 21
      Hint = 'Enter the hostname or IP address of the host to connect to'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = ResetSsl
      Items.Strings = (
        'https://www.google.co.uk/'
        'https://www.embarcadero.com/'
        'https://www.telecom-tariffs.co.uk/serverinfo.htm'
        'https://ipv4.telecom-tariffs.co.uk/serverinfo.htm'
        'https://ipv6.telecom-tariffs.co.uk/serverinfo.htm'
        'https://www.magsys.co.uk/'
        'https://ipv4.magsys.co.uk/'
        'https://ipv6.magsys.co.uk/')
    end
    object ResetButton: TButton
      Left = 461
      Top = 252
      Width = 69
      Height = 21
      Caption = '&Reset SSL'
      TabOrder = 41
      OnClick = ResetButtonClick
    end
    object SslMinVersion: TComboBox
      Left = 122
      Top = 148
      Width = 108
      Height = 21
      Hint = 'Only used if Security Level set to Ignore '
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnChange = ResetSsl
      Items.Strings = (
        'SSLv3'
        'TLSv1'
        'TLSv1.1'
        'TLSv1.2'
        'TLSv1.3'
        'Best Version')
    end
    object StoreButton: TButton
      Left = 536
      Top = 252
      Width = 79
      Height = 21
      Caption = '&List Cert Store'
      TabOrder = 42
      OnClick = StoreButtonClick
    end
    object SslSecLevel: TComboBox
      Left = 92
      Top = 198
      Width = 209
      Height = 21
      Hint = 'Overrides Protocol Min/Max and Ciphers'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnChange = ResetSsl
      Items.Strings = (
        'Ignore'
        'None'
        'SSLv3 Only'
        'TLSv1 Only'
        'TLSv1.1 Only'
        'TLSv1.2 Only'
        'TLSv1.3 Only'
        'TLSv1 or Better'
        'TLSv1.1 or Better'
        'TLSv1.2 or Better'
        'Backward Ciphers'
        'Intermediate Ciphers'
        'High Ciphers, 2048 keys'
        'High Ciphers, 3072 keys'
        'High Ciphers, 7680 keys')
    end
    object DebugDumpCheckBox: TCheckBox
      Left = 510
      Top = 126
      Width = 54
      Height = 17
      Caption = 'Dump'
      TabOrder = 26
    end
    object ProxyLoginEdit: TEdit
      Left = 556
      Top = 76
      Width = 81
      Height = 21
      TabOrder = 21
      Text = 'ProxyLoginEdit'
    end
    object ProxyPwEdit: TEdit
      Left = 706
      Top = 76
      Width = 100
      Height = 21
      TabOrder = 22
      Text = 'ProxyPwEdit'
    end
    object IpSockFamily: TRadioGroup
      Left = 812
      Top = 7
      Width = 87
      Height = 99
      Caption = 'Socket Family'
      ItemIndex = 0
      Items.Strings = (
        'Any'
        'Prefer IPv4'
        'Prefer IPv6'
        'Only IPv4 '
        'Only IPv6')
      TabOrder = 24
    end
    object ProxyURLEdit: TEdit
      Left = 443
      Top = 100
      Width = 263
      Height = 21
      Hint = 
        'Combines above four proxy fields as URL, ie http://[user[:passwo' +
        'rd]@]host:port'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 23
      Text = 'ProxyURLEdit'
    end
    object LastEtagEdit: TEdit
      Left = 368
      Top = 198
      Width = 116
      Height = 21
      TabOrder = 14
      Text = 'LastEtagEdit'
    end
    object ContentEncodingCheckBox: TCheckBox
      Left = 628
      Top = 29
      Width = 138
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Content Encoding Gzip'
      TabOrder = 18
      WordWrap = True
    end
    object UploadMethod: TRadioGroup
      Left = 725
      Top = 110
      Width = 174
      Height = 109
      Caption = 'File Upload Method'
      ItemIndex = 0
      Items.Strings = (
        'POST data'
        'PUT data'
        'POST file - name as page'
        'POST file - name as query'
        'PUT file - name as query'
        'POSTfile - MIME multipart')
      TabOrder = 31
    end
    object FileNameEdit: TEdit
      Left = 72
      Top = 225
      Width = 290
      Height = 21
      Hint = 'Upload full file path and name'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 32
      Text = 'FileNameEdit'
    end
    object PostDataEdit: TEdit
      Left = 469
      Top = 225
      Width = 400
      Height = 21
      Hint = 
        'For Post/Put Data, this should be UrlEncoded fields of data, for' +
        ' Files it should be a description'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 34
    end
    object PostButton: TButton
      Left = 621
      Top = 252
      Width = 69
      Height = 21
      Caption = '&Post/Put'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 43
      OnClick = PostButtonClick
    end
    object SelectFile: TBitBtn
      Left = 372
      Top = 225
      Width = 31
      Height = 25
      Hint = 'Select File to Upload'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 33
      OnClick = SelectFileClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
        333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
        300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
        333337F373F773333333303330033333333337F3377333333333303333333333
        333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
        333337777F337F33333330330BB00333333337F373F773333333303330033333
        333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
        333377777F77377733330BBB0333333333337F337F33333333330BB003333333
        333373F773333333333330033333333333333773333333333333}
      NumGlyphs = 2
    end
  end
  object SslHttpCli1: TSslHttpCli
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0 (compatible; ICS; MSIE 4.0)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    ProxyConnection = 'Keep-Alive'
    NoCache = True
    ResponseNoException = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    RequestVer = '1.0'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = []
    IcsLogger = IcsLogger1
    Timeout = 30
    OnSessionConnected = SslHttpCli1SessionConnected
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
    OnSocksConnected = SslHttpCli1SocksConnected
    OnSocksAuthState = SslHttpCli1SocksAuthState
    OnSocksError = SslHttpCli1SocksError
    OnSocketError = SslHttpCli1SocketError
    SocketFamily = sfIPv4
    SocketErrs = wsErrFriendly
    SslContext = SslContext1
    OnSslVerifyPeer = SslHttpCli1SslVerifyPeer
    OnSslCliGetSession = SslHttpCli1SslCliGetSession
    OnSslCliNewSession = SslHttpCli1SslCliNewSession
    OnSslHandshakeDone = SslHttpCli1SslHandshakeDone
    OnSslCliCertRequest = SslHttpCli1SslCliCertRequest
    Left = 45
    Top = 380
  end
  object SslContext1: TSslContext
    IcsLogger = IcsLogger1
    SslDHParamLines.Strings = (
      '-----BEGIN DH PARAMETERS-----'
      'MIIBCAKCAQEA5lgSzWKPV8ZthosYUuPWuawgmUFfSyR/1srizVn7tXNPYE10Pz/t'
      'z1i0f1JppaoBBdFQMQnVlTrZjEIinavAZwLH9HRbmjvglO0gNL46NpgzgcXQbKbn'
      'jZs4BSFF9LbhP4VvvIIKI7lR/yQFNw5GtKtV+Pi/tZ5dCaRvALadAtzAXOmEadv0'
      'KNZXc7hONXf9kyRmtwr6C5AdeIH50enVBss6zRwwGi3fW7e5D6z3FvUrHzD9fot+'
      'y89hX5iXD/v3BurTkN3rG12JoTypQ3W1VD1lEfRrJm8rbvQTqO0RCSgxc2KwIULb'
      '3ONsf1ln/Lb+UuRiUpGeb4GQqPDkn7XW8wIBAg=='
      '-----END DH PARAMETERS-----')
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslCheckHostFlags = []
    SslSecLevel = sslSecLevel80bits
    SslOptions = [sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_MICROSOFT_BIG_SSLV3_BUFFER, sslOpt_SSLEAY_080_CLIENT_DH_BUG, sslOpt_TLS_D5_BUG, sslOpt_TLS_BLOCK_PADDING_BUG, sslOpt_TLS_ROLLBACK_BUG, sslOpt_NO_SSLv2, sslOpt_NO_SSLv3, sslOpt_NETSCAPE_CA_DN_BUG, sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG]
    SslOptions2 = [sslOpt2_NO_COMPRESSION, SslOpt2_ALLOW_NO_DHE_KEX]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslBestVer_CLIENT
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslAlpnProtocols.Strings = (
      'http/1.1')
    SslOcspStatus = False
    SslSessionTimeout = 300
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 77
    Top = 380
  end
  object IcsLogger1: TIcsLogger
    TimeStampFormatString = 'hh:nn:ss:zzz'
    TimeStampSeparator = ' '
    LogFileOption = lfoOverwrite
    LogFileName = 'Debug_Out_HttpsTst.txt'
    LogOptions = [loDestFile, loSslErr, loSslInfo, loProtSpecErr, loProtSpecInfo, loProtSpecDump]
    OnIcsLogEvent = IcsLogger1IcsLogEvent
    Left = 107
    Top = 380
  end
  object SslAvlSessionCache1: TSslAvlSessionCache
    IcsLogger = IcsLogger1
    IdleTimeout = 30
    FlushInterval = 10000
    MaxCacheSize = 1000
    Left = 145
    Top = 380
  end
  object OcspHttp1: TOcspHttp
    OcspStapleOnly = False
    CacheFName = 'OcspHttpsTstCache.recs'
    CacheFileShared = False
    OcspMaxDays = 7
    CacheRefrDays = 3
    CacheFlushMins = 2
    CacheStapled = False
    DebugLevel = DebugNone
    OnOcspProg = OcspHttp1OcspProg
    Left = 185
    Top = 380
  end
  object OpenDialog: TOpenDialog
    Left = 231
    Top = 379
  end
  object IcsCookies1: TIcsCookies
    AutoSave = False
    OnNewCookie = IcsCookies1NewCookie
    Left = 275
    Top = 380
  end
  object MimeTypesList1: TMimeTypesList
    LoadOSonDemand = True
    MimeTypesFile = '/etc/apache2/mime.types'
    DefaultTypes.Strings = (
      '.htm=text/html'
      '.html=text/html'
      '.gif=image/gif'
      '.bmp=image/bmp'
      '.jpg=image/jpeg'
      '.jpeg=image/jpeg'
      '.tif=image/tiff'
      '.tiff=image/tiff'
      '.txt=text/plain'
      '.css=text/css'
      '.wav=audio/x-wav'
      '.ico=image/x-icon'
      '.wml=text/vnd.wap.wml'
      '.wbmp=image/vnd.wap.wbmp'
      '.wmlc=application/vnd.wap.wmlc'
      '.wmlscript=text/vnd.wap.wmlscript'
      '.wmlscriptc=application/vnd.wap.wmlscriptc'
      '.pdf=application/pdf'
      '.png=image/png'
      '.xml=application/xml'
      '.xhtml=application/xhtml+xml'
      '.zip=application/zip'
      '.exe=application/x-msdownload'
      '.msi=application/x-msdownload'
      '.bin=application/octet-stream'
      '.iso=application/octet-stream'
      '.js=application/javascript'
      '.json=application/json')
    MimeTypeSrc = MTypeList
    UnknownType = 'application/octet-stream'
    Left = 325
    Top = 380
  end
end
