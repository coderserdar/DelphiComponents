object POP3ExcercizerForm: TPOP3ExcercizerForm
  Left = 43
  Top = 43
  ActiveControl = ConnectButton
  Caption = 'SSL POP3 Excercizer V8.65'
  ClientHeight = 406
  ClientWidth = 526
  Color = clBtnFace
  Constraints.MinHeight = 370
  Constraints.MinWidth = 530
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
  object DisplayMemo: TMemo
    Left = 0
    Top = 279
    Width = 526
    Height = 127
    Hint = 'This area show the activity with the host'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 526
    Height = 279
    Align = alTop
    TabOrder = 0
    object InfoLabel: TLabel
      Left = 32
      Top = 190
      Width = 44
      Height = 13
      Caption = 'InfoLabel'
    end
    object Label1: TLabel
      Left = 20
      Top = 11
      Width = 53
      Height = 13
      Caption = 'POP3 Host'
    end
    object Label2: TLabel
      Left = 208
      Top = 11
      Width = 50
      Height = 13
      Caption = 'UserName'
    end
    object Label3: TLabel
      Left = 212
      Top = 35
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label4: TLabel
      Left = 416
      Top = 11
      Width = 42
      Height = 13
      Caption = 'MsgNum'
    end
    object Label5: TLabel
      Left = 432
      Top = 35
      Width = 25
      Height = 13
      Caption = 'Lines'
    end
    object Label6: TLabel
      Left = 23
      Top = 35
      Width = 50
      Height = 13
      Caption = 'POP3 Port'
    end
    object Label7: TLabel
      Left = 8
      Top = 217
      Width = 39
      Height = 13
      Caption = 'Subject:'
    end
    object Label8: TLabel
      Left = 8
      Top = 241
      Width = 29
      Height = 13
      Caption = 'From :'
    end
    object Label9: TLabel
      Left = 288
      Top = 241
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object Label11: TLabel
      Left = 5
      Top = 60
      Width = 68
      Height = 13
      Caption = 'Authentication'
    end
    object Label10: TLabel
      Left = 211
      Top = 60
      Width = 47
      Height = 13
      Caption = 'SSL-Type'
    end
    object Label12: TLabel
      Left = 40
      Top = 82
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label13: TLabel
      Left = 219
      Top = 82
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object ConnectButton: TButton
      Left = 8
      Top = 114
      Width = 60
      Height = 21
      Caption = '&Connect'
      TabOrder = 11
      OnClick = ConnectButtonClick
    end
    object QuittButton: TButton
      Left = 264
      Top = 114
      Width = 60
      Height = 21
      Caption = '&Quit'
      TabOrder = 15
      OnClick = QuittButtonClick
    end
    object UserButton: TButton
      Left = 72
      Top = 114
      Width = 60
      Height = 21
      Caption = '&User'
      TabOrder = 12
      OnClick = UserButtonClick
    end
    object HostEdit: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'HostEdit'
    end
    object UserNameEdit: TEdit
      Left = 268
      Top = 8
      Width = 129
      Height = 21
      TabOrder = 1
      Text = 'UserNameEdit'
    end
    object PassWordEdit: TEdit
      Left = 268
      Top = 32
      Width = 129
      Height = 21
      TabOrder = 4
      Text = 'PassWordEdit'
    end
    object PassButton: TButton
      Left = 136
      Top = 114
      Width = 60
      Height = 21
      Caption = '&Pass'
      TabOrder = 13
      OnClick = PassButtonClick
    end
    object MsgNumEdit: TEdit
      Left = 464
      Top = 8
      Width = 33
      Height = 21
      TabOrder = 2
      Text = '1'
    end
    object RetrButton: TButton
      Left = 200
      Top = 114
      Width = 60
      Height = 21
      Caption = '&Retr'
      TabOrder = 14
      OnClick = RetrButtonClick
    end
    object StatButton: TButton
      Left = 8
      Top = 138
      Width = 60
      Height = 21
      Caption = '&Stat'
      TabOrder = 19
      OnClick = StatButtonClick
    end
    object ListAllButton: TButton
      Left = 72
      Top = 138
      Width = 60
      Height = 21
      Caption = 'List All'
      TabOrder = 20
      OnClick = ListAllButtonClick
    end
    object ListButton: TButton
      Left = 136
      Top = 138
      Width = 60
      Height = 21
      Caption = 'List'
      TabOrder = 21
      OnClick = ListButtonClick
    end
    object DeleteButton: TButton
      Left = 200
      Top = 138
      Width = 60
      Height = 21
      Caption = 'Delete'
      TabOrder = 22
      OnClick = DeleteButtonClick
    end
    object NoopButton: TButton
      Left = 264
      Top = 138
      Width = 60
      Height = 21
      Caption = 'Noop'
      TabOrder = 23
      OnClick = NoopButtonClick
    end
    object LastButton: TButton
      Left = 328
      Top = 138
      Width = 60
      Height = 21
      Caption = 'Last'
      TabOrder = 24
      OnClick = LastButtonClick
    end
    object ResetButton: TButton
      Left = 392
      Top = 138
      Width = 60
      Height = 21
      Caption = 'Reset'
      TabOrder = 25
      OnClick = ResetButtonClick
    end
    object TopButton: TButton
      Left = 328
      Top = 114
      Width = 60
      Height = 21
      Caption = 'Top'
      TabOrder = 16
      OnClick = TopButtonClick
    end
    object MsgLinesEdit: TEdit
      Left = 464
      Top = 32
      Width = 33
      Height = 21
      TabOrder = 5
      Text = '0'
    end
    object RpopButton: TButton
      Left = 392
      Top = 114
      Width = 60
      Height = 21
      Caption = 'Rpop'
      TabOrder = 17
      OnClick = RpopButtonClick
    end
    object UidlButton: TButton
      Left = 328
      Top = 162
      Width = 60
      Height = 21
      Caption = 'Uidl'
      TabOrder = 30
      OnClick = UidlButtonClick
    end
    object ApopButton: TButton
      Left = 392
      Top = 162
      Width = 60
      Height = 21
      Caption = 'Apop'
      TabOrder = 31
      OnClick = ApopButtonClick
    end
    object NextButton: TButton
      Left = 456
      Top = 162
      Width = 60
      Height = 21
      Caption = '&Next'
      Default = True
      TabOrder = 32
      OnClick = NextButtonClick
    end
    object GetAllButton: TButton
      Left = 456
      Top = 114
      Width = 60
      Height = 21
      Caption = 'Get All'
      TabOrder = 18
      OnClick = GetAllButtonClick
    end
    object PortEdit: TEdit
      Left = 80
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'PortEdit'
    end
    object OpenButton: TButton
      Left = 456
      Top = 138
      Width = 60
      Height = 21
      Caption = 'Open'
      TabOrder = 26
      OnClick = OpenButtonClick
    end
    object AbortButton: TButton
      Left = 264
      Top = 162
      Width = 60
      Height = 21
      Caption = 'Abort'
      TabOrder = 29
      OnClick = AbortButtonClick
    end
    object SubjectEdit: TEdit
      Left = 56
      Top = 214
      Width = 457
      Height = 21
      TabStop = False
      Color = clSilver
      ReadOnly = True
      TabOrder = 33
      Text = 'SubjectEdit'
    end
    object FromEdit: TEdit
      Left = 56
      Top = 238
      Width = 225
      Height = 21
      TabStop = False
      Color = clSilver
      ReadOnly = True
      TabOrder = 34
      Text = 'FromEdit'
    end
    object ToEdit: TEdit
      Left = 312
      Top = 238
      Width = 201
      Height = 21
      TabStop = False
      Color = clSilver
      ReadOnly = True
      TabOrder = 35
      Text = 'ToEdit'
    end
    object AuthComboBox: TComboBox
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      Items.Strings = (
        'None'
        'Login'
        'CramMD5'
        'CramSHA1'
        'NTLM'
        'XOAuth2'
        'OAuthBearer')
    end
    object AuthButton: TButton
      Left = 200
      Top = 162
      Width = 60
      Height = 21
      Caption = 'Auth'
      TabOrder = 28
      OnClick = AuthButtonClick
    end
    object SslTypeComboBox: TComboBox
      Left = 268
      Top = 56
      Width = 129
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 7
      Text = 'None (Plain Text)'
      Items.Strings = (
        'None (Plain Text)'
        'Implicit (TLS Connection)'
        'Explicit (STLS)')
    end
    object STlsButton: TButton
      Left = 136
      Top = 162
      Width = 60
      Height = 21
      Caption = 'STLS'
      TabOrder = 27
      OnClick = STlsButtonClick
    end
    object VerifyCheckBox: TCheckBox
      Left = 412
      Top = 58
      Width = 85
      Height = 17
      Alignment = taLeftJustify
      Caption = 'SslVerifyPeer'
      TabOrder = 8
    end
    object CAFileEdit: TEdit
      Left = 80
      Top = 80
      Width = 121
      Height = 21
      TabOrder = 9
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 268
      Top = 80
      Width = 129
      Height = 21
      TabOrder = 10
      Text = 'CAPathEdit'
    end
  end
  object SslPop3Client: TSslPop3Cli
    Tag = 0
    SocketFamily = sfIPv4
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    Port = 'pop3'
    AuthType = popAuthNone
    MsgLines = 0
    MsgNum = 0
    SocketErrs = wsErrTech
    OnGetNewToken = SslPop3ClientGetNewToken
    OnDisplay = SslPop3ClientDisplay
    OnMessageBegin = SslPop3ClientMessageBegin
    OnMessageEnd = SslPop3ClientMessageEnd
    OnMessageLine = SslPop3ClientMessageLine
    OnListBegin = SslPop3ClientListBegin
    OnListEnd = SslPop3ClientListEnd
    OnListLine = SslPop3ClientListLine
    OnUidlBegin = SslPop3ClientUidlBegin
    OnUidlEnd = SslPop3ClientUidlEnd
    OnUidlLine = SslPop3ClientUidlLine
    OnHeaderEnd = SslPop3ClientHeaderEnd
    OnRequestDone = SslPop3ClientRequestDone
    Timeout = 15
    SslType = pop3TlsNone
    SslContext = SslContext1
    OnSslVerifyPeer = SslPop3ClientSslVerifyPeer
    OnSslHandshakeDone = SslPop3ClientSslHandshakeDone
    Left = 130
    Top = 286
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
    SslOptions = [sslOpt_NO_SSLv2]
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_CLIENT
    SslMinVersion = sslVerTLS1_2
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslOcspStatus = False
    SslSessionTimeout = 30000
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 158
    Top = 286
  end
  object IcsRestEmail1: TIcsRestEmail
    RestEmailType = RestEmailGoogle
    ForceLogin = False
    LoginTimeout = 30
    HdrFieldList = 'to,from,subject,date'
    DebugLevel = DebugConn
    OAAuthType = OAuthTypeWeb
    OnEmailProg = IcsRestEmail1EmailProg
    OnEmailNewToken = IcsRestEmail1EmailNewToken
    Left = 195
    Top = 290
  end
end
