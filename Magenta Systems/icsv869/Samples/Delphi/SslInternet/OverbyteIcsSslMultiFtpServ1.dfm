object FtpServerForm: TFtpServerForm
  Left = 339
  Top = 282
  Caption = 'ICS Multi FTP Server - V8.69 12th April 2022'
  ClientHeight = 600
  ClientWidth = 843
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
    Width = 843
    Height = 41
    Align = alTop
    TabOrder = 0
    object StartButton: TButton
      Left = 29
      Top = 8
      Width = 57
      Height = 21
      Caption = '&Start'
      Default = True
      TabOrder = 0
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 111
      Top = 8
      Width = 57
      Height = 21
      Caption = 'S&top'
      Enabled = False
      TabOrder = 1
      OnClick = StopButtonClick
    end
    object RecheckCertsButton: TButton
      Left = 184
      Top = 8
      Width = 102
      Height = 21
      Caption = 'Recheck Ssl Certs'
      TabOrder = 2
      OnClick = RecheckCertsButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 843
    Height = 559
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      
        'Please note that this sample takes all it'#39's settings from an INI' +
        ' file.  '
      ''
      
        'The INI file needs to be manually edited to add multiple Host se' +
        'ctions to '
      
        'define the web server behaviour.  A default INI file is copied t' +
        'o user'
      'space when first run and the file to edit shown on start-up. ')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 290
    Top = 125
  end
  object IcsSslX509Certs: TSslX509Certs
    AcmeAccKeyType = PrivKeyRsa2048
    AutoOrderComplete = False
    CertSubAltNames = <>
    CertCsrOrigin = CsrOriginProps
    CertOutFmts = []
    CertSerNumType = SerNumRandom
    CertSignDigestType = Digest_sha256
    CertValidity = 365
    DebugLevel = DebugConn
    DomWebSrvIP = '0.0.0.0'
    KeepOldCA = False
    LogJson = False
    LogPkeys = False
    OAAuthType = OAuthTypeWeb
    OARefreshAuto = False
    OARefrMinsPrior = 120
    OAWebSrvIP = '127.0.0.1'
    OAWebSrvPort = '8080'
    PrivKeyCipher = PrivKeyEncNone
    PrivKeyType = PrivKeyRsa2048
    AutoAccountClose = False
    AccountTimeOutMins = 10
    SeqOrderNum = 0
    SocketFamily = sfAny
    SuppCertChallenge = ChallNone
    SupplierProto = SuppProtoNone
    OnCertProg = IcsSslX509CertsCertProg
    OnNewCert = IcsSslX509CertsNewCert
    OnOAuthAuthUrl = IcsSslX509CertsOAuthAuthUrl
    OnChallengeDNS = IcsSslX509CertsChallengeDNS
    Left = 240
    Top = 125
  end
  object IcsMailQueue: TIcsMailQueue
    MailServers = <>
    Active = False
    ArchiveSent = False
    DeleteFailed = True
    Debug = False
    BodyDebug = False
    RetryList = '5,5,10,10,30,30,60,90,300,300,300,300'
    QuStartDelay = 3
    SslVerMethod = MailSslVerNone
    SslRevocation = False
    SslReportChain = False
    SslRootFile = 'RootCaCertsBundle.pem'
    MailCliSecurity = sslCliSecTls11
    SmtpMethod = MailSmtpRelay
    FileQuSent = '"MailQuSent-"yyyymmdd".log'
    LogQuSent = False
    MxSrvUseSsl = False
    MxSocketFamily = sfIPv4
    LogEvent = IcsMailQueueLogEvent
    Left = 190
    Top = 125
  end
  object SslFtpServer1: TSslFtpServer
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    Port = 'ftp'
    ListenBackLog = 5
    MultiListenSockets = <>
    Banner = '220 ICS FTP Server ready.'
    UserData = 0
    MaxClients = 0
    PasvPortRangeStart = 0
    PasvPortRangeSize = 0
    Options = [ftpsCwdCheck, ftpsCdupHome, ftpsSiteXmlsd]
    MD5UseThreadFileSize = 0
    TimeoutSecsLogin = 60
    TimeoutSecsIdle = 300
    TimeoutSecsXfer = 60
    ZlibMinLevel = 1
    ZlibMaxLevel = 9
    ZlibNoCompExt = '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'
    AlloExtraSpace = 1000000
    ZlibMinSpace = 50000000
    ZlibMaxSize = 500000000
    CodePage = 0
    Language = 'EN*'
    MaxAttempts = 12
    BandwidthLimit = 0
    BandwidthSampling = 1000
    OnStart = SslFtpServer1Start
    OnStop = SslFtpServer1Stop
    OnAuthenticate = SslFtpServer1Authenticate
    OnOtpMethod = SslFtpServer1OtpMethod
    OnOtpGetPassword = SslFtpServer1OtpGetPassword
    OnClientDisconnect = SslFtpServer1ClientDisconnect
    OnClientConnect = SslFtpServer1ClientConnect
    OnClientCommand = SslFtpServer1ClientCommand
    OnAnswerToClient = SslFtpServer1AnswerToClient
    OnChangeDirectory = SslFtpServer1ChangeDirectory
    OnMakeDirectory = SslFtpServer1MakeDirectory
    OnBuildDirectory = SslFtpServer1BuildDirectory
    OnAlterDirectory = SslFtpServer1AlterDirectory
    OnStorSessionConnected = SslFtpServer1StorSessionConnected
    OnRetrSessionConnected = SslFtpServer1RetrSessionConnected
    OnRetrSessionClosed = SslFtpServer1RetrSessionClosed
    OnRetrDataSent = SslFtpServer1RetrDataSent
    OnValidatePut = SslFtpServer1ValidatePut
    OnValidateDele = SslFtpServer1ValidateDele
    OnValidateRnFr = SslFtpServer1ValidateRnFr
    OnGetProcessing = SslFtpServer1GetProcessing
    OnBuildFilePath = SslFtpServer1BuildFilePath
    OnDisplay = SslFtpServer1Display
    OnHost = SslFtpServer1Host
    OnRein = SslFtpServer1Rein
    OnBgException = SslFtpServer1BgException
    SocketErrs = wsErrTech
    ExclusiveAddr = True
    OnSslVerifyPeer = SslFtpServer1SslVerifyPeer
    OnSslHandshakeDone = SslFtpServer1SslHandshakeDone
    FtpSslTypes = []
    IcsHosts = <>
    SslCertAutoOrder = False
    CertExpireDays = 30
    SslX509Certs = IcsSslX509Certs
    OnSslServerName = SslFtpServer1SslServerName
    Left = 345
    Top = 130
  end
end
