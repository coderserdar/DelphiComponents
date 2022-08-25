object ProxySslServerForm: TProxySslServerForm
  Left = 339
  Top = 282
  Caption = 'Proxy Server '
  ClientHeight = 437
  ClientWidth = 587
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
    Width = 587
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
    Width = 587
    Height = 396
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
      
        'The INI file needs to be manually edited to add multiple Source ' +
        'and '
      'Target sections to define the proxy server behaviour.  ')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object IcsHttpProxy1: TIcsHttpProxy
    IcsHosts = <>
    ProxyTargets = <>
    RxBuffSize = 65536
    MaxClients = 999
    SocketErrs = wsErrFriendly
    ExclusiveAddr = False
    DebugLevel = DebugSsl
    TarSecLevel = sslSecLevel80bits
    CertVerTar = CertVerNone
    SslRevocation = False
    SslReportChain = False
    SslCliCertMethod = sslCliCertNone
    SslCertAutoOrder = False
    CertExpireDays = 30
    SslX509Certs = IcsSslX509Certs
    OcspSrvStapling = False
    onProxyProg = IcsHttpProxy1ProxyProg
    OnSetTarget = IcsHttpProxy1SetTarget
    OnDataSendTar = IcsHttpProxy1DataSendTar
    OnDataRecvTar = IcsHttpProxy1DataRecvTar
    HttpIgnoreClose = False
    HttpSrcCompress = False
    HttpTarCompress = False
    HttpCompMinSize = 0
    HttpStripUpgrade = True
    HttpStopCached = False
    HttpMaxBody = 1000000
    onHttpReqBody = IcsHttpProxy1HttpReqBody
    onHttpRespBody = IcsHttpProxy1HttpRespBody
    onHttpWellKnown = IcsHttpProxy1HttpWellKnown
    onConfReqBody = IcsHttpProxy1ConfReqBody
    onConfRespBody = IcsHttpProxy1ConfRespBody
    OnLogHeader = IcsHttpProxy1LogHeader
    Left = 355
    Top = 5
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 320
    Top = 5
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
    Left = 390
    Top = 5
  end
end
