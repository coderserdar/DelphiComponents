object SslFtpServerForm: TSslFtpServerForm
  Left = 309
  Top = 162
  Caption = 'SslFtpServerForm'
  ClientHeight = 282
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InfoMemo: TMemo
    Left = 0
    Top = 137
    Width = 461
    Height = 145
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'InfoMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 461
    Height = 137
    Align = alTop
    TabOrder = 1
    object GreenImage: TImage
      Left = 16
      Top = 12
      Width = 9
      Height = 15
      Picture.Data = {
        07544269746D6170EE000000424DEE0000000000000076000000280000000900
        00000F0000000100040000000000780000000000000000000000100000001000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF003A00000A3000000007A000A700000000070AAA07000000000A0AAA0A0000
        0000070AAA070000000007A000A7000000000A07770A000000000707B7070000
        0000070777070000000007700077000000000707770700000000070797070000
        0000070777070000000007700077000000003000000030000000}
      OnDblClick = ImagesDblClick
    end
    object ClientCountLabel: TLabel
      Left = 44
      Top = 12
      Width = 80
      Height = 13
      Caption = 'ClientCountLabel'
    end
    object RedImage: TImage
      Left = 32
      Top = 12
      Width = 9
      Height = 15
      Picture.Data = {
        07544269746D6170EE000000424DEE0000000000000076000000280000000900
        00000F0000000100040000000000780000000000000000000000100000001000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF003000000030000000077000770000000007077707000000000707A7070000
        00000707770700000000077000770000000007077707000000000707B7070000
        0000090777090000000007900097000000000709990700000000090999090000
        0000070999070000000007900097000000003900000930000000}
      OnDblClick = ImagesDblClick
    end
    object Label4: TLabel
      Left = 29
      Top = 41
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label6: TLabel
      Left = 13
      Top = 65
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label7: TLabel
      Left = 8
      Top = 89
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label11: TLabel
      Left = 252
      Top = 42
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label10: TLabel
      Left = 246
      Top = 66
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label1: TLabel
      Left = 228
      Top = 90
      Width = 59
      Height = 13
      Caption = 'Port Server2'
    end
    object Label17: TLabel
      Left = 2
      Top = 119
      Width = 62
      Height = 13
      Caption = ' Reneg. after'
    end
    object Label18: TLabel
      Left = 132
      Top = 114
      Width = 88
      Height = 13
      Caption = '(Sec. 0 = disabled)'
    end
    object Label16: TLabel
      Left = 24
      Top = 108
      Width = 40
      Height = 13
      Caption = 'Req. Ssl'
    end
    object StartMinimizedCheckBox: TCheckBox
      Left = 134
      Top = 12
      Width = 95
      Height = 17
      Caption = 'Start Minimized'
      TabOrder = 0
    end
    object CertFileEdit: TEdit
      Left = 70
      Top = 38
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'CertFileEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 70
      Top = 62
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 70
      Top = 86
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'PassPhraseEdit'
    end
    object CAFileEdit: TEdit
      Left = 290
      Top = 38
      Width = 153
      Height = 21
      TabOrder = 4
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 290
      Top = 62
      Width = 153
      Height = 21
      TabOrder = 5
      Text = 'CAPathEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 290
      Top = 110
      Width = 71
      Height = 17
      Caption = 'Verify Peer'
      TabOrder = 6
    end
    object SslTypeConnPortEdit: TEdit
      Left = 290
      Top = 86
      Width = 153
      Height = 21
      TabOrder = 7
      Text = 'SslTypeConnPortEdit'
    end
    object RenegotiationIntervalEdit: TEdit
      Left = 70
      Top = 110
      Width = 53
      Height = 21
      TabOrder = 8
      Text = '0'
      OnChange = RenegotiationIntervalEditChange
    end
    object DisplaySslInfoCheckBox: TCheckBox
      Left = 290
      Top = 13
      Width = 101
      Height = 17
      Caption = 'Display SSL Info'
      TabOrder = 9
    end
  end
  object SslFtpServer1: TSslFtpServer
    IcsLogger = IcsLogger1
    Addr = '0.0.0.0'
    Port = 'ftp'
    ListenBackLog = 5
    Banner = '220 ICS FTP Server ready'
    UserData = 0
    MaxClients = 0
    PasvPortRangeStart = 16384
    PasvPortRangeSize = 5
    Options = [ftpsCwdCheck, ftpsHidePhysicalPath, ftpsModeZCompress, ftpsSiteXmlsd, ftpsThreadRecurDirs, ftpsEnableUtf8]
    MD5UseThreadFileSize = 0
    TimeoutSecsLogin = 60
    TimeoutSecsIdle = 300
    TimeoutSecsXfer = 900
    ZlibMinLevel = 1
    ZlibMaxLevel = 9
    ZlibNoCompExt = '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'
    AlloExtraSpace = 1000000
    ZlibMinSpace = 50000000
    ZlibMaxSize = 500000000
    CodePage = 0
    Language = 'EN*'
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
    OnStorSessionClosed = SslFtpServer1StorSessionClosed
    OnRetrSessionClosed = SslFtpServer1RetrSessionClosed
    OnRetrDataSent = SslFtpServer1RetrDataSent
    OnValidatePut = SslFtpServer1ValidatePut
    OnValidateDele = SslFtpServer1ValidateDele
    OnValidateRnFr = SslFtpServer1ValidateRnFr
    OnGetProcessing = SslFtpServer1GetProcessing
    OnHost = SslFtpServer1Host
    OnRein = SslFtpServer1Rein
    SslContext = SslContext1
    OnSslVerifyPeer = SslFtpServer1SslVerifyPeer
    OnSslHandshakeDone = SslFtpServer1SslHandshakeDone
    FtpSslTypes = []
    Left = 30
    Top = 162
  end
  object MainMenu1: TMainMenu
    Left = 102
    Top = 210
    object File1: TMenuItem
      Caption = '&File'
      object MnuStartServer: TMenuItem
        Caption = '&Start server'
        OnClick = MnuStartServerClick
      end
      object MnuStopServer: TMenuItem
        Caption = 'S&top server'
        OnClick = MnuStopServerClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuListClients: TMenuItem
        Caption = '&List clients'
        OnClick = MnuListClientsClick
      end
      object DisconnectAllMnu: TMenuItem
        Caption = '&Disconnect all'
        OnClick = DisconnectAllMnuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuQuit: TMenuItem
        Caption = '&Quit'
        OnClick = MnuQuitClick
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      object Cleardisplay1: TMenuItem
        Caption = '&Clear display'
        OnClick = Cleardisplay1Click
      end
    end
    object About1: TMenuItem
      Caption = '&About'
      object OpenSslVer1: TMenuItem
        Caption = '&OpenSsl Version'
        OnClick = OpenSslVer1Click
      end
    end
  end
  object SslContext1: TSslContext
    IcsLogger = IcsLogger1
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = [sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    Left = 102
    Top = 164
  end
  object SslFtpServer2: TSslFtpServer
    IcsLogger = IcsLogger1
    Addr = '0.0.0.0'
    Port = 'ftp'
    ListenBackLog = 5
    Banner = '220 ICS FTP Server ready.'
    UserData = 0
    MaxClients = 0
    PasvPortRangeStart = 16380
    PasvPortRangeSize = 5
    Options = [ftpsCwdCheck, ftpsHidePhysicalPath, ftpsModeZCompress, ftpsSiteXmlsd, ftpsThreadRecurDirs, ftpsEnableUtf8]
    MD5UseThreadFileSize = 0
    TimeoutSecsLogin = 60
    TimeoutSecsIdle = 300
    TimeoutSecsXfer = 900
    ZlibMinLevel = 1
    ZlibMaxLevel = 9
    ZlibNoCompExt = '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'
    AlloExtraSpace = 1000000
    ZlibMinSpace = 50000000
    ZlibMaxSize = 500000000
    CodePage = 0
    Language = 'EN*'
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
    OnStorSessionClosed = SslFtpServer1StorSessionClosed
    OnRetrSessionClosed = SslFtpServer1RetrSessionClosed
    OnRetrDataSent = SslFtpServer1RetrDataSent
    OnValidatePut = SslFtpServer1ValidatePut
    OnValidateDele = SslFtpServer1ValidateDele
    OnValidateRnFr = SslFtpServer1ValidateRnFr
    OnGetProcessing = SslFtpServer1GetProcessing
    OnHost = SslFtpServer1Host
    OnRein = SslFtpServer1Rein
    SslContext = SslContext1
    OnSslVerifyPeer = SslFtpServer1SslVerifyPeer
    OnSslHandshakeDone = SslFtpServer1SslHandshakeDone
    FtpSslTypes = []
    Left = 35
    Top = 205
  end
  object IcsLogger1: TIcsLogger
    LogFileOption = lfoOverwrite
    LogFileName = 'DEBUG_SSLFtpSrv.txt'
    LogOptions = []
    Left = 140
    Top = 184
  end
end
