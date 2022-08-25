object FtpServerForm: TFtpServerForm
  Left = 205
  Top = 121
  Caption = 'FtpServerSForm'
  ClientHeight = 283
  ClientWidth = 376
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
    Top = 66
    Width = 376
    Height = 217
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
    Width = 376
    Height = 66
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
    object Label1: TLabel
      Left = 10
      Top = 40
      Width = 68
      Height = 13
      Caption = 'Root Directory'
    end
    object Label13: TLabel
      Left = 242
      Top = 12
      Width = 56
      Height = 13
      Caption = 'Max KBytes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object StartMinimizedCheckBox: TCheckBox
      Left = 136
      Top = 12
      Width = 100
      Height = 17
      Caption = 'Start Minimized'
      TabOrder = 0
    end
    object RootDirectory: TEdit
      Left = 85
      Top = 35
      Width = 281
      Height = 21
      TabOrder = 1
      Text = 'c:\temp'
    end
    object MaxKB: TEdit
      Left = 304
      Top = 8
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = 'MaxKB'
    end
  end
  object FtpServer1: TFtpServer
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    Port = 'ftp'
    ListenBackLog = 5
    MultiListenSockets = <>
    Banner = '220 ICS FTP Server ready'
    UserData = 0
    MaxClients = 0
    PasvPortRangeStart = 21001
    PasvPortRangeSize = 99
    Options = [ftpsCwdCheck, ftpsCalcMD5OnTheFly, ftpsModeZCompress, ftpsSiteXmlsd, ftpsThreadRecurDirs, ftpsEnableUtf8, ftpsAutoDetectCodePage]
    MD5UseThreadFileSize = 1000000
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
    MaxAttempts = 12
    BandwidthLimit = 0
    BandwidthSampling = 1000
    OnStart = FtpServer1Start
    OnStop = FtpServer1Stop
    OnAuthenticate = FtpServer1Authenticate
    OnOtpMethod = FtpServer1OtpMethodEvent
    OnOtpGetPassword = FtpServer1OtpGetPasswordEvent
    OnClientDisconnect = FtpServer1ClientDisconnect
    OnClientConnect = FtpServer1ClientConnect
    OnClientCommand = FtpServer1ClientCommand
    OnAnswerToClient = FtpServer1AnswerToClient
    OnChangeDirectory = FtpServer1ChangeDirectory
    OnMakeDirectory = FtpServer1MakeDirectory
    OnBuildDirectory = FtpServer1BuildDirectory
    OnAlterDirectory = FtpServer1AlterDirectory
    OnStorSessionConnected = FtpServer1StorSessionConnected
    OnRetrSessionConnected = FtpServer1RetrSessionConnected
    OnStorSessionClosed = FtpServer1StorSessionClosed
    OnRetrSessionClosed = FtpServer1RetrSessionClosed
    OnRetrDataSent = FtpServer1RetrDataSent
    OnValidatePut = FtpServer1ValidatePut
    OnValidateDele = FtpServer1ValidateDele
    OnValidateRnFr = FtpServer1ValidateRnFr
    OnGetProcessing = FtpServer1GetProcessing
    OnValidateMfmt = FtpServer1ValidateMfmt
    OnCalculateMd5 = FtpServer1CalculateMd5
    OnMd5Calculated = FtpServer1Md5Calculated
    OnCalculateCrc = FtpServer1CalculateCrc
    OnEnterSecurityContext = FtpServer1EnterSecurityContext
    OnLeaveSecurityContext = FtpServer1LeaveSecurityContext
    OnValidateAllo = FtpServer1ValidateAllo
    OnSiteMsg = FtpServer1SiteMsg
    OnSiteExec = FtpServer1SiteExec
    OnSitePaswd = FtpServer1SitePaswd
    OnCombine = FtpServer1Combine
    OnTimeout = FtpServer1Timeout
    OnUpCompressFile = FtpServer1UpCompressFile
    OnUpCompressedFile = FtpServer1UpCompressedFile
    OnDisplay = FtpServer1Display
    OnHost = FtpServer1Host
    OnRein = FtpServer1Rein
    SocketErrs = wsErrTech
    ExclusiveAddr = True
    Left = 43
    Top = 103
  end
  object MainMenu1: TMainMenu
    Left = 116
    Top = 99
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
      object DisplayDirectories1: TMenuItem
        Caption = 'Display &Directories'
        OnClick = DisplayDirectories1Click
      end
    end
    object About1: TMenuItem
      Caption = '&About'
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object ForceHomeDir: TMenuItem
        AutoCheck = True
        Caption = '&ForceHomeDir'
        OnClick = ForceHomeDirClick
      end
      object HidePhysicalPath: TMenuItem
        AutoCheck = True
        Caption = '&HidePhysicalPath'
        OnClick = HidePhysicalPathClick
      end
      object Authenticateotpmd5: TMenuItem
        Caption = 'Authenticate otp-md&5'
        OnClick = Authenticateotpmd5Click
      end
      object Authenticateotpmd4: TMenuItem
        Caption = 'Authenticate otp-md&4'
        OnClick = Authenticateotpmd4Click
      end
      object Authenticateotpsha1: TMenuItem
        Caption = 'Authenticate otp-&sha1'
        OnClick = Authenticateotpsha1Click
      end
    end
  end
end
