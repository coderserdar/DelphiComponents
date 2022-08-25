object FtpServerForm: TFtpServerForm
  Left = 296
  Top = 284
  Caption = 'FtpServerForm'
  ClientHeight = 242
  ClientWidth = 387
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 387
    Height = 41
    Align = alTop
    TabOrder = 0
    object GreenImage: TImage
      Left = 16
      Top = 12
      Width = 9
      Height = 15
      Picture.Data = {
        07544269746D6170EE000000424DEE0000000000000076000000280000000900
        00000F0000000100040000000000780000000000000000000000100000000000
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
        00000F0000000100040000000000780000000000000000000000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF003000000030000000077000770000000007077707000000000707A7070000
        00000707770700000000077000770000000007077707000000000707B7070000
        0000090777090000000007900097000000000709990700000000090999090000
        0000070999070000000007900097000000003900000930000000}
      OnDblClick = ImagesDblClick
    end
    object StartMinimizedCheckBox: TCheckBox
      Left = 236
      Top = 12
      Width = 113
      Height = 17
      Caption = 'Start Minimized'
      TabOrder = 0
    end
  end
  object InfoMemo: TMemo
    Left = 0
    Top = 41
    Width = 387
    Height = 201
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
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 132
    Top = 72
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
      object Disconnectall1: TMenuItem
        Caption = '&Disconnect all'
        OnClick = Disconnectall1Click
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
    PasvPortRangeStart = 0
    PasvPortRangeSize = 0
    Options = []
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
    OnStart = FtpServer1Start
    OnStop = FtpServer1Stop
    OnAuthenticate = FtpServer1Authenticate
    OnClientDisconnect = FtpServer1ClientDisconnect
    OnClientConnect = FtpServer1ClientConnect
    OnClientCommand = FtpServer1ClientCommand
    OnAnswerToClient = FtpServer1AnswerToClient
    OnChangeDirectory = FtpServer1ChangeDirectory
    OnBuildDirectory = FtpServer1BuildDirectory
    OnAlterDirectory = FtpServer1AlterDirectory
    OnStorSessionConnected = FtpServer1StorSessionConnected
    OnRetrSessionConnected = FtpServer1RetrSessionConnected
    OnStorSessionClosed = FtpServer1StorSessionClosed
    OnRetrSessionClosed = FtpServer1RetrSessionClosed
    OnRetrDataSent = FtpServer1RetrDataSent
    OnGetProcessing = FtpServer1GetProcessing
    Left = 52
    Top = 72
  end
end
