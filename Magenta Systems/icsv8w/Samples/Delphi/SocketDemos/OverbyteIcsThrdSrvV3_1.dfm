object ThrdSrvForm: TThrdSrvForm
  Left = 295
  Top = 249
  Caption = 'ThrdSrvForm'
  ClientHeight = 241
  ClientWidth = 380
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
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 380
    Height = 57
    Align = alTop
    TabOrder = 0
    DesignSize = (
      380
      57)
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 90
      Height = 13
      Caption = 'Clients Per Thread:'
    end
    object ClientsPerThreadEdit: TEdit
      Left = 100
      Top = 8
      Width = 39
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = ClientsPerThreadEditChange
    end
    object DisconnectAllButton: TButton
      Left = 295
      Top = 6
      Width = 81
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Disconnect All'
      TabOrder = 1
      OnClick = DisconnectAllButtonClick
    end
    object ClearMemoButton: TButton
      Left = 295
      Top = 30
      Width = 81
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Clear'
      TabOrder = 2
      OnClick = ClearMemoButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 57
    Width = 380
    Height = 184
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
    TabOrder = 1
  end
  object WSocketThrdServer1: TWSocketThrdServer
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Port = 'telnet'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 2
    ReqVerHigh = 2
    OnBgException = WSocketThrdServer1BgException
    Banner = 'Welcome to TcpSrv'
    BannerTooBusy = 'Sorry, too many clients'
    MaxClients = 0
    OnClientDisconnect = WSocketThrdServer1ClientDisconnect
    OnClientConnect = WSocketThrdServer1ClientConnect
    OnClientCreate = WSocketThrdServer1ClientCreate
    ClientsPerThread = 1
    OnThreadException = WSocketThrdServer1ThreadException
    Left = 72
    Top = 120
  end
end
