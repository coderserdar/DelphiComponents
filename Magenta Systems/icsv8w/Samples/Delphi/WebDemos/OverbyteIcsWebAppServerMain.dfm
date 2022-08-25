object WebAppSrvForm: TWebAppSrvForm
  Left = 12
  Top = 206
  Caption = 'Overbyte ICS Web Application Server Demo'
  ClientHeight = 213
  ClientWidth = 371
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
    Width = 371
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 371
    Height = 172
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
  object HttpAppSrv1: THttpAppSrv
    ListenBacklog = 5
    MultiListenSockets = <>
    Port = '80'
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    MaxClients = 0
    DocDir = 'c:\wwwroot'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 0
    Options = []
    KeepAliveTimeSec = 10
    KeepAliveTimeXferSec = 300
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 5000
    SizeCompressMax = 5000000
    MaxBlkSize = 8192
    BandwidthLimit = 0
    BandwidthSampling = 1000
    ServerHeader = 'Server: ICS-HttpServer-8.50'
    OnServerStarted = HttpAppSrv1ServerStarted
    OnServerStopped = HttpAppSrv1ServerStopped
    OnClientConnect = HttpAppSrv1ClientConnect
    OnGetDocument = HttpAppSrv1GetDocument
    OnBeforeProcessRequest = HttpAppSrv1BeforeProcessRequest
    OnAfterAnswer = HttpAppSrv1AfterAnswer
    AuthTypes = []
    AuthRealm = 'ics'
    OnBgException = HttpAppSrv1BgException
    SocketErrs = wsErrTech
    ExclusiveAddr = True
    SessionTimeout = 300
    MaxSessions = 100
    OnDeleteSession = HttpAppSrv1DeleteSession
    OnVirtualException = HttpAppSrv1VirtualException
    OnDisplay = HttpAppSrv1Display
    Left = 52
    Top = 88
  end
  object HousekeepingTimer: TTimer
    Enabled = False
    OnTimer = HousekeepingTimerTimer
    Left = 176
    Top = 88
  end
end
