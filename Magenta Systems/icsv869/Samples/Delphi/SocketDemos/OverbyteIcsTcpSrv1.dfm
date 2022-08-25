object TcpSrvForm: TTcpSrvForm
  Left = 327
  Top = 392
  Caption = 'TcpSrvForm'
  ClientHeight = 243
  ClientWidth = 389
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
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 389
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 389
    Height = 202
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
  object WSocketServer1: TWSocketServer
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    SocketFamily = sfIPv4
    Proto = 'tcp'
    LocalAddr = '127.0.0.1'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 2
    ReqVerHigh = 2
    OnBgException = WSocketServer1BgException
    Banner = 'Welcome to TcpSrv'
    BannerTooBusy = 'Sorry, too many clients'
    MaxClients = 0
    OnClientDisconnect = WSocketServer1ClientDisconnect
    OnClientConnect = WSocketServer1ClientConnect
    MultiListenSockets = <>
    Left = 40
    Top = 96
  end
end
