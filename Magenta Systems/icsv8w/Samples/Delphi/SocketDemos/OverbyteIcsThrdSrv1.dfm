object TcpSrvForm: TTcpSrvForm
  Left = 241
  Top = 152
  Width = 389
  Height = 282
  Caption = 'ThrdSrvForm'
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
    Width = 381
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 381
    Height = 207
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object WSocketServer1: TWSocketServer
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
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
    OnBgException = WSocketServer1BgException
    Banner = 'Welcome to TcpSrv'
    BannerTooBusy = 'Sorry, too many clients'
    MaxClients = 0
    OnClientDisconnect = WSocketServer1ClientDisconnect
    OnClientConnect = WSocketServer1ClientConnect
    OnClientCreate = WSocketServer1ClientCreate
    Left = 40
    Top = 96
  end
end
