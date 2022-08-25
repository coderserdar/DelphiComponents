object WebSocketForm: TWebSocketForm
  Left = 327
  Top = 392
  Caption = 'WebSocketForm'
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
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '127.0.0.1'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    ReqVerLow = 1
    ReqVerHigh = 1
    OnBgException = WSocketServer1BgException
    SocketErrs = wsErrTech
    Banner = 'Welcome to TcpSrv'
    OnClientDisconnect = WSocketServer1ClientDisconnect
    OnClientConnect = WSocketServer1ClientConnect
    MultiListenSockets = <>
    Left = 40
    Top = 96
  end
end
