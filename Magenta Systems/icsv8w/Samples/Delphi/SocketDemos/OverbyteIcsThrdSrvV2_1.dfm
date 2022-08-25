object TcpSrvForm: TTcpSrvForm
  Left = 336
  Top = 251
  Caption = 'ThrdSrvForm'
  ClientHeight = 241
  ClientWidth = 381
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
    Height = 200
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object WSocketServer1: TWSocketServer
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    OnBgException = WSocketServer1BgException
    SocketErrs = wsErrTech
    Banner = 'Welcome to TcpSrv'
    OnClientDisconnect = WSocketServer1ClientDisconnect
    OnClientConnect = WSocketServer1ClientConnect
    OnClientCreate = WSocketServer1ClientCreate
    MultiListenSockets = <>
    Left = 40
    Top = 96
  end
end
