object ServerForm: TServerForm
  Left = 148
  Top = 132
  Caption = 'ServerForm'
  ClientHeight = 272
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 267
    Height = 13
    Caption = 'See ThrdSrv demo, much better than this one !'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object DisconnectButton: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 0
    OnClick = DisconnectButtonClick
  end
  object QuitButton: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Quit'
    TabOrder = 1
    OnClick = QuitButtonClick
  end
  object ClientListBox: TListBox
    Left = 16
    Top = 64
    Width = 169
    Height = 185
    ItemHeight = 13
    TabOrder = 2
  end
  object DisconnectAllButton: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'DisconnectAll'
    TabOrder = 3
    OnClick = DisconnectAllButtonClick
  end
  object ServerWSocket: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Addr = '0.0.0.0'
    Port = 'telnet'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = True
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
    OnSessionAvailable = ServerWSocketSessionAvailable
    Left = 216
    Top = 136
  end
end
