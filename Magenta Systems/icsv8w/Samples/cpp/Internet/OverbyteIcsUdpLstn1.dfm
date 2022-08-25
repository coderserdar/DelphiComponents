object MainForm: TMainForm
  Left = 247
  Top = 425
  Caption = 'UdpListener'
  ClientHeight = 103
  ClientWidth = 350
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
  object DataAvailableLabel: TLabel
    Left = 16
    Top = 72
    Width = 92
    Height = 13
    Caption = 'DataAvailableLabel'
  end
  object InfoLabel: TLabel
    Left = 16
    Top = 48
    Width = 44
    Height = 13
    Caption = 'InfoLabel'
  end
  object Label1: TLabel
    Left = 176
    Top = 11
    Width = 44
    Height = 13
    Caption = 'UDP port'
  end
  object Label2: TLabel
    Left = 176
    Top = 35
    Width = 31
    Height = 13
    Caption = 'Server'
  end
  object StartButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start'
    Default = True
    TabOrder = 0
    OnClick = StartButtonClick
  end
  object StopButton: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'S&top'
    TabOrder = 1
    OnClick = StopButtonClick
  end
  object PortEdit: TEdit
    Left = 224
    Top = 8
    Width = 57
    Height = 21
    TabOrder = 2
    Text = 'PortEdit'
  end
  object ServerEdit: TEdit
    Left = 224
    Top = 32
    Width = 57
    Height = 21
    TabOrder = 3
    Text = 'ServerEdit'
    OnChange = ServerEditChange
  end
  object AnyServerCheckBox: TCheckBox
    Left = 296
    Top = 32
    Width = 49
    Height = 17
    Caption = 'Any'
    TabOrder = 4
    OnClick = AnyServerCheckBoxClick
  end
  object WSocket: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Addr = '0.0.0.0'
    Port = '600'
    Proto = 'udp'
    LocalAddr = '0.0.0.0'
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
    OnDataAvailable = WSocketDataAvailable
    OnSessionClosed = WSocketSessionClosed
    OnSessionConnected = WSocketSessionConnected
    Left = 240
    Top = 72
  end
end
