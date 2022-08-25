object ClientForm: TClientForm
  Left = 17
  Top = 324
  Caption = 'ClientForm'
  ClientHeight = 100
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object InfoLabel: TLabel
    Left = 56
    Top = 8
    Width = 44
    Height = 13
    Caption = 'InfoLabel'
  end
  object DataLabel: TLabel
    Left = 56
    Top = 32
    Width = 49
    Height = 13
    Caption = 'DataLabel'
  end
  object ConnectButton: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Connect'
    Default = True
    TabOrder = 0
    OnClick = ConnectButtonClick
  end
  object DisconnectButton: TButton
    Left = 96
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Disconnect'
    Enabled = False
    TabOrder = 1
    OnClick = DisconnectButtonClick
  end
  object IPButton: TButton
    Left = 192
    Top = 56
    Width = 75
    Height = 25
    Caption = '&IP address'
    TabOrder = 2
    OnClick = IPButtonClick
  end
  object CliSocket: TWSocket
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
    OnDataAvailable = CliSocketDataAvailable
    OnSessionClosed = CliSocketSessionClosed
    OnSessionConnected = CliSocketSessionConnected
    Left = 8
    Top = 8
  end
end
