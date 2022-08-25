object MainForm: TMainForm
  Left = 156
  Top = 209
  Caption = 'Udp Sender (Broadcast)'
  ClientHeight = 120
  ClientWidth = 384
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
  object Label1: TLabel
    Left = 20
    Top = 55
    Width = 74
    Height = 13
    Caption = 'Destination port'
  end
  object Label2: TLabel
    Left = 200
    Top = 56
    Width = 48
    Height = 13
    Caption = 'Local Port'
  end
  object SendButton: TButton
    Left = 240
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Send'
    Default = True
    TabOrder = 1
    OnClick = SendButtonClick
  end
  object MessageEdit: TEdit
    Left = 16
    Top = 16
    Width = 217
    Height = 21
    TabOrder = 0
    Text = 'MessageEdit'
  end
  object PortEdit: TEdit
    Left = 120
    Top = 52
    Width = 41
    Height = 21
    TabOrder = 2
    Text = 'PortEdit'
  end
  object LocalPortEdit: TEdit
    Left = 272
    Top = 52
    Width = 41
    Height = 21
    TabOrder = 3
    Text = 'LocalPortEdit'
    OnChange = LocalPortEditChange
  end
  object AnyPortCheckBox: TCheckBox
    Left = 200
    Top = 80
    Width = 113
    Height = 17
    Alignment = taLeftJustify
    Caption = 'System choose port'
    TabOrder = 4
    OnClick = AnyPortCheckBoxClick
  end
  object IPv6CheckBox: TCheckBox
    Left = 256
    Top = 100
    Width = 57
    Height = 17
    Alignment = taLeftJustify
    Caption = 'IPv6'
    TabOrder = 5
  end
  object WSocket: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    SocketFamily = sfAny
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
    OnDataSent = WSocketDataSent
    Left = 324
    Top = 16
  end
end
