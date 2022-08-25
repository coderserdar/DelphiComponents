object MainForm: TMainForm
  Left = 40
  Top = 85
  Caption = 'Udp Sender'
  ClientHeight = 49
  ClientWidth = 430
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
    Left = 320
    Top = 19
    Width = 44
    Height = 13
    Caption = 'UDP port'
  end
  object SendButton: TButton
    Left = 232
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Send'
    Default = True
    TabOrder = 0
    OnClick = SendButtonClick
  end
  object MessageEdit: TEdit
    Left = 16
    Top = 16
    Width = 209
    Height = 21
    TabOrder = 1
    Text = 'MessageEdit'
  end
  object PortEdit: TEdit
    Left = 368
    Top = 16
    Width = 41
    Height = 21
    TabOrder = 2
    Text = 'PortEdit'
  end
  object WSocket: TWSocket
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
    Left = 160
  end
end
