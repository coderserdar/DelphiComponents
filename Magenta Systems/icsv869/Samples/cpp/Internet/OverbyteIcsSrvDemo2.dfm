object CliForm: TCliForm
  Left = 373
  Top = 312
  Caption = 'Client'
  ClientHeight = 219
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 365
    Height = 137
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 178
    Width = 365
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 1
    object SendEdit: TEdit
      Left = 16
      Top = 10
      Width = 249
      Height = 21
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 280
      Top = 10
      Width = 75
      Height = 21
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 365
    Height = 41
    Align = alTop
    TabOrder = 2
    object LineLabel: TLabel
      Left = 96
      Top = 12
      Width = 46
      Height = 13
      Caption = 'LineLabel'
    end
    object DisconnectButton: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Disconnect'
      TabOrder = 0
      OnClick = DisconnectButtonClick
    end
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
    Left = 40
    Top = 88
  end
end
