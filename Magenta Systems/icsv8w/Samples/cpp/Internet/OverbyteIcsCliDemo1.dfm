object ClientForm: TClientForm
  Left = 216
  Top = 283
  Caption = 'Client'
  ClientHeight = 257
  ClientWidth = 367
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
    Top = 105
    Width = 367
    Height = 152
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 367
    Height = 105
    Align = alTop
    TabOrder = 1
    object LineLabel: TLabel
      Left = 16
      Top = 48
      Width = 46
      Height = 13
      Caption = 'LineLabel'
    end
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label2: TLabel
      Left = 104
      Top = 16
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object SendEdit: TEdit
      Left = 16
      Top = 72
      Width = 185
      Height = 21
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 214
      Top = 72
      Width = 75
      Height = 21
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
    object DisconnectButton: TButton
      Left = 214
      Top = 40
      Width = 75
      Height = 21
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = DisconnectButtonClick
    end
    object PortEdit: TEdit
      Left = 32
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 3
      Text = 'telnet'
    end
    object ServerEdit: TEdit
      Left = 144
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 4
      Text = 'localhost'
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
    OnSessionConnected = CliSocketSessionConnected
    Left = 240
    Top = 128
  end
end
