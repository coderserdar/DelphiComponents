object Form1: TForm1
  Left = 221
  Top = 298
  Caption = 'Form1'
  ClientHeight = 344
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    539
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 322
    Width = 26
    Height = 13
    Caption = 'Host:'
  end
  object Label2: TLabel
    Left = 164
    Top = 322
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label3: TLabel
    Left = 232
    Top = 324
    Width = 23
    Height = 13
    Caption = 'URL:'
  end
  object Label4: TLabel
    Left = 24
    Top = 276
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Memo1: TMemo
    Left = 92
    Top = 0
    Width = 439
    Height = 314
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 6
    Top = 58
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Close'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object EditHost: TEdit
    Left = 46
    Top = 318
    Width = 109
    Height = 21
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object EditPort: TEdit
    Left = 192
    Top = 320
    Width = 29
    Height = 21
    TabOrder = 4
    Text = '443'
  end
  object EditUrl: TEdit
    Left = 260
    Top = 320
    Width = 259
    Height = 21
    TabOrder = 5
    Text = '/big5000.txt'
  end
  object Button3: TButton
    Left = 6
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Get URL'
    Enabled = False
    TabOrder = 6
    OnClick = Button3Click
  end
  object Sock: TSslWSocket
    LineEnd = #13#10
    Port = '443'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ComponentOptions = []
    OnDataAvailable = SockDataAvailable
    OnSessionClosed = SockSessionClosed
    OnSessionConnected = SockSessionConnected
    SslContext = SslContext1
    SslEnable = False
    SslMode = sslModeClient
    OnSslHandshakeDone = SockSslHandshakeDone
    Left = 20
    Top = 24
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslOptions = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23
    SslECDHMethod = sslECDHNone
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 48
    Top = 24
  end
end
