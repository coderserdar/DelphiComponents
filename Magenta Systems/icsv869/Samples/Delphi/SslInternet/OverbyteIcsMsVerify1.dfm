object MsVerifyForm: TMsVerifyForm
  Left = 69
  Top = 0
  ActiveControl = ConnectButton
  Caption = 'Test MS Crypto API Certificate Verification'
  ClientHeight = 358
  ClientWidth = 522
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 530
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 65
    Align = alTop
    TabOrder = 0
    DesignSize = (
      522
      65)
    object Label1: TLabel
      Left = 24
      Top = 13
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object Label2: TLabel
      Left = 359
      Top = 14
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Port:'
    end
    object Label3: TLabel
      Left = 173
      Top = 40
      Width = 105
      Height = 13
      Caption = 'URL retrieval timeout:'
    end
    object HostEdit: TEdit
      Left = 56
      Top = 10
      Width = 297
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'www.embarcadero.com'
    end
    object PortEdit: TEdit
      Left = 385
      Top = 10
      Width = 43
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '443'
    end
    object ConnectButton: TButton
      Left = 437
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Connect'
      TabOrder = 2
      OnClick = ConnectButtonClick
    end
    object RevocationCheckBox: TCheckBox
      Left = 56
      Top = 37
      Width = 117
      Height = 17
      Caption = 'Revocation check'
      TabOrder = 3
    end
    object TimeoutEdit: TEdit
      Left = 284
      Top = 37
      Width = 69
      Height = 21
      TabOrder = 4
      Text = 'TimeoutEdit'
    end
    object ShowCertButton: TButton
      Left = 56
      Top = 10
      Width = 75
      Height = 21
      Caption = 'ShowCertButton'
      TabOrder = 5
      OnClick = ShowCertButtonClick
    end
    object DisconnectButton: TButton
      Left = 437
      Top = 36
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Disconnect'
      TabOrder = 6
      OnClick = DisconnectButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 65
    Width = 522
    Height = 293
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object SslWSocket1: TSslWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ComponentOptions = []
    OnDataAvailable = SslWSocket1DataAvailable
    OnSessionClosed = SslWSocket1SessionClosed
    OnSessionConnected = SslWSocket1SessionConnected
    SslContext = SslContext1
    SslEnable = True
    SslMode = sslModeClient
    OnSslVerifyPeer = SslWSocket1SslVerifyPeer
    OnSslHandshakeDone = SslWSocket1SslHandshakeDone
    OnSslCliGetSession = SslWSocket1SslCliGetSession
    OnSslCliNewSession = SslWSocket1SslCliNewSession
    Left = 36
    Top = 86
  end
  object SslContext1: TSslContext
    SslDHParamLines.Strings = (
      '-----BEGIN DH PARAMETERS-----'
      'MIICCAKCAgEA45KZVdTCptcakXZb7jJvSuuOdMlUbl1tpncHbQcYbFhRbcFmmefp'
      'bOmZsTowlWHQpoYRRTe6NEvYox8J+44i/X5cJkMTlIgMb0ZBty7t76U9f6qAId/O'
      '6elE0gnk2ThER9nmBcUA0ZKgSXn0XCBu6j5lzZ0FS+bx9OVNhlzvIFBclRPXbI58'
      '71dRoTjOjfO1SIzV69T3FoKJcqur58l8b+no/TOQzekMzz4XJTRDefqvePhj7ULP'
      'Z/Zg7vtEh11h8gHR0/rlF378S05nRMq5hbbJeLxIbj9kxQunETSbwwy9qx0SyQgH'
      'g+90+iUCrKCJ9Fb7WKqtQLkQuzJIkkXkXUyuxUuyBOeeP9XBUAOQu+eYnRPYSmTH'
      'GkhyRbIRTPCDiBWDFOskdyGYYDrxiK7LYJQanqHlEFtjDv9t1XmyzDm0k7W9oP/J'
      'p0ox1+WIpFgkfv6nvihqCPHtAP5wevqXNIQADhDk5EyrR3XWRFaySeKcmREM9tbc'
      'bOvmsEp5MWCC81ZsnaPAcVpO66aOPojNiYQZUbmm70fJsr8BDzXGpcQ44+wmL4Ds'
      'k3+ldVWAXEXs9s1vfl4nLNXefYl74cV8E5Mtki9hCjUrUQ4dzbmNA5fg1CyQM/v7'
      'JuP6PBYFK7baFDjG1F5YJiO0uHo8sQx+SWdJnGsq8piI3w0ON9JhUvMCAQI='
      '-----END DH PARAMETERS-----')
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslOptions = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslBestVer_CLIENT
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslSessionTimeout = 5000
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 112
    Top = 86
  end
  object SslAvlSessionCache1: TSslAvlSessionCache
    IdleTimeout = 30
    FlushInterval = 10000
    MaxCacheSize = 1000
    Left = 206
    Top = 86
  end
end
