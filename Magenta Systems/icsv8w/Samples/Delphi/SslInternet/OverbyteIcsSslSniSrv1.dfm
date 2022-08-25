object MainForm: TMainForm
  Left = 298
  Top = 146
  Caption = 'ICS SSL SNI Server Demo'
  ClientHeight = 273
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 0
    Width = 453
    Height = 273
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object SslWSocketServer1: TSslWSocketServer
    LineEnd = #13#10
    Addr = '0.0.0.0'
    Port = '443'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ComponentOptions = []
    OnClientDisconnect = ClientDisconnect
    OnClientConnect = ClientConnect
    MultiListenSockets = <>
    SslContext = SslContext1
    SslEnable = True
    OnSslServerName = ClientSslServerName
    Left = 70
    Top = 104
  end
  object SslContext1: TSslContext
    SslCertFile = 'SelfSignedServer1.pem'
    SslPrivKeyFile = 'SelfSignedServer1.pem'
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslOptions = [sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslECDHMethod = sslECDHNone
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 160
    Top = 104
  end
  object SslContext2: TSslContext
    SslCertFile = 'SelfSignedServer2.pem'
    SslPrivKeyFile = 'SelfSignedServer2.pem'
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslOptions = [sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslECDHMethod = sslECDHNone
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 240
    Top = 104
  end
end
