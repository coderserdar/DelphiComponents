object HttpsSrvForm: THttpsSrvForm
  Left = 258
  Top = 172
  Width = 495
  Height = 395
  Caption = 'HTTPS Server - http://www.overbyte.be'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 479
    Height = 157
    Align = alTop
    TabOrder = 0
    object Label3: TLabel
      Left = 27
      Top = 11
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label6: TLabel
      Left = 11
      Top = 35
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label5: TLabel
      Left = 44
      Top = 84
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label7: TLabel
      Left = 252
      Top = 12
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label4: TLabel
      Left = 8
      Top = 59
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label1: TLabel
      Left = 252
      Top = 84
      Width = 33
      Height = 13
      Caption = 'DocDir'
    end
    object Label2: TLabel
      Left = 244
      Top = 36
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label8: TLabel
      Left = 12
      Top = 100
      Width = 54
      Height = 13
      Caption = 'Acceptable'
    end
    object Label9: TLabel
      Left = 40
      Top = 112
      Width = 25
      Height = 13
      Caption = 'hosts'
    end
    object ClientCountLabel: TLabel
      Left = 184
      Top = 136
      Width = 80
      Height = 13
      Caption = 'ClientCountLabel'
    end
    object PortEdit: TEdit
      Left = 68
      Top = 80
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'PortEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 68
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'PrivKeyFileEdit'
    end
    object CertFileEdit: TEdit
      Left = 68
      Top = 8
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'CertFileEdit'
    end
    object DocDirEdit: TEdit
      Left = 288
      Top = 80
      Width = 153
      Height = 21
      TabOrder = 7
      Text = 'DocDirEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 68
      Top = 56
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'PassPhraseEdit'
    end
    object CAFileEdit: TEdit
      Left = 288
      Top = 8
      Width = 153
      Height = 21
      TabOrder = 4
      Text = 'CAFileEdit'
    end
    object ClearButton: TButton
      Left = 364
      Top = 128
      Width = 75
      Height = 21
      Caption = 'C&lear'
      TabOrder = 10
    end
    object CloseButton: TButton
      Left = 88
      Top = 128
      Width = 75
      Height = 21
      Caption = 'Cl&ose'
      TabOrder = 9
      OnClick = CloseButtonClick
    end
    object ListenButton: TButton
      Left = 8
      Top = 128
      Width = 75
      Height = 21
      Caption = '&Listen'
      TabOrder = 8
      OnClick = ListenButtonClick
    end
    object CAPathEdit: TEdit
      Left = 288
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 5
      Text = 'CAPathEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 230
      Top = 60
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 6
    end
    object AcceptableHostsEdit: TEdit
      Left = 68
      Top = 104
      Width = 373
      Height = 21
      TabOrder = 11
      Text = 'AcceptableHostsEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 157
    Width = 479
    Height = 202
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object SslWSocket1: TSslWSocket
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
    ReqVerLow = 1
    ReqVerHigh = 1
    OnSessionAvailable = SslWSocket1SessionAvailable
    SslContext = SslContext1
    SslEnable = True
    SslMode = sslModeServer
    Left = 64
    Top = 180
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    Left = 94
    Top = 180
  end
end
