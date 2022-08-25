object IpLogForm: TIpLogForm
  Left = 25
  Top = 84
  Caption = 'IP Log Streaming Component Demo - 14th December 2018'
  ClientHeight = 767
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object PanelLogs: TPanel
    Left = 0
    Top = 0
    Width = 1000
    Height = 728
    Align = alClient
    TabOrder = 1
    object Label1: TLabel
      Left = 290
      Top = 5
      Width = 315
      Height = 14
      Caption = 'Data Received by Server (S) or Client (C) from Socket Number [x]'
    end
    object Panel2: TPanel
      Left = 7
      Top = 677
      Width = 277
      Height = 45
      TabOrder = 3
      object LabelSendClient: TLabel
        Left = 5
        Top = 5
        Width = 77
        Height = 14
        Caption = 'LabelSendClient'
      end
      object LabelSendServer: TLabel
        Left = 5
        Top = 22
        Width = 69
        Height = 14
        Caption = 'LabelSendInfo'
      end
    end
    object BoxLocalMode: TGroupBox
      Left = 5
      Top = 2
      Width = 279
      Height = 50
      Caption = 'Local - Client and Server'
      TabOrder = 0
      object Label4: TLabel
        Left = 5
        Top = 25
        Width = 48
        Height = 14
        Caption = 'Local Port'
      end
      object LocalPort: TEdit
        Left = 70
        Top = 20
        Width = 81
        Height = 22
        TabOrder = 0
        Text = '25678'
      end
    end
    object BoxClient: TGroupBox
      Left = 5
      Top = 53
      Width = 279
      Height = 183
      Caption = 'Client Mode'
      TabOrder = 1
      object Label5: TLabel
        Left = 5
        Top = 90
        Width = 58
        Height = 14
        Caption = 'Remote Port'
      end
      object Label2: TLabel
        Left = 5
        Top = 15
        Width = 129
        Height = 14
        Caption = 'Remote Host or IP Address'
      end
      object Label12: TLabel
        Left = 5
        Top = 155
        Width = 66
        Height = 14
        Caption = 'Cert Authority'
      end
      object Label13: TLabel
        Left = 5
        Top = 105
        Width = 119
        Height = 14
        Caption = 'Acceptable Host Names:'
      end
      object RemoteHosts: TMemo
        Left = 5
        Top = 30
        Width = 264
        Height = 51
        Lines.Strings = (
          '192.168.1.119'
          '192.168.1.108')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object RemotePort: TComboBox
        Left = 74
        Top = 85
        Width = 71
        Height = 22
        ItemHeight = 14
        ItemIndex = 0
        MaxLength = 5
        TabOrder = 1
        Text = '514'
        Items.Strings = (
          '514'
          '7777'
          '40000')
      end
      object SslAllowNames: TEdit
        Left = 5
        Top = 120
        Width = 265
        Height = 22
        TabOrder = 2
      end
      object SslCertAuth: TEdit
        Left = 80
        Top = 150
        Width = 189
        Height = 22
        TabOrder = 3
        Text = 'RootCaCertsBundle.pem'
      end
    end
    object BoxServer: TGroupBox
      Left = 5
      Top = 228
      Width = 279
      Height = 143
      Caption = 'Server Mode'
      TabOrder = 2
      object Label6: TLabel
        Left = 5
        Top = 20
        Width = 48
        Height = 14
        Caption = 'Local Port'
      end
      object Label7: TLabel
        Left = 158
        Top = 20
        Width = 56
        Height = 14
        Caption = 'Total Conns'
      end
      object Label11: TLabel
        Left = 5
        Top = 45
        Width = 56
        Height = 14
        Caption = 'Server Cert'
      end
      object Label10: TLabel
        Left = 5
        Top = 70
        Width = 66
        Height = 14
        Caption = 'Serv Priv Key'
      end
      object Label14: TLabel
        Left = 5
        Top = 95
        Width = 70
        Height = 14
        Caption = 'Serv CA Chain'
      end
      object Label15: TLabel
        Left = 5
        Top = 120
        Width = 73
        Height = 14
        Caption = 'Serv DH Param'
      end
      object ServerPort: TComboBox
        Left = 80
        Top = 15
        Width = 71
        Height = 22
        ItemHeight = 14
        ItemIndex = 0
        MaxLength = 5
        TabOrder = 0
        Text = '514'
        Items.Strings = (
          '514'
          '7777'
          '40000')
      end
      object MaxSockets: TEdit
        Left = 226
        Top = 15
        Width = 26
        Height = 22
        TabOrder = 1
        Text = '1'
      end
      object SslServCert: TEdit
        Left = 80
        Top = 40
        Width = 190
        Height = 22
        TabOrder = 2
        Text = 'iplog-cert.pem'
      end
      object SslCertKey: TEdit
        Left = 80
        Top = 65
        Width = 190
        Height = 22
        TabOrder = 3
        Text = 'iplog-prvkey.pem'
      end
      object SslCACerts: TEdit
        Left = 80
        Top = 90
        Width = 190
        Height = 22
        TabOrder = 4
      end
      object SslDHParams: TEdit
        Left = 80
        Top = 115
        Width = 190
        Height = 22
        TabOrder = 5
        Text = 'dhparam1024.pem'
      end
    end
    object DataWin: TMemo
      Left = 290
      Top = 20
      Width = 701
      Height = 271
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 4
      WordWrap = False
    end
    object LogWin: TMemo
      Left = 290
      Top = 297
      Width = 701
      Height = 425
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 5
      WordWrap = False
    end
    object BoxLocalAddr: TGroupBox
      Left = 5
      Top = 371
      Width = 279
      Height = 305
      Caption = 'Client and Server'
      TabOrder = 6
      object Label3: TLabel
        Left = 130
        Top = 115
        Width = 70
        Height = 14
        Caption = 'Data Gap (ms)'
      end
      object Label9: TLabel
        Left = 5
        Top = 20
        Width = 52
        Height = 14
        Caption = 'Local Addr'
      end
      object Label8: TLabel
        Left = 5
        Top = 45
        Width = 66
        Height = 14
        Caption = 'Socket Family'
      end
      object Label16: TLabel
        Left = 130
        Top = 130
        Width = 76
        Height = 28
        Caption = 'Server Timeout (secs)'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 3
        Top = 255
        Width = 105
        Height = 14
        Caption = 'Server Security Level'
      end
      object Label18: TLabel
        Left = 3
        Top = 280
        Width = 98
        Height = 14
        Caption = 'Client Security Level'
      end
      object Protocol: TRadioGroup
        Left = 130
        Top = 68
        Width = 70
        Height = 47
        Caption = 'Protocol'
        ItemIndex = 0
        Items.Strings = (
          'UDP'
          'TCP')
        TabOrder = 2
      end
      object DataClient: TCheckBox
        Left = 5
        Top = 70
        Width = 106
        Height = 17
        Caption = 'Client Send Data'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object DataServer: TCheckBox
        Left = 5
        Top = 90
        Width = 116
        Height = 17
        Caption = 'Server Send Data'
        TabOrder = 4
      end
      object PingRemote: TCheckBox
        Left = 5
        Top = 150
        Width = 88
        Height = 17
        Caption = 'Ping Remote'
        TabOrder = 7
      end
      object DataGap: TEdit
        Left = 212
        Top = 110
        Width = 57
        Height = 22
        TabOrder = 12
        Text = '1000'
      end
      object HeavyTraffic: TCheckBox
        Left = 5
        Top = 110
        Width = 116
        Height = 17
        Caption = 'Heavy Traffic'
        TabOrder = 5
      end
      object LocalAddr: TComboBox
        Left = 76
        Top = 15
        Width = 194
        Height = 22
        ItemHeight = 14
        MaxLength = 5
        TabOrder = 0
        Text = '0.0.0.0'
        OnChange = LocalAddrChange
      end
      object SocketFamily: TComboBox
        Left = 75
        Top = 40
        Width = 110
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        MaxLength = 5
        TabOrder = 1
        OnChange = SocketFamilyChange
      end
      object UseSSL: TCheckBox
        Left = 5
        Top = 170
        Width = 75
        Height = 17
        Caption = 'Use SSL'
        TabOrder = 8
      end
      object LogErrors: TCheckBox
        Left = 5
        Top = 190
        Width = 75
        Height = 17
        Caption = 'Debug Errors'
        TabOrder = 9
      end
      object VerifyCertMode: TRadioGroup
        Left = 130
        Top = 165
        Width = 131
        Height = 61
        Caption = 'Verify Certificate Mode'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'PEM Bundle File'
          'Windows Cert Store')
        TabOrder = 14
      end
      object RevokeCheck: TCheckBox
        Left = 5
        Top = 230
        Width = 103
        Height = 17
        Caption = 'Revoke Check'
        TabOrder = 11
      end
      object ReportChain: TCheckBox
        Left = 125
        Top = 230
        Width = 151
        Height = 17
        Caption = 'Report Certificate Chain'
        TabOrder = 15
      end
      object RawData: TCheckBox
        Left = 5
        Top = 130
        Width = 111
        Height = 17
        Caption = 'Receive Raw Data'
        TabOrder = 6
      end
      object LogInfo: TCheckBox
        Left = 5
        Top = 210
        Width = 75
        Height = 17
        Caption = 'Debug Info'
        TabOrder = 10
      end
      object SrvTimeout: TEdit
        Left = 212
        Top = 135
        Width = 57
        Height = 22
        TabOrder = 13
        Text = '300'
      end
      object SslSecLevel: TComboBox
        Left = 121
        Top = 250
        Width = 155
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        MaxLength = 5
        TabOrder = 16
        OnChange = SocketFamilyChange
      end
      object SslCliSecurity: TComboBox
        Left = 121
        Top = 275
        Width = 155
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        ItemIndex = 0
        TabOrder = 17
        Text = 'Ignore'
        Items.Strings = (
          'Ignore'
          'None'
          'SSLv3 Only'
          'TLSv1.2 Only'
          'TLSv1.3 Only'
          'TLSv1 or better'
          'TLSv1.1 or better'
          'TLSv1.2 or better'
          'Backward Ciphers'
          'Intermedate Ciphers'
          'High Ciphers, 2048 keys'
          'High Ciphers, 3072 keys'
          'High Ciphers, 7680 keys')
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 728
    Width = 1000
    Height = 39
    Align = alBottom
    TabOrder = 0
    object doStop: TButton
      Left = 245
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 3
      OnClick = doStopClick
    end
    object doLocal: TButton
      Left = 5
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Start Local'
      TabOrder = 0
      OnClick = doLocalClick
    end
    object doClient: TButton
      Left = 85
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Start Client'
      TabOrder = 1
      OnClick = doClientClick
    end
    object doExit: TButton
      Left = 915
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 9
      OnClick = doExitClick
    end
    object doServer: TButton
      Left = 165
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Start Server'
      TabOrder = 2
      OnClick = doServerClick
    end
    object doClear: TButton
      Left = 831
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Clear Logs'
      TabOrder = 8
      OnClick = doClearClick
    end
    object doCliSendFile: TButton
      Left = 325
      Top = 7
      Width = 81
      Height = 25
      Caption = 'Client Send File'
      TabOrder = 4
      OnClick = doCliSendFileClick
    end
    object SendFileName: TEdit
      Left = 505
      Top = 9
      Width = 276
      Height = 22
      TabOrder = 6
    end
    object SelectFile: TBitBtn
      Left = 793
      Top = 7
      Width = 31
      Height = 25
      TabOrder = 7
      OnClick = SelectFileClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
        333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
        300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
        333337F373F773333333303330033333333337F3377333333333303333333333
        333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
        333337777F337F33333330330BB00333333337F373F773333333303330033333
        333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
        333377777F77377733330BBB0333333333337F337F33333333330BB003333333
        333373F773333333333330033333333333333773333333333333}
      NumGlyphs = 2
    end
    object doSrvSendFile: TButton
      Left = 410
      Top = 7
      Width = 89
      Height = 25
      Caption = 'Server Send File'
      TabOrder = 5
      OnClick = doSrvSendFileClick
    end
  end
  object DataTimer: TTimer
    Enabled = False
    OnTimer = DataTimerTimer
    Left = 487
    Top = 406
  end
  object SslContextSrv: TSslContext
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
    SslCheckHostFlags = []
    SslSecLevel = sslSecLevel80bits
    SslOptions = [sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_MICROSOFT_BIG_SSLV3_BUFFER, sslOpt_SSLEAY_080_CLIENT_DH_BUG, sslOpt_TLS_D5_BUG, sslOpt_TLS_BLOCK_PADDING_BUG, sslOpt_TLS_ROLLBACK_BUG, sslOpt_SINGLE_DH_USE, sslOpt_NO_SSLv2, sslOpt_NO_SSLv3, sslOpt_NETSCAPE_CA_DN_BUG, sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG, SslOpt_SINGLE_ECDH_USE]
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_SERVER, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslBestVer_SERVER
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 435
    Top = 410
  end
  object SslContextCli: TSslContext
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
    SslVerifyPeer = True
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslCheckHostFlags = []
    SslSecLevel = sslSecLevel80bits
    SslOptions = [sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_MICROSOFT_BIG_SSLV3_BUFFER, sslOpt_SSLEAY_080_CLIENT_DH_BUG, sslOpt_TLS_D5_BUG, sslOpt_TLS_BLOCK_PADDING_BUG, sslOpt_TLS_ROLLBACK_BUG, sslOpt_SINGLE_DH_USE, sslOpt_NO_SSLv2, sslOpt_NO_SSLv3, sslOpt_NETSCAPE_CA_DN_BUG, sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG, SslOpt_SINGLE_ECDH_USE]
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL'
    SslVersionMethod = sslBestVer_CLIENT
    SslMinVersion = sslVerTLS1
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 385
    Top = 410
  end
  object SslAvlSessionCache: TSslAvlSessionCache
    IdleTimeout = 30
    FlushInterval = 10000
    MaxCacheSize = 1000
    Left = 340
    Top = 410
  end
  object OpenDialog: TOpenDialog
    Filter = 'All Files *.*|*.*|PEM Files *.pem|*.pem'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 533
    Top = 407
  end
end
