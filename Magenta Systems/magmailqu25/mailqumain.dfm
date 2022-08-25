object DemoForm: TDemoForm
  Left = 8
  Top = 83
  Caption = 'Magenta Mail Queue Demo  - 23rd November 2018'
  ClientHeight = 758
  ClientWidth = 932
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FF00
    00000000000000000000000000FFF0F7777070707077777777777777770F0FF8
    770707080F777F777F777F777F700F8000007FF8F000F000F000F000F8700F07
    77770788888888888888888888700F08787807FFFFFFFFFFFFFFFFFFFF700F08
    8888078F8F8F8F8F8F8F8F8F8F700F08888807F8FF77F77F77F8FF787F700F0F
    FFFF078F800F00F00F8F8000FF700F08F8F807F8FF77F77F77F8FF787F700F0F
    8F8F078F800F00F00F8F8000FF700F08F8F807F8FF77F77F77F8FF787F700F0F
    878F078F800F00F00F8F8000FF700F087F7807F8FF77F77F77F8FF787F700F0F
    878F078F800F00F00F8F8000FF700F08F8F807F8F8F8F8F8F8F8F8F8F8700F0F
    8F8F078F8F8F8F8F8F8F8F8F8F700F08F8F807FFFFFFFFFFFFFFFFFFF8700F0F
    8F8F078777777777777777770F700F0FFFFF07F0888888888888888808700F0F
    8F8F07808F000F80008F008F0F700F08F8F807F0FFFFFFFFFFFFFFFF08700F0F
    8F8F0780FF00FF000FF000FF0F700F0FFFFF0FF0FFFFFFFFFFFFFFFF08700F80
    00008880FF000FF000FF00FF0F700FF8F8F8F8F0FFFFFFFFFFFFFFFF0870F0FF
    FFFFFFF0FF00FF000FF000FF0F0FFF0000000000FFFFFFFFFFFFFFFF00FFFFFF
    FFFFFFF0FF000FF000FF00FF0FFFFFFFFFFFFFF0FFFFFFFFFFFFFFFF0FFFFFFF
    FFFFFFF0FFFFFFFFFFFFFFFF0FFFFFFFFFFFFFF000000000000000000FFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = True
  PrintScale = poNone
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object PanelControls: TPanel
    Left = 0
    Top = 0
    Width = 591
    Height = 486
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 5
      Width = 67
      Height = 14
      Caption = 'Recipient List '
    end
    object Label2: TLabel
      Left = 10
      Top = 469
      Width = 72
      Height = 14
      Caption = 'Message Body'
    end
    object Label3: TLabel
      Left = 10
      Top = 395
      Width = 36
      Height = 14
      Caption = 'Subject'
    end
    object Label4: TLabel
      Left = 10
      Top = 345
      Width = 24
      Height = 14
      Caption = 'From'
    end
    object Label5: TLabel
      Left = 10
      Top = 195
      Width = 93
      Height = 14
      Caption = 'Relay SMTP Server'
    end
    object Label6: TLabel
      Left = 10
      Top = 295
      Width = 111
      Height = 14
      Caption = 'Mail Queue Root Folder'
    end
    object LabelUser: TLabel
      Left = 10
      Top = 245
      Width = 23
      Height = 14
      Caption = 'User'
    end
    object LabelPass: TLabel
      Left = 227
      Top = 245
      Width = 50
      Height = 14
      Caption = 'Password'
    end
    object Label10: TLabel
      Left = 10
      Top = 320
      Width = 246
      Height = 16
      AutoSize = False
      Caption = 'Minutes Between Queue Attempts (comma list)'
      WordWrap = True
    end
    object Label14: TLabel
      Left = 10
      Top = 420
      Width = 33
      Height = 14
      Caption = 'Priority'
    end
    object Label11: TLabel
      Left = 162
      Top = 420
      Width = 55
      Height = 14
      Caption = 'Attachment'
    end
    object Label7: TLabel
      Left = 10
      Top = 370
      Width = 14
      Height = 14
      Caption = 'CC'
    end
    object Label8: TLabel
      Left = 10
      Top = 270
      Width = 136
      Height = 14
      Caption = 'DNS Server for MX Lookups'
      WordWrap = True
    end
    object Label9: TLabel
      Left = 287
      Top = 270
      Width = 82
      Height = 14
      Caption = 'HELO Host Name'
      WordWrap = True
    end
    object Label12: TLabel
      Left = 10
      Top = 220
      Width = 19
      Height = 14
      Caption = 'Port'
    end
    object Label13: TLabel
      Left = 10
      Top = 450
      Width = 63
      Height = 14
      Caption = 'SSL Security'
    end
    object doSend: TButton
      Left = 340
      Top = 15
      Width = 75
      Height = 25
      Caption = 'Send Mail'
      TabOrder = 21
      OnClick = doSendClick
    end
    object RecipList: TMemo
      Left = 5
      Top = 20
      Width = 326
      Height = 161
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'To???'
        '')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object MailSubject: TEdit
      Left = 60
      Top = 390
      Width = 512
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      Text = 'Subject???'
    end
    object MailFrom: TEdit
      Left = 60
      Top = 340
      Width = 512
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      Text = 'From??'
    end
    object doExit: TButton
      Left = 440
      Top = 15
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 24
      OnClick = doExitClick
    end
    object SMTPServer: TEdit
      Left = 111
      Top = 190
      Width = 216
      Height = 22
      TabOrder = 1
      Text = 'mail.magsys.co.uk'
      OnChange = SMTPChange
    end
    object doClear: TButton
      Left = 340
      Top = 50
      Width = 75
      Height = 25
      Caption = 'Clear Text'
      TabOrder = 22
      OnClick = doClearClick
    end
    object ServSmtpAuth: TRadioGroup
      Left = 340
      Top = 120
      Width = 91
      Height = 111
      Caption = 'Authentication'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Plain'
        'Login'
        'Cram-Md5'
        'Cram-Sha1'
        'NTLM'
        'Auto Select')
      TabOrder = 4
      OnClick = SMTPChange
    end
    object ServSmtpPass: TEdit
      Left = 295
      Top = 240
      Width = 137
      Height = 22
      PasswordChar = '*'
      TabOrder = 6
      OnChange = SMTPChange
    end
    object ServSmtpUser: TEdit
      Left = 50
      Top = 240
      Width = 151
      Height = 22
      TabOrder = 5
      OnChange = SMTPChange
    end
    object AttemptsList: TEdit
      Left = 251
      Top = 315
      Width = 320
      Height = 22
      TabOrder = 10
      Text = '5,5,10,10,30,30,60,90,300,300,300,300'
      OnChange = SMTPChange
    end
    object MailPriority: TComboBox
      Left = 60
      Top = 415
      Width = 93
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      ItemIndex = 4
      TabOrder = 14
      Text = 'Low'
      Items.Strings = (
        'Not specified'
        'Highest'
        'High'
        'Normal'
        'Low'
        'Lowest')
    end
    object MailQuFolder: TEdit
      Left = 133
      Top = 290
      Width = 298
      Height = 22
      TabOrder = 9
      Text = '\mailqueue'
      OnChange = SMTPChange
    end
    object FileAttachment: TEdit
      Left = 230
      Top = 415
      Width = 344
      Height = 22
      TabOrder = 15
    end
    object doQueue: TButton
      Left = 340
      Top = 85
      Width = 75
      Height = 25
      Caption = 'Start Queue'
      TabOrder = 23
      OnClick = doQueueClick
    end
    object VerifyCertMode: TRadioGroup
      Left = 445
      Top = 93
      Width = 131
      Height = 68
      Caption = 'Verify SSL Cert Mode'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'PEM Bundle File'
        'Windows Cert Store')
      TabOrder = 18
    end
    object SendMethod: TRadioGroup
      Left = 445
      Top = 190
      Width = 131
      Height = 66
      Caption = 'SMTP Send Method'
      ItemIndex = 0
      Items.Strings = (
        'Relay Servers'
        'Specific Servers'
        'Lookup MX Servers')
      TabOrder = 20
    end
    object MailCC: TEdit
      Left = 60
      Top = 365
      Width = 512
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
    end
    object CheckSslRevoke: TCheckBox
      Left = 445
      Top = 170
      Width = 141
      Height = 17
      Caption = 'Check SSL Cert Revoke'
      Checked = True
      State = cbChecked
      TabOrder = 19
    end
    object DnsServer: TEdit
      Left = 165
      Top = 265
      Width = 111
      Height = 22
      TabOrder = 7
      Text = '8.8.8.8'
      OnChange = SMTPChange
    end
    object HeloHostName: TEdit
      Left = 380
      Top = 265
      Width = 201
      Height = 22
      TabOrder = 8
      Text = 'my.host.name'
      OnChange = SMTPChange
    end
    object doShowQu: TButton
      Left = 440
      Top = 50
      Width = 75
      Height = 25
      Caption = 'Show Queue'
      TabOrder = 25
      OnClick = doShowQuClick
    end
    object ServSmtpSecure: TComboBox
      Left = 135
      Top = 215
      Width = 160
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      ItemIndex = 0
      TabOrder = 3
      Text = 'Not Secured'
      OnChange = ServSmtpSecureChange
      Items.Strings = (
        'Not Secured'
        'SSL/TLS Connection'
        'SSL/TLS Authentication')
    end
    object ServSmtpPort: TEdit
      Left = 50
      Top = 215
      Width = 50
      Height = 22
      TabOrder = 2
      Text = '25'
      OnChange = SMTPChange
    end
    object SslSecurity: TComboBox
      Left = 95
      Top = 445
      Width = 182
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      ItemIndex = 0
      TabOrder = 16
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
    object ServRetryNoSsl: TCheckBox
      Left = 295
      Top = 445
      Width = 141
      Height = 17
      Caption = 'Retry Without SSL'
      Checked = True
      State = cbChecked
      TabOrder = 17
    end
  end
  object MailLog: TRichEdit
    Left = 590
    Top = 0
    Width = 334
    Height = 461
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 718
    Width = 932
    Height = 21
    Align = alBottom
    TabOrder = 3
    object LabelProg: TLabel
      Left = 1
      Top = 3
      Width = 212
      Height = 13
      AutoSize = False
      Caption = 'Current'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object LabelCount: TLabel
      Left = 241
      Top = 2
      Width = 41
      Height = 13
      Caption = 'Progress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LabelQueue: TLabel
      Left = 490
      Top = 1
      Width = 32
      Height = 13
      Caption = 'Queue'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object MailBody: TMemo
    Left = 0
    Top = 489
    Width = 924
    Height = 226
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      
        'This is a test message from the Magenta Mail Queue Demo applicat' +
        'ion')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Status: TStatusBar
    Left = 0
    Top = 739
    Width = 932
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object SslContext: TSslContext
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
    SslOptions = []
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslBestVer_CLIENT
    SslMinVersion = sslVerTLS1
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 740
    Top = 81
  end
  object TimerUpdates: TTimer
    OnTimer = TimerUpdatesTimer
    Left = 660
    Top = 80
  end
end
