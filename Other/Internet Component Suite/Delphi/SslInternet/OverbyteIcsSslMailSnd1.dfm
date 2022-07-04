object SslSmtpTestForm: TSslSmtpTestForm
  Left = 456
  Top = 161
  Width = 609
  Height = 474
  Caption = 'SMTP SSL Test - http://www.overbyte.be'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MsgMemo: TMemo
    Left = 0
    Top = 245
    Width = 601
    Height = 66
    Hint = 'Enter the message text in this memo'
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'MsgMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 394
    Width = 601
    Height = 46
    Hint = 'This memo shows info messages'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 2
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 601
    Height = 245
    Align = alTop
    TabOrder = 3
    DesignSize = (
      601
      245)
    object Label1: TLabel
      Left = 22
      Top = 11
      Width = 55
      Height = 13
      Caption = 'SMTP Host'
    end
    object Label2: TLabel
      Left = 54
      Top = 36
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object Label3: TLabel
      Left = 252
      Top = 36
      Width = 13
      Height = 13
      Caption = 'To'
    end
    object Subject: TLabel
      Left = 41
      Top = 82
      Width = 36
      Height = 13
      Caption = 'Subject'
    end
    object Label4: TLabel
      Left = 246
      Top = 11
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label5: TLabel
      Left = 6
      Top = 226
      Width = 66
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Message text:'
    end
    object Label8: TLabel
      Left = 244
      Top = 84
      Width = 21
      Height = 13
      Caption = 'Sign'
    end
    object Label9: TLabel
      Left = 29
      Top = 108
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label10: TLabel
      Left = 242
      Top = 108
      Width = 23
      Height = 13
      Caption = 'Pass'
    end
    object Label11: TLabel
      Left = 9
      Top = 132
      Width = 68
      Height = 13
      Caption = 'Authentication'
    end
    object Label12: TLabel
      Left = 64
      Top = 60
      Width = 13
      Height = 13
      Caption = 'Cc'
    end
    object Label13: TLabel
      Left = 246
      Top = 60
      Width = 19
      Height = 13
      Caption = 'Bcc'
    end
    object Label14: TLabel
      Left = 234
      Top = 132
      Width = 31
      Height = 13
      Caption = 'Priority'
    end
    object Label15: TLabel
      Left = 30
      Top = 156
      Width = 47
      Height = 13
      Caption = 'SSL-Type'
    end
    object Label16: TLabel
      Left = 228
      Top = 204
      Width = 37
      Height = 13
      Caption = 'Key File'
    end
    object Label17: TLabel
      Left = 39
      Top = 204
      Width = 38
      Height = 13
      Caption = 'Cert File'
    end
    object Label18: TLabel
      Left = 44
      Top = 180
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label19: TLabel
      Left = 226
      Top = 180
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label21: TLabel
      Left = 398
      Top = 204
      Width = 50
      Height = 13
      Caption = 'PassPrase'
    end
    object HostEdit: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Mail server hostname or IP address'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'HostEdit'
    end
    object FromEdit: TEdit
      Left = 80
      Top = 32
      Width = 121
      Height = 21
      Hint = 'Author'#39's EMail'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'FromEdit'
    end
    object ToEdit: TEdit
      Left = 270
      Top = 32
      Width = 121
      Height = 21
      Hint = 'Destinators, delimited by semicolons'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'ToEdit'
    end
    object SubjectEdit: TEdit
      Left = 80
      Top = 80
      Width = 121
      Height = 21
      Hint = 'Message subject'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'SubjectEdit'
    end
    object SignOnEdit: TEdit
      Left = 270
      Top = 80
      Width = 121
      Height = 21
      Hint = 'Signon message for the HELO/EHLO command'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'SignOnEdit'
    end
    object PortEdit: TEdit
      Left = 270
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Mail server port (should be smtp)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'PortEdit'
    end
    object ClearDisplayButton: TButton
      Left = 487
      Top = 88
      Width = 73
      Height = 17
      Hint = 'Clear info message memo'
      Caption = 'Clear &Info'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 31
      OnClick = ClearDisplayButtonClick
    end
    object ConnectButton: TButton
      Left = 407
      Top = 8
      Width = 73
      Height = 17
      Hint = 'Connect to the mail server'
      Caption = 'Connect'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = ConnectButtonClick
    end
    object HeloButton: TButton
      Left = 407
      Top = 28
      Width = 73
      Height = 17
      Hint = 'Send the signon message'
      Caption = 'Helo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = HeloButtonClick
    end
    object MailFromButton: TButton
      Left = 407
      Top = 108
      Width = 73
      Height = 17
      Hint = 'Send the mail originator'
      Caption = 'MailFrom'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 24
      OnClick = MailFromButtonClick
    end
    object RcptToButton: TButton
      Left = 407
      Top = 128
      Width = 73
      Height = 17
      Hint = 'Send the mail recipents'
      Caption = 'RcptTo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 25
      OnClick = RcptToButtonClick
    end
    object DataButton: TButton
      Left = 407
      Top = 148
      Width = 73
      Height = 17
      Hint = 'Send mail text and attached files'
      Caption = 'Data'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 26
      OnClick = DataButtonClick
    end
    object AbortButton: TButton
      Left = 487
      Top = 68
      Width = 73
      Height = 17
      Hint = 'Abort current operation and close'
      Caption = 'Abort'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 30
      OnClick = AbortButtonClick
    end
    object QuitButton: TButton
      Left = 487
      Top = 48
      Width = 73
      Height = 17
      Hint = 'Quit mail server'
      Caption = 'Quit'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 29
      OnClick = QuitButtonClick
    end
    object MailButton: TButton
      Left = 487
      Top = 28
      Width = 73
      Height = 17
      Hint = 'MailFrom, RcptTo and Data combined'
      Caption = 'Mail'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 28
      OnClick = MailButtonClick
    end
    object OpenButton: TButton
      Left = 487
      Top = 8
      Width = 73
      Height = 17
      Hint = 'Connect and Helo combined'
      Caption = 'Open'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 27
      OnClick = OpenButtonClick
    end
    object UsernameEdit: TEdit
      Left = 80
      Top = 104
      Width = 121
      Height = 21
      Hint = 'SMTP user name/account'
      TabOrder = 8
      Text = 'UsernameEdit'
    end
    object PasswordEdit: TEdit
      Left = 270
      Top = 104
      Width = 121
      Height = 21
      Hint = 'SMTP password'
      TabOrder = 9
      Text = 'PasswordEdit'
    end
    object AuthComboBox: TComboBox
      Left = 80
      Top = 128
      Width = 121
      Height = 21
      Hint = 'Select authentication type'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 10
      Items.Strings = (
        'None'
        'Plain'
        'Login'
        'CramMD5'
        'CramSHA1'
        'NTLM'
        'AutoSelect')
    end
    object EhloButton: TButton
      Left = 407
      Top = 48
      Width = 73
      Height = 17
      Hint = 'Send the EHLO command'
      Caption = 'Ehlo'
      TabOrder = 21
      OnClick = EhloButtonClick
    end
    object AuthButton: TButton
      Left = 407
      Top = 88
      Width = 73
      Height = 17
      Hint = 'Send the AUTH command'
      Caption = 'Auth'
      TabOrder = 23
      OnClick = AuthButtonClick
    end
    object CcEdit: TEdit
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      Hint = 'Carbon copies, coma separated list allowed'
      TabOrder = 4
      Text = 'CcEdit'
    end
    object BccEdit: TEdit
      Left = 270
      Top = 56
      Width = 121
      Height = 21
      Hint = 'Blind carbon copies, coma separated list allowed'
      TabOrder = 5
      Text = 'BccEdit'
    end
    object AllInOneButton: TButton
      Left = 487
      Top = 148
      Width = 73
      Height = 17
      Hint = 
        'Connect, Helo, MailFrom, RcptTo, Data and Quit all chained in a ' +
        'single action.'
      Caption = 'All In One'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 32
      OnClick = AllInOneButtonClick
    end
    object PriorityComboBox: TComboBox
      Left = 270
      Top = 128
      Width = 121
      Height = 21
      Hint = 'Select e-mail priority'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
      Items.Strings = (
        'Not specified'
        'Highest'
        'High'
        'Normal'
        'Low'
        'Lowest')
    end
    object TlsButton: TButton
      Left = 407
      Top = 68
      Width = 73
      Height = 17
      Hint = 'Start SMTP over TLS negotiation'
      Caption = 'StartTls'
      TabOrder = 22
      OnClick = TlsButtonClick
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 270
      Top = 154
      Width = 115
      Height = 17
      Hint = 'Verify peer'
      Caption = 'SslVerifyPeer'
      TabOrder = 13
    end
    object SmtpSslModeCombobox: TComboBox
      Left = 80
      Top = 152
      Width = 121
      Height = 21
      Hint = 'Select the SMTP SSL mode '
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 12
      Items.Strings = (
        'None'
        'Implicit (TLS Connection)'
        'Explicit (STARTTLS)')
    end
    object KeyEdit: TEdit
      Left = 270
      Top = 200
      Width = 121
      Height = 21
      Hint = 
        'Key file name, required only if the server requests a client cer' +
        'tificate'
      TabOrder = 17
      Text = 'KeyEdit'
    end
    object CertEdit: TEdit
      Left = 80
      Top = 200
      Width = 121
      Height = 21
      Hint = 
        'Certificate file name, required only if the server requests a cl' +
        'ient certificate'
      TabOrder = 16
      Text = 'CertEdit'
    end
    object CaFileEdit: TEdit
      Left = 80
      Top = 176
      Width = 121
      Height = 21
      Hint = 
        'CA file name, this is a file containing one or multiple, trusted' +
        ' CA certificates'
      TabOrder = 14
      Text = 'CaFileEdit'
    end
    object CaPathEdit: TEdit
      Left = 270
      Top = 176
      Width = 121
      Height = 21
      Hint = 'Path to a folder containing CA pem files.'
      TabOrder = 15
      Text = 'CaPathEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 454
      Top = 200
      Width = 107
      Height = 21
      Hint = 
        'Key Password, , required only if the server requests a client ce' +
        'rtificate'
      TabOrder = 18
      Text = 'PassPhraseEdit'
    end
  end
  object AttachPanel: TPanel
    Left = 0
    Top = 311
    Width = 601
    Height = 17
    Align = alTop
    TabOrder = 4
    object Label6: TLabel
      Left = 4
      Top = 2
      Width = 67
      Height = 13
      Caption = 'Attached files:'
    end
  end
  object FileAttachMemo: TMemo
    Left = 0
    Top = 328
    Width = 601
    Height = 49
    Hint = 'Enter the attached file path, one per line'
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'FileAttachMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 1
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 377
    Width = 601
    Height = 17
    Align = alTop
    TabOrder = 5
    object Label7: TLabel
      Left = 4
      Top = 2
      Width = 71
      Height = 13
      Caption = 'Info messages:'
    end
  end
  object SslSmtpClient: TSslSmtpCli
    Tag = 0
    ShareMode = smtpShareDenyWrite
    LocalAddr = '0.0.0.0'
    Port = 'smtp'
    AuthType = smtpAuthNone
    ConfirmReceipt = False
    HdrPriority = smtpPriorityNone
    CharSet = 'iso-8859-1'
    SendMode = smtpToSocket
    DefaultEncoding = smtpEnc7bit
    Allow8bitChars = True
    FoldHeaders = False
    WrapMessageText = False
    ContentType = smtpPlainText
    OwnHeaders = False
    OnDisplay = SmtpClientDisplay
    OnGetData = SslSmtpClientGetData
    OnHeaderLine = SslSmtpClientHeaderLine
    OnRequestDone = SslSmtpClientRequestDone
    OnBeforeFileOpen = SslSmtpClientBeforeFileOpen
    Timeout = 15
    MultiThreaded = False
    SslType = smtpTlsNone
    SslContext = SslContext1
    Left = 68
    Top = 252
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = [sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_CLIENT
    SslSessionTimeout = 300000
    SslSessionCacheSize = 20480
    Left = 104
    Top = 252
  end
end
