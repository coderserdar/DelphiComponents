object FtpReceiveForm: TFtpReceiveForm
  Left = 227
  Top = 114
  Caption = 'FTP - http://www.overbyte.be'
  ClientHeight = 605
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object DisplayMemo: TMemo
    Left = 0
    Top = 425
    Width = 635
    Height = 180
    Hint = 'This area show the activity with the host'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 149
    Width = 635
    Height = 276
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object InfoLabel: TLabel
      Left = 512
      Top = 6
      Width = 44
      Height = 13
      Caption = 'InfoLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object StateLabel: TLabel
      Left = 512
      Top = 25
      Width = 51
      Height = 13
      Caption = 'StateLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 506
      Top = 140
      Width = 51
      Height = 13
      Caption = 'Resume at'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 35
      Top = 179
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label12: TLabel
      Left = 282
      Top = 179
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label13: TLabel
      Left = 19
      Top = 203
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object Label14: TLabel
      Left = 14
      Top = 227
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label15: TLabel
      Left = 20
      Top = 244
      Width = 54
      Height = 13
      Caption = 'Acceptable'
    end
    object Label16: TLabel
      Left = 48
      Top = 256
      Width = 25
      Height = 13
      Caption = 'hosts'
    end
    object Label17: TLabel
      Left = 504
      Top = 179
      Width = 51
      Height = 13
      Caption = 'PBSZ Size'
    end
    object Label18: TLabel
      Left = 496
      Top = 203
      Width = 59
      Height = 13
      Caption = 'PROT Level'
    end
    object Label19: TLabel
      Left = 268
      Top = 228
      Width = 47
      Height = 13
      Caption = 'SSL Type'
    end
    object Label20: TLabel
      Left = 276
      Top = 202
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label21: TLabel
      Left = 512
      Top = 228
      Width = 42
      Height = 13
      Caption = 'SSL Port'
    end
    object Label22: TLabel
      Left = 498
      Top = 251
      Width = 56
      Height = 13
      Caption = 'Max KBytes'
    end
    object ExitButton: TButton
      Left = 512
      Top = 78
      Width = 105
      Height = 17
      Hint = 'Terminate the program'
      Caption = '&Exit Program'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ExitButtonClick
    end
    object OpenAsyncButton: TButton
      Left = 8
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Open'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = OpenAsyncButtonClick
    end
    object QuitAsyncButton: TButton
      Left = 8
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Quit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = QuitAsyncButtonClick
    end
    object CwdAsyncButton: TButton
      Left = 254
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Cwd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = CwdAsyncButtonClick
    end
    object UserAsyncButton: TButton
      Left = 90
      Top = 6
      Width = 81
      Height = 17
      Caption = 'User'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = UserAsyncButtonClick
    end
    object PassAsyncButton: TButton
      Left = 172
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Pass'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = PassAsyncButtonClick
    end
    object ConnectAsyncButton: TButton
      Left = 90
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Connect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = ConnectAsyncButtonClick
    end
    object GetAsyncButton: TButton
      Left = 8
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Get'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = GetAsyncButtonClick
    end
    object ReceiveAsyncButton: TButton
      Left = 8
      Top = 60
      Width = 81
      Height = 17
      Caption = 'Receive'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      OnClick = ReceiveAsyncButtonClick
    end
    object AbortAsyncButton: TButton
      Left = 172
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Abort'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnClick = AbortAsyncButtonClick
    end
    object DirAsyncButton: TButton
      Left = 172
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Dir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = DirAsyncButtonClick
    end
    object DirectoryAsyncButton: TButton
      Left = 172
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Directory'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnClick = DirectoryAsyncButtonClick
    end
    object LsAsyncButton: TButton
      Left = 254
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Ls'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
      OnClick = LsAsyncButtonClick
    end
    object ListAsyncButton: TButton
      Left = 254
      Top = 96
      Width = 81
      Height = 17
      Caption = 'List'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
      OnClick = ListAsyncButtonClick
    end
    object SystAsyncButton: TButton
      Left = 418
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Syst'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 16
      OnClick = SystAsyncButtonClick
    end
    object SystemAsyncButton: TButton
      Left = 418
      Top = 24
      Width = 81
      Height = 17
      Caption = 'System'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 17
      OnClick = SystemAsyncButtonClick
    end
    object FileSizeAsyncButton: TButton
      Left = 336
      Top = 60
      Width = 81
      Height = 17
      Caption = 'FileSize'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 18
      OnClick = FileSizeAsyncButtonClick
    end
    object SizeAsyncButton: TButton
      Left = 336
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 19
      OnClick = SizeAsyncButtonClick
    end
    object MkdAsyncButton: TButton
      Left = 418
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Mkd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 20
      OnClick = MkdAsyncButtonClick
    end
    object MkdirAsyncButton: TButton
      Left = 418
      Top = 60
      Width = 81
      Height = 17
      Caption = 'Mkdir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 21
      OnClick = MkdirAsyncButtonClick
    end
    object RmdAsyncButton: TButton
      Left = 418
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Rmd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 22
      OnClick = RmdAsyncButtonClick
    end
    object RmdirAsyncButton: TButton
      Left = 418
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Rmdir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 23
      OnClick = RmdirAsyncButtonClick
    end
    object RenAsyncButton: TButton
      Left = 8
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Ren'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 25
      OnClick = RenAsyncButtonClick
    end
    object RenameAsyncButton: TButton
      Left = 8
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Rename'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 26
      OnClick = RenameAsyncButtonClick
    end
    object DeleAsyncButton: TButton
      Left = 90
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Dele'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 27
      OnClick = DeleAsyncButtonClick
    end
    object DeleteAsyncButton: TButton
      Left = 90
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Delete'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 28
      OnClick = DeleteAsyncButtonClick
    end
    object PwdAsyncButton: TButton
      Left = 254
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Pwd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 29
      OnClick = PwdAsyncButtonClick
    end
    object QuoteAsyncButton: TButton
      Left = 336
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Quote'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 30
      OnClick = QuoteAsyncButtonClick
    end
    object DoQuoteAsyncButton: TButton
      Left = 336
      Top = 96
      Width = 81
      Height = 17
      Caption = 'DoQuote'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 31
      OnClick = DoQuoteAsyncButtonClick
    end
    object PutAsyncButton: TButton
      Left = 90
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Put'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 32
      OnClick = PutAsyncButtonClick
    end
    object TransmitAsyncButton: TButton
      Left = 90
      Top = 60
      Width = 81
      Height = 17
      Caption = 'Transmit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 33
      OnClick = TransmitAsyncButtonClick
    end
    object TypeSetAsyncButton: TButton
      Left = 336
      Top = 6
      Width = 81
      Height = 17
      Caption = 'TypeSet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 34
      OnClick = TypeSetAsyncButtonClick
    end
    object RestGetAsyncButton: TButton
      Left = 172
      Top = 42
      Width = 81
      Height = 17
      Caption = 'RestGet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 35
      OnClick = RestGetAsyncButtonClick
    end
    object RestartGetAsyncButton: TButton
      Left = 172
      Top = 60
      Width = 81
      Height = 17
      Caption = 'RestartGet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 36
      OnClick = RestartGetAsyncButtonClick
    end
    object CDupAsyncButton: TButton
      Left = 336
      Top = 24
      Width = 81
      Height = 17
      Caption = 'CDup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 37
      OnClick = CDupAsyncButtonClick
    end
    object ClearButton: TButton
      Left = 512
      Top = 60
      Width = 105
      Height = 17
      Caption = 'Clear Display'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 38
      OnClick = ClearButtonClick
    end
    object AppendFileAsyncButton: TButton
      Left = 254
      Top = 60
      Width = 81
      Height = 17
      Caption = 'AppendFile'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 39
      OnClick = AppendFileAsyncButtonClick
    end
    object AppendAsyncButton: TButton
      Left = 254
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Append'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 40
      OnClick = AppendAsyncButtonClick
    end
    object Button1: TButton
      Left = 512
      Top = 42
      Width = 105
      Height = 17
      Caption = 'Button1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 41
      OnClick = Button1Click
    end
    object RestPutAsyncButton: TButton
      Left = 8
      Top = 114
      Width = 81
      Height = 17
      Caption = 'RestPut'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 42
      OnClick = RestPutAsyncButtonClick
    end
    object RestartPutAsyncButton: TButton
      Left = 8
      Top = 132
      Width = 81
      Height = 17
      Caption = 'RestartPut'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 43
      OnClick = RestartPutAsyncButtonClick
    end
    object ResumeAtEdit: TEdit
      Left = 562
      Top = 136
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 44
      Text = 'ResumeAtEdit'
    end
    object TransmitUsingStreamButton: TButton
      Left = 336
      Top = 114
      Width = 163
      Height = 17
      Caption = 'Transmit using a stream'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 45
      OnClick = TransmitUsingStreamButtonClick
    end
    object ReceiveUsingStreamButton: TButton
      Left = 336
      Top = 132
      Width = 163
      Height = 17
      Caption = 'Receive using a stream'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 46
      OnClick = ReceiveUsingStreamButtonClick
    end
    object StressPutButton: TButton
      Left = 512
      Top = 96
      Width = 105
      Height = 17
      Caption = 'Stress Put'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 24
      OnClick = StressPutButtonClick
    end
    object AbortXferAsyncButton: TButton
      Left = 254
      Top = 114
      Width = 81
      Height = 17
      Caption = 'AbortXfer'
      TabOrder = 48
      OnClick = AbortXferAsyncButtonClick
    end
    object AuthButton: TButton
      Left = 254
      Top = 132
      Width = 81
      Height = 17
      Caption = 'Auth TLS/SSL'
      TabOrder = 49
      OnClick = AuthButtonClick
    end
    object AcctAsyncButton: TButton
      Left = 90
      Top = 114
      Width = 81
      Height = 17
      Caption = 'Acct'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 50
      OnClick = AcctAsyncButtonClick
    end
    object CertFileEdit: TEdit
      Left = 76
      Top = 176
      Width = 165
      Height = 21
      Hint = 'Enter the certificate file name. PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 52
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 320
      Top = 176
      Width = 165
      Height = 21
      Hint = 'Enter the CA certificate file name.  PEM file format.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 56
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 320
      Top = 200
      Width = 165
      Height = 21
      Hint = 'Enter CA certicate directory (can be empty).'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 57
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 76
      Top = 200
      Width = 165
      Height = 21
      Hint = 
        'Enter the private file name. Could be the same as CertFile if th' +
        'is file contains both certificate and private key.  PEM file for' +
        'mat.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 53
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 76
      Top = 224
      Width = 165
      Height = 21
      Hint = 'Enter pass phrase protecting private key file.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 54
      Text = 'PassPhraseEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 262
      Top = 250
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 59
    end
    object AcceptableHostsEdit: TEdit
      Left = 76
      Top = 248
      Width = 165
      Height = 21
      TabOrder = 55
      Text = 'AcceptableHostsEdit'
    end
    object ProtButton: TButton
      Left = 172
      Top = 132
      Width = 81
      Height = 17
      Caption = 'Prot'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      OnClick = ProtButtonClick
    end
    object PbszButton: TButton
      Left = 172
      Top = 114
      Width = 81
      Height = 17
      Caption = 'Pbsz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = PbszButtonClick
    end
    object PbszSizeEdit: TEdit
      Left = 560
      Top = 176
      Width = 57
      Height = 21
      TabOrder = 61
      Text = 'PbszSizeEdit'
    end
    object ProtLevelEdit: TEdit
      Left = 560
      Top = 200
      Width = 57
      Height = 21
      TabOrder = 62
      Text = 'ProtLevelEdit'
    end
    object FeatButton: TButton
      Left = 90
      Top = 132
      Width = 81
      Height = 17
      Caption = 'Feat'
      TabOrder = 51
      OnClick = FeatButtonClick
    end
    object SessCacheCheckBox: TCheckBox
      Left = 364
      Top = 250
      Width = 121
      Height = 17
      Alignment = taLeftJustify
      Caption = 'SSL Session Caching'
      TabOrder = 60
    end
    object SslTypeComboBox: TComboBox
      Left = 320
      Top = 224
      Width = 165
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 58
      Items.Strings = (
        'sslTypeNone'
        'sslTypeAuthTls'
        'sslTypeAuthSsl'
        'sslTypeImplicit')
    end
    object SslPortEdit: TEdit
      Left = 560
      Top = 224
      Width = 57
      Height = 21
      TabOrder = 63
      Text = 'SslPortEdit'
    end
    object SslRenegotiateButton: TButton
      Left = 336
      Top = 150
      Width = 163
      Height = 17
      Caption = 'SSL Renegotiate CtrlSocket'
      TabOrder = 65
      OnClick = SslRenegotiateButtonClick
    end
    object CccButton: TButton
      Left = 8
      Top = 150
      Width = 81
      Height = 17
      Caption = 'Ccc'
      TabOrder = 64
      OnClick = CccButtonClick
    end
    object ClearTraceButton: TButton
      Left = 512
      Top = 114
      Width = 105
      Height = 17
      Caption = 'Clear Trace'
      TabOrder = 47
      OnClick = ClearTraceButtonClick
    end
    object OptsAsyncButton: TButton
      Left = 90
      Top = 150
      Width = 81
      Height = 17
      Caption = 'Opts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 66
      OnClick = OptsAsyncButtonClick
    end
    object MaxKbEdit: TEdit
      Left = 560
      Top = 247
      Width = 57
      Height = 21
      TabOrder = 67
      Text = 'MaxKbEdit'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 149
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 11
      Top = 12
      Width = 50
      Height = 13
      Caption = 'HostName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 21
      Top = 95
      Width = 41
      Height = 13
      Caption = 'Host File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 10
      Top = 39
      Width = 50
      Height = 13
      Caption = 'UserName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 220
      Top = 39
      Width = 49
      Height = 13
      Caption = 'PassWord'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 123
      Width = 45
      Height = 13
      Caption = 'Local File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 252
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 4
      Top = 63
      Width = 54
      Height = 13
      Caption = 'Port Range'
    end
    object Label9: TLabel
      Left = 137
      Top = 63
      Width = 9
      Height = 13
      Caption = 'to'
    end
    object Label10: TLabel
      Left = 232
      Top = 64
      Width = 40
      Height = 13
      Caption = 'Account'
    end
    object Label23: TLabel
      Left = 462
      Top = 74
      Width = 25
      Height = 13
      Caption = 'Opts:'
    end
    object HostNameEdit: TEdit
      Left = 72
      Top = 8
      Width = 137
      Height = 21
      Hint = 'Host where the file is located'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'HostNameEdit'
    end
    object HostFileEdit: TEdit
      Left = 212
      Top = 92
      Width = 261
      Height = 21
      Hint = 'Enter the host file name or directory name for Mkdir and Rmdir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'HostFileEdit'
    end
    object UserNameEdit: TEdit
      Left = 72
      Top = 36
      Width = 137
      Height = 21
      Hint = 'User name used to log on the host'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'UserNameEdit'
    end
    object PassWordEdit: TEdit
      Left = 276
      Top = 36
      Width = 137
      Height = 21
      Hint = 'Password used to validate user access'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'PassWordEdit'
    end
    object DisplayCheckBox: TCheckBox
      Left = 492
      Top = 8
      Width = 117
      Height = 17
      Hint = 'Enable data display during file transfert'
      Caption = 'Display Data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
    end
    object LocalFileEdit: TEdit
      Left = 72
      Top = 119
      Width = 545
      Height = 21
      Hint = 'Enter local file name and path or new file name for rename'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'LocalFileEdit'
    end
    object BinaryCheckBox: TCheckBox
      Left = 406
      Top = 10
      Width = 81
      Height = 17
      Hint = 'Select to use binary mode transfert'
      Caption = 'Binary Mode'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 8
    end
    object HostDirEdit: TEdit
      Left = 72
      Top = 92
      Width = 137
      Height = 21
      Hint = 'Enter the host directory where the file is located'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'HostDirEdit'
    end
    object PortEdit: TEdit
      Left = 276
      Top = 8
      Width = 53
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = 'PortEdit'
    end
    object SyncCheckBox: TCheckBox
      Left = 492
      Top = 38
      Width = 117
      Height = 17
      Caption = 'Synchronous'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
    object PassiveCheckBox: TCheckBox
      Left = 492
      Top = 23
      Width = 117
      Height = 17
      Caption = 'Passive Mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
    end
    object NoAutoResumeAtCheckBox: TCheckBox
      Left = 492
      Top = 53
      Width = 117
      Height = 17
      Caption = 'NoAutoResumeAt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
    end
    object DataPortRangeStartEdit: TEdit
      Left = 72
      Top = 60
      Width = 57
      Height = 21
      TabOrder = 13
      Text = 'DataPortRangeStartEdit'
    end
    object DataPortRangeEndEdit: TEdit
      Left = 156
      Top = 60
      Width = 53
      Height = 21
      TabOrder = 14
      Text = 'DataPortRangeEndEdit'
    end
    object AccountEdit: TEdit
      Left = 276
      Top = 60
      Width = 137
      Height = 21
      Hint = 'Account used to validate user access'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'AccountEdit'
    end
    object OptsEdit: TComboBox
      Left = 491
      Top = 71
      Width = 126
      Height = 21
      ItemHeight = 13
      TabOrder = 15
      Text = 'UTF8 ON'
      Items.Strings = (
        'UTF8 ON'
        'UTF8 OFF')
    end
  end
  object FtpClient1: TSslFtpClient
    Timeout = 15
    Port = 'ftp'
    CodePage = 0
    DataPortRangeStart = 0
    DataPortRangeEnd = 0
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    DisplayFileFlag = False
    Binary = True
    ShareMode = ftpShareExclusive
    Options = [ftpAcceptLF]
    ConnectionType = ftpDirect
    ProxyPort = 'ftp'
    Language = 'EN'
    OnDisplayFile = FtpClient1DisplayFile
    OnProgress64 = FtpClient1Progress64
    OnSessionConnected = FtpClient1SessionConnected
    OnSessionClosed = FtpClient1SessionClosed
    OnRequestDone = FtpClient1RequestDone
    OnStateChange = FtpClient1StateChange
    IcsLogger = IcsLogger1
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    SocketFamily = sfIPv4
    SocketErrs = wsErrTech
    SslContext = SslContext1
    SslType = sslTypeNone
    SslAcceptableHosts.Strings = (
      '')
    ProtLevel = 'P'
    PBSZSize = 0
    OnSslVerifyPeer = FtpClient1SslVerifyPeer
    OnSslCliGetSession = FtpClient1SslCliGetSession
    OnSslCliNewSession = FtpClient1SslCliNewSession
    OnSslHandshakeDone = FtpClient1SslHandshakeDone
    OnSslCliCertRequest = FtpClient1SslCliCertRequest
    Left = 32
    Top = 440
  end
  object SslContext1: TSslContext
    IcsLogger = IcsLogger1
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
    SslOptions = [sslOpt_NO_SSLv2]
    SslOptions2 = []
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslBestVer_CLIENT
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHAuto
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 90
    Top = 440
  end
  object IcsLogger1: TIcsLogger
    TimeStampFormatString = 'hh:nn:ss:zzz'
    TimeStampSeparator = ' '
    LogFileOption = lfoOverwrite
    LogFileName = 'Debug_SslFtpTst.txt'
    LogOptions = []
    Left = 152
    Top = 440
  end
end
