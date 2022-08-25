object FtpReceiveForm: TFtpReceiveForm
  Left = 227
  Top = 183
  Caption = 'FTP - http://www.overbyte.be'
  ClientHeight = 558
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object DisplayMemo: TMemo
    Left = 0
    Top = 401
    Width = 628
    Height = 157
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
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 173
    Width = 628
    Height = 228
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
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
      Left = 504
      Top = 122
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
      Left = 504
      Top = 142
      Width = 43
      Height = 13
      Caption = 'Start Pos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 504
      Top = 162
      Width = 40
      Height = 13
      Caption = 'End Pos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 504
      Top = 182
      Width = 56
      Height = 13
      Caption = 'Max KBytes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 505
      Top = 202
      Width = 48
      Height = 13
      Caption = 'Language'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
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
      TabOrder = 12
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
      TabOrder = 13
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
      TabOrder = 14
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
      TabOrder = 15
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
      TabOrder = 16
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
      TabOrder = 17
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
      TabOrder = 18
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
      TabOrder = 19
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
      TabOrder = 20
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
      TabOrder = 21
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
      TabOrder = 22
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
      TabOrder = 23
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
      TabOrder = 24
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
      TabOrder = 25
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
      TabOrder = 26
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
      TabOrder = 27
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
      TabOrder = 28
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
      TabOrder = 29
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
      TabOrder = 30
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
      TabOrder = 31
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
      TabOrder = 32
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
      TabOrder = 33
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
      TabOrder = 34
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
      TabOrder = 35
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
      TabOrder = 36
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
      TabOrder = 37
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
      TabOrder = 38
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
      TabOrder = 39
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
      TabOrder = 40
      OnClick = RestartPutAsyncButtonClick
    end
    object ResumeAtEdit: TEdit
      Left = 560
      Top = 118
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 41
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
      TabOrder = 42
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
      TabOrder = 43
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
      TabOrder = 44
      OnClick = StressPutButtonClick
    end
    object AbortXferAsyncButton: TButton
      Left = 254
      Top = 114
      Width = 81
      Height = 17
      Caption = 'AbortXfer'
      TabOrder = 45
      OnClick = AbortXferAsyncButtonClick
    end
    object AuthSslButton: TButton
      Left = 254
      Top = 132
      Width = 81
      Height = 17
      Caption = 'AuthSsl'
      TabOrder = 46
    end
    object AcctAsyncButton: TButton
      Left = 90
      Top = 114
      Width = 81
      Height = 17
      Caption = 'Account'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 47
      OnClick = AcctAsyncButtonClick
    end
    object SiteExecAsyncButton: TButton
      Left = 8
      Top = 186
      Width = 81
      Height = 17
      Caption = 'SiteExec'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 48
      OnClick = SiteExecAsyncButtonClick
    end
    object ClntAsyncButton: TButton
      Left = 172
      Top = 114
      Width = 81
      Height = 17
      Caption = 'Client'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 49
      OnClick = ClntAsyncButtonClick
    end
    object MlsdAsyncButton: TButton
      Left = 90
      Top = 132
      Width = 81
      Height = 17
      Caption = 'MLSD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 50
      OnClick = MlsdAsyncButtonClick
    end
    object MlstAsyncButton: TButton
      Left = 172
      Top = 132
      Width = 81
      Height = 17
      Caption = 'MLST'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 51
      OnClick = MlstAsyncButtonClick
    end
    object XCmlsdAsyncButton: TButton
      Left = 90
      Top = 168
      Width = 81
      Height = 17
      Caption = 'XCMLSD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 52
      OnClick = XCmlsdAsyncButtonClick
    end
    object FeatAsyncButton: TButton
      Left = 8
      Top = 150
      Width = 81
      Height = 17
      Caption = 'Feat'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 53
      OnClick = FeatAsyncButtonClick
    end
    object XDmlsdAsyncButton: TButton
      Left = 172
      Top = 168
      Width = 81
      Height = 17
      Caption = 'XDMLSD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 54
      OnClick = XDmlsdAsyncButtonClick
    end
    object Md5AsyncButton: TButton
      Left = 254
      Top = 150
      Width = 81
      Height = 17
      Caption = 'MD5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 55
      OnClick = Md5AsyncButtonClick
    end
    object MdtmAsyncButton: TButton
      Left = 90
      Top = 150
      Width = 81
      Height = 17
      Caption = 'MDTM'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 56
      OnClick = MdtmAsyncButtonClick
    end
    object XcrcAsyncButton: TButton
      Left = 418
      Top = 150
      Width = 81
      Height = 17
      Caption = 'XCRC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 57
      OnClick = XcrcAsyncButtonClick
    end
    object MfmtAsyncButton: TButton
      Left = 172
      Top = 150
      Width = 81
      Height = 17
      Caption = 'MFMT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 58
      OnClick = MfmtAsyncButtonClick
    end
    object SiteZoneAsyncButton: TButton
      Left = 8
      Top = 168
      Width = 81
      Height = 17
      Caption = 'SiteZone'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 59
      OnClick = SiteZoneAsyncButtonClick
    end
    object SiteIndexAsyncButton: TButton
      Left = 254
      Top = 168
      Width = 81
      Height = 17
      Caption = 'SiteIndex'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 60
      OnClick = SiteIndexAsyncButtonClick
    end
    object SitePaswdAsyncButton: TButton
      Left = 336
      Top = 168
      Width = 81
      Height = 17
      Caption = 'SitePaswd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 61
      OnClick = SitePaswdAsyncButtonClick
    end
    object SiteMsgAsyncButton: TButton
      Left = 418
      Top = 168
      Width = 81
      Height = 17
      Caption = 'SiteMsg'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 62
      OnClick = SiteMsgAsyncButtonClick
    end
    object XMd5AsyncButton: TButton
      Left = 336
      Top = 150
      Width = 81
      Height = 17
      Caption = 'XMD5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 63
      OnClick = XMd5AsyncButtonClick
    end
    object AlloAsyncButton: TButton
      Left = 90
      Top = 186
      Width = 81
      Height = 17
      Caption = 'Allo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 64
      OnClick = AlloAsyncButtonClick
    end
    object PosStartEdit: TEdit
      Left = 560
      Top = 138
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 65
      Text = 'PosStartEdit'
    end
    object PosEndEdit: TEdit
      Left = 560
      Top = 158
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 66
      Text = 'PosEndEdit'
    end
    object CombAsyncButton: TButton
      Left = 172
      Top = 186
      Width = 81
      Height = 17
      Caption = 'Comb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 67
      OnClick = CombAsyncButtonClick
    end
    object OptsAsyncButton: TButton
      Left = 254
      Top = 186
      Width = 81
      Height = 17
      Caption = 'Opts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 68
      OnClick = OptsAsyncButtonClick
    end
    object ModeZAsyncButton: TButton
      Left = 336
      Top = 186
      Width = 81
      Height = 17
      Caption = 'Mode Z'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 69
      OnClick = ModeZAsyncButtonClick
    end
    object ModeSAsyncButton: TButton
      Left = 418
      Top = 186
      Width = 81
      Height = 17
      Caption = 'Mode S'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 70
      OnClick = ModeSAsyncButtonClick
    end
    object MaxKB: TEdit
      Left = 560
      Top = 178
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 71
      Text = 'MaxKB'
    end
    object ConnectHostAsyncButton: TButton
      Left = 8
      Top = 204
      Width = 81
      Height = 17
      Caption = 'Connect Host'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 72
      OnClick = ConnectHostAsyncButtonClick
    end
    object HostAsyncButton: TButton
      Left = 90
      Top = 204
      Width = 81
      Height = 17
      Caption = 'Host'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 73
      OnClick = HostAsyncButtonClick
    end
    object ReinAsyncButton: TButton
      Left = 172
      Top = 204
      Width = 81
      Height = 17
      Caption = 'Reinitialize'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 74
      OnClick = ReinAsyncButtonClick
    end
    object LangAsyncButton: TButton
      Left = 254
      Top = 204
      Width = 81
      Height = 17
      Caption = 'LANG'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 75
      OnClick = LangAsyncButtonClick
    end
    object Lanugage: TEdit
      Left = 560
      Top = 200
      Width = 57
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 76
      Text = 'EN'
    end
    object ConnectFeatAsyncButton: TButton
      Left = 336
      Top = 204
      Width = 81
      Height = 17
      Caption = 'Connect Feat'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 77
      OnClick = ConnectFeatAsyncButtonClick
    end
  end
  object SettingsPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 628
    Height = 173
    ActivePage = MainSettingsTabSheet
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object MainSettingsTabSheet: TTabSheet
      Caption = 'Main'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 620
        Height = 145
        Align = alClient
        BevelOuter = bvNone
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
          Left = 20
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
          Left = 11
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
          Left = 7
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
        object Label14: TLabel
          Left = 485
          Top = 100
          Width = 53
          Height = 13
          Caption = 'CodePage:'
        end
        object Label16: TLabel
          Left = 460
          Top = 76
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
          Left = 491
          Top = 4
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
          TabOrder = 10
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
          TabOrder = 8
          Text = 'LocalFileEdit'
        end
        object BinaryCheckBox: TCheckBox
          Left = 376
          Top = 12
          Width = 93
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
          TabOrder = 9
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
          Width = 85
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
          Left = 491
          Top = 34
          Width = 117
          Height = 17
          Caption = 'Synchronous'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
        end
        object PassiveCheckBox: TCheckBox
          Left = 491
          Top = 19
          Width = 117
          Height = 17
          Caption = 'Passive Mode'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
        end
        object NoAutoResumeAtCheckBox: TCheckBox
          Left = 491
          Top = 49
          Width = 117
          Height = 17
          Caption = 'NoAutoResumeAt'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 13
        end
        object DataPortRangeStartEdit: TEdit
          Left = 72
          Top = 60
          Width = 57
          Height = 21
          TabOrder = 14
          Text = 'DataPortRangeStartEdit'
        end
        object DataPortRangeEndEdit: TEdit
          Left = 156
          Top = 60
          Width = 53
          Height = 21
          TabOrder = 15
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
        object CodePageEdit: TEdit
          Left = 560
          Top = 95
          Width = 57
          Height = 21
          Hint = '0 = System default code page, 65001 = UTF-8'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Text = 'CodePageEdit'
          OnChange = CodePageEditChange
        end
        object OptsEdit: TComboBox
          Left = 491
          Top = 73
          Width = 126
          Height = 21
          TabOrder = 16
          Text = 'UTF8 ON'
          Items.Strings = (
            'UTF8 ON'
            'UTF8 OFF'
            'MODE Z LEVEL 8'
            'MODE Z LEVEL 4'
            'MODE Z LEVEL 2')
        end
      end
    end
    object ConnectionTypeTabSheet: TTabSheet
      Caption = 'Connection Type'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label22: TLabel
        Left = 39
        Top = 10
        Width = 24
        Height = 13
        Caption = 'Type'
      end
      object Label23: TLabel
        Left = 225
        Top = 11
        Width = 84
        Height = 13
        Caption = 'HTTP proxy auth.'
      end
      object Label18: TLabel
        Left = 14
        Top = 37
        Width = 49
        Height = 13
        Caption = 'Proxy host'
      end
      object Label19: TLabel
        Left = 16
        Top = 63
        Width = 47
        Height = 13
        Caption = 'Proxy port'
      end
      object Label20: TLabel
        Left = 260
        Top = 37
        Width = 49
        Height = 13
        Caption = 'Proxy user'
      end
      object Label21: TLabel
        Left = 235
        Top = 64
        Width = 74
        Height = 13
        Caption = 'Proxy password'
      end
      object Label17: TLabel
        Left = 16
        Top = 92
        Width = 431
        Height = 13
        Caption = 
          'Note: Property Passive is set (overwritten) when HTTP or SOCKS p' +
          'roxy connection is used.'
      end
      object Label24: TLabel
        Left = 16
        Top = 106
        Width = 459
        Height = 13
        Caption = 
          'The ftpProxy connection-type supports type USER@HOST FTP-proxies' +
          ', active or passive mode. '
      end
      object ProxyTypeComboBox: TComboBox
        Left = 71
        Top = 7
        Width = 145
        Height = 21
        TabOrder = 0
        Text = 'ProxyTypeComboBox'
        OnCloseUp = ProxyTypeComboBoxCloseUp
      end
      object ProxyHttpAuthTypeComboBox: TComboBox
        Left = 317
        Top = 8
        Width = 145
        Height = 21
        TabOrder = 1
        Text = 'ProxyHttpAuthTypeComboBox'
        OnCloseUp = ProxyHttpAuthTypeComboBoxCloseUp
      end
      object ProxyHostEdit: TEdit
        Left = 71
        Top = 34
        Width = 145
        Height = 21
        TabOrder = 2
        Text = 'ProxyHostEdit'
      end
      object ProxyPortEdit: TEdit
        Left = 71
        Top = 61
        Width = 56
        Height = 21
        TabOrder = 3
        Text = 'ProxyPortEdit'
      end
      object ProxyUserEdit: TEdit
        Left = 317
        Top = 34
        Width = 145
        Height = 21
        TabOrder = 4
        Text = 'ProxyUserEdit'
      end
      object ProxyPasswordEdit: TEdit
        Left = 317
        Top = 60
        Width = 145
        Height = 21
        TabOrder = 5
        Text = 'ProxyPasswordEdit'
      end
    end
  end
  object FtpClient1: TFtpClient
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
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    SocketFamily = sfIPv4
    SocketErrs = wsErrTech
    Left = 38
    Top = 446
  end
end
