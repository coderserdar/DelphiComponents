object FtpReceiveForm: TFtpReceiveForm
  Left = 5
  Top = 85
  Caption = 'FTP - http://www.overbyte.be'
  ClientHeight = 410
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 241
    Width = 631
    Height = 169
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
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 121
    Width = 631
    Height = 120
    Align = alTop
    TabOrder = 1
    object InfoLabel: TLabel
      Left = 512
      Top = 6
      Width = 44
      Height = 13
      Caption = 'InfoLabel'
    end
    object StateLabel: TLabel
      Left = 512
      Top = 25
      Width = 51
      Height = 13
      Caption = 'StateLabel'
    end
    object ExitButton: TButton
      Left = 512
      Top = 88
      Width = 105
      Height = 25
      Hint = 'Terminate the program'
      Caption = '&Exit Program'
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
      TabOrder = 1
      OnClick = OpenAsyncButtonClick
    end
    object QuitAsyncButton: TButton
      Left = 8
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Quit'
      TabOrder = 2
      OnClick = QuitAsyncButtonClick
    end
    object CwdAsyncButton: TButton
      Left = 254
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Cwd'
      TabOrder = 3
      OnClick = CwdAsyncButtonClick
    end
    object UserAsyncButton: TButton
      Left = 90
      Top = 6
      Width = 81
      Height = 17
      Caption = 'User'
      TabOrder = 4
      OnClick = UserAsyncButtonClick
    end
    object PassAsyncButton: TButton
      Left = 172
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Pass'
      TabOrder = 5
      OnClick = PassAsyncButtonClick
    end
    object ConnectAsyncButton: TButton
      Left = 90
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Connect'
      TabOrder = 6
      OnClick = ConnectAsyncButtonClick
    end
    object GetAsyncButton: TButton
      Left = 8
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Get'
      TabOrder = 7
      OnClick = GetAsyncButtonClick
    end
    object ReceiveAsyncButton: TButton
      Left = 8
      Top = 60
      Width = 81
      Height = 17
      Caption = 'Receive'
      TabOrder = 8
      OnClick = ReceiveAsyncButtonClick
    end
    object AbortAsyncButton: TButton
      Left = 172
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Abort'
      TabOrder = 9
      OnClick = AbortAsyncButtonClick
    end
    object DirAsyncButton: TButton
      Left = 172
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Dir'
      TabOrder = 10
      OnClick = DirAsyncButtonClick
    end
    object DirectoryAsyncButton: TButton
      Left = 172
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Directory'
      TabOrder = 11
      OnClick = DirectoryAsyncButtonClick
    end
    object LsAsyncButton: TButton
      Left = 254
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Ls'
      TabOrder = 12
      OnClick = LsAsyncButtonClick
    end
    object ListAsyncButton: TButton
      Left = 254
      Top = 96
      Width = 81
      Height = 17
      Caption = 'List'
      TabOrder = 13
      OnClick = ListAsyncButtonClick
    end
    object SystAsyncButton: TButton
      Left = 418
      Top = 6
      Width = 81
      Height = 17
      Caption = 'Syst'
      TabOrder = 14
      OnClick = SystAsyncButtonClick
    end
    object SystemAsyncButton: TButton
      Left = 418
      Top = 24
      Width = 81
      Height = 17
      Caption = 'System'
      TabOrder = 15
      OnClick = SystemAsyncButtonClick
    end
    object FileSizeAsyncButton: TButton
      Left = 336
      Top = 60
      Width = 81
      Height = 17
      Caption = 'FileSize'
      TabOrder = 16
      OnClick = FileSizeAsyncButtonClick
    end
    object SizeAsyncButton: TButton
      Left = 336
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Size'
      TabOrder = 17
      OnClick = SizeAsyncButtonClick
    end
    object MkdAsyncButton: TButton
      Left = 418
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Mkd'
      TabOrder = 18
      OnClick = MkdAsyncButtonClick
    end
    object MkdirAsyncButton: TButton
      Left = 418
      Top = 60
      Width = 81
      Height = 17
      Caption = 'Mkdir'
      TabOrder = 19
      OnClick = MkdirAsyncButtonClick
    end
    object RmdAsyncButton: TButton
      Left = 418
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Rmd'
      TabOrder = 20
      OnClick = RmdAsyncButtonClick
    end
    object RmdirAsyncButton: TButton
      Left = 418
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Rmdir'
      TabOrder = 21
      OnClick = RmdirAsyncButtonClick
    end
    object RenAsyncButton: TButton
      Left = 8
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Ren'
      TabOrder = 22
      OnClick = RenAsyncButtonClick
    end
    object RenameAsyncButton: TButton
      Left = 8
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Rename'
      TabOrder = 23
      OnClick = RenameAsyncButtonClick
    end
    object DeleAsyncButton: TButton
      Left = 90
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Dele'
      TabOrder = 24
      OnClick = DeleAsyncButtonClick
    end
    object DeleteAsyncButton: TButton
      Left = 90
      Top = 96
      Width = 81
      Height = 17
      Caption = 'Delete'
      TabOrder = 25
      OnClick = DeleteAsyncButtonClick
    end
    object PwdAsyncButton: TButton
      Left = 254
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Pwd'
      TabOrder = 26
      OnClick = PwdAsyncButtonClick
    end
    object QuoteAsyncButton: TButton
      Left = 336
      Top = 78
      Width = 81
      Height = 17
      Caption = 'Quote'
      TabOrder = 27
      OnClick = QuoteAsyncButtonClick
    end
    object DoQuoteAsyncButton: TButton
      Left = 336
      Top = 96
      Width = 81
      Height = 17
      Caption = 'DoQuote'
      TabOrder = 28
      OnClick = DoQuoteAsyncButtonClick
    end
    object PutAsyncButton: TButton
      Left = 90
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Put'
      TabOrder = 29
      OnClick = PutAsyncButtonClick
    end
    object TransmitAsyncButton: TButton
      Left = 90
      Top = 60
      Width = 81
      Height = 17
      Caption = 'Transmit'
      TabOrder = 30
      OnClick = TransmitAsyncButtonClick
    end
    object TypeSetAsyncButton: TButton
      Left = 336
      Top = 6
      Width = 81
      Height = 17
      Caption = 'TypeSet'
      TabOrder = 31
      OnClick = TypeSetAsyncButtonClick
    end
    object RestGetAsyncButton: TButton
      Left = 172
      Top = 42
      Width = 81
      Height = 17
      Caption = 'RestGet'
      TabOrder = 32
      OnClick = RestGetAsyncButtonClick
    end
    object RestartGetAsyncButton: TButton
      Left = 172
      Top = 60
      Width = 81
      Height = 17
      Caption = 'RestartGet'
      TabOrder = 33
      OnClick = RestartGetAsyncButtonClick
    end
    object CDupAsyncButton: TButton
      Left = 336
      Top = 24
      Width = 81
      Height = 17
      Caption = 'CDup'
      TabOrder = 34
      OnClick = CDupAsyncButtonClick
    end
    object ClearButton: TButton
      Left = 512
      Top = 60
      Width = 105
      Height = 25
      Caption = 'Clear Display'
      TabOrder = 35
      OnClick = ClearButtonClick
    end
    object AppendFileAsyncButton: TButton
      Left = 254
      Top = 60
      Width = 81
      Height = 17
      Caption = 'AppendFile'
      TabOrder = 36
      OnClick = AppendFileAsyncButtonClick
    end
    object AppendAsyncButton: TButton
      Left = 254
      Top = 42
      Width = 81
      Height = 17
      Caption = 'Append'
      TabOrder = 37
      OnClick = AppendAsyncButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 631
    Height = 121
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 11
      Top = 12
      Width = 50
      Height = 13
      Caption = 'HostName'
    end
    object Label3: TLabel
      Left = 21
      Top = 67
      Width = 41
      Height = 13
      Caption = 'Host File'
    end
    object Label4: TLabel
      Left = 10
      Top = 39
      Width = 50
      Height = 13
      Caption = 'UserName'
    end
    object Label5: TLabel
      Left = 224
      Top = 39
      Width = 49
      Height = 13
      Caption = 'PassWord'
    end
    object Label2: TLabel
      Left = 16
      Top = 95
      Width = 45
      Height = 13
      Caption = 'Local File'
    end
    object Label6: TLabel
      Left = 264
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object HostNameEdit: TEdit
      Left = 88
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Host where the file is located'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'HostNameEdit'
    end
    object HostFileEdit: TEdit
      Left = 304
      Top = 64
      Width = 313
      Height = 21
      Hint = 'Enter the host file name or directory name for Mkdir and Rmdir'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'HostFileEdit'
    end
    object UserNameEdit: TEdit
      Left = 88
      Top = 36
      Width = 121
      Height = 21
      Hint = 'User name used to log on the host'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'UserNameEdit'
    end
    object PassWordEdit: TEdit
      Left = 304
      Top = 36
      Width = 169
      Height = 21
      Hint = 'Password used to validate user access'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'PassWordEdit'
    end
    object cbDisplay: TCheckBox
      Left = 480
      Top = 12
      Width = 121
      Height = 17
      Hint = 'Enable data display during file transfert'
      Caption = 'Display Data'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
    end
    object LocalFileEdit: TEdit
      Left = 88
      Top = 91
      Width = 529
      Height = 21
      Hint = 'Enter local file name and path or new file name for rename'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'LocalFileEdit'
    end
    object cbBinary: TCheckBox
      Left = 376
      Top = 12
      Width = 105
      Height = 17
      Hint = 'Select to use binary mode transfert'
      Caption = 'Binary Mode'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 7
    end
    object HostDirEdit: TEdit
      Left = 88
      Top = 64
      Width = 209
      Height = 21
      Hint = 'Enter the host directory where the file is located'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'HostDirEdit'
    end
    object PortEdit: TEdit
      Left = 304
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 6
      Text = 'PortEdit'
    end
    object SyncCheckBox: TCheckBox
      Left = 480
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Synchronous'
      TabOrder = 9
    end
  end
  object FtpClient1: TFtpClient
    Timeout = 15
    MultiThreaded = False
    Port = 'ftp'
    CodePage = 0
    DataPortRangeStart = 0
    DataPortRangeEnd = 0
    LocalAddr = '0.0.0.0'
    DisplayFileFlag = False
    Binary = True
    ShareMode = ftpShareExclusive
    Options = [ftpAcceptLF]
    ConnectionType = ftpDirect
    Language = 'EN'
    OnProgress64 = FtpClient1Progress64
    OnSessionConnected = FtpClient1SessionConnected
    OnSessionClosed = FtpClient1SessionClosed
    OnRequestDone = FtpClient1RequestDone
    OnStateChange = FtpClient1StateChange
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Left = 24
    Top = 288
  end
end
