object MultipartFtpDownloadForm: TMultipartFtpDownloadForm
  Left = 111
  Top = 34
  Caption = 'Multipart FTP Downloader - ICS - (c) 2007 Fran'#231'ois PIETTE'
  ClientHeight = 448
  ClientWidth = 548
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
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 548
    Height = 197
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 20
      Top = 18
      Width = 54
      Height = 13
      Caption = 'FTP Server'
    end
    object Label2: TLabel
      Left = 247
      Top = 18
      Width = 37
      Height = 13
      Caption = 'Ftp Port'
    end
    object Label3: TLabel
      Left = 38
      Top = 66
      Width = 36
      Height = 13
      Caption = 'FTP Dir'
    end
    object Label4: TLabel
      Left = 4
      Top = 114
      Width = 70
      Height = 13
      Caption = 'Local File Path'
    end
    object Label5: TLabel
      Left = 5
      Top = 42
      Width = 69
      Height = 13
      Caption = 'FTP Usercode'
    end
    object Label6: TLabel
      Left = 216
      Top = 42
      Width = 69
      Height = 13
      Caption = 'FTP Password'
    end
    object Label7: TLabel
      Left = 5
      Top = 90
      Width = 70
      Height = 13
      Caption = 'FTP File Name'
    end
    object PartCountLabel: TLabel
      Left = 240
      Top = 138
      Width = 49
      Height = 13
      Caption = 'Part count'
    end
    object Label8: TLabel
      Left = 8
      Top = 164
      Width = 64
      Height = 13
      Caption = 'Assumed size'
    end
    object FtpServerEdit: TEdit
      Left = 80
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'FtpServerEdit'
    end
    object FtpPortEdit: TEdit
      Left = 288
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'FtpPortEdit'
    end
    object FtpDirEdit: TEdit
      Left = 80
      Top = 64
      Width = 329
      Height = 21
      TabOrder = 4
      Text = 'FtpDirEdit'
    end
    object LocalFilePathEdit: TEdit
      Left = 80
      Top = 112
      Width = 329
      Height = 21
      TabOrder = 6
      Text = 'LocalFilePathEdit'
    end
    object FtpUserEdit: TEdit
      Left = 80
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'FtpUserEdit'
    end
    object FtpPassEdit: TEdit
      Left = 288
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'FtpPassEdit'
    end
    object PassiveCheckBox: TCheckBox
      Left = 7
      Top = 138
      Width = 86
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Passive mode'
      TabOrder = 7
    end
    object BinaryCheckBox: TCheckBox
      Left = 122
      Top = 138
      Width = 80
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Binary mode'
      TabOrder = 8
    end
    object FtpFileNameEdit: TEdit
      Left = 80
      Top = 88
      Width = 329
      Height = 21
      TabOrder = 5
      Text = 'FtpFileNameEdit'
    end
    object PartCountEdit: TEdit
      Left = 296
      Top = 136
      Width = 28
      Height = 21
      TabOrder = 9
      Text = 'PartCountEdit'
    end
    object AbortButton: TButton
      Left = 416
      Top = 42
      Width = 75
      Height = 21
      Caption = '&Abort'
      TabOrder = 10
      OnClick = AbortButtonClick
    end
    object DownloadButton: TButton
      Left = 416
      Top = 16
      Width = 75
      Height = 21
      Caption = '&Download'
      TabOrder = 11
      OnClick = DownloadButtonClick
    end
    object ResumeButton: TButton
      Left = 416
      Top = 88
      Width = 75
      Height = 21
      Caption = '&Resume'
      TabOrder = 12
      OnClick = ResumeButtonClick
    end
    object PauseButton: TButton
      Left = 416
      Top = 113
      Width = 75
      Height = 21
      Caption = '&Pause'
      TabOrder = 13
      OnClick = PauseButtonClick
    end
    object ClearButton: TButton
      Left = 416
      Top = 144
      Width = 75
      Height = 21
      Caption = '&Clear display'
      TabOrder = 14
      OnClick = ClearButtonClick
    end
    object AssumedSizeEdit: TEdit
      Left = 80
      Top = 160
      Width = 121
      Height = 21
      TabOrder = 15
      Text = 'AssumedSizeEdit'
    end
    object TestButton: TButton
      Left = 264
      Top = 164
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 16
      OnClick = TestButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 197
    Width = 548
    Height = 210
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
  object BottomPanel: TPanel
    Left = 0
    Top = 407
    Width = 548
    Height = 41
    Align = alBottom
    TabOrder = 2
    object CountLabel: TLabel
      Left = 8
      Top = 4
      Width = 54
      Height = 13
      Caption = 'CountLabel'
    end
    object MPBar: TMultiProgressBar
      Left = 1
      Top = 23
      Width = 546
      Height = 17
      Align = alBottom
    end
  end
  object MPFtp: TMultipartFtpDownloader
    Passive = False
    Binary = False
    PartCount = 0
    TimeoutValue = 30000
    OnDisplay = MPFtpDisplay
    OnRequestDone = MPFtpRequestDone
    OnProgressAddSegment = MPFtpProgressAddSegment
    OnProgressSetPosition = MPFtpProgressSetPosition
    OnShowStats = MPFtpShowStats
    Left = 120
    Top = 216
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
    ProxyPort = 'ftp'
    Language = 'EN'
    OnDisplay = FtpClient1Display
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    SocketFamily = sfIPv4
    Left = 244
    Top = 220
  end
end
