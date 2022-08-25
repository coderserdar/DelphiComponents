object BasicFtpClientForm: TBasicFtpClientForm
  Left = 81
  Top = 115
  Caption = 'Basic FTP Client with ICS - http://www.overbyte.be'
  ClientHeight = 626
  ClientWidth = 474
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
    Width = 474
    Height = 41
    Align = alTop
    TabOrder = 0
    object GetButton: TButton
      Left = 376
      Top = 12
      Width = 75
      Height = 21
      Caption = '&Get'
      Default = True
      TabOrder = 0
      OnClick = GetButtonClick
    end
    object FtpUrlEdit: TEdit
      Left = 12
      Top = 12
      Width = 345
      Height = 21
      TabOrder = 1
      Text = 'FtpUrlEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 474
    Height = 585
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
    OnDisplay = FtpClient1Display
    OnRequestDone = FtpClient1RequestDone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Left = 24
    Top = 80
  end
end
