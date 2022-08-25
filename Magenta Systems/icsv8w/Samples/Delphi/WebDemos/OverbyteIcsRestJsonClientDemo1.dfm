object GoogleSearchJsonClientForm: TGoogleSearchJsonClientForm
  Left = 54
  Top = 411
  Caption = 'OverByte ICS - Google Search JSON Client demo'
  ClientHeight = 334
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 251
    Width = 645
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 41
    ExplicitWidth = 143
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 645
    Height = 41
    Align = alTop
    TabOrder = 0
    object GoogleGetButton: TButton
      Left = 384
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Get'
      Default = True
      TabOrder = 0
      OnClick = GoogleGetButtonClick
    end
    object GSearch: TEdit
      Left = 8
      Top = 8
      Width = 370
      Height = 21
      TabOrder = 1
      Text = 'json progdigy'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 254
    Width = 645
    Height = 80
    Align = alBottom
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 1
  end
  object ResultStringGrid: TStringGrid
    Left = 0
    Top = 41
    Width = 645
    Height = 210
    Align = alClient
    ColCount = 2
    DefaultRowHeight = 19
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 2
  end
  object HttpCli1: THttpCli
    LocalAddr = '0.0.0.0'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0 (compatible; ICS)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    MultiThreaded = False
    RequestVer = '1.0'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = []
    Timeout = 30
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfIPv4
    Left = 68
    Top = 96
  end
end
