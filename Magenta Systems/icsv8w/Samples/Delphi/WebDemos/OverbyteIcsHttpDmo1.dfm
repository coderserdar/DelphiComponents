object HttpToMemoForm: THttpToMemoForm
  Left = 124
  Top = 116
  Caption = 'Http://www.overbyte.be'
  ClientHeight = 280
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 388
    Height = 65
    Align = alTop
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 25
      Height = 13
      Caption = 'URL:'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object URLEdit: TEdit
      Left = 40
      Top = 8
      Width = 305
      Height = 21
      TabOrder = 0
      Text = 'http://www.faqs.org/rfcs/rfc775.txt'
    end
    object GoButton: TButton
      Left = 360
      Top = 8
      Width = 21
      Height = 21
      Caption = '&Go'
      Default = True
      TabOrder = 2
      OnClick = GoButtonClick
    end
    object ProxyEdit: TEdit
      Left = 40
      Top = 32
      Width = 305
      Height = 21
      TabOrder = 1
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 65
    Width = 388
    Height = 215
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
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
  object HttpCli1: THttpCli
    LocalAddr = '0.0.0.0'
    ProxyPort = '80'
    Agent = 'Mozilla/3.0 (compatible)'
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
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    Left = 32
    Top = 88
  end
end
