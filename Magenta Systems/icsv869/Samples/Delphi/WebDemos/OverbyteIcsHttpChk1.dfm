object CheckUrlForm: TCheckUrlForm
  Left = 205
  Top = 122
  Caption = 'CheckUrlForm'
  ClientHeight = 251
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 25
    Height = 13
    Caption = 'URL:'
  end
  object ResultLabel: TLabel
    Left = 16
    Top = 40
    Width = 56
    Height = 13
    Caption = 'ResultLabel'
  end
  object URLEdit: TEdit
    Left = 44
    Top = 8
    Width = 189
    Height = 21
    TabOrder = 0
    Text = 'http://localhost/index.htm'
  end
  object CheckButton: TButton
    Left = 244
    Top = 8
    Width = 47
    Height = 21
    Caption = 'Check'
    Default = True
    TabOrder = 1
    OnClick = CheckButtonClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 56
    Width = 273
    Height = 177
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
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
    OnHeaderData = HttpCli1HeaderData
    OnRequestDone = HttpCli1RequestDone
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    Left = 92
    Top = 80
  end
end
