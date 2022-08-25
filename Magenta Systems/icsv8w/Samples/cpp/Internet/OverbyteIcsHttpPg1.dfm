object HttpTestForm: THttpTestForm
  Left = 240
  Top = 120
  Caption = 'HttpPg - http://www.overbyte.be'
  ClientHeight = 266
  ClientWidth = 383
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
    Top = 113
    Width = 383
    Height = 153
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 383
    Height = 113
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 40
      Width = 36
      Height = 13
      Caption = 'User ID'
    end
    object Label2: TLabel
      Left = 24
      Top = 64
      Width = 26
      Height = 13
      Caption = 'EMail'
    end
    object Label3: TLabel
      Left = 24
      Top = 16
      Width = 43
      Height = 13
      Caption = 'Message'
    end
    object Label4: TLabel
      Left = 24
      Top = 84
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object UserIDEdit: TEdit
      Left = 80
      Top = 32
      Width = 273
      Height = 21
      TabOrder = 1
      Text = 'UserIDEdit'
    end
    object EMailEdit: TEdit
      Left = 80
      Top = 56
      Width = 273
      Height = 21
      TabOrder = 2
      Text = 'EMailEdit'
    end
    object MessageEdit: TEdit
      Left = 80
      Top = 8
      Width = 273
      Height = 21
      TabOrder = 0
      Text = 'MessageEdit'
    end
    object SendButton: TButton
      Left = 296
      Top = 80
      Width = 59
      Height = 21
      Caption = '&Send'
      Default = True
      TabOrder = 4
      OnClick = SendButtonClick
    end
    object ProxyEdit: TEdit
      Left = 80
      Top = 80
      Width = 145
      Height = 21
      Hint = 'Enter the proxy or leave blank if none.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'ProxyEdit'
    end
    object AbortButton: TButton
      Left = 232
      Top = 80
      Width = 59
      Height = 21
      Caption = '&Abort'
      Enabled = False
      TabOrder = 5
      OnClick = AbortButtonClick
    end
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
    SocksAuthentication = socksNoAuthentication
    Left = 16
    Top = 176
  end
end
