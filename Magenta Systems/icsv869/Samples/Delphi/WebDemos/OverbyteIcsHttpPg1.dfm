object HttpTestForm: THttpTestForm
  Left = 164
  Top = 119
  Caption = 'HttpPg - http://www.rtfm.be/fpiette'
  ClientHeight = 292
  ClientWidth = 364
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
    Width = 364
    Height = 179
    Hint = 'This are will display the webserver response.'
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 364
    Height = 113
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 36
      Width = 36
      Height = 13
      Caption = 'User ID'
    end
    object Label2: TLabel
      Left = 24
      Top = 62
      Width = 26
      Height = 13
      Caption = 'EMail'
    end
    object Label3: TLabel
      Left = 24
      Top = 12
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
      Hint = 'Enter the USERID you wants to send a message to'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'UserIDEdit'
    end
    object EMailEdit: TEdit
      Left = 80
      Top = 56
      Width = 273
      Height = 21
      Hint = 'Enter your own EMail address, will be used to send confirmation.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'EMailEdit'
    end
    object MessageEdit: TEdit
      Left = 80
      Top = 8
      Width = 273
      Height = 21
      Hint = 'Enter the message you wants to send.'
      CharCase = ecUpperCase
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'MESSAGEEDIT'
    end
    object SendButton: TButton
      Left = 296
      Top = 80
      Width = 59
      Height = 21
      Hint = 'Click here to send your message to the requested user.'
      Caption = '&Send'
      Default = True
      ParentShowHint = False
      ShowHint = True
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
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    Left = 16
    Top = 176
  end
end
