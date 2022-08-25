object HttpTestForm: THttpTestForm
  Left = 193
  Top = 246
  Caption = 'HttpAsp - http://www.overbyte.be'
  ClientHeight = 292
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 129
    Width = 364
    Height = 163
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 364
    Height = 129
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 16
      Width = 38
      Height = 13
      Caption = 'Supplier'
    end
    object Label2: TLabel
      Left = 24
      Top = 48
      Width = 15
      Height = 13
      Caption = 'Pin'
    end
    object SupplierIDEdit: TEdit
      Left = 80
      Top = 16
      Width = 273
      Height = 21
      TabOrder = 0
      Text = 'SupplierIDEdit'
    end
    object PinEdit: TEdit
      Left = 80
      Top = 40
      Width = 273
      Height = 21
      TabOrder = 1
      Text = 'PinEdit'
    end
    object SendButton: TButton
      Left = 280
      Top = 72
      Width = 75
      Height = 25
      Caption = '&Send'
      Default = True
      TabOrder = 2
      OnClick = SendButtonClick
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
