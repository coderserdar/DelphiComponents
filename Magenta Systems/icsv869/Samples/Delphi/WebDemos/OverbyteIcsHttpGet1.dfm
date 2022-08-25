object HttpGetForm: THttpGetForm
  Left = 128
  Top = 123
  Caption = 'Http Get - http://www.orverbyte.be'
  ClientHeight = 194
  ClientWidth = 339
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
  object Label1: TLabel
    Left = 24
    Top = 12
    Width = 22
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 24
    Top = 75
    Width = 26
    Height = 13
    Caption = 'Proxy'
  end
  object Label3: TLabel
    Left = 224
    Top = 68
    Width = 26
    Height = 13
    Caption = 'Proxy'
  end
  object Label4: TLabel
    Left = 224
    Top = 82
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label5: TLabel
    Left = 8
    Top = 43
    Width = 44
    Height = 13
    Caption = 'FileName'
  end
  object InfoLabel: TLabel
    Left = 5
    Top = 128
    Width = 44
    Height = 13
    Caption = 'InfoLabel'
  end
  object Label10: TLabel
    Left = 5
    Top = 107
    Width = 153
    Height = 13
    Caption = 'Maximum Bandwidth (bytes/sec)'
    WordWrap = True
  end
  object URLEdit: TEdit
    Left = 56
    Top = 8
    Width = 281
    Height = 21
    TabOrder = 0
    Text = 'URLEdit'
  end
  object ProxyHostEdit: TEdit
    Left = 56
    Top = 72
    Width = 161
    Height = 21
    TabOrder = 2
    Text = 'ProxyHostEdit'
  end
  object ProxyPortEdit: TEdit
    Left = 264
    Top = 72
    Width = 73
    Height = 21
    TabOrder = 3
    Text = 'ProxyPortEdit'
  end
  object FileNameEdit: TEdit
    Left = 56
    Top = 40
    Width = 281
    Height = 21
    TabOrder = 1
    Text = 'FileNameEdit'
  end
  object GetButton: TButton
    Left = 111
    Top = 154
    Width = 75
    Height = 25
    Caption = '&Get'
    Default = True
    TabOrder = 4
    OnClick = GetButtonClick
  end
  object AbortButton: TButton
    Left = 197
    Top = 155
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Abort'
    Enabled = False
    TabOrder = 5
    OnClick = AbortButtonClick
  end
  object BandwidthLimitEdit: TEdit
    Left = 170
    Top = 104
    Width = 65
    Height = 21
    TabOrder = 6
    Text = 'BandwidthLimitEdit'
  end
  object HttpCli1: THttpCli
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    ProxyPort = '80'
    Agent = 'Mozilla/3.0 (compatible)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ResponseNoException = False
    ContentTypePost = 'application/x-www-form-urlencoded'
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
    OnDocData = HttpCli1DocData
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfIPv4
    SocketErrs = wsErrTech
    Left = 16
    Top = 148
  end
end
