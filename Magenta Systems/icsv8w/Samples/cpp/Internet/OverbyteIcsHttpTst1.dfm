object HttpTestForm: THttpTestForm
  Left = 214
  Top = 97
  Caption = 'Http Test - http://www.overbyte.be'
  ClientHeight = 415
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 368
    Height = 113
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 11
      Width = 22
      Height = 13
      Caption = 'URL'
    end
    object Label2: TLabel
      Left = 8
      Top = 59
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object Label3: TLabel
      Left = 8
      Top = 35
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object Label4: TLabel
      Left = 8
      Top = 83
      Width = 124
      Height = 13
      Caption = 'Modified Since Date/Time'
    end
    object Label5: TLabel
      Left = 136
      Top = 66
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label6: TLabel
      Left = 136
      Top = 54
      Width = 26
      Height = 13
      Caption = 'Proxy'
    end
    object GetButton: TButton
      Left = 296
      Top = 8
      Width = 49
      Height = 21
      Caption = '&Get'
      Default = True
      TabOrder = 4
      OnClick = GetButtonClick
    end
    object URLEdit: TEdit
      Left = 40
      Top = 8
      Width = 249
      Height = 21
      Hint = 'protocol://[user[:password]@]server[:port]/path'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'http://www.overbyte.be'
    end
    object ProxyHostEdit: TEdit
      Left = 40
      Top = 56
      Width = 89
      Height = 21
      TabOrder = 2
      Text = 'ProxyHostEdit'
    end
    object ProxyPortEdit: TEdit
      Left = 168
      Top = 56
      Width = 65
      Height = 21
      TabOrder = 3
      Text = 'ProxyPortEdit'
    end
    object PostButton: TButton
      Left = 296
      Top = 32
      Width = 49
      Height = 21
      Caption = '&Post'
      TabOrder = 5
      OnClick = PostButtonClick
    end
    object Check64Button: TButton
      Left = 240
      Top = 80
      Width = 49
      Height = 21
      Caption = '&Check64'
      TabOrder = 6
      OnClick = Check64ButtonClick
    end
    object DataEdit: TEdit
      Left = 40
      Top = 32
      Width = 249
      Height = 21
      TabOrder = 1
      Text = 'DataEdit'
    end
    object DateTimeEdit: TEdit
      Left = 136
      Top = 80
      Width = 97
      Height = 21
      TabOrder = 7
      Text = 'DateTimeEdit'
    end
    object HeadButton: TButton
      Left = 296
      Top = 56
      Width = 49
      Height = 21
      Caption = '&Head'
      TabOrder = 8
      OnClick = HeadButtonClick
    end
    object AbortButton: TButton
      Left = 296
      Top = 80
      Width = 49
      Height = 21
      Caption = '&Abort'
      Enabled = False
      TabOrder = 9
      OnClick = AbortButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 113
    Width = 368
    Height = 136
    Align = alTop
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
  object DocumentMemo: TMemo
    Left = 0
    Top = 249
    Width = 368
    Height = 166
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DocumentMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object HttpCli1: THttpCli
    URL = 'http://www.rtfm.be'
    LocalAddr = '0.0.0.0'
    Proxy = 'intsrv02'
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
    OnDocBegin = HttpCli1DocBegin
    OnDocEnd = HttpCli1DocEnd
    OnRequestDone = HttpCli1RequestDone
    SocksAuthentication = socksNoAuthentication
    Left = 16
    Top = 136
  end
end
