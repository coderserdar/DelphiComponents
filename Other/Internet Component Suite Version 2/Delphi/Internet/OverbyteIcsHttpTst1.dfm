object HttpTestForm: THttpTestForm
  Left = 61
  Top = 411
  Caption = 'Http Test - http://www.overbyte.be'
  ClientHeight = 521
  ClientWidth = 624
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
    Width = 624
    Height = 113
    Align = alTop
    TabOrder = 0
    OnResize = Panel1Resize
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
    object Label7: TLabel
      Left = 244
      Top = 66
      Width = 35
      Height = 13
      Caption = 'Version'
    end
    object Label8: TLabel
      Left = 244
      Top = 54
      Width = 20
      Height = 13
      Caption = 'Http'
    end
    object Label9: TLabel
      Left = 375
      Top = 54
      Width = 48
      Height = 13
      Caption = 'Post / Put'
    end
    object Label10: TLabel
      Left = 374
      Top = 65
      Width = 64
      Height = 13
      Caption = 'Content-Type'
    end
    object URLEdit: TEdit
      Left = 40
      Top = 8
      Width = 329
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
    object DataEdit: TEdit
      Left = 40
      Top = 32
      Width = 329
      Height = 21
      TabOrder = 1
      Text = 'DataEdit'
    end
    object DateTimeEdit: TEdit
      Left = 136
      Top = 80
      Width = 97
      Height = 21
      TabOrder = 4
      Text = 'DateTimeEdit'
    end
    object HttpVersionComboBox: TComboBox
      Left = 284
      Top = 56
      Width = 85
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      Items.Strings = (
        'HTTP/1.0'
        'HTTP/1.1')
    end
    object Panel2: TPanel
      Left = 512
      Top = 1
      Width = 111
      Height = 111
      Align = alRight
      TabOrder = 6
      object GetButton: TButton
        Left = 4
        Top = 8
        Width = 49
        Height = 21
        Caption = '&Get'
        Default = True
        TabOrder = 0
        OnClick = GetButtonClick
      end
      object HeadButton: TButton
        Left = 4
        Top = 56
        Width = 49
        Height = 21
        Caption = '&Head'
        TabOrder = 2
        OnClick = HeadButtonClick
      end
      object PostButton: TButton
        Left = 4
        Top = 32
        Width = 49
        Height = 21
        Caption = '&Post'
        TabOrder = 1
        OnClick = PostButtonClick
      end
      object AbortButton: TButton
        Left = 4
        Top = 80
        Width = 49
        Height = 21
        Caption = '&Abort'
        TabOrder = 3
        OnClick = AbortButtonClick
      end
      object ParseButton: TButton
        Left = 56
        Top = 80
        Width = 49
        Height = 21
        Caption = 'Par&se'
        TabOrder = 7
        OnClick = ParseButtonClick
      end
      object PutButton: TButton
        Left = 56
        Top = 32
        Width = 49
        Height = 21
        Caption = 'P&ut'
        TabOrder = 5
        OnClick = PutButtonClick
      end
      object CloseButton: TButton
        Left = 56
        Top = 8
        Width = 49
        Height = 21
        Caption = '&Close'
        TabOrder = 4
        OnClick = CloseButtonClick
      end
      object ClearButton: TButton
        Left = 56
        Top = 56
        Width = 49
        Height = 21
        Caption = 'Clea&r'
        TabOrder = 6
        OnClick = ClearButtonClick
      end
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 272
      Top = 84
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Header'
      TabOrder = 7
    end
    object PostContentTypeEdit: TEdit
      Left = 444
      Top = 56
      Width = 69
      Height = 21
      TabOrder = 8
      Text = 'PostContentTypeEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 113
    Width = 624
    Height = 148
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
    Top = 261
    Width = 624
    Height = 260
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
    IcsLogger = IcsLogger1
    OnHeaderData = HttpCli1HeaderData
    OnCommand = HttpCli1Command
    OnDocBegin = HttpCli1DocBegin
    OnDocEnd = HttpCli1DocEnd
    OnRequestDone = HttpCli1RequestDone
    OnLocationChange = HttpCli1LocationChange
    OnCookie = HttpCli1Cookie
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    Left = 16
    Top = 136
  end
  object IcsLogger1: TIcsLogger
    LogFileOption = lfoAppend
    LogOptions = [loDestOutDebug, loProtSpecErr, loProtSpecInfo]
    Left = 136
    Top = 136
  end
end
