object HttpPostForm: THttpPostForm
  Left = 161
  Top = 486
  Caption = 'Http Post - http://www.orverbyte.be'
  ClientHeight = 213
  ClientWidth = 371
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
    Width = 371
    Height = 113
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 379
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 50
      Height = 13
      Caption = 'First Name'
    end
    object Label2: TLabel
      Left = 12
      Top = 40
      Width = 51
      Height = 13
      Caption = 'Last Name'
    end
    object Label3: TLabel
      Left = 8
      Top = 64
      Width = 55
      Height = 13
      Caption = 'Action URL'
    end
    object Label4: TLabel
      Left = 164
      Top = 88
      Width = 177
      Height = 13
      Caption = 'Use WebServ demo as server !'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object FirstNameEdit: TEdit
      Left = 72
      Top = 8
      Width = 289
      Height = 21
      TabOrder = 0
      Text = 'FirstNameEdit'
    end
    object LastNameEdit: TEdit
      Left = 72
      Top = 36
      Width = 289
      Height = 21
      TabOrder = 1
      Text = 'LastNameEdit'
    end
    object ActionURLEdit: TEdit
      Left = 72
      Top = 60
      Width = 289
      Height = 21
      TabOrder = 2
      Text = 'ActionURLEdit'
    end
    object PostButton: TButton
      Left = 72
      Top = 84
      Width = 75
      Height = 21
      Caption = '&Post'
      TabOrder = 3
      OnClick = PostButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 113
    Width = 371
    Height = 100
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
    ExplicitWidth = 379
    ExplicitHeight = 102
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
    OnRequestDone = HttpCli1RequestDone
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    Left = 80
    Top = 140
  end
end
