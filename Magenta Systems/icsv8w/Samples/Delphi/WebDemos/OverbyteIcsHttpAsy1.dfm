object HttpAsyForm: THttpAsyForm
  Left = 145
  Top = 240
  Caption = 'Http Async Demo - http://www.overbyte.be'
  ClientHeight = 304
  ClientWidth = 624
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
  object URLListBox: TListBox
    Left = 0
    Top = 57
    Width = 273
    Height = 247
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = URLListBoxClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 57
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 22
      Height = 13
      Caption = 'URL'
    end
    object Label7: TLabel
      Left = 468
      Top = 14
      Width = 35
      Height = 13
      Caption = 'Version'
    end
    object Label8: TLabel
      Left = 468
      Top = 5
      Width = 20
      Height = 13
      Caption = 'Http'
    end
    object URLEdit: TEdit
      Left = 40
      Top = 8
      Width = 233
      Height = 21
      TabOrder = 0
      Text = 'URLEdit'
    end
    object AddButton: TButton
      Left = 280
      Top = 8
      Width = 65
      Height = 21
      Caption = 'Add URL'
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object HeaderCheckBox: TCheckBox
      Left = 360
      Top = 4
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Header'
      TabOrder = 2
    end
    object DataCheckBox: TCheckBox
      Left = 360
      Top = 20
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Data'
      TabOrder = 3
    end
    object SimultCheckBox: TCheckBox
      Left = 360
      Top = 36
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Simultaneous'
      TabOrder = 4
    end
    object HttpVersionComboBox: TComboBox
      Left = 508
      Top = 6
      Width = 85
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      Items.Strings = (
        'HTTP/1.0'
        'HTTP/1.1')
    end
  end
  object Panel2: TPanel
    Left = 273
    Top = 57
    Width = 80
    Height = 247
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object ExecButton: TButton
      Left = 8
      Top = 16
      Width = 65
      Height = 21
      Caption = 'Execute'
      TabOrder = 0
      OnClick = ExecButtonClick
    end
    object RemoveButton: TButton
      Left = 8
      Top = 48
      Width = 65
      Height = 21
      Caption = 'Remove'
      TabOrder = 1
      OnClick = RemoveButtonClick
    end
    object ReplaceButton: TButton
      Left = 8
      Top = 80
      Width = 65
      Height = 21
      Caption = 'Replace'
      TabOrder = 2
      OnClick = ReplaceButtonClick
    end
    object ClearDisplayButton: TButton
      Left = 8
      Top = 208
      Width = 65
      Height = 21
      Caption = 'ClearDisplay'
      TabOrder = 3
      OnClick = ClearDisplayButtonClick
    end
    object AbortButton: TButton
      Left = 8
      Top = 112
      Width = 65
      Height = 21
      Caption = 'Abort'
      TabOrder = 4
      OnClick = AbortButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 353
    Top = 57
    Width = 271
    Height = 247
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 3
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
    OnRequestDone = HttpCli1RequestDone
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfIPv4
    SocketErrs = wsErrTech
    Left = 32
    Top = 176
  end
end
