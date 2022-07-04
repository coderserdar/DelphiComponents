object BrowserForm: TBrowserForm
  Left = 247
  Top = 234
  Width = 678
  Height = 433
  ActiveControl = WebBrowserControl1
  Caption = 'Internet Explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 298
    Width = 670
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object ListBox1: TListBox
    Left = 0
    Top = 301
    Width = 670
    Height = 105
    Align = alBottom
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 670
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 10
      Width = 22
      Height = 13
      Caption = 'URL'
    end
    object Edit1: TEdit
      Left = 48
      Top = 6
      Width = 528
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'http://www.microsoft.com'
    end
    object Button1: TButton
      Left = 579
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'GO'
      Default = True
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object WebBrowserControl1: TWebBrowserControl
    Left = 0
    Top = 33
    Width = 670
    Height = 265
    Align = alClient
    TabOrder = 2
    OnStatusTextChange = WebBrowserControl1StatusTextChange
    OnNavigateComplete2 = WebBrowserControl1NavigateComplete2
    UIFlags = []
    DCTLFlags = []
    ControlData = {
      4C0000003F450000631B00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object WebProvider1: TWebProvider
    XMLParserLevel = MSXML20
    Active = False
    Items = <
      item
        XSLT = False
        CacheTemplates = False
        MimeType = 'image/jpeg'
        Enabled = True
        Default = False
        Location = 'www.microsoft.com/library/toolbar/images/mslogo.gif'
        Name = 'WebDisp2'
        OnAction = WebProvider1Items1Action
      end
      item
        XSLT = False
        CacheTemplates = False
        MimeType = 'text/html'
        Enabled = True
        Default = True
        Location = '*'
        Name = 'WebDisp1'
        OnAction = WebProvider1Items0Action
      end>
    RefreshDelay = 300
    Protocol = 'http'
    Cached = False
    Left = 96
    Top = 64
  end
end
