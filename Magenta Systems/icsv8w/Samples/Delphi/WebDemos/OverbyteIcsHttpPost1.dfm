object HttpPostForm: THttpPostForm
  Left = 250
  Top = 147
  Caption = 'Http Post - http://www.orverbyte.be'
  ClientHeight = 572
  ClientWidth = 608
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
    Width = 608
    Height = 281
    Align = alTop
    TabOrder = 0
    DesignSize = (
      608
      281)
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 50
      Height = 13
      Caption = 'First Name'
    end
    object Label2: TLabel
      Left = 199
      Top = 11
      Width = 51
      Height = 13
      Caption = 'Last Name'
    end
    object Label3: TLabel
      Left = 13
      Top = 35
      Width = 55
      Height = 13
      Caption = 'Action URL'
    end
    object Label4: TLabel
      Left = 400
      Top = 43
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
    object Label5: TLabel
      Left = 12
      Top = 179
      Width = 38
      Height = 13
      Caption = 'FilePath'
    end
    object Shape1: TShape
      Left = 14
      Top = 62
      Width = 571
      Height = 3
    end
    object Label6: TLabel
      Left = 12
      Top = 148
      Width = 59
      Height = 13
      Caption = 'Upload URL'
    end
    object Label10: TLabel
      Left = 259
      Top = 86
      Width = 153
      Height = 13
      Caption = 'Maximum Bandwidth (bytes/sec)'
      WordWrap = True
    end
    object Label7: TLabel
      Left = 12
      Top = 206
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object FirstNameEdit: TEdit
      Left = 72
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'FirstNameEdit'
    end
    object LastNameEdit: TEdit
      Left = 256
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'LastNameEdit'
    end
    object ActionURLEdit: TEdit
      Left = 72
      Top = 35
      Width = 303
      Height = 21
      TabOrder = 2
      Text = 'ActionURLEdit'
    end
    object PostButton: TButton
      Left = 424
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Post Fields'
      TabOrder = 3
      OnClick = PostButtonClick
    end
    object FileNameEdit: TEdit
      Left = 83
      Top = 175
      Width = 290
      Height = 21
      TabOrder = 7
      Text = 'FileNameEdit'
    end
    object UploadButton: TButton
      Left = 480
      Top = 174
      Width = 81
      Height = 21
      Caption = '&Upload File'
      TabOrder = 10
      OnClick = UploadButtonClick
    end
    object UploadURLEdit: TEdit
      Left = 83
      Top = 148
      Width = 335
      Height = 21
      TabOrder = 6
      Text = 'UploadURLEdit'
    end
    object BandwidthLimitEdit: TEdit
      Left = 434
      Top = 83
      Width = 65
      Height = 21
      TabOrder = 5
      Text = 'BandwidthLimitEdit'
    end
    object UploadMethod: TRadioGroup
      Left = 12
      Top = 71
      Width = 230
      Height = 71
      Caption = 'File Upload Method'
      ItemIndex = 0
      Items.Strings = (
        'POST binary file - name as page'
        'POST binary file - name as argument'
        'PUT binary file - name as argument'
        'POST MIME type multipart/form-data')
      TabOrder = 4
    end
    object SelectFile: TBitBtn
      Left = 379
      Top = 172
      Width = 31
      Height = 25
      Hint = 'Select File to Upload'
      Anchors = []
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = SelectFileClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
        333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
        300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
        333337F373F773333333303330033333333337F3377333333333303333333333
        333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
        333337777F337F33333330330BB00333333337F373F773333333303330033333
        333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
        333377777F77377733330BBB0333333333337F337F33333333330BB003333333
        333373F773333333333330033333333333333773333333333333}
      NumGlyphs = 2
    end
    object FileDescr: TEdit
      Left = 83
      Top = 203
      Width = 335
      Height = 21
      TabOrder = 9
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 281
    Width = 608
    Height = 291
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
  end
  object HttpCli1: THttpCli
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0 (compatible; ICS)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
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
    OnRequestDone = HttpCli1RequestDone
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfIPv4
    SocketErrs = wsErrTech
    Left = 96
    Top = 340
  end
  object OpenDialog: TOpenDialog
    Left = 136
    Top = 344
  end
  object MimeTypesList1: TMimeTypesList
    LoadOSonDemand = True
    MimeTypesFile = '/etc/apache2/mime.types'
    DefaultTypes.Strings = (
      '.htm=text/html'
      '.html=text/html'
      '.gif=image/gif'
      '.bmp=image/bmp'
      '.jpg=image/jpeg'
      '.jpeg=image/jpeg'
      '.tif=image/tiff'
      '.tiff=image/tiff'
      '.txt=text/plain'
      '.css=text/css'
      '.wav=audio/x-wav'
      '.ico=image/x-icon'
      '.wml=text/vnd.wap.wml'
      '.wbmp=image/vnd.wap.wbmp'
      '.wmlc=application/vnd.wap.wmlc'
      '.wmlscript=text/vnd.wap.wmlscript'
      '.wmlscriptc=application/vnd.wap.wmlscriptc'
      '.pdf=application/pdf'
      '.png=image/png'
      '.xml=application/xml'
      '.xhtml=application/xhtml+xml'
      '.zip=application/zip'
      '.exe=application/x-msdownload'
      '.msi=application/x-msdownload'
      '.bin=application/octet-stream'
      '.iso=application/octet-stream'
      '.js=application/javascript'
      '.json=application/json')
    MimeTypeSrc = MTypeList
    UnknownType = 'application/octet-stream'
    Left = 184
    Top = 344
  end
end
