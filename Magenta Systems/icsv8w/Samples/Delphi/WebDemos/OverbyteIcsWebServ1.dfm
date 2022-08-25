object WebServForm: TWebServForm
  Left = 253
  Top = 158
  Caption = 'ICS WebServer Demo - http://www.overbyte.be'
  ClientHeight = 342
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 502
    Height = 129
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 40
      Top = 8
      Width = 33
      Height = 13
      Caption = 'DocDir'
    end
    object Label2: TLabel
      Left = 19
      Top = 32
      Width = 54
      Height = 13
      Caption = 'DefaultDoc'
    end
    object Label3: TLabel
      Left = 259
      Top = 7
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object ClientCountLabel: TLabel
      Left = 294
      Top = 34
      Width = 80
      Height = 13
      Caption = 'ClientCountLabel'
    end
    object Label5: TLabel
      Left = 258
      Top = 34
      Width = 34
      Height = 13
      Caption = 'Clients:'
    end
    object Label4: TLabel
      Left = 23
      Top = 81
      Width = 50
      Height = 13
      Caption = 'Redir URL'
    end
    object Label6: TLabel
      Left = 16
      Top = 57
      Width = 57
      Height = 13
      Caption = 'TemplateDir'
    end
    object Label7: TLabel
      Left = 357
      Top = 83
      Width = 90
      Height = 13
      Caption = 'KeepAliveTimeSec'
    end
    object Label8: TLabel
      Left = 334
      Top = 105
      Width = 113
      Height = 13
      Alignment = taRightJustify
      Caption = 'MaxRequestsKeepAlive'
    end
    object Label9: TLabel
      Left = 15
      Top = 105
      Width = 153
      Height = 13
      Caption = 'Maximum Bandwidth (bytes/sec)'
    end
    object DocDirEdit: TEdit
      Left = 80
      Top = 4
      Width = 165
      Height = 21
      TabOrder = 0
      Text = 'DocDirEdit'
    end
    object DefaultDocEdit: TEdit
      Left = 80
      Top = 29
      Width = 165
      Height = 21
      TabOrder = 1
      Text = 'DefaultDocEdit'
    end
    object StartButton: TButton
      Left = 251
      Top = 52
      Width = 53
      Height = 21
      Caption = '&Start'
      TabOrder = 3
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 251
      Top = 79
      Width = 53
      Height = 21
      Caption = 'St&op'
      TabOrder = 4
      OnClick = StopButtonClick
    end
    object PortEdit: TEdit
      Left = 283
      Top = 3
      Width = 53
      Height = 21
      TabOrder = 5
      Text = 'PortEdit'
    end
    object ClearButton: TButton
      Left = 311
      Top = 52
      Width = 53
      Height = 21
      Caption = '&Clear'
      TabOrder = 6
      OnClick = ClearButtonClick
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 389
      Top = 57
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Header'
      TabOrder = 7
    end
    object WriteLogFileCheckBox: TCheckBox
      Left = 389
      Top = 40
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Write to log file'
      TabOrder = 8
      OnClick = WriteLogFileCheckBoxClick
    end
    object DirListCheckBox: TCheckBox
      Left = 401
      Top = 23
      Width = 85
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Allow Dir List'
      TabOrder = 2
    end
    object OutsideRootCheckBox: TCheckBox
      Left = 373
      Top = 6
      Width = 113
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Allow Outside Root'
      TabOrder = 9
    end
    object RedirURLEdit: TEdit
      Left = 80
      Top = 79
      Width = 165
      Height = 21
      Hint = 'Enter here the URL used for the redir.htm virtual page.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      Text = 'RedirURLEdit'
    end
    object TemplateDirEdit: TEdit
      Left = 80
      Top = 54
      Width = 165
      Height = 21
      TabOrder = 11
      Text = 'TemplateDirEdit'
    end
    object KeepAliveTimeSecEdit: TEdit
      Left = 453
      Top = 79
      Width = 33
      Height = 21
      Hint = 'Idle timeout in seconds upon persistent connections (Keep-Alive)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      Text = 'KeepAliveTimeSecEdit'
    end
    object MaxRequestsKeepAliveEdit: TEdit
      Left = 453
      Top = 103
      Width = 33
      Height = 21
      Hint = 
        'Maximum number of requests during live time of persistent connec' +
        'tions (Keep-Alive)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      Text = 'MaxRequestsKeepAliveEdit'
    end
    object BandwidthLimitEdit: TEdit
      Left = 174
      Top = 103
      Width = 71
      Height = 21
      Hint = 'Bytes Per Second'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      Text = 'BandwidthLimitEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 129
    Width = 502
    Height = 213
    Align = alClient
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
    WordWrap = False
  end
  object HttpServer1: THttpServer
    ListenBacklog = 5
    MultiListenSockets = <>
    Port = '80'
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    MaxClients = 0
    DocDir = '\WebShare'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 1
    Options = [hoAllowOptions, hoAllowPut, hoAllowDelete, hoAllowTrace, hoAllowPatch, hoSendServerHdr]
    KeepAliveTimeSec = 10
    KeepAliveTimeXferSec = 300
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 5000
    SizeCompressMax = 5000000
    MaxBlkSize = 8192
    MimeTypesList = MimeTypesList1
    BandwidthLimit = 0
    BandwidthSampling = 1000
    ServerHeader = 'Server: ICS-HttpServer-8.08'
    OnServerStarted = HttpServer1ServerStarted
    OnServerStopped = HttpServer1ServerStopped
    OnClientConnect = HttpServer1ClientConnect
    OnClientDisconnect = HttpServer1ClientDisconnect
    OnGetDocument = HttpServer1GetDocument
    OnHeadDocument = HttpServer1HeadDocument
    OnPostDocument = HttpServer1PostDocument
    OnPostedData = HttpServer1PostedData
    OnOptionsDocument = HttpServer1OptionsDocument
    OnPutDocument = HttpServer1PutDocument
    OnDeleteDocument = HttpServer1DeleteDocument
    OnTraceDocument = HttpServer1TraceDocument
    OnPatchDocument = HttpServer1PatchDocument
    OnHttpMimeContentType = HttpServer1HttpMimeContentType
    OnAuthGetPassword = HttpServer1AuthGetPassword
    OnAuthResult = HttpServer1AuthResult
    OnAuthGetType = HttpServer1AuthGetType
    OnAuthNtlmBeforeValidate = HttpServer1AuthNtlmBeforeValidate
    AuthTypes = []
    AuthRealm = 'ics'
    SocketErrs = wsErrFriendly
    ExclusiveAddr = True
    Left = 18
    Top = 179
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
      '.iso=application/octet-stream')
    MimeTypeSrc = MTypeList
    UnknownType = 'application/octet-stream'
    Left = 60
    Top = 180
  end
end
