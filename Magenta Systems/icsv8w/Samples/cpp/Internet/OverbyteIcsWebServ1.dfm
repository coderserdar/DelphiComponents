object WebServForm: TWebServForm
  Left = 412
  Top = 122
  Caption = 'WebServForm'
  ClientHeight = 255
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 57
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 36
      Top = 8
      Width = 33
      Height = 13
      Caption = 'DocDir'
    end
    object Label2: TLabel
      Left = 16
      Top = 32
      Width = 54
      Height = 13
      Caption = 'DefaultDoc'
    end
    object Label3: TLabel
      Left = 220
      Top = 8
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object ClientCountLabel: TLabel
      Left = 244
      Top = 32
      Width = 80
      Height = 13
      Caption = 'ClientCountLabel'
    end
    object Label5: TLabel
      Left = 208
      Top = 32
      Width = 31
      Height = 13
      Caption = 'Clients'
    end
    object DocDirEdit: TEdit
      Left = 80
      Top = 4
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'DocDirEdit'
    end
    object DefaultDocEdit: TEdit
      Left = 80
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'DefaultDocEdit'
    end
    object StartButton: TButton
      Left = 352
      Top = 4
      Width = 53
      Height = 21
      Caption = '&Start'
      TabOrder = 2
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 352
      Top = 28
      Width = 53
      Height = 21
      Caption = 'St&op'
      TabOrder = 3
      OnClick = StopButtonClick
    end
    object PortEdit: TEdit
      Left = 244
      Top = 4
      Width = 53
      Height = 21
      TabOrder = 4
      Text = 'PortEdit'
    end
    object ClearButton: TButton
      Left = 412
      Top = 4
      Width = 53
      Height = 21
      Caption = '&Clear'
      TabOrder = 5
      OnClick = ClearButtonClick
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 416
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Display Header'
      TabOrder = 6
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 57
    Width = 536
    Height = 198
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
    WordWrap = False
  end
  object HttpServer1: THttpServer
    ListenBacklog = 5
    Port = '80'
    Addr = '0.0.0.0'
    MaxClients = 0
    DocDir = '\WebShare'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerOn
    LingerTimeout = 1
    Options = []
    KeepAliveTimeSec = 10
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 5000
    SizeCompressMax = 5000000
    OnServerStarted = HttpServer1ServerStarted
    OnServerStopped = HttpServer1ServerStopped
    OnClientConnect = HttpServer1ClientConnect
    OnClientDisconnect = HttpServer1ClientDisconnect
    OnGetDocument = HttpServer1GetDocument
    OnHeadDocument = HttpServer1HeadDocument
    AuthTypes = []
    AuthRealm = 'ics'
    Left = 96
    Top = 80
  end
end
