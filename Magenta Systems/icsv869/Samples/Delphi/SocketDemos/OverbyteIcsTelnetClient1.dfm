object TelnetForm: TTelnetForm
  Left = 36
  Top = 353
  Caption = 'Basic Telnet Program'
  ClientHeight = 401
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 50
    Height = 13
    Caption = 'HostName'
  end
  object Label2: TLabel
    Left = 40
    Top = 48
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object StatusLabel: TLabel
    Left = 208
    Top = 16
    Width = 56
    Height = 13
    Caption = 'StatusLabel'
  end
  object TnEmulVT1: TTnEmulVT
    Tag = 0
    Left = 0
    Top = 80
    Width = 577
    Height = 305
    BorderStyle = bsSingle
    AutoRepaint = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = vtsWhite
    Font.Height = -8
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    Font.BackColor = vtsBlack
    LocalEcho = False
    AutoLF = False
    AutoCR = False
    Xlat = True
    MonoChrome = False
    Log = False
    LogFileName = 'EMULVT.LOG'
    Rows = 25
    Cols = 80
    BackRows = 50
    BackColor = vtsYellow
    Options = [vtoBackColor]
    LineHeight = 11.880000114440920000
    CharWidth = 6.912499904632568000
    SoundOn = False
    AutoReSize = True
    TabOrder = 9
    FKeys = 1
    TopMargin = 4
    LeftMargin = 6
    RightMargin = 6
    BottomMargin = 4
    MarginColor = 0
    IniFilename = 'TNEMULVT.INI'
    SectionName = 'Windows'
    KeyName = 'TnEmulVT'
    Error = 0
    HostName = 'localhost'
    Port = 'telnet'
    Location = 'TNCHR'
    UpperLock = False
    SocketFamily = sfAny
    OnSessionClosed = TnEmulVT1SessionClosed
    OnSessionConnected = TnEmulVT1SessionConnected
  end
  object ConnectButton: TButton
    Left = 208
    Top = 40
    Width = 75
    Height = 21
    Caption = '&Connect'
    TabOrder = 2
    OnClick = ConnectButtonClick
  end
  object HostNameEdit: TEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'HostNameEdit'
  end
  object PortEdit: TEdit
    Left = 72
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'PortEdit'
  end
  object DisconnectButton: TButton
    Left = 296
    Top = 40
    Width = 75
    Height = 21
    Caption = '&Disconnect'
    Enabled = False
    TabOrder = 3
    OnClick = DisconnectButtonClick
  end
  object SendButton: TButton
    Left = 384
    Top = 40
    Width = 75
    Height = 21
    Caption = 'Send Hello'
    TabOrder = 5
    OnClick = SendButtonClick
  end
  object OptionsButton: TButton
    Left = 384
    Top = 8
    Width = 75
    Height = 21
    Caption = '&Options'
    TabOrder = 4
    OnClick = OptionsButtonClick
  end
  object LocalEchoCheckBox: TCheckBox
    Left = 472
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Local Echo'
    TabOrder = 8
    OnClick = LocalEchoCheckBoxClick
  end
  object RequestLocalEchoOnButton: TButton
    Left = 464
    Top = 8
    Width = 121
    Height = 17
    Caption = 'RequestLocal Echo On'
    TabOrder = 6
    OnClick = RequestLocalEchoOnButtonClick
  end
  object RequestLocalEchoOffButton: TButton
    Left = 464
    Top = 28
    Width = 121
    Height = 17
    Caption = 'RequestLocal Echo Off'
    TabOrder = 7
    OnClick = RequestLocalEchoOffButtonClick
  end
end
