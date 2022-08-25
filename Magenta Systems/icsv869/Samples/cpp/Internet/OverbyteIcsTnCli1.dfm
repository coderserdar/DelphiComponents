object TelnetForm: TTelnetForm
  Left = 121
  Top = 82
  Caption = 'Basic Telnet Program'
  ClientHeight = 449
  ClientWidth = 648
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusLabel: TLabel
    Left = 208
    Top = 8
    Width = 56
    Height = 13
    Caption = 'StatusLabel'
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 50
    Height = 13
    Caption = 'HostName'
  end
  object Label2: TLabel
    Left = 48
    Top = 48
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object ConnectButton: TButton
    Left = 208
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Connect'
    TabOrder = 0
    OnClick = ConnectButtonClick
  end
  object DisconnectButton: TButton
    Left = 320
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Disconnect'
    TabOrder = 1
    OnClick = DisconnectButtonClick
  end
  object TnEmulVT1: TTnEmulVT
    Tag = 0
    Left = 0
    Top = 76
    Width = 648
    Height = 373
    Align = alBottom
    BorderStyle = bsSingle
    AutoRepaint = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Terminal'
    Font.Style = []
    LocalEcho = False
    AutoLF = False
    AutoCR = False
    Xlat = True
    MonoChrome = False
    Log = False
    Rows = 25
    Cols = 80
    BackRows = 0
    BackColor = vtsWhite
    Options = [vtoBackColor]
    LineHeight = 16
    CharWidth = 12
    TabOrder = 2
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
    Port = 'telnet'
    Location = 'TNCNX'
    UpperLock = False
    OnSessionClosed = TnEmulVT1SessionClosed
    OnSessionConnected = TnEmulVT1SessionConnected
    ExplicitTop = 83
  end
  object HostNameEdit: TEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'localhost'
  end
  object PortEdit: TEdit
    Left = 72
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'telnet'
  end
  object SendButton: TButton
    Left = 424
    Top = 40
    Width = 75
    Height = 25
    Caption = 'SendButton'
    TabOrder = 5
    OnClick = SendButtonClick
  end
  object LocalEchoCheckBox: TCheckBox
    Left = 512
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Local Echo'
    TabOrder = 6
    OnClick = LocalEchoCheckBoxClick
  end
  object RequestLocalEchoOffButton: TButton
    Left = 504
    Top = 28
    Width = 121
    Height = 17
    Caption = 'RequestLocal Echo Off'
    TabOrder = 7
    OnClick = RequestLocalEchoOffButtonClick
  end
  object RequestLocalEchoOnButton: TButton
    Left = 504
    Top = 8
    Width = 121
    Height = 17
    Caption = 'RequestLocal Echo On'
    TabOrder = 8
    OnClick = RequestLocalEchoOnButtonClick
  end
  object OptionsButton: TButton
    Left = 424
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Options'
    TabOrder = 9
    OnClick = OptionsButtonClick
  end
end
