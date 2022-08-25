object MainForm: TMainForm
  Left = 158
  Top = 169
  Width = 365
  Height = 149
  Caption = 'UdpListener'
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
  object DataAvailableLabel: TLabel
    Left = 16
    Top = 72
    Width = 92
    Height = 13
    Caption = 'DataAvailableLabel'
  end
  object InfoLabel: TLabel
    Left = 16
    Top = 48
    Width = 44
    Height = 13
    Caption = 'InfoLabel'
  end
  object Label1: TLabel
    Left = 176
    Top = 11
    Width = 44
    Height = 13
    Caption = 'UDP port'
  end
  object Label2: TLabel
    Left = 173
    Top = 35
    Width = 47
    Height = 13
    Caption = 'Sender IP'
  end
  object StartButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start'
    Default = True
    TabOrder = 0
    OnClick = StartButtonClick
  end
  object StopButton: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'S&top'
    TabOrder = 1
    OnClick = StopButtonClick
  end
  object PortEdit: TEdit
    Left = 228
    Top = 8
    Width = 57
    Height = 21
    TabOrder = 2
    Text = 'PortEdit'
  end
  object SenderEdit: TEdit
    Left = 228
    Top = 32
    Width = 57
    Height = 21
    Hint = 
      'Enter IP address from the sender you want to listen. Use 0.0.0.0' +
      ' to accept from any sender.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = 'SenderEdit'
  end
  object WSocket: TWSocket
    LineEnd = #13#10
    Addr = '0.0.0.0'
    Port = '600'
    Proto = 'udp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ComponentOptions = []
    OnDataAvailable = WSocketDataAvailable
    OnSessionClosed = WSocketSessionClosed
    OnSessionConnected = WSocketSessionConnected
    Left = 128
    Top = 36
  end
end
