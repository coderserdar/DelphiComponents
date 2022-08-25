object TnDemoForm: TTnDemoForm
  Left = 305
  Top = 196
  Caption = 'TnDemo - http://www.overbyte.be'
  ClientHeight = 249
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 61
    Width = 407
    Height = 188
    Align = alClient
    Enabled = False
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyDown = DisplayMemoKeyDown
    OnKeyPress = DisplayMemoKeyPress
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 407
    Height = 61
    Align = alTop
    TabOrder = 1
    object HostLabel: TLabel
      Left = 8
      Top = 11
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object InfoLabel: TLabel
      Left = 336
      Top = 11
      Width = 66
      Height = 13
      Caption = 'Disconnected'
    end
    object PortLabel: TLabel
      Left = 112
      Top = 11
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label1: TLabel
      Left = 8
      Top = 36
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object HostEdit: TEdit
      Left = 40
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object ConnectButton: TButton
      Left = 192
      Top = 8
      Width = 65
      Height = 21
      Caption = '&Connect'
      TabOrder = 1
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 264
      Top = 8
      Width = 65
      Height = 21
      Caption = '&Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = DisconnectButtonClick
    end
    object PortEdit: TEdit
      Left = 136
      Top = 8
      Width = 41
      Height = 21
      TabOrder = 3
      Text = 'telnet'
    end
    object DataEdit: TEdit
      Left = 40
      Top = 32
      Width = 137
      Height = 21
      TabOrder = 4
      Text = 'help'
    end
    object SendButton: TButton
      Left = 192
      Top = 32
      Width = 65
      Height = 21
      Caption = '&Send'
      TabOrder = 5
      OnClick = SendButtonClick
    end
  end
  object TnCnx: TTnCnx
    Port = '23'
    Location = 'TNCNX'
    TermType = 'VT100'
    LocalEcho = False
    OnSessionConnected = TnCnxSessionConnected
    OnSessionClosed = TnCnxSessionClosed
    OnDataAvailable = TnCnxDataAvailable
    Left = 308
    Top = 136
  end
end
