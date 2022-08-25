object TnDemoForm: TTnDemoForm
  Left = 261
  Top = 272
  Caption = 'TnDemo - http://www.overbyte.be'
  ClientHeight = 261
  ClientWidth = 429
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
    Top = 41
    Width = 429
    Height = 220
    Align = alClient
    Enabled = False
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyDown = DisplayMemoKeyDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 429
    Height = 41
    Align = alTop
    Caption = 'Panel1'
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
  end
  object TnCnx: TTnCnx
    Port = '23'
    Location = 'TNCNX'
    TermType = 'VT100'
    LocalEcho = False
    OnSessionConnected = TnCnxSessionConnected
    OnSessionClosed = TnCnxSessionClosed
    OnDataAvailable = TnCnxDataAvailable
    Left = 304
    Top = 56
  end
end
