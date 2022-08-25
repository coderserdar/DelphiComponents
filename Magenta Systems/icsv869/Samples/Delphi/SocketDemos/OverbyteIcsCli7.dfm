object Cli7Form: TCli7Form
  Left = 70
  Top = 103
  Caption = 'Client 7'
  ClientHeight = 227
  ClientWidth = 378
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 378
    Height = 105
    Align = alTop
    TabOrder = 0
    object Label6: TLabel
      Left = 196
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label1: TLabel
      Left = 11
      Top = 12
      Width = 50
      Height = 13
      Caption = 'HostName'
    end
    object Label2: TLabel
      Left = 12
      Top = 84
      Width = 23
      Height = 13
      Caption = 'Data'
    end
    object PortEdit: TEdit
      Left = 233
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 0
      Text = 'PortEdit'
    end
    object HostNameEdit: TEdit
      Left = 68
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Host where the file is located'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'HostNameEdit'
    end
    object ConnectButton: TButton
      Left = 12
      Top = 40
      Width = 75
      Height = 17
      Caption = '&Connect'
      TabOrder = 2
      OnClick = ConnectButtonClick
    end
    object LineOnButton: TButton
      Left = 92
      Top = 40
      Width = 75
      Height = 17
      Caption = 'Line &On'
      TabOrder = 3
      OnClick = LineOnButtonClick
    end
    object LineOffButton: TButton
      Left = 92
      Top = 60
      Width = 75
      Height = 17
      Caption = 'Line O&ff'
      TabOrder = 4
      OnClick = LineOffButtonClick
    end
    object DisconnectButton: TButton
      Left = 12
      Top = 60
      Width = 75
      Height = 17
      Caption = '&Disconnect'
      TabOrder = 5
      OnClick = DisconnectButtonClick
    end
    object DataEdit: TEdit
      Left = 40
      Top = 80
      Width = 333
      Height = 21
      TabOrder = 6
      Text = 'DataEdit'
    end
    object SendButton: TButton
      Left = 172
      Top = 60
      Width = 75
      Height = 17
      Caption = '&Send'
      TabOrder = 7
      OnClick = SendButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 105
    Width = 378
    Height = 122
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 1
  end
  object WSocket1: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = [wsoAsyncDnsLookup, wsoIcsDnsLookup]
    OnDataAvailable = WSocket1DataAvailable
    OnSessionClosed = WSocket1SessionClosed
    OnSessionConnected = WSocket1SessionConnected
    SocketErrs = wsErrTech
    Left = 112
    Top = 116
  end
end
