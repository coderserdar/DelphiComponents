object CliForm: TCliForm
  Left = 419
  Top = 200
  Width = 373
  Height = 230
  Caption = 'Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 365
    Height = 114
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 155
    Width = 365
    Height = 41
    Align = alBottom
    TabOrder = 1
    object SendEdit: TEdit
      Left = 16
      Top = 10
      Width = 249
      Height = 21
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 280
      Top = 10
      Width = 75
      Height = 21
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 365
    Height = 41
    Align = alTop
    TabOrder = 2
    object LineLabel: TLabel
      Left = 96
      Top = 12
      Width = 46
      Height = 13
      Caption = 'LineLabel'
    end
    object DisconnectButton: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Disconnect'
      TabOrder = 0
      OnClick = DisconnectButtonClick
    end
  end
  object CliSocket: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ComponentOptions = []
    OnDataAvailable = CliSocketDataAvailable
    OnSessionClosed = CliSocketSessionClosed
    Left = 40
    Top = 88
  end
end
