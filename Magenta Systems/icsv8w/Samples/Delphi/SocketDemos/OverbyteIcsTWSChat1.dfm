object TWSChatForm: TTWSChatForm
  Left = 190
  Top = 176
  Width = 405
  Height = 302
  Caption = 'TWSChat - http://www.overbyte.be'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 397
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object ServerEdit: TEdit
      Left = 48
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object ConnectButton: TButton
      Left = 176
      Top = 8
      Width = 57
      Height = 21
      Caption = '&Connect'
      TabOrder = 1
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 240
      Top = 8
      Width = 65
      Height = 21
      Caption = '&Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = DisconnectButtonClick
    end
    object RunningRadioButton: TRadioButton
      Left = 320
      Top = 8
      Width = 113
      Height = 17
      Caption = '&Running'
      TabOrder = 3
      OnClick = RunningRadioButtonClick
    end
    object StoppedRadioButton: TRadioButton
      Left = 320
      Top = 24
      Width = 113
      Height = 17
      Caption = 'St&opped'
      TabOrder = 4
      OnClick = StoppedRadioButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 227
    Width = 397
    Height = 41
    Align = alBottom
    TabOrder = 1
    object MessageEdit: TEdit
      Left = 8
      Top = 8
      Width = 329
      Height = 21
      Enabled = False
      TabOrder = 0
      Text = 'MessageEdit'
    end
    object SendButton: TButton
      Left = 344
      Top = 8
      Width = 49
      Height = 21
      Caption = '&Send'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = SendButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 397
    Height = 186
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    TabOrder = 2
  end
  object SrvWSocket: TWSocket
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
    OnSessionAvailable = SrvWSocketSessionAvailable
    Left = 336
    Top = 56
  end
  object CliWSocket: TWSocket
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
    OnDataAvailable = CliWSocketDataAvailable
    OnSessionClosed = CliWSocketSessionClosed
    OnSessionConnected = CliWSocketSessionConnected
    OnDnsLookupDone = CliWSocketDnsLookupDone
    Left = 248
    Top = 56
  end
  object TmpWSocket: TWSocket
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
    Left = 248
    Top = 112
  end
end
