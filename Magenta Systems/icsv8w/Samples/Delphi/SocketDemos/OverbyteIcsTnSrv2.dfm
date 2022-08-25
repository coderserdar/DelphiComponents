object ClientForm: TClientForm
  Left = 340
  Top = 208
  Caption = 'ClientForm'
  ClientHeight = 253
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 419
    Height = 171
    Align = alClient
    Lines.Strings = (
      'Memo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 171
    Width = 419
    Height = 82
    Align = alBottom
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 52
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object SendButton: TButton
      Left = 92
      Top = 52
      Width = 89
      Height = 25
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
    object DisconnectButton: TButton
      Left = 196
      Top = 52
      Width = 89
      Height = 25
      Caption = '&Disconnect'
      TabOrder = 2
      OnClick = DisconnectButtonClick
    end
    object DataMemo: TMemo
      Left = 1
      Top = 1
      Width = 417
      Height = 48
      Align = alTop
      Lines.Strings = (
        'DataMemo')
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object EchoCheckBox: TCheckBox
    Left = 312
    Top = 226
    Width = 65
    Height = 17
    Caption = 'Echo'
    TabOrder = 2
    OnClick = EchoCheckBoxClick
  end
  object Socket: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    OnDataAvailable = SocketDataAvailable
    OnSessionClosed = SocketSessionClosed
    SocketErrs = wsErrTech
    Left = 8
    Top = 40
  end
end
