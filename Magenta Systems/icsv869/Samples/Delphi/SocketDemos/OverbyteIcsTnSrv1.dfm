object ServerForm: TServerForm
  Left = 269
  Top = 236
  Caption = 'ServerForm'
  ClientHeight = 276
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object PortLabel: TLabel
    Left = 8
    Top = 240
    Width = 26
    Height = 16
    Caption = 'Port'
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 552
    Height = 225
    Align = alTop
    Lines.Strings = (
      'Memo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object QuitButton: TButton
    Left = 471
    Top = 234
    Width = 81
    Height = 25
    Cancel = True
    Caption = '&Quit'
    TabOrder = 1
    OnClick = QuitButtonClick
  end
  object AboutButton: TButton
    Left = 390
    Top = 234
    Width = 75
    Height = 25
    Caption = '&About'
    TabOrder = 2
    OnClick = AboutButtonClick
  end
  object PortEdit: TEdit
    Left = 40
    Top = 235
    Width = 49
    Height = 24
    TabOrder = 3
    Text = 'telnet'
  end
  object ChangePortButton: TButton
    Left = 95
    Top = 234
    Width = 75
    Height = 25
    Caption = 'Change'
    TabOrder = 4
    OnClick = ChangePortButtonClick
  end
  object IPv4RadioButton: TRadioButton
    Left = 192
    Top = 231
    Width = 57
    Height = 17
    Caption = 'IPv4'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = IPv4RadioButtonClick
  end
  object IPv6RadioButton: TRadioButton
    Left = 192
    Top = 254
    Width = 57
    Height = 17
    Caption = 'IPv6'
    TabOrder = 6
    OnClick = IPv6RadioButtonClick
  end
  object EchoCheckBox: TCheckBox
    Left = 288
    Top = 240
    Width = 65
    Height = 17
    Caption = 'Echo'
    TabOrder = 7
  end
  object SrvSocket: TWSocket
    LineEnd = #13#10
    Addr = '0.0.0.0'
    Port = 'telnet'
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
    ListenBacklog = 15
    OnSessionClosed = SrvSocketSessionClosed
    OnSessionAvailable = SrvSocketSessionAvailable
    SocketErrs = wsErrTech
    Left = 28
    Top = 36
  end
end
