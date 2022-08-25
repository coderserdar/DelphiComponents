object RecvForm: TRecvForm
  Left = 106
  Top = 123
  Caption = 'Receiver - http://www.overbyte.be'
  ClientHeight = 233
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 396
    Height = 49
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 16
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label2: TLabel
      Left = 368
      Top = 12
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object PortEdit: TEdit
      Left = 40
      Top = 12
      Width = 45
      Height = 21
      TabOrder = 0
      Text = '4321'
      OnChange = PortEditChange
    end
    object ActionButton: TButton
      Left = 92
      Top = 12
      Width = 49
      Height = 21
      Caption = '&Start'
      Default = True
      TabOrder = 1
      OnClick = ActionButtonClick
    end
    object CloseAllButton: TButton
      Left = 148
      Top = 12
      Width = 53
      Height = 21
      Caption = '&Close All'
      TabOrder = 2
      OnClick = CloseAllButtonClick
    end
    object LingerCheckBox: TCheckBox
      Left = 280
      Top = 4
      Width = 57
      Height = 17
      Caption = '&Linger'
      TabOrder = 3
    end
    object BannerCheckBox: TCheckBox
      Left = 280
      Top = 20
      Width = 65
      Height = 17
      Caption = 'Banner'
      TabOrder = 4
    end
    object LineModeOnButton: TButton
      Left = 212
      Top = 4
      Width = 45
      Height = 17
      Caption = 'Line &On'
      TabOrder = 5
      OnClick = LineModeOnButtonClick
    end
    object LineOffButton: TButton
      Left = 212
      Top = 24
      Width = 45
      Height = 17
      Caption = 'Line O&ff'
      TabOrder = 6
      OnClick = LineOffButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 49
    Width = 396
    Height = 184
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
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    OnSessionAvailable = WSocket1SessionAvailable
    SocketErrs = wsErrTech
    Left = 276
    Top = 60
  end
end
