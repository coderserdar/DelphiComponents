object FingerDemoForm: TFingerDemoForm
  Left = 199
  Top = 156
  Caption = 'Finger Demo - http://www.rtfm.be/fpiette'
  ClientHeight = 300
  ClientWidth = 510
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
    Width = 510
    Height = 259
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 510
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
    object QueryEdit: TEdit
      Left = 17
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'user@mimosa'
    end
    object QueryButton: TButton
      Left = 151
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Query'
      Default = True
      TabOrder = 1
      OnClick = QueryButtonClick
    end
    object CancelButton: TButton
      Left = 232
      Top = 8
      Width = 75
      Height = 21
      Cancel = True
      Caption = '&Cancel'
      Enabled = False
      TabOrder = 2
      OnClick = CancelButtonClick
    end
  end
  object FingerCli1: TFingerCli
    OnSessionConnected = FingerCli1SessionConnected
    OnDataAvailable = FingerCli1DataAvailable
    OnQueryDone = FingerCli1QueryDone
    Left = 32
    Top = 104
  end
  object WSocket1: TWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 2
    ReqVerHigh = 2
    Left = 152
    Top = 88
  end
end
