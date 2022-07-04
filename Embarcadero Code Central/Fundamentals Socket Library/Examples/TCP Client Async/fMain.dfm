object Form1: TForm1
  Left = 343
  Top = 200
  Width = 413
  Height = 177
  Caption = 'TCP Client Asynchronous Mode Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'GET'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 56
    Width = 393
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object fndTCPClient: TfndTCPClient
    Host = 'www.torry.net'
    Port = '80'
    StreamMode = smAsynchronous
    TimeOut = 300000
    RunInThread = False
    Active = False
    ThrottleWrite = False
    WriteThrottleRate = 0
    ThrottleRead = False
    ReadThrottleRate = 0
    OnConnected = fndTCPClientConnected
    OnConnectFailed = fndTCPClientConnectFailed
    OnDataAvailable = fndTCPClientDataAvailable
    OnClose = fndTCPClientClose
    Left = 120
    Top = 8
  end
end
