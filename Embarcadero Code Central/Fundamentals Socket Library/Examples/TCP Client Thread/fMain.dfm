object Form1: TForm1
  Left = 292
  Top = 171
  Width = 413
  Height = 177
  Caption = 'TCP Client Threaded Mode Example'
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
    Caption = 'Button1'
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
    StreamMode = smBlockWaitMessage
    TimeOut = 300000
    RunInThread = True
    Active = False
    ThrottleWrite = False
    WriteThrottleRate = 0
    ThrottleRead = False
    ReadThrottleRate = 0
    OnThreadRun = fndTCPClientThreadRun
    OnThreadRunComplete = fndTCPClientThreadRunComplete
    OnSyncConnected = fndTCPClientSyncConnected
    OnSyncClose = fndTCPClientSyncClose
    Left = 120
    Top = 8
  end
end
