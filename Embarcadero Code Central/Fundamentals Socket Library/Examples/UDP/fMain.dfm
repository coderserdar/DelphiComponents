object Form1: TForm1
  Left = 295
  Top = 137
  Width = 211
  Height = 234
  Caption = 'UDP Example'
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
    Caption = 'Bind'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 185
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 2
    OnClick = Button2Click
  end
  object eTx: TEdit
    Left = 8
    Top = 40
    Width = 185
    Height = 21
    TabOrder = 3
  end
  object UDPSocket: TfndUDPClientSocket
    LocalHost = '127.0.0.1'
    LocalPort = '1235'
    UseSendBuffer = True
    Host = '127.0.0.1'
    Port = '1235'
    OnError = UDPSocketError
    OnDataAvailable = UDPSocketDataAvailable
    Left = 80
    Top = 128
  end
end
