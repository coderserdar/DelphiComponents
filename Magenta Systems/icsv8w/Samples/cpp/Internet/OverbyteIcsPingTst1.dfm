object PingTstForm: TPingTstForm
  Left = 401
  Top = 332
  Caption = 'Ping - http://www.overbyte.be'
  ClientHeight = 194
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object HostEdit: TEdit
    Left = 40
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '193.121.25.27'
  end
  object PingButton: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 21
    Caption = '&Ping'
    Default = True
    TabOrder = 1
    OnClick = PingButtonClick
  end
  object DisplayMemo: TMemo
    Left = 8
    Top = 40
    Width = 305
    Height = 153
    Lines.Strings = (
      'DisplayMemo')
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 248
    Top = 8
    Width = 65
    Height = 21
    Cancel = True
    Caption = '&Cancel'
    Enabled = False
    TabOrder = 3
    OnClick = CancelButtonClick
  end
  object Ping1: TPing
    Size = 56
    Timeout = 4000
    TTL = 64
    Flags = 0
    OnDisplay = Ping1Display
    OnEchoRequest = Ping1EchoRequest
    OnEchoReply = Ping1EchoReply
    OnDnsLookupDone = Ping1DnsLookupDone
    Left = 24
    Top = 88
  end
end
