object FormDNS: TFormDNS
  Left = 192
  Top = 130
  Width = 588
  Height = 480
  Caption = 'DNS'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblNameServer: TLabel
    Left = 24
    Top = 32
    Width = 60
    Height = 13
    Caption = 'DNS Server:'
  end
  object lblHostName: TLabel
    Left = 24
    Top = 72
    Width = 53
    Height = 13
    Caption = 'HostName:'
  end
  object edtDNSServer: TEdit
    Left = 104
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '8.8.8.8'
  end
  object edtHostName: TEdit
    Left = 104
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'www.cnpack.org'
  end
  object btnQuery: TButton
    Left = 248
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Query using CnUDP'
    TabOrder = 2
    OnClick = btnQueryClick
  end
  object mmoResponse: TMemo
    Left = 24
    Top = 128
    Width = 505
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object btnTestParseString: TButton
    Left = 440
    Top = 72
    Width = 91
    Height = 25
    Caption = 'Parse String Test'
    TabOrder = 4
    OnClick = btnTestParseStringClick
  end
  object btnDNS: TButton
    Left = 248
    Top = 32
    Width = 113
    Height = 25
    Caption = 'Query Using CnDNS'
    TabOrder = 5
    OnClick = btnDNSClick
  end
  object btnDNSDesign: TButton
    Left = 440
    Top = 32
    Width = 89
    Height = 25
    Caption = 'Query Design'
    TabOrder = 6
    OnClick = btnDNSDesignClick
  end
  object udpDNS: TCnUDP
    RemotePort = 0
    LocalPort = 0
    OnDataReceived = udpDNSDataReceived
    Left = 392
    Top = 32
  end
  object CnDNS1: TCnDNS
    NameServerPort = 53
    OnResponse = CnDNS1Response
    Left = 392
    Top = 72
  end
end
