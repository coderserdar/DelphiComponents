object DnsResolverForm: TDnsResolverForm
  Left = 56
  Top = 116
  Caption = 'DnsResolverForm'
  ClientHeight = 377
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 0
    Width = 38
    Height = 28
    Caption = 'DNS Server'
    WordWrap = True
  end
  object DomainListBox: TListBox
    Left = 16
    Top = 52
    Width = 169
    Height = 153
    ItemHeight = 13
    Items.Strings = (
      'overbyte.be'
      'embarcadero.com'
      'unknown.test'
      'microsoft.com'
      'e-naxos.com')
    TabOrder = 0
  end
  object ResultListBox: TListBox
    Left = 192
    Top = 52
    Width = 165
    Height = 153
    ItemHeight = 13
    TabOrder = 1
  end
  object QueryButton: TButton
    Left = 196
    Top = 4
    Width = 97
    Height = 21
    Caption = '&Query'
    TabOrder = 2
    OnClick = QueryButtonClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 212
    Width = 341
    Height = 149
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object DnsIPEdit: TEdit
    Left = 60
    Top = 4
    Width = 121
    Height = 21
    Hint = 
      'Use your ISP DNS, you'#39'll get better results. Type "IPCONFIG /ALL' +
      '" at command line prompt to get the IP address.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Text = '8.8.8.8'
  end
  object MXRecordRadioButton: TRadioButton
    Left = 32
    Top = 32
    Width = 113
    Height = 17
    Hint = 'Select here to query all MX record for domains'
    Caption = '&MX Record'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    TabStop = True
  end
  object AddressRadioButton: TRadioButton
    Left = 192
    Top = 32
    Width = 113
    Height = 17
    Hint = 'Select here to query www address for all domains'
    Caption = '&Address (www)'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
  end
  object DnsQuery1: TDnsQuery
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    OnRequestDone = DnsQuery1RequestDone
    Left = 228
    Top = 136
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 296
    Top = 132
  end
end
