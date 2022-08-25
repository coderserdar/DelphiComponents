object NsLookupForm: TNsLookupForm
  Left = 257
  Top = 327
  Caption = 'Name Server Lookup'
  ClientHeight = 643
  ClientWidth = 676
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 65
    Width = 676
    Height = 578
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 676
    Height = 65
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 11
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object Label2: TLabel
      Left = 10
      Top = 30
      Width = 31
      Height = 26
      Caption = 'DNS Server'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 260
      Top = 11
      Width = 94
      Height = 13
      Caption = 'Lookup Query Type'
    end
    object ClearDisplayBitBtn: TBitBtn
      Left = 582
      Top = 6
      Width = 25
      Height = 25
      TabOrder = 7
      OnClick = ClearDisplayBitBtnClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777700000000000777770FFFFFFFFF0777770FFFFFFFFF0777770FFFFFFFFF07
        77770FFFFFFFF00777770F0F00FF0B0777770FFFFFF0BFB077770FFFFF0BFBFB
        07770F00F08FBFBFB0770FFFFFF8FBFBFB070FF00F0F8FBFBFB00FFFFFFFF8FB
        FBF80F00F0FF0F8FBF870FFFFFFFFF08F8770000000000078777}
    end
    object LookupButton: TButton
      Left = 385
      Top = 38
      Width = 75
      Height = 21
      Caption = '&Start Lookup'
      TabOrder = 5
      OnClick = LookupButtonClick
    end
    object TcpRadioButton: TRadioButton
      Left = 260
      Top = 42
      Width = 49
      Height = 17
      Caption = 'TCP'
      TabOrder = 3
    end
    object UdpRadioButton: TRadioButton
      Left = 305
      Top = 42
      Width = 49
      Height = 17
      Caption = 'UDP'
      Checked = True
      TabOrder = 4
      TabStop = True
    end
    object DnsEdit: TComboBox
      Left = 58
      Top = 36
      Width = 190
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      Text = '8.8.8.8'
      Items.Strings = (
        '')
    end
    object NameEdit: TComboBox
      Left = 47
      Top = 9
      Width = 201
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'pool.ntp.org'
      Items.Strings = (
        'pool.ntp.org'
        'www.google.com'
        'google.com'
        'www.overbyte.eu'
        'overbyte.eu'
        'wiki.overbyte.eu'
        'magsys.co.uk'
        'www.magsys.co.uk'
        'mail.magsys.co.uk'
        'embarcadero.com'
        'www.embarcadero.com'
        'str'#248'm.no'
        #233'x'#224'mpl'#234'.ftptest.co.uk'
        'scr'#250'd'#250'.ftptest.co.uk'
        'pr'#248've.ftptest.co.uk'
        ''
        '')
    end
    object DnsQueryType: TComboBox
      Left = 360
      Top = 11
      Width = 201
      Height = 21
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 1
    end
    object AllButton: TButton
      Left = 475
      Top = 38
      Width = 75
      Height = 21
      Caption = '&Lookup All '
      TabOrder = 6
      OnClick = AllButtonClick
    end
    object AbortButton: TButton
      Left = 565
      Top = 38
      Width = 75
      Height = 21
      Caption = '&Abort '
      TabOrder = 8
      OnClick = AbortButtonClick
    end
  end
  object DnsQuery1: TDnsQuery
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    OnRequestDone = DnsQuery1RequestDone
    Left = 248
    Top = 88
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = Timer1Timer
    Left = 295
    Top = 95
  end
end
