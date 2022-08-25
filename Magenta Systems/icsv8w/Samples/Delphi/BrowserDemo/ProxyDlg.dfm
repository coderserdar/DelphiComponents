object ProxyForm: TProxyForm
  Left = 298
  Top = 180
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 314
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 231
    Height = 13
    Caption = 'Proxy URL, ie http://[user[:password]@]host:port'
  end
  object Label3: TLabel
    Left = 24
    Top = 64
    Width = 77
    Height = 13
    Caption = 'Proxy Username'
  end
  object Label4: TLabel
    Left = 24
    Top = 112
    Width = 75
    Height = 13
    Caption = 'Proxy Password'
  end
  object Label5: TLabel
    Left = 24
    Top = 162
    Width = 53
    Height = 13
    Caption = 'User Agent'
  end
  object lbl1: TLabel
    Left = 291
    Top = 35
    Width = 61
    Height = 13
    Caption = 'SSL Security'
  end
  object lbl2: TLabel
    Left = 216
    Top = 59
    Width = 120
    Height = 13
    Caption = 'Root Certificate Store File'
  end
  object ProxyUrlEdit: TEdit
    Left = 24
    Top = 32
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object OKBurron: TBitBtn
    Left = 200
    Top = 248
    Width = 65
    Height = 25
    TabOrder = 9
    Kind = bkOK
  end
  object CancelButton: TBitBtn
    Left = 291
    Top = 248
    Width = 65
    Height = 25
    TabOrder = 10
    Kind = bkCancel
  end
  object ProxyUsername: TEdit
    Left = 24
    Top = 80
    Width = 177
    Height = 21
    TabOrder = 1
  end
  object ProxyPassword: TEdit
    Left = 24
    Top = 128
    Width = 177
    Height = 21
    TabOrder = 2
  end
  object UserAgent: TEdit
    Left = 24
    Top = 181
    Width = 241
    Height = 21
    TabOrder = 3
    Text = 'Mozilla/4.0'
  end
  object SslVersionList: TComboBox
    Left = 363
    Top = 32
    Width = 182
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Ignore'
      'None'
      'SSLv3 Only'
      'TLSv1.2 Only'
      'TLSv1.3 Only'
      'TLSv1 or better'
      'TLSv1.1 or better'
      'TLSv1.2 or better'
      'Backward Ciphers'
      'Intermedate Ciphers'
      'High Ciphers, 2048 keys'
      'High Ciphers, 3072 keys'
      'High Ciphers, 7680 keys')
  end
  object SslRootCertsFileEdit: TEdit
    Left = 216
    Top = 80
    Width = 325
    Height = 21
    TabOrder = 5
  end
  object SslVerifyCertMode: TRadioGroup
    Left = 391
    Top = 122
    Width = 143
    Height = 68
    Caption = 'Verify Certificate Mode'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'PEM Bundle File'
      'Windows Cert Store')
    TabOrder = 8
  end
  object SslRevokeCheck: TCheckBox
    Left = 216
    Top = 122
    Width = 155
    Height = 17
    Caption = 'Certificate Revoke Check'
    TabOrder = 6
  end
  object SslReportChain: TCheckBox
    Left = 216
    Top = 150
    Width = 155
    Height = 17
    Caption = 'Report SSL Certificates'
    TabOrder = 7
  end
end
