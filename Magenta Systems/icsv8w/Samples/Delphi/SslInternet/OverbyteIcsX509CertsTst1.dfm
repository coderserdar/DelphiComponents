object X509CertsForm: TX509CertsForm
  Left = 0
  Top = 0
  Caption = 
    'ICS Automatically Download SSL X509 Certificates Development Dem' +
    'o - http://www.overbyte.be - V8.69 - 18th May 2022'
  ClientHeight = 800
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object LogWin: TMemo
    Left = 0
    Top = 291
    Width = 1024
    Height = 509
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1024
    Height = 291
    ActivePage = TabCommon
    Align = alTop
    TabOrder = 1
    object TabCommon: TTabSheet
      Caption = 'Common'
      object Label3: TLabel
        Left = 600
        Top = 10
        Width = 142
        Height = 14
        Caption = 'Local Web Server IP Address'
      end
      object Label15: TLabel
        Left = 160
        Top = 10
        Width = 65
        Height = 14
        Caption = 'Log Directory'
      end
      object Label14: TLabel
        Left = 507
        Top = 60
        Width = 72
        Height = 14
        Caption = 'Debug Logging'
      end
      object Label41: TLabel
        Left = 219
        Top = 54
        Width = 127
        Height = 14
        Caption = 'Private Key File Password'
      end
      object Label50: TLabel
        Left = 667
        Top = 85
        Width = 82
        Height = 28
        Caption = 'Supplier Account Email'
        WordWrap = True
      end
      object Label51: TLabel
        Left = 428
        Top = 145
        Width = 149
        Height = 14
        Caption = 'Certificate Signing Request File'
      end
      object Label52: TLabel
        Left = 427
        Top = 165
        Width = 122
        Height = 28
        Caption = 'Private Key File, optional'#13#10'(needed to build bundles)'
      end
      object Label54: TLabel
        Left = 540
        Top = 195
        Width = 44
        Height = 34
        AutoSize = False
        Caption = 'Socket Family'
        WordWrap = True
      end
      object Label1: TLabel
        Left = 700
        Top = 195
        Width = 34
        Height = 34
        AutoSize = False
        Caption = 'Proxy URL'
        WordWrap = True
      end
      object DomWebSrvIP: TComboBox
        Left = 600
        Top = 25
        Width = 176
        Height = 22
        ItemHeight = 14
        TabOrder = 4
        Text = '0.0.0.0'
      end
      object LogJson: TCheckBox
        Left = 10
        Top = 10
        Width = 97
        Height = 17
        Caption = 'Log Raw Json'
        TabOrder = 0
      end
      object DirLogs: TEdit
        Left = 160
        Top = 25
        Width = 382
        Height = 22
        TabOrder = 2
        OnExit = DirLogsExit
      end
      object LogPkeys: TCheckBox
        Left = 10
        Top = 30
        Width = 97
        Height = 17
        Caption = 'Log Private Keys'
        TabOrder = 1
      end
      object SuppCertChallenge: TRadioGroup
        Left = 10
        Top = 51
        Width = 196
        Height = 208
        Caption = 'Domain Challenge Method'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'File - Web Server - UNC'
          'File - Web Server - FTP'
          'File - Local Web Server'
          'File - App Web Server'
          'DNS - Automatic'
          'DNS - Manual'
          'Email manually'
          'TLS-ALPN Cert - Web UNC'
          'TLS-ALPN Cert - Local Web'
          'TLS-ALPN Cert - App Web')
        TabOrder = 8
      end
      object DebugLogging: TComboBox
        Left = 600
        Top = 55
        Width = 175
        Height = 22
        ItemHeight = 14
        ItemIndex = 3
        TabOrder = 6
        Text = 'SSL Negotiation'
        Items.Strings = (
          'None'
          'Connections'
          'Parameters'
          'SSL Negotiation'
          'HTTP Headers'
          'HTML Body'
          'Ssl Low Level')
      end
      object PrivKeyCipher: TRadioGroup
        Left = 219
        Top = 80
        Width = 185
        Height = 113
        Caption = 'Private Key File Encryption'
        ItemIndex = 1
        Items.Strings = (
          'None'
          'Triple DES'
          'IDEA'
          'AES128'
          'AES192'
          'AES256')
        TabOrder = 9
      end
      object PrivKeyPassword: TEdit
        Left = 370
        Top = 50
        Width = 108
        Height = 22
        PasswordChar = '*'
        TabOrder = 5
        Text = 'password'
      end
      object AutoOrderComplete: TCheckBox
        Left = 787
        Top = 30
        Width = 189
        Height = 17
        Caption = 'Automatic Order Completion'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object CertCsrOrigin: TRadioGroup
        Left = 428
        Top = 80
        Width = 214
        Height = 54
        Caption = 'Certificate CSR Origin'
        ItemIndex = 0
        Items.Strings = (
          'From Properties, creates new key'
          'From CSR File, old private key')
        TabOrder = 10
      end
      object SupplierEmail: TEdit
        Left = 755
        Top = 85
        Width = 221
        Height = 22
        TabOrder = 11
      end
      object CertOldCsrFile: TEdit
        Left = 590
        Top = 140
        Width = 369
        Height = 22
        TabOrder = 12
      end
      object CertOldPrvKey: TEdit
        Left = 590
        Top = 170
        Width = 371
        Height = 22
        TabOrder = 14
      end
      object SelDirLogs: TBitBtn
        Left = 548
        Top = 24
        Width = 31
        Height = 25
        TabOrder = 3
        OnClick = SelDirLogsClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object SelCertOldCsrFile: TBitBtn
        Left = 973
        Top = 140
        Width = 31
        Height = 25
        TabOrder = 13
        OnClick = SelCertOldCsrFileClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object SelCertOldPrvKey: TBitBtn
        Left = 973
        Top = 170
        Width = 31
        Height = 25
        TabOrder = 15
        OnClick = SelCertOldPrvKeyClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object doCheckCSR: TButton
        Left = 267
        Top = 211
        Width = 96
        Height = 25
        Caption = 'Check CSR'
        TabOrder = 16
        OnClick = doCheckCSRClick
      end
      object IpSocFamily: TComboBox
        Left = 590
        Top = 200
        Width = 102
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        ItemIndex = 0
        TabOrder = 17
        Text = 'Any'
        Items.Strings = (
          'Any'
          'Prefer IPv4'
          'Prefer IPv6'
          'Only IPv4 '
          'Only IPv6')
      end
      object ProxyURL: TEdit
        Left = 750
        Top = 201
        Width = 241
        Height = 22
        Hint = 'Use a proxy URL, ie http://[user[:password]@]host:port'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 18
      end
    end
    object TabDomain: TTabSheet
      Caption = 'Domain'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label9: TLabel
        Left = 5
        Top = 5
        Width = 161
        Height = 14
        Caption = 'Certificate Domain Common Name'
      end
      object Label2: TLabel
        Left = 5
        Top = 45
        Width = 222
        Height = 14
        Caption = 'Web Server UNC HTTP .Well-Known Directory'
      end
      object Label13: TLabel
        Left = 5
        Top = 85
        Width = 218
        Height = 14
        Caption = 'Web Server UNC Public Certificates Directory'
      end
      object Label23: TLabel
        Left = 400
        Top = 5
        Width = 469
        Height = 14
        Caption = 
          'Certificate SAN (Subject Alternate Names)  - Common Name will be' +
          ' added automatically, if missing'
      end
      object DirWellKnown: TEdit
        Left = 5
        Top = 60
        Width = 352
        Height = 22
        TabOrder = 2
      end
      object DirPubWebCert: TEdit
        Left = 5
        Top = 100
        Width = 352
        Height = 22
        TabOrder = 5
      end
      object doTestChallenge: TButton
        Left = 10
        Top = 125
        Width = 142
        Height = 25
        Caption = 'Test Domain Challenges'
        TabOrder = 4
        OnClick = doTestChallengeClick
      end
      object CertSANGrid: TStringGrid
        Left = 400
        Top = 25
        Width = 600
        Height = 234
        ColCount = 4
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 10
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
        TabOrder = 8
        ColWidths = (
          160
          281
          350
          225)
      end
      object CertCommonName: TEdit
        Left = 5
        Top = 20
        Width = 242
        Height = 22
        TabOrder = 0
      end
      object doClearDomain: TButton
        Left = 268
        Top = 20
        Width = 96
        Height = 25
        Caption = 'Clear Domains'
        TabOrder = 1
        OnClick = doClearDomainClick
      end
      object BoxCertFmts: TGroupBox
        Left = 3
        Top = 155
        Width = 365
        Height = 104
        Caption = 'Output Certificate Formats'
        TabOrder = 7
        object CertOutFmtSep: TCheckBox
          Left = 15
          Top = 20
          Width = 146
          Height = 17
          Caption = 'Separate PEM Files'
          TabOrder = 0
        end
        object CertOutFmtBudl: TCheckBox
          Left = 15
          Top = 40
          Width = 97
          Height = 17
          Caption = 'PEM Bundle File'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object CertOutFmtP12: TCheckBox
          Left = 15
          Top = 60
          Width = 146
          Height = 17
          Caption = 'PKCS12/PFX Bundle File'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object CertOutFmtP7: TCheckBox
          Left = 15
          Top = 80
          Width = 121
          Height = 17
          Caption = 'PKCS7 Bundle File'
          TabOrder = 3
        end
        object CertOutFmtReq: TCheckBox
          Left = 185
          Top = 20
          Width = 97
          Height = 21
          Caption = 'CSR PEM File'
          TabOrder = 4
        end
        object CertPwPemKeys: TCheckBox
          Left = 185
          Top = 40
          Width = 146
          Height = 17
          Caption = 'Password PEM Keys'
          TabOrder = 5
        end
        object CertPwPkcs12: TCheckBox
          Left = 185
          Top = 60
          Width = 154
          Height = 17
          Caption = 'Password PKCS12 Keys'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object CertWindowsStore: TCheckBox
          Left = 185
          Top = 80
          Width = 154
          Height = 17
          Caption = 'Windows Cert Store'
          TabOrder = 7
        end
      end
      object SelDirWellKnown: TBitBtn
        Left = 363
        Top = 60
        Width = 31
        Height = 25
        TabOrder = 3
        OnClick = SelDirWellKnownClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object SelDirPubWebCert: TBitBtn
        Left = 363
        Top = 100
        Width = 31
        Height = 25
        TabOrder = 6
        OnClick = SelDirPubWebCertClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
    end
    object TabInfo: TTabSheet
      Caption = 'Cert Admin'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbCountry: TLabel
        Left = 585
        Top = 165
        Width = 66
        Height = 14
        Caption = 'Country Code'
      end
      object lbState: TLabel
        Left = 585
        Top = 135
        Width = 62
        Height = 14
        Caption = 'State/County'
      end
      object lbLocality: TLabel
        Left = 585
        Top = 105
        Width = 37
        Height = 14
        Caption = 'Locality'
      end
      object lbOrganization: TLabel
        Left = 585
        Top = 15
        Width = 91
        Height = 14
        Caption = 'Organization Name'
      end
      object lbOrganizationalUnit: TLabel
        Left = 585
        Top = 45
        Width = 90
        Height = 14
        Caption = 'Organizational Unit'
      end
      object lbEMail: TLabel
        Left = 300
        Top = 45
        Width = 24
        Height = 14
        Caption = 'Email'
      end
      object Label17: TLabel
        Left = 340
        Top = 15
        Width = 51
        Height = 14
        Caption = 'Last Name'
      end
      object Label18: TLabel
        Left = 300
        Top = 75
        Width = 70
        Height = 14
        Caption = 'Phone Number'
      end
      object Label19: TLabel
        Left = 10
        Top = 15
        Width = 62
        Height = 14
        Caption = 'Contact: Title'
      end
      object Label20: TLabel
        Left = 585
        Top = 75
        Width = 42
        Height = 14
        Caption = 'Address'
      end
      object Label21: TLabel
        Left = 762
        Top = 165
        Width = 67
        Height = 14
        Caption = 'Zip/Post Code'
      end
      object Label24: TLabel
        Left = 135
        Top = 15
        Width = 54
        Height = 14
        Caption = ' First Name'
      end
      object Label42: TLabel
        Left = 585
        Top = 190
        Width = 142
        Height = 28
        Caption = 'Certificate Period (days)'#13#10'(1 year = 365, 2 year >= 366)'
      end
      object CertCountry: TEdit
        Left = 706
        Top = 160
        Width = 31
        Height = 22
        TabOrder = 10
      end
      object CertState: TEdit
        Left = 706
        Top = 130
        Width = 190
        Height = 22
        TabOrder = 9
      end
      object CertLocality: TEdit
        Left = 706
        Top = 100
        Width = 190
        Height = 22
        TabOrder = 8
      end
      object CertOrganization: TEdit
        Left = 706
        Top = 10
        Width = 215
        Height = 22
        TabOrder = 5
      end
      object CertOrganizationalUnit: TEdit
        Left = 706
        Top = 40
        Width = 215
        Height = 22
        TabOrder = 6
      end
      object CertContactEmail: TEdit
        Left = 340
        Top = 40
        Width = 221
        Height = 22
        TabOrder = 3
      end
      object CertContactFirst: TEdit
        Left = 200
        Top = 10
        Width = 121
        Height = 22
        TabOrder = 1
      end
      object CertContactLast: TEdit
        Left = 410
        Top = 10
        Width = 151
        Height = 22
        TabOrder = 2
      end
      object CertPhone: TEdit
        Left = 385
        Top = 70
        Width = 176
        Height = 22
        TabOrder = 4
      end
      object CertAddress: TEdit
        Left = 706
        Top = 70
        Width = 215
        Height = 22
        TabOrder = 7
      end
      object CertPostCode: TEdit
        Left = 850
        Top = 160
        Width = 96
        Height = 22
        TabOrder = 11
      end
      object CertContactTitle: TEdit
        Left = 91
        Top = 10
        Width = 38
        Height = 22
        TabOrder = 0
      end
      object PrivKeyType: TRadioGroup
        Left = 10
        Top = 38
        Width = 266
        Height = 191
        Caption = 'Private Key Type and Size'
        ItemIndex = 1
        Items.Strings = (
          'RSA 1,024 bits (level 1 - 80 bits)'
          'RSA 2,048 bits (level 2 - 112 bits) '
          'RSA 3,072 bits (level 3 - 128 bits, NIST min)'
          'RSA 4,096 bits (level 3 - 128 bits)'
          'RSA 7,680 bits (level 4 - 192 bits)'
          'RSA 15,360 bits (level 5 - 256 bits)'
          'Elliptic Curve secp256  (level 3 - 128 bits) '
          'Elliptic Curve secp384  (level 4 - 192 bits) '
          'Elliptic Curve secp512  (level 5 - 256 bits) '
          'EdDSA ED25519 (level 3 - 128 bits)  ')
        TabOrder = 13
      end
      object CertSignDigestType: TRadioGroup
        Left = 282
        Top = 125
        Width = 176
        Height = 105
        Caption = 'Sign Digest Type'
        Columns = 2
        ItemIndex = 2
        Items.Strings = (
          'SHA1 (old)'
          'SHA224'
          'SHA256'
          'SHA384'
          'SHA512'
          'SHA3_224'
          'SHA3_256'
          'SHA3_384'
          'SHA3_512')
        TabOrder = 14
      end
      object CertValidity: TEdit
        Left = 740
        Top = 190
        Width = 35
        Height = 22
        MaxLength = 3
        TabOrder = 12
        Text = '366'
      end
      object CertSerNumType: TRadioGroup
        Left = 464
        Top = 125
        Width = 110
        Height = 54
        Caption = 'Serial Number Type'
        ItemIndex = 0
        Items.Strings = (
          'Random'
          'Sequential')
        TabOrder = 15
      end
    end
    object TabAcme2: TTabSheet
      Caption = 'Acme V2'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label30: TLabel
        Left = 10
        Top = 15
        Width = 114
        Height = 14
        Caption = 'Let'#39's Encrypt v2 Server'
      end
      object Label31: TLabel
        Left = 10
        Top = 43
        Width = 153
        Height = 14
        Caption = 'Let'#39's Encrypt Account Directory'
      end
      object Label32: TLabel
        Left = 10
        Top = 73
        Width = 134
        Height = 14
        Caption = 'Account Key Type and Size'
      end
      object Label33: TLabel
        Left = 15
        Top = 103
        Width = 237
        Height = 70
        Caption = 
          'Each directory is a dedicated Let'#39's Encrypt account with a priva' +
          'te key and account number. Certificates for the account are crea' +
          'ted there.  If there is no private key, a new accoint must be re' +
          'gistered before certificates can be ordered. '
        WordWrap = True
      end
      object LabelAcme2Info: TLabel
        Left = 610
        Top = 5
        Width = 403
        Height = 254
        AutoSize = False
        Caption = 'Information:'
        Color = clYellow
        ParentColor = False
        Transparent = False
        WordWrap = True
      end
      object Label5: TLabel
        Left = 267
        Top = 105
        Width = 71
        Height = 14
        Caption = 'DNS Challenge'
      end
      object AcmeServerV2: TComboBox
        Left = 170
        Top = 10
        Width = 431
        Height = 22
        ItemHeight = 0
        TabOrder = 0
      end
      object DirAcmeConfV2: TEdit
        Left = 170
        Top = 40
        Width = 381
        Height = 22
        TabOrder = 1
      end
      object AccAcmeKeyV2: TComboBox
        Left = 170
        Top = 70
        Width = 251
        Height = 22
        Style = csDropDownList
        ItemHeight = 0
        ItemIndex = 1
        TabOrder = 3
        Text = 'RSA 2,048 bits (level 2 - 112 bits) '
        Items.Strings = (
          'RSA 1,024 bits (level 1 - 80 bits)'
          'RSA 2,048 bits (level 2 - 112 bits) '
          'RSA 3,072 bits (level 3 - 128 bits, NIST min)'
          'RSA 4,096 bits (level 3 - 128 bits)'
          'RSA 7,680 bits (level 4 - 192 bits)'
          'RSA 15,360 bits (level 5 - 256 bits)'
          'Elliptic Curve secp256  (level 3 - 128 bits) '
          'Elliptic Curve secp384  (level 4 - 192 bits) '
          'Elliptic Curve secp512  (level 5 - 256 bits) '
          'EdDSA ED25519 (level 3 - 128 bits)  ')
      end
      object doAcmeAcc: TButton
        Left = 10
        Top = 190
        Width = 121
        Height = 25
        Caption = 'Register Account (1)'
        TabOrder = 4
        OnClick = doAcmeAccClick
      end
      object doAcmeCheckOrder: TButton
        Left = 10
        Top = 225
        Width = 121
        Height = 25
        Caption = 'Check Order (2)'
        TabOrder = 5
        OnClick = doAcmeCheckOrderClick
      end
      object doAcmeStartChallng: TButton
        Left = 270
        Top = 192
        Width = 121
        Height = 25
        Caption = 'Start Challenges (5)'
        Enabled = False
        TabOrder = 8
        OnClick = doAcmeStartChallngClick
      end
      object doAcmeGetCert: TButton
        Left = 270
        Top = 227
        Width = 121
        Height = 25
        Caption = 'Collect Certificate (6)'
        Enabled = False
        TabOrder = 9
        OnClick = doAcmeGetCertClick
      end
      object SelDirAcmeConfV2: TBitBtn
        Left = 570
        Top = 40
        Width = 31
        Height = 25
        TabOrder = 2
        OnClick = SelDirAcmeConfV2Click
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object doAcmeSaveOrder: TButton
        Left = 405
        Top = 190
        Width = 121
        Height = 25
        Caption = 'Save Order'
        TabOrder = 10
        OnClick = doAcmeSaveOrderClick
      end
      object DnsChlgType: TComboBox
        Left = 374
        Top = 100
        Width = 187
        Height = 22
        ItemHeight = 0
        ItemIndex = 0
        TabOrder = 11
        Text = 'Local Windows DNS Server'
        Items.Strings = (
          'Local Windows DNS Server'
          'Cloudfare DNS Server')
      end
      object doAcmeGetChallng: TButton
        Left = 140
        Top = 192
        Width = 121
        Height = 25
        Caption = 'Get Challenges (3)'
        Enabled = False
        TabOrder = 6
        OnClick = doAcmeGetChallngClick
      end
      object doAcmeTestChallng: TButton
        Left = 140
        Top = 227
        Width = 121
        Height = 25
        Caption = 'Test Challenges (4)'
        Enabled = False
        TabOrder = 7
        OnClick = doAcmeTestChallngClick
      end
    end
    object TabCCOrder: TTabSheet
      Caption = 'CertCentre Order'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        Left = 10
        Top = 15
        Width = 88
        Height = 14
        Caption = 'CertCentre Server'
      end
      object Label8: TLabel
        Left = 10
        Top = 45
        Width = 119
        Height = 14
        Caption = 'CertCentre Cert Work Dir'
      end
      object Label10: TLabel
        Left = 355
        Top = 125
        Width = 55
        Height = 28
        Caption = 'CertCentre Order ID'
        WordWrap = True
      end
      object Label12: TLabel
        Left = 10
        Top = 75
        Width = 97
        Height = 42
        Caption = 'Certificate Product'#13#10'(Click for certificate '#13#10'details and cost) '
        WordWrap = True
      end
      object Label16: TLabel
        Left = 365
        Top = 170
        Width = 150
        Height = 14
        Caption = 'Type BUY for commercial certs'
      end
      object LabelCertInfo: TLabel
        Left = 630
        Top = 5
        Width = 383
        Height = 256
        AutoSize = False
        Caption = 'LabelCertInfo'
        Color = clYellow
        ParentColor = False
        Transparent = False
        WordWrap = True
      end
      object Label4: TLabel
        Left = 355
        Top = 68
        Width = 71
        Height = 28
        Caption = 'Domain Approval Email'
        WordWrap = True
      end
      object Label22: TLabel
        Left = 355
        Top = 105
        Width = 72
        Height = 14
        Caption = 'Our Reference'
      end
      object CertCentreServer: TComboBox
        Left = 170
        Top = 10
        Width = 431
        Height = 22
        ItemHeight = 0
        TabOrder = 0
      end
      object DirCertCenConf: TEdit
        Left = 170
        Top = 40
        Width = 396
        Height = 22
        TabOrder = 1
      end
      object doCCProfile: TButton
        Left = 10
        Top = 160
        Width = 82
        Height = 25
        Caption = 'Get Profile'
        TabOrder = 8
        OnClick = doCCProfileClick
      end
      object CertCentreOrderId: TEdit
        Left = 445
        Top = 130
        Width = 179
        Height = 22
        TabOrder = 6
      end
      object doCertCentreCollect: TButton
        Left = 374
        Top = 199
        Width = 84
        Height = 25
        Caption = 'Collect Order'
        TabOrder = 12
        OnClick = doCertCentreCollectClick
      end
      object doCertCentreOther: TButton
        Left = 115
        Top = 199
        Width = 123
        Height = 25
        Caption = 'Order Commercial Cert'
        Enabled = False
        TabOrder = 10
        OnClick = doCertCentreOtherClick
      end
      object doCertCentreOrders: TButton
        Left = 277
        Top = 199
        Width = 84
        Height = 25
        Caption = 'List Orders'
        TabOrder = 11
        OnClick = doCertCentreOrdersClick
      end
      object CertCentreApprovEmail: TComboBox
        Left = 445
        Top = 70
        Width = 179
        Height = 22
        ItemHeight = 0
        TabOrder = 4
      end
      object CertBuy: TEdit
        Left = 540
        Top = 167
        Width = 82
        Height = 22
        Enabled = False
        TabOrder = 7
      end
      object doCertCentreCheck: TButton
        Left = 10
        Top = 200
        Width = 82
        Height = 25
        Caption = 'Check Order'
        TabOrder = 9
        OnClick = doCertCentreCheckClick
      end
      object CertCentreProducts: TListBox
        Left = 115
        Top = 70
        Width = 226
        Height = 116
        ItemHeight = 14
        TabOrder = 3
        OnClick = CertCentreProductsClick
      end
      object CertCentreOrderRef: TEdit
        Left = 445
        Top = 100
        Width = 179
        Height = 22
        TabOrder = 5
      end
      object SelDirCertCenConf: TBitBtn
        Left = 577
        Top = 40
        Width = 31
        Height = 25
        TabOrder = 2
        OnClick = SelDirCertCenConfClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object doCertCentreSaveOrder: TButton
        Left = 477
        Top = 199
        Width = 84
        Height = 25
        Caption = 'Save Order'
        TabOrder = 13
        OnClick = doCertCentreSaveOrderClick
      end
    end
    object TabCCOAuth: TTabSheet
      Caption = 'CertCentre OAuth'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BoxOAuthApp: TGroupBox
        Left = 10
        Top = 3
        Width = 381
        Height = 202
        Caption = 'CertCentre Permanent Web Server OAuth App'
        TabOrder = 0
        object Label7: TLabel
          Left = 5
          Top = 25
          Width = 68
          Height = 14
          Caption = 'App Auth URL'
        end
        object Label11: TLabel
          Left = 5
          Top = 55
          Width = 37
          Height = 14
          Caption = 'Client Id'
        end
        object Label25: TLabel
          Left = 5
          Top = 85
          Width = 61
          Height = 14
          Caption = 'Client Secret'
        end
        object Label26: TLabel
          Left = 5
          Top = 115
          Width = 60
          Height = 14
          Caption = 'Redirect-URI'
        end
        object Label27: TLabel
          Left = 5
          Top = 145
          Width = 74
          Height = 14
          Caption = 'App Token URL'
        end
        object Label34: TLabel
          Left = 5
          Top = 175
          Width = 31
          Height = 14
          Caption = 'Scope'
        end
        object OAuthAppUrl: TEdit
          Left = 90
          Top = 20
          Width = 281
          Height = 22
          TabOrder = 0
        end
        object OAuthClientId: TEdit
          Left = 90
          Top = 50
          Width = 281
          Height = 22
          TabOrder = 1
        end
        object OAuthClientSecret: TEdit
          Left = 90
          Top = 80
          Width = 146
          Height = 22
          TabOrder = 2
        end
        object OAuthRedirectUrl: TEdit
          Left = 90
          Top = 110
          Width = 281
          Height = 22
          TabOrder = 3
        end
        object OAuthTokenUrl: TEdit
          Left = 90
          Top = 140
          Width = 281
          Height = 22
          TabOrder = 4
        end
        object OAuthScope: TEdit
          Left = 90
          Top = 170
          Width = 281
          Height = 22
          TabOrder = 5
        end
      end
      object BoxOAuthTokens: TGroupBox
        Left = 410
        Top = 5
        Width = 371
        Height = 202
        Caption = 'Short Lived Codes and Tokens'
        TabOrder = 1
        object Label35: TLabel
          Left = 5
          Top = 25
          Width = 92
          Height = 14
          Caption = 'Authorization Code'
        end
        object Label36: TLabel
          Left = 5
          Top = 55
          Width = 69
          Height = 14
          Caption = 'Access Token'
        end
        object Label37: TLabel
          Left = 5
          Top = 85
          Width = 70
          Height = 14
          Caption = 'Refresh Token'
        end
        object Label38: TLabel
          Left = 5
          Top = 115
          Width = 67
          Height = 14
          Caption = 'Tokens Expire'
        end
        object OAuthAuthCode: TEdit
          Left = 110
          Top = 20
          Width = 231
          Height = 22
          TabOrder = 0
        end
        object OAuthAccToken: TEdit
          Left = 110
          Top = 50
          Width = 231
          Height = 22
          TabOrder = 1
        end
        object OAuthRefToken: TEdit
          Left = 110
          Top = 80
          Width = 231
          Height = 22
          TabOrder = 2
        end
        object OAuthExpire: TEdit
          Left = 110
          Top = 110
          Width = 181
          Height = 22
          TabOrder = 3
        end
        object OAuthAutoRefresh: TCheckBox
          Left = 5
          Top = 142
          Width = 231
          Height = 17
          Caption = 'Automatic Refresh, Minutes Before Expiry'
          TabOrder = 4
        end
        object OAuthRefrMins: TEdit
          Left = 257
          Top = 140
          Width = 64
          Height = 22
          TabOrder = 5
          Text = '120'
        end
        object doOARefreshNow: TButton
          Left = 10
          Top = 165
          Width = 96
          Height = 25
          Caption = 'Refresh Now'
          TabOrder = 6
          OnClick = doOARefreshNowClick
        end
        object doOACodeToken: TButton
          Left = 125
          Top = 165
          Width = 121
          Height = 25
          Caption = 'Token from Auth Code'
          TabOrder = 7
          OnClick = doOACodeTokenClick
        end
      end
      object BoxOAuthWeb: TGroupBox
        Left = 795
        Top = 3
        Width = 185
        Height = 105
        Caption = 'OAuth Redirect Web Server'
        TabOrder = 2
        object Label39: TLabel
          Left = 5
          Top = 25
          Width = 80
          Height = 14
          Caption = 'Web Server Port'
        end
        object Label40: TLabel
          Left = 5
          Top = 50
          Width = 69
          Height = 14
          Caption = 'Web Server IP'
        end
        object OAuthWebPort: TEdit
          Left = 99
          Top = 20
          Width = 56
          Height = 22
          MaxLength = 5
          TabOrder = 0
          Text = '8080'
        end
        object OAuthWebIP: TComboBox
          Left = 5
          Top = 70
          Width = 171
          Height = 22
          ItemHeight = 0
          TabOrder = 1
          Text = '127.0.0.1'
        end
      end
    end
    object TabServtas: TTabSheet
      Caption = 'Servertastic'
      ImageIndex = 7
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabOwnCA: TTabSheet
      Caption = 'Own CA'
      ImageIndex = 9
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label48: TLabel
        Left = 10
        Top = 205
        Width = 96
        Height = 14
        Caption = 'Certificate Directory'
      end
      object LabelCertOwnCA: TLabel
        Left = 650
        Top = 65
        Width = 363
        Height = 195
        AutoSize = False
        Caption = 'Information'
        Color = clYellow
        ParentColor = False
        Transparent = False
        WordWrap = True
      end
      object CABox: TGroupBox
        Left = 10
        Top = 6
        Width = 631
        Height = 183
        Caption = 'Own Certificate Authority - for signing certificates'
        TabOrder = 0
        object Label44: TLabel
          Left = 10
          Top = 55
          Width = 91
          Height = 14
          Caption = 'CA Private Key File'
        end
        object Label46: TLabel
          Left = 10
          Top = 20
          Width = 82
          Height = 28
          Caption = 'CA Certificate or Bundle File'
          WordWrap = True
        end
        object LabelOwnCA: TLabel
          Left = 118
          Top = 110
          Width = 501
          Height = 61
          AutoSize = False
          Caption = 'CA Information'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object Label49: TLabel
          Left = 10
          Top = 85
          Width = 125
          Height = 14
          Caption = 'CA Private Key Password'
        end
        object Label28: TLabel
          Left = 270
          Top = 85
          Width = 321
          Height = 14
          Caption = 
            'You can create the CA certificate using the Self Signed Cert but' +
            'ton'
        end
        object CAPkeyFile: TEdit
          Left = 115
          Top = 50
          Width = 454
          Height = 22
          TabOrder = 2
        end
        object CACertFile: TEdit
          Left = 115
          Top = 20
          Width = 454
          Height = 22
          TabOrder = 0
        end
        object SelCACertFile: TBitBtn
          Left = 583
          Top = 20
          Width = 31
          Height = 25
          TabOrder = 1
          OnClick = SelCACertFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object SelCAPkeyFile: TBitBtn
          Left = 583
          Top = 50
          Width = 31
          Height = 25
          TabOrder = 3
          OnClick = SelCAPkeyFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object doLoadCA: TButton
          Left = 14
          Top = 123
          Width = 75
          Height = 25
          Caption = 'Load CA'
          TabOrder = 5
          OnClick = doLoadCAClick
        end
        object CAPkeyPw: TEdit
          Left = 154
          Top = 80
          Width = 97
          Height = 22
          PasswordChar = '*'
          TabOrder = 4
        end
      end
      object doSelfSigned: TButton
        Left = 801
        Top = 22
        Width = 103
        Height = 25
        Caption = 'Self Signed Cert'
        TabOrder = 3
        OnClick = doSelfSignedClick
      end
      object doCASignCert: TButton
        Left = 657
        Top = 25
        Width = 102
        Height = 25
        Caption = 'CA Signed Cert'
        TabOrder = 2
        OnClick = doCASignCertClick
      end
      object OwnCACertDir: TEdit
        Left = 137
        Top = 200
        Width = 442
        Height = 22
        TabOrder = 1
      end
      object SelOwnCACertDir: TBitBtn
        Left = 593
        Top = 200
        Width = 31
        Height = 25
        TabOrder = 4
        OnClick = SelOwnCACertDirClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object SelfSignedCA: TCheckBox
        Left = 910
        Top = 29
        Width = 97
        Height = 17
        Caption = 'CA Cert'
        TabOrder = 5
      end
    end
    object TabDatabase: TTabSheet
      Caption = 'Supplier Database'
      ImageIndex = 8
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label43: TLabel
        Left = 10
        Top = 15
        Width = 86
        Height = 14
        Caption = 'Supplier Directory'
      end
      object LabelDB: TLabel
        Left = 635
        Top = 45
        Width = 378
        Height = 56
        AutoSize = False
        Caption = 'Supplier Information:'
        Color = clYellow
        ParentColor = False
        Transparent = False
        WordWrap = True
      end
      object Label45: TLabel
        Left = 10
        Top = 35
        Width = 278
        Height = 16
        AutoSize = False
        Caption = 'Supplier Certificate Orders'
        WordWrap = True
      end
      object LabelInfoDomain: TLabel
        Left = 635
        Top = 105
        Width = 378
        Height = 155
        AutoSize = False
        Caption = 'Order Information:'
        Color = clYellow
        ParentColor = False
        Transparent = False
        WordWrap = True
      end
      object DirDatabase: TComboBox
        Left = 120
        Top = 10
        Width = 381
        Height = 22
        ItemHeight = 0
        TabOrder = 0
      end
      object SelDirDatabase: TBitBtn
        Left = 513
        Top = 10
        Width = 31
        Height = 25
        TabOrder = 1
        OnClick = SelDirDatabaseClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object doOpenDatabase: TButton
        Left = 572
        Top = 10
        Width = 99
        Height = 25
        Caption = 'Open Supplier'
        TabOrder = 2
        OnClick = doOpenDatabaseClick
      end
      object doDBCheck: TButton
        Left = 524
        Top = 44
        Width = 100
        Height = 20
        Caption = 'Check Order'
        Enabled = False
        TabOrder = 4
        OnClick = doDBCheckClick
      end
      object doDBOrder: TButton
        Left = 525
        Top = 94
        Width = 100
        Height = 20
        Caption = 'Order Certificate'
        Enabled = False
        TabOrder = 6
        OnClick = doDBOrderClick
      end
      object doCloseDatabase: TButton
        Left = 690
        Top = 10
        Width = 99
        Height = 25
        Caption = 'Close Supplier'
        Enabled = False
        TabOrder = 3
        OnClick = doCloseDatabaseClick
      end
      object DatabaseDomains: TListView
        Left = 9
        Top = 55
        Width = 510
        Height = 204
        Columns = <
          item
            Caption = 'Common Name'
            Width = 180
          end
          item
            Caption = 'Issue State'
            Width = 70
          end
          item
            Caption = 'Order Id'
            Width = 80
          end
          item
            Caption = 'Issued'
            Width = 70
          end
          item
            Caption = 'Exoires'
            Width = 70
          end
          item
            Caption = 'Challenge'
            Width = 130
          end
          item
            Caption = 'Product'
            Width = 150
          end
          item
            Caption = 'Subject Alternate Names'
            Width = 300
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 12
        ViewStyle = vsReport
        OnClick = DatabaseDomainsClick
        OnSelectItem = DatabaseDomainsSelectItem
      end
      object doDBRevoke: TButton
        Left = 525
        Top = 195
        Width = 100
        Height = 20
        Caption = 'Revoke Certificate'
        Enabled = False
        TabOrder = 10
        OnClick = doDBRevokeClick
      end
      object doDBCollect: TButton
        Left = 525
        Top = 120
        Width = 100
        Height = 20
        Caption = 'Collect Order'
        Enabled = False
        TabOrder = 8
        OnClick = doDBCollectClick
      end
      object doDBCancel: TButton
        Left = 525
        Top = 171
        Width = 100
        Height = 20
        Caption = 'Cancel Order'
        Enabled = False
        TabOrder = 9
        OnClick = doDBCancelClick
      end
      object doDBRemove: TButton
        Left = 525
        Top = 220
        Width = 100
        Height = 20
        Caption = 'Remove Order'
        Enabled = False
        TabOrder = 11
        OnClick = doDBRemoveClick
      end
      object doDBRedist: TButton
        Left = 525
        Top = 145
        Width = 100
        Height = 20
        Caption = 'Redistribute'
        Enabled = False
        TabOrder = 7
        OnClick = doDBRedistClick
      end
      object doDBListChallg: TButton
        Left = 805
        Top = 9
        Width = 99
        Height = 25
        Caption = 'List Challenges'
        Enabled = False
        TabOrder = 13
        OnClick = doDBListChallgClick
      end
      object doDBGetChallg: TButton
        Left = 525
        Top = 70
        Width = 100
        Height = 20
        Caption = 'Get Challenges'
        Enabled = False
        TabOrder = 5
        OnClick = doDBGetChallgClick
      end
    end
    object TabChallenges: TTabSheet
      Caption = 'Challenges'
      ImageIndex = 10
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label47: TLabel
        Left = 10
        Top = 5
        Width = 152
        Height = 14
        Caption = 'Outstanding Domain Challenges'
      end
      object DatabaseChallg: TListView
        Left = 3
        Top = 25
        Width = 468
        Height = 196
        Columns = <>
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object X509Certs1: TSslX509Certs
    AcmeAccKeyType = PrivKeyRsa2048
    AutoOrderComplete = False
    CertSubAltNames = <>
    CertCsrOrigin = CsrOriginProps
    CertOutFmts = []
    CertSerNumType = SerNumRandom
    CertSignDigestType = Digest_sha256
    CertValidity = 0
    DebugLevel = DebugConn
    DomWebSrvIP = '0.0.0.0'
    KeepOldCA = False
    LogJson = False
    LogPkeys = False
    OAAuthType = OAuthTypeWeb
    OARefreshAuto = False
    OARefrMinsPrior = 120
    OAWebSrvIP = '127.0.0.1'
    OAWebSrvPort = '8080'
    PrivKeyCipher = PrivKeyEncNone
    PrivKeyType = PrivKeyRsa2048
    AutoAccountClose = False
    AccountTimeOutMins = 10
    SeqOrderNum = 0
    SocketFamily = sfAny
    SuppCertChallenge = ChallFileUNC
    SupplierProto = SuppProtoNone
    OnCertProg = X509Certs1CertProg
    OnNewCert = X509Certs1NewCert
    OnNewToken = X509Certs1NewToken
    OnOAuthAuthUrl = X509Certs1OAuthAuthUrl
    OnDomainsRefresh = X509Certs1DomainsRefresh
    OnSuppDBRefresh = X509Certs1SuppDBRefresh
    OnChallgRefresh = X509Certs1ChallgRefresh
    OnChallengeEmail = X509Certs1ChallengeEmail
    OnChallengeFTP = X509Certs1ChallengeFTP
    OnChallengeDNS = X509Certs1ChallengeDNS
    Left = 75
    Top = 325
  end
  object OpenDirDiag: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Select Directory'
    Left = 215
    Top = 750
  end
  object OpenFileDlg: TOpenDialog
    Filter = 'All Files *.*|*.*|PEM Files *.pem|*.pem'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Title = 'Select Certificate File'
    Left = 178
    Top = 747
  end
end
