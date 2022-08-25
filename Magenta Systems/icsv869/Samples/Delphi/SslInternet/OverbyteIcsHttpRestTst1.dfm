object HttpRestForm: THttpRestForm
  Left = 86
  Top = 176
  Caption = 
    'ICS HTTPS REST and OAuth Demo - http://www.overbyte.be - V8.69 -' +
    ' 20th May 2022'
  ClientHeight = 741
  ClientWidth = 900
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 900
    Height = 376
    ActivePage = TabREST
    Align = alTop
    TabOrder = 0
    object TabREST: TTabSheet
      Caption = 'HTTPS REST'
      object Label1: TLabel
        Left = 10
        Top = 5
        Width = 87
        Height = 14
        Caption = 'REST Paramaters '
      end
      object Label3: TLabel
        Left = 10
        Top = 145
        Width = 271
        Height = 14
        Caption = 'Raw Parameters (not blank overrides REST Parameters)'
      end
      object Label5: TLabel
        Left = 10
        Top = 185
        Width = 104
        Height = 14
        Caption = 'URL (no ? or Params)'
      end
      object Label33: TLabel
        Left = 10
        Top = 293
        Width = 153
        Height = 28
        Caption = 'Double click on a Json stObject or stArray item to expand.  '
        WordWrap = True
      end
      object Label64: TLabel
        Left = 10
        Top = 235
        Width = 68
        Height = 14
        Caption = 'Download File'
      end
      object LabelProgress: TLabel
        Left = 10
        Top = 327
        Width = 879
        Height = 16
        AutoSize = False
        Caption = 'LabelProgress'
        Color = clYellow
        ParentColor = False
      end
      object Label65: TLabel
        Left = 10
        Top = 265
        Width = 55
        Height = 14
        Caption = 'Upload File '
      end
      object RestURL: TComboBox
        Left = 10
        Top = 200
        Width = 591
        Height = 22
        ItemHeight = 14
        TabOrder = 4
        Text = 'https://jsonplaceholder.typicode.com/posts/1'
        Items.Strings = (
          'https://acme-v02.api.letsencrypt.org/directory'
          'https://jsonplaceholder.typicode.com/posts/1'
          'https://jsonplaceholder.typicode.com/posts'
          'https://jsonplaceholder.typicode.com/users'
          'https://reqres.in/api/users/2'
          'https://reqres.in/api/users'
          'https://fakerestapi.azurewebsites.net/api/Activities'
          'https://www.magsys.co.uk/delphi/dfiles/default.asp'
          'https://www.telecom-tariffs.co.uk/serverinfo.htm'
          'https://api.cix.uk/.well-known/openid-configuration'
          'https://accounts.google.com/.well-known/openid-configuration'
          'https://gmail.googleapis.com/$discovery/rest?version=v1'
          'https://www.googleapis.com/gmail/v1/users/me/profile'
          'https://www.googleapis.com/gmail/v1/users/me/messages'
          'https://www.googleapis.com/gmail/v1/users/me/messages/{id}'
          'https://www.googleapis.com/gmail/v1/users/me/settings/Pop'
          'https://www.w3schools.com/xml/note.xml'
          'https://www.w3schools.com/xml/simple.xml'
          'https://www.w3schools.com/xml/cd_catalog.xml')
      end
      object GridParams: TStringGrid
        Left = 10
        Top = 20
        Width = 576
        Height = 120
        Hint = 
          'Possible Data Types are: '#13#10'RPTypeStr'#13#10'RPTypeInt'#13#10'RPTypeDate (ISO' +
          ' format)'#13#10'RPTypeFloat'#13#10'RPTypeBool'#13#10'RPTypeObj (Json) '#13#10'RPTypeArra' +
          'y (comma list) '
        ColCount = 3
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 10
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnSelectCell = GridParamsSelectCell
        ColWidths = (
          133
          254
          125)
      end
      object ParamContent: TRadioGroup
        Left = 600
        Top = 20
        Width = 149
        Height = 129
        Caption = 'REST Content'
        ItemIndex = 0
        Items.Strings = (
          'URL Eencoded ? URL'
          'Json ? URL'
          'XML ? URL'
          'URL Eencoded Body'
          'Json Body'
          'XML Body')
        TabOrder = 1
      end
      object ReqMode: TRadioGroup
        Left = 618
        Top = 185
        Width = 109
        Height = 56
        Caption = 'Request Mode'
        ItemIndex = 0
        Items.Strings = (
          'Sync Request'
          'Async Request')
        TabOrder = 5
      end
      object RawParams: TEdit
        Left = 10
        Top = 160
        Width = 717
        Height = 22
        TabOrder = 3
      end
      object ReqType: TRadioGroup
        Left = 770
        Top = 20
        Width = 96
        Height = 129
        Caption = 'Request Type'
        ItemIndex = 0
        Items.Strings = (
          'GET '
          'POST '
          'HEAD '
          'PUT '
          'DELETE'
          'PATCH ')
        TabOrder = 2
      end
      object doStartReq: TButton
        Left = 175
        Top = 298
        Width = 122
        Height = 25
        Caption = 'Start REST Request'
        TabOrder = 13
        OnClick = doStartReqClick
      end
      object doClear: TButton
        Left = 411
        Top = 298
        Width = 75
        Height = 25
        Caption = 'Clear Log'
        TabOrder = 15
        OnClick = doClearClick
      end
      object doAbort: TButton
        Left = 318
        Top = 298
        Width = 75
        Height = 25
        Caption = 'Abort'
        TabOrder = 14
        OnClick = doAbortClick
      end
      object doClearParams: TButton
        Left = 506
        Top = 298
        Width = 111
        Height = 25
        Caption = 'Clear REST Params'
        TabOrder = 16
        OnClick = doClearParamsClick
      end
      object ReqMemStrategy: TRadioGroup
        Left = 739
        Top = 160
        Width = 141
        Height = 83
        Caption = 'Memory Strategy'
        ItemIndex = 0
        Items.Strings = (
          'Memory Stream'
          'Memory/Temp File'
          'Download File'
          'Resume Download')
        TabOrder = 11
      end
      object ReqReplFile: TCheckBox
        Left = 618
        Top = 258
        Width = 103
        Height = 30
        Caption = 'Replace Download File'
        TabOrder = 10
        WordWrap = True
      end
      object RestDownFile: TEdit
        Left = 89
        Top = 230
        Width = 460
        Height = 22
        TabOrder = 6
      end
      object SelDirDown: TBitBtn
        Left = 570
        Top = 227
        Width = 31
        Height = 25
        TabOrder = 7
        OnClick = SelDirDownClick
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
      object doExit: TButton
        Left = 639
        Top = 298
        Width = 75
        Height = 25
        Caption = 'Exit'
        TabOrder = 17
        OnClick = doExitClick
      end
      object HttpUploadFile: TEdit
        Left = 89
        Top = 260
        Width = 460
        Height = 22
        TabOrder = 8
      end
      object SelDirUpload: TBitBtn
        Left = 570
        Top = 258
        Width = 31
        Height = 25
        TabOrder = 9
        OnClick = SelDirUploadClick
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
      object HttpUploadStrat: TRadioGroup
        Left = 739
        Top = 258
        Width = 141
        Height = 63
        Caption = 'Upload File (post/put)'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Simple'
          'MIME Multipart')
        TabOrder = 12
      end
    end
    object TabSettings: TTabSheet
      Caption = 'REST Settings'
      ImageIndex = 1
      object Label2: TLabel
        Left = 10
        Top = 278
        Width = 136
        Height = 14
        Caption = 'Optional Root CA Bundle File'
      end
      object Label4: TLabel
        Left = 480
        Top = 5
        Width = 56
        Height = 44
        AutoSize = False
        Caption = 'Special Extra Header(s)'
        WordWrap = True
      end
      object Label6: TLabel
        Left = 460
        Top = 110
        Width = 52
        Height = 14
        Caption = 'Auth Login'
      end
      object Label7: TLabel
        Left = 460
        Top = 140
        Width = 76
        Height = 14
        Caption = 'Auth Password'
      end
      object Label8: TLabel
        Left = 460
        Top = 170
        Width = 64
        Height = 14
        Caption = 'Bearer/Token'
      end
      object Label9: TLabel
        Left = 460
        Top = 247
        Width = 253
        Height = 14
        Caption = 'Optional Client SSL Certificate (if required by server)'
      end
      object Label22: TLabel
        Left = 460
        Top = 230
        Width = 65
        Height = 14
        Caption = 'Log Directory'
      end
      object Label31: TLabel
        Left = 460
        Top = 200
        Width = 51
        Height = 14
        Caption = 'Proxy URL'
      end
      object Label32: TLabel
        Left = 460
        Top = 80
        Width = 139
        Height = 14
        Caption = 'App Layer Proto Neg (ALPN)'
      end
      object Label63: TLabel
        Left = 430
        Top = 300
        Width = 102
        Height = 14
        Caption = 'Accept Content-Type'
      end
      object DebugLogging: TRadioGroup
        Left = 10
        Top = 5
        Width = 121
        Height = 136
        Caption = 'Debug Logging'
        ItemIndex = 3
        Items.Strings = (
          'None'
          'Connections'
          'Parameters'
          'SSL Negotiation'
          'HTTP Headers'
          'HTML Body'
          'Ssl Low Level')
        TabOrder = 0
      end
      object SslSecurity: TRadioGroup
        Left = 145
        Top = 5
        Width = 169
        Height = 246
        Caption = 'SSL Security Level'
        ItemIndex = 6
        Items.Strings = (
          'Ignore'
          'None'
          'SSLv3 Only'
          'TLSv1 Only'
          'TLSv1.1 Only'
          'TLSv1.2 Only'
          'TLSv1.3 Only'
          'TLSv1 or Better'
          'TLSv1.1 or Better'
          'TLSv1.2 or Better'
          'Backward Ciphers'
          'Intermediate Ciphers'
          'High Ciphers, 2048 keys'
          'High Ciphers, 3072 keys'
          'High Ciphers, 7680 keys')
        TabOrder = 2
        OnClick = SettingsChange
      end
      object CertVerMethod: TRadioGroup
        Left = 325
        Top = 5
        Width = 134
        Height = 69
        Caption = 'SSL Certificate Check'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'PEM Bundle File'
          'Windows Cert Store')
        TabOrder = 5
      end
      object ReportCertChain: TCheckBox
        Left = 325
        Top = 100
        Width = 111
        Height = 29
        Caption = 'Report SSL Certificate Chain'
        TabOrder = 7
        WordWrap = True
      end
      object SslClientCertFile: TEdit
        Left = 460
        Top = 265
        Width = 420
        Height = 22
        TabOrder = 17
      end
      object ExtraHeaders: TMemo
        Left = 545
        Top = 5
        Width = 290
        Height = 59
        Lines.Strings = (
          'ExtraHeaders')
        TabOrder = 9
      end
      object AuthType: TRadioGroup
        Left = 325
        Top = 135
        Width = 117
        Height = 154
        Caption = 'Authentication'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Basic'
          'NTLM'
          'Digest MD5'
          'Bearer Token'
          'XAuth Token'
          'Json Web Token'
          'OAuth Bearer'
          'Digest SHA-256')
        TabOrder = 8
      end
      object AuthLogin: TEdit
        Left = 542
        Top = 105
        Width = 293
        Height = 22
        TabOrder = 11
      end
      object AuthPassword: TEdit
        Left = 543
        Top = 135
        Width = 292
        Height = 22
        PasswordChar = '*'
        TabOrder = 12
      end
      object AuthBearer: TEdit
        Left = 543
        Top = 165
        Width = 337
        Height = 22
        Hint = 'A token is used instead of Auth Login credentials'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
      end
      object SslRootBundleFile: TEdit
        Left = 10
        Top = 298
        Width = 414
        Height = 22
        TabOrder = 3
      end
      object IpSockFamily: TRadioGroup
        Left = 10
        Top = 150
        Width = 121
        Height = 92
        Caption = 'Socket Family'
        ItemIndex = 0
        Items.Strings = (
          'Any'
          'Prefer IPv4'
          'Prefer IPv6'
          'Only IPv4 '
          'Only IPv6')
        TabOrder = 1
      end
      object SelDirLogs: TBitBtn
        Left = 849
        Top = 221
        Width = 31
        Height = 25
        TabOrder = 16
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
      object DirLogs: TEdit
        Left = 542
        Top = 225
        Width = 293
        Height = 22
        TabOrder = 15
      end
      object ProxyURL: TEdit
        Left = 542
        Top = 195
        Width = 338
        Height = 22
        Hint = 'Use a proxy URL, ie http://[user[:password]@]host:port'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 14
      end
      object AlpnProtos: TEdit
        Left = 645
        Top = 75
        Width = 141
        Height = 22
        Hint = 
          'May be blank, or http/1.1 to negotiate that protocol specificall' +
          'y'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
      end
      object doListCertStore: TButton
        Left = 164
        Top = 264
        Width = 85
        Height = 25
        Caption = 'List Cert Store'
        TabOrder = 4
        OnClick = doListCertStoreClick
      end
      object ReqAccept: TComboBox
        Left = 545
        Top = 295
        Width = 335
        Height = 22
        ItemHeight = 14
        TabOrder = 18
        Text = '*.*'
        Items.Strings = (
          '*/*'
          'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
          'text/*, application/*'
          'application/json'
          'application/xml')
      end
      object CertRevoke: TCheckBox
        Left = 325
        Top = 80
        Width = 124
        Height = 17
        Caption = 'Check Cert Revoke'
        TabOrder = 6
      end
    end
    object TabOAuth: TTabSheet
      Caption = 'OAuth2'
      ImageIndex = 2
      object Label19: TLabel
        Left = 170
        Top = 265
        Width = 69
        Height = 14
        Caption = 'Web Server IP'
      end
      object Label20: TLabel
        Left = 170
        Top = 290
        Width = 80
        Height = 14
        Caption = 'Web Server Port'
      end
      object GroupBox1: TGroupBox
        Left = 10
        Top = 5
        Width = 486
        Height = 246
        Caption = 'Permanent Web Server OAuth2 App'
        TabOrder = 0
        object Label10: TLabel
          Left = 5
          Top = 25
          Width = 63
          Height = 14
          Caption = 'Account Title'
        end
        object Label11: TLabel
          Left = 3
          Top = 40
          Width = 81
          Height = 29
          AutoSize = False
          Caption = 'Console URL (optional)'
          WordWrap = True
        end
        object Label12: TLabel
          Left = 5
          Top = 125
          Width = 61
          Height = 14
          Caption = 'Client Secret'
        end
        object Label13: TLabel
          Left = 5
          Top = 150
          Width = 60
          Height = 14
          Caption = 'Redirect-URI'
        end
        object Label14: TLabel
          Left = 5
          Top = 175
          Width = 74
          Height = 14
          Caption = 'App Token URL'
        end
        object Label21: TLabel
          Left = 5
          Top = 200
          Width = 31
          Height = 14
          Caption = 'Scope'
        end
        object Label34: TLabel
          Left = 217
          Top = 220
          Width = 33
          Height = 14
          Caption = 'Prompt'
        end
        object Label54: TLabel
          Left = 5
          Top = 100
          Width = 37
          Height = 14
          Caption = 'Client Id'
        end
        object Label55: TLabel
          Left = 5
          Top = 75
          Width = 76
          Height = 14
          Caption = 'Application URL'
        end
        object OAuthAppUrl: TEdit
          Left = 90
          Top = 70
          Width = 376
          Height = 22
          TabOrder = 2
        end
        object OAuthClientId: TEdit
          Left = 90
          Top = 95
          Width = 376
          Height = 22
          TabOrder = 3
        end
        object OAuthClientSecret: TEdit
          Left = 90
          Top = 120
          Width = 236
          Height = 22
          TabOrder = 4
        end
        object OAuthRedirectUrl: TEdit
          Left = 90
          Top = 145
          Width = 376
          Height = 22
          TabOrder = 6
        end
        object OAuthTokenUrl: TEdit
          Left = 90
          Top = 170
          Width = 376
          Height = 22
          TabOrder = 7
        end
        object OAuthScope: TEdit
          Left = 90
          Top = 195
          Width = 376
          Height = 22
          TabOrder = 8
        end
        object OAuthOptNoRedir: TCheckBox
          Left = 345
          Top = 120
          Width = 121
          Height = 17
          Caption = 'Auth No Redirect'
          TabOrder = 5
        end
        object OAuthPrompt: TEdit
          Left = 263
          Top = 220
          Width = 203
          Height = 22
          TabOrder = 10
        end
        object OAuthAccess: TCheckBox
          Left = 8
          Top = 220
          Width = 203
          Height = 17
          Caption = 'Access Offline (get Refresh Token)'
          TabOrder = 9
        end
        object OAuthAccTitle: TComboBox
          Left = 90
          Top = 20
          Width = 160
          Height = 22
          Hint = 'Click Button for List of Common Accounts'
          Style = csDropDownList
          ItemHeight = 14
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = OAuthAccTitleChange
        end
        object OAuthConsoleUrl: TEdit
          Left = 90
          Top = 45
          Width = 376
          Height = 22
          TabOrder = 1
        end
      end
      object GroupBox2: TGroupBox
        Left = 520
        Top = 5
        Width = 360
        Height = 171
        Caption = 'Short Lived OAuth2 Codes and Tokens'
        TabOrder = 1
        object Label15: TLabel
          Left = 5
          Top = 25
          Width = 92
          Height = 14
          Caption = 'Authorization Code'
        end
        object Label16: TLabel
          Left = 5
          Top = 75
          Width = 69
          Height = 14
          Caption = 'Access Token'
        end
        object Label17: TLabel
          Left = 5
          Top = 50
          Width = 70
          Height = 14
          Caption = 'Refresh Token'
        end
        object Label18: TLabel
          Left = 5
          Top = 100
          Width = 67
          Height = 14
          Caption = 'Token Expires'
        end
        object LabelResult: TLabel
          Left = 10
          Top = 145
          Width = 36
          Height = 14
          Caption = 'Result: '
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
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
          Top = 70
          Width = 231
          Height = 22
          Enabled = False
          TabOrder = 2
        end
        object OAuthRefToken: TEdit
          Left = 110
          Top = 45
          Width = 231
          Height = 22
          TabOrder = 1
        end
        object OAuthExpire: TEdit
          Left = 110
          Top = 95
          Width = 181
          Height = 22
          Enabled = False
          TabOrder = 3
        end
        object OAuthAutoRefresh: TCheckBox
          Left = 5
          Top = 120
          Width = 231
          Height = 17
          Caption = 'Automatic Refresh, Minutes Before Expiry'
          TabOrder = 4
        end
        object OAuthRefrMins: TEdit
          Left = 257
          Top = 120
          Width = 64
          Height = 22
          TabOrder = 5
          Text = '120'
        end
      end
      object OAuthAuthType: TRadioGroup
        Left = 10
        Top = 255
        Width = 135
        Height = 70
        Caption = 'OAuth Authentication'
        ItemIndex = 0
        Items.Strings = (
          'Local Web Server'
          'Manual Code'
          'Embedded Browser')
        TabOrder = 2
      end
      object OAuthWebIP: TComboBox
        Left = 270
        Top = 260
        Width = 123
        Height = 22
        ItemHeight = 14
        TabOrder = 3
        Text = '127.0.0.1'
      end
      object OAuthWebPort: TEdit
        Left = 270
        Top = 288
        Width = 56
        Height = 22
        MaxLength = 5
        TabOrder = 4
        Text = '8080'
      end
      object doOAuthLogin: TButton
        Left = 527
        Top = 227
        Width = 106
        Height = 25
        Caption = 'Login to App (2)'
        TabOrder = 6
        OnClick = doOAuthLoginClick
      end
      object doOAuthToken: TButton
        Left = 527
        Top = 258
        Width = 106
        Height = 25
        Caption = 'Get New Token (3)'
        TabOrder = 7
        OnClick = doOAuthTokenClick
      end
      object doOAuthRefresh: TButton
        Left = 527
        Top = 289
        Width = 106
        Height = 25
        Caption = 'Refresh Token (4)'
        TabOrder = 8
        OnClick = doOAuthRefreshClick
      end
      object doTestRedir: TButton
        Left = 527
        Top = 196
        Width = 106
        Height = 25
        Caption = 'Test Redirect (1)'
        TabOrder = 5
        OnClick = doTestRedirClick
      end
      object doGrantCred: TButton
        Left = 650
        Top = 227
        Width = 106
        Height = 25
        Caption = 'Grant Credentials'
        TabOrder = 10
        OnClick = doGrantCredClick
      end
      object doGrantPassword: TButton
        Left = 650
        Top = 258
        Width = 106
        Height = 25
        Caption = 'Grant Password'
        TabOrder = 11
        OnClick = doGrantPasswordClick
      end
      object doOAuthConsole: TButton
        Left = 650
        Top = 196
        Width = 106
        Height = 25
        Caption = 'Access Console'
        TabOrder = 9
        OnClick = doOAuthConsoleClick
      end
    end
    object TabDNSHTTPS: TTabSheet
      Caption = 'DNS over HTTPS'
      ImageIndex = 3
      object Label23: TLabel
        Left = 10
        Top = 15
        Width = 104
        Height = 14
        Caption = 'DNS over HTTPS URL'
      end
      object Label222: TLabel
        Left = 10
        Top = 45
        Width = 115
        Height = 14
        Caption = 'Domain Name to Lookup'
      end
      object Label25: TLabel
        Left = 10
        Top = 75
        Width = 94
        Height = 14
        Caption = 'Lookup Query Type'
      end
      object DnsHttpsUrl: TComboBox
        Left = 150
        Top = 10
        Width = 431
        Height = 22
        ItemHeight = 14
        TabOrder = 0
        Text = 'https://cloudflare-dns.com/dns-query'
        Items.Strings = (
          'https://cloudflare-dns.com/dns-query'
          'https://dns.google.com/resolve'
          'https://dns.quad9.net/dns-query'
          'https://doh.powerdns.org'
          'https://doh.securedns.eu/dns-query'
          'https://doh.appliedprivacy.net/query')
      end
      object DnsDomainName: TComboBox
        Left = 150
        Top = 40
        Width = 201
        Height = 22
        ItemHeight = 14
        TabOrder = 1
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
          'ftp.magsys.co.uk'
          'mail.magsys.co.uk'
          'embarcadero.com'
          'www.embarcadero.com')
      end
      object DnsQueryType: TComboBox
        Left = 150
        Top = 68
        Width = 201
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 2
      end
      object doDNSJson: TButton
        Left = 10
        Top = 115
        Width = 104
        Height = 25
        Caption = 'DNS Using Json'
        TabOrder = 5
        OnClick = doDNSJsonClick
      end
      object DnsDnssec: TCheckBox
        Left = 370
        Top = 45
        Width = 106
        Height = 17
        Caption = 'DNSSEC Data'
        TabOrder = 3
      end
      object DnsNoValidation: TCheckBox
        Left = 370
        Top = 75
        Width = 116
        Height = 17
        Caption = 'Disable Validation'
        TabOrder = 4
      end
      object doDnsQuery1: TButton
        Left = 127
        Top = 115
        Width = 138
        Height = 25
        Caption = 'Single Query (Wire Format)'
        TabOrder = 6
        OnClick = doDnsQuery1Click
      end
      object doDnsQueryAll: TButton
        Left = 278
        Top = 115
        Width = 138
        Height = 25
        Caption = 'All Queries (Wire Format)'
        TabOrder = 7
        OnClick = doDnsQueryAllClick
      end
    end
    object TabTwitter: TTabSheet
      Caption = 'Twiiter OAuth1A'
      ImageIndex = 4
      object Label37: TLabel
        Left = 450
        Top = 5
        Width = 53
        Height = 14
        Caption = 'Tweet Text'
      end
      object Label42: TLabel
        Left = 450
        Top = 145
        Width = 66
        Height = 14
        Caption = 'Twets Query '
      end
      object Label43: TLabel
        Left = 450
        Top = 165
        Width = 64
        Height = 26
        AutoSize = False
        Caption = 'Tweet ID List (commas)'
        WordWrap = True
      end
      object BoxTritterSettings: TGroupBox
        Left = 10
        Top = 5
        Width = 429
        Height = 271
        Caption = 
          'Twitter OAuth1A Consumer API Keys and Tokens for Application (no' +
          't user) '
        TabOrder = 0
        object Label36: TLabel
          Left = 5
          Top = 50
          Width = 73
          Height = 14
          Caption = 'API Secret Key'
        end
        object Label38: TLabel
          Left = 5
          Top = 143
          Width = 98
          Height = 14
          Caption = 'Screen Name and ID'
        end
        object Label39: TLabel
          Left = 5
          Top = 95
          Width = 69
          Height = 14
          Caption = 'Access Token'
        end
        object Label40: TLabel
          Left = 5
          Top = 120
          Width = 73
          Height = 14
          Caption = 'Access Secret'
        end
        object Label35: TLabel
          Left = 5
          Top = 25
          Width = 38
          Height = 14
          Caption = 'API Key'
        end
        object Label41: TLabel
          Left = 5
          Top = 170
          Width = 411
          Height = 64
          AutoSize = False
          Caption = 
            'To read and send Tweets, you need a developer account and then s' +
            'et-up an App'#13#10'with a Callback URL of http://localhost:8080/twitt' +
            'er/ and copy API Key/Secret'#13#10'above. The Access Token and Secret ' +
            'may be copied from the developer account'#13#10'to avoid needing to lo' +
            'gin for the account owning the application.  Tokens don'#39't expire' +
            '.'
          WordWrap = True
        end
        object TwitResultLabel: TLabel
          Left = 10
          Top = 243
          Width = 36
          Height = 14
          Caption = 'Result: '
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object TwitApiSecret: TEdit
          Left = 90
          Top = 46
          Width = 326
          Height = 22
          TabOrder = 1
        end
        object TwitAccToken: TEdit
          Left = 90
          Top = 90
          Width = 326
          Height = 22
          TabOrder = 3
        end
        object TwitAccSecret: TEdit
          Left = 90
          Top = 115
          Width = 326
          Height = 22
          TabOrder = 4
        end
        object TwitApiKey: TEdit
          Left = 90
          Top = 20
          Width = 326
          Height = 22
          TabOrder = 0
        end
        object TwitForceLogin: TCheckBox
          Left = 10
          Top = 70
          Width = 191
          Height = 17
          Caption = 'Force Login (for different users) '
          TabOrder = 2
        end
        object TwitScrnName: TEdit
          Left = 117
          Top = 140
          Width = 145
          Height = 22
          TabOrder = 5
        end
        object TwitScrnId: TEdit
          Left = 277
          Top = 140
          Width = 129
          Height = 22
          TabOrder = 6
        end
      end
      object doTwitLogin: TButton
        Left = 450
        Top = 210
        Width = 92
        Height = 25
        Caption = 'Login'
        TabOrder = 4
        OnClick = doTwitLoginClick
      end
      object TwitMsg: TMemo
        Left = 454
        Top = 25
        Width = 417
        Height = 104
        Lines.Strings = (
          'Testing Tweeting from a Delphi Windows application.  ')
        MaxLength = 280
        TabOrder = 1
        WantReturns = False
      end
      object doTwitMsg: TButton
        Left = 555
        Top = 210
        Width = 95
        Height = 25
        Caption = 'Send Tweet'
        TabOrder = 5
        OnClick = doTwitMsgClick
      end
      object TwitQuery: TEdit
        Left = 535
        Top = 140
        Width = 336
        Height = 22
        TabOrder = 2
        Text = 'from:twitterdev'
      end
      object TwitIdList: TEdit
        Left = 535
        Top = 170
        Width = 265
        Height = 22
        TabOrder = 3
        Text = '1285241443603279872'
      end
      object doTwitAccSett: TButton
        Left = 450
        Top = 241
        Width = 95
        Height = 25
        Caption = 'Account Settings'
        TabOrder = 6
        OnClick = doTwitAccSettClick
      end
      object doTwitQuery: TButton
        Left = 662
        Top = 210
        Width = 101
        Height = 25
        Caption = 'Query Tweets'
        TabOrder = 7
        OnClick = doTwitQueryClick
      end
      object doTwitGetId: TButton
        Left = 662
        Top = 241
        Width = 101
        Height = 25
        Caption = 'Get Tweets by LD'
        TabOrder = 8
        OnClick = doTwitGetIdClick
      end
    end
    object TabSms: TTabSheet
      Caption = 'Send SMS'
      ImageIndex = 5
      object BoxSmsMsg: TGroupBox
        Left = 10
        Top = 5
        Width = 426
        Height = 161
        Caption = 'SMS Destination and Message'
        TabOrder = 0
        object Label26: TLabel
          Left = 5
          Top = 15
          Width = 38
          Height = 28
          Caption = 'Sender Number'
          WordWrap = True
        end
        object Label28: TLabel
          Left = 200
          Top = 20
          Width = 51
          Height = 28
          Caption = 'SMS Dest Number(s)'
          WordWrap = True
        end
        object Label29: TLabel
          Left = 5
          Top = 82
          Width = 44
          Height = 14
          Caption = 'Message'
        end
        object SmsAccSender: TEdit
          Left = 64
          Top = 20
          Width = 126
          Height = 22
          TabOrder = 0
          Text = 'Delphi-ICS'
        end
        object SmsMsgText: TMemo
          Left = 64
          Top = 80
          Width = 353
          Height = 71
          Lines.Strings = (
            'My test SMS message text. ')
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object SmsDestNums: TMemo
          Left = 270
          Top = 15
          Width = 147
          Height = 56
          Lines.Strings = (
            '')
          ScrollBars = ssVertical
          TabOrder = 2
        end
      end
      object GroupBox3: TGroupBox
        Left = 10
        Top = 170
        Width = 388
        Height = 113
        Caption = 'Kapow - https://www.kapow.co.uk/'
        TabOrder = 1
        object Label24: TLabel
          Left = 168
          Top = 15
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label27: TLabel
          Left = 10
          Top = 15
          Width = 74
          Height = 14
          Caption = 'Account: Name'
        end
        object LabelKapowCredit: TLabel
          Left = 290
          Top = 68
          Width = 40
          Height = 14
          Caption = 'Credits: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object LabelKapowResult: TLabel
          Left = 10
          Top = 90
          Width = 33
          Height = 14
          Caption = 'Result:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object KapowAccPw: TEdit
          Left = 165
          Top = 33
          Width = 126
          Height = 22
          PasswordChar = '*'
          TabOrder = 1
        end
        object KapowAccName: TEdit
          Left = 10
          Top = 30
          Width = 126
          Height = 22
          TabOrder = 0
        end
        object doKapowSend: TButton
          Left = 10
          Top = 63
          Width = 75
          Height = 25
          Caption = 'Send SMS'
          TabOrder = 2
          OnClick = doKapowSendClick
        end
        object doKapowCheck: TButton
          Left = 97
          Top = 63
          Width = 85
          Height = 25
          Caption = 'Check Delivery'
          Enabled = False
          TabOrder = 3
          OnClick = doKapowCheckClick
        end
        object doKapowCredit: TButton
          Left = 200
          Top = 63
          Width = 75
          Height = 25
          Caption = 'Check Credit'
          TabOrder = 4
          OnClick = doKapowCreditClick
        end
      end
      object BoxSmsWorks: TGroupBox
        Left = 450
        Top = 5
        Width = 365
        Height = 187
        Caption = 'SMS Works - https://thesmsworks.co.uk/'
        TabOrder = 2
        object Label30: TLabel
          Left = 10
          Top = 17
          Width = 81
          Height = 14
          Caption = 'Login Json Lines'
        end
        object LabelSmsWorksCredits: TLabel
          Left = 289
          Top = 132
          Width = 40
          Height = 14
          Caption = 'Credits: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object LabelSmsWorksResult: TLabel
          Left = 10
          Top = 162
          Width = 33
          Height = 14
          Caption = 'Result:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object SmsWorksLoginJson: TMemo
          Left = 14
          Top = 37
          Width = 329
          Height = 84
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object doSmsWorksSend: TButton
          Left = 12
          Top = 127
          Width = 75
          Height = 25
          Caption = 'Send SMS'
          TabOrder = 1
          OnClick = doSmsWorksSendClick
        end
        object doSmsWorksCheck: TButton
          Left = 99
          Top = 127
          Width = 85
          Height = 25
          Caption = 'Check Delivery'
          Enabled = False
          TabOrder = 2
          OnClick = doSmsWorksCheckClick
        end
        object doSmsWorksCredit: TButton
          Left = 202
          Top = 127
          Width = 75
          Height = 25
          Caption = 'Check Credit'
          TabOrder = 3
          OnClick = doSmsWorksCreditClick
        end
      end
    end
    object TabEmail: TTabSheet
      Caption = 'Send Email'
      ImageIndex = 6
      object Label56: TLabel
        Left = 10
        Top = 110
        Width = 24
        Height = 14
        Caption = 'From'
      end
      object Label57: TLabel
        Left = 10
        Top = 135
        Width = 14
        Height = 14
        Caption = 'CC'
      end
      object Label58: TLabel
        Left = 10
        Top = 160
        Width = 36
        Height = 14
        Caption = 'Subject'
      end
      object Label59: TLabel
        Left = 10
        Top = 10
        Width = 56
        Height = 14
        Caption = 'Recipienets'
      end
      object Label60: TLabel
        Left = 10
        Top = 194
        Width = 44
        Height = 14
        Caption = 'Message'
      end
      object Label61: TLabel
        Left = 645
        Top = 10
        Width = 95
        Height = 14
        Caption = 'Message Id to Read'
      end
      object LabelEmailResult: TLabel
        Left = 430
        Top = 145
        Width = 201
        Height = 36
        AutoSize = False
        Caption = 'Result:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label62: TLabel
        Left = 700
        Top = 188
        Width = 119
        Height = 14
        Caption = 'Message Label or Folder'
      end
      object EmailRecipList: TMemo
        Left = 80
        Top = 10
        Width = 326
        Height = 81
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'To???'
          '')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object EmailFrom: TEdit
        Left = 80
        Top = 105
        Width = 326
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'From??'
      end
      object EmailCC: TEdit
        Left = 80
        Top = 130
        Width = 326
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
      object EmailSubject: TEdit
        Left = 80
        Top = 155
        Width = 326
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = 'Subject???'
      end
      object doEmailSend: TButton
        Left = 426
        Top = 14
        Width = 96
        Height = 25
        Caption = 'Send Mail'
        TabOrder = 5
        OnClick = doEmailSendClick
      end
      object EmailMessage: TMemo
        Left = 80
        Top = 190
        Width = 591
        Height = 126
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'Test email from TIcsRestEmail component. ')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
      end
      object doEmailList: TButton
        Left = 425
        Top = 49
        Width = 96
        Height = 25
        Caption = 'List Gmail Ids'
        TabOrder = 6
        OnClick = doEmailListClick
      end
      object doEmailRead: TButton
        Left = 535
        Top = 49
        Width = 96
        Height = 25
        Caption = 'Read Mail'
        TabOrder = 9
        OnClick = doEmailReadClick
      end
      object EmailReadMessId: TListBox
        Left = 645
        Top = 30
        Width = 218
        Height = 146
        ItemHeight = 14
        TabOrder = 11
      end
      object doEmailHdrs: TButton
        Left = 535
        Top = 14
        Width = 96
        Height = 25
        Caption = 'Read Headers'
        TabOrder = 8
        OnClick = doEmailHdrsClick
      end
      object EmailRawHdrs: TCheckBox
        Left = 425
        Top = 117
        Width = 169
        Height = 27
        Caption = 'Show Raw Headers and Content (Gmail)'
        TabOrder = 7
        WordWrap = True
      end
      object doEmailDelete: TButton
        Left = 535
        Top = 83
        Width = 96
        Height = 25
        Caption = 'Delete Email'
        TabOrder = 10
        OnClick = doEmailDeleteClick
      end
      object EmailHdrFolder: TListBox
        Left = 700
        Top = 208
        Width = 121
        Height = 106
        ItemHeight = 14
        Items.Strings = (
          'Inbox'
          'Sent'
          'SentItems'
          'Unread'
          'Important'
          'Drafts'
          'DeletedItems'
          'Outbox'
          'Trash '
          'JunkEmail'
          'Category_Personal'
          'Category_Updates')
        TabOrder = 12
      end
    end
    object TabEmailSet: TTabSheet
      Caption = 'Email Settings'
      ImageIndex = 7
      object GroupBox4: TGroupBox
        Left = 10
        Top = 5
        Width = 636
        Height = 316
        Caption = 
          'Email OAuth2 Application Client API Keys and Tokens  (not accoun' +
          't  login)'
        TabOrder = 0
        object Label44: TLabel
          Left = 5
          Top = 50
          Width = 61
          Height = 14
          Caption = 'App Client ID'
        end
        object Label45: TLabel
          Left = 5
          Top = 176
          Width = 71
          Height = 14
          Caption = 'Token Account'
        end
        object Label46: TLabel
          Left = 5
          Top = 150
          Width = 69
          Height = 14
          Caption = 'Access Token'
        end
        object Label47: TLabel
          Left = 5
          Top = 125
          Width = 70
          Height = 14
          Caption = 'Refresh Token'
        end
        object Label48: TLabel
          Left = 5
          Top = 25
          Width = 65
          Height = 14
          Caption = 'Enail Provider'
        end
        object Label49: TLabel
          Left = 10
          Top = 208
          Width = 606
          Height = 48
          AutoSize = False
          Caption = 
            'To access email using REST APIs or OAuth2/SMTP/POP3 an '#39'applicat' +
            'ion account'#39' needs to be created though the provider console. Th' +
            'is is generally done once by the developer and the application A' +
            'PI ID and secret are then distributed with the application (usua' +
            'lly hidden).'
          WordWrap = True
        end
        object EmailOAResultLabel: TLabel
          Left = 5
          Top = 293
          Width = 36
          Height = 14
          Caption = 'Result: '
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object Label51: TLabel
          Left = 5
          Top = 75
          Width = 61
          Height = 14
          Caption = 'Client Secret'
        end
        object Label52: TLabel
          Left = 5
          Top = 100
          Width = 67
          Height = 14
          Caption = 'Email Account'
        end
        object Label53: TLabel
          Left = 207
          Top = 98
          Width = 127
          Height = 14
          Caption = '(me for any)  Account Hint'
        end
        object Label50: TLabel
          Left = 437
          Top = 175
          Width = 30
          Height = 14
          Caption = 'Expiry'
        end
        object EmailClientId: TEdit
          Left = 90
          Top = 45
          Width = 526
          Height = 22
          TabOrder = 2
        end
        object EmailAccToken: TEdit
          Left = 90
          Top = 145
          Width = 526
          Height = 22
          Enabled = False
          TabOrder = 7
        end
        object EmailRefrToken: TEdit
          Left = 90
          Top = 120
          Width = 526
          Height = 22
          TabOrder = 6
        end
        object EmailAccount: TEdit
          Left = 90
          Top = 95
          Width = 111
          Height = 22
          TabOrder = 4
        end
        object EmailForceLogin: TCheckBox
          Left = 290
          Top = 20
          Width = 191
          Height = 17
          Caption = 'Force Login (for different users) '
          TabOrder = 1
        end
        object EmailTokenAccnt: TEdit
          Left = 90
          Top = 170
          Width = 331
          Height = 22
          TabOrder = 8
        end
        object EmailAccExpiry: TEdit
          Left = 487
          Top = 170
          Width = 129
          Height = 22
          Enabled = False
          TabOrder = 9
        end
        object EmailRestType: TComboBox
          Left = 90
          Top = 20
          Width = 171
          Height = 22
          Style = csDropDownList
          ItemHeight = 14
          TabOrder = 0
          Items.Strings = (
            'Google Gmail'
            'Microsoft REST Mail APIs'
            'Microsoft SMTP/POP3')
        end
        object EmailClientSecret: TEdit
          Left = 89
          Top = 70
          Width = 527
          Height = 22
          TabOrder = 3
        end
        object doEmaiTstRedir: TButton
          Left = 125
          Top = 262
          Width = 106
          Height = 25
          Caption = 'Test Redirect'
          TabOrder = 10
          OnClick = doEmaiTstRedirClick
        end
        object doEmailOALogin: TButton
          Left = 245
          Top = 262
          Width = 106
          Height = 25
          Caption = 'Login to Email'
          TabOrder = 11
          OnClick = doEmailOALoginClick
        end
        object doEmailOARefresh: TButton
          Left = 363
          Top = 262
          Width = 106
          Height = 25
          Caption = 'Refresh Token'
          TabOrder = 12
          OnClick = doEmailOARefreshClick
        end
        object doEmailOACons: TButton
          Left = 5
          Top = 262
          Width = 106
          Height = 25
          Caption = 'Access Console'
          TabOrder = 13
          OnClick = doEmailOAConsClick
        end
        object EmailAccountHint: TEdit
          Left = 347
          Top = 95
          Width = 269
          Height = 22
          TabOrder = 5
        end
      end
    end
    object TabInetAlive: TTabSheet
      Caption = 'Internet Alive'
      ImageIndex = 8
      object GroupBoxAlive: TGroupBox
        Left = 10
        Top = 0
        Width = 326
        Height = 154
        Caption = 'Check if Internet is Alive'
        TabOrder = 0
        object LabelInetAlive: TLabel
          Left = 10
          Top = 100
          Width = 101
          Height = 14
          Caption = 'Internet Alive Result: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object AliveMethod: TRadioGroup
          Left = 10
          Top = 20
          Width = 86
          Height = 66
          Caption = 'Alive Method'
          ItemIndex = 1
          Items.Strings = (
            'Ping'
            'Http'
            'Both')
          TabOrder = 0
        end
        object AliveNets: TRadioGroup
          Left = 115
          Top = 20
          Width = 96
          Height = 66
          Caption = 'Alive Network'
          ItemIndex = 0
          Items.Strings = (
            'IPv4'
            'IPv6'
            'Both')
          TabOrder = 1
        end
        object doAliveStart: TButton
          Left = 230
          Top = 30
          Width = 75
          Height = 25
          Caption = 'Start'
          TabOrder = 2
          OnClick = doAliveStartClick
        end
        object doAliveStop: TButton
          Left = 230
          Top = 61
          Width = 75
          Height = 25
          Caption = 'Stop'
          Enabled = False
          TabOrder = 3
          OnClick = doAliveStopClick
        end
      end
    end
  end
  object LogWin: TMemo
    Left = 0
    Top = 539
    Width = 900
    Height = 202
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object RespList: TListView
    Left = 0
    Top = 376
    Width = 900
    Height = 163
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 70
      end
      item
        Caption = 'Value'
        Width = 1000
      end
      item
        Width = 100
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = RespListDblClick
  end
  object HttpRest1: TSslHttpRest
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ResponseNoException = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    RequestVer = '1.1'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = [httpoEnableContentCoding]
    Timeout = 30
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfAny
    SocketErrs = wsErrTech
    RestParams.PContent = PContUrlencoded
    RestParams.RfcStrict = False
    RestParams = <>
    DebugLevel = DebugSsl
    MaxBodySize = 100000000
    SslCliSecurity = sslCliSecTls1
    SslSessCache = True
    CertVerMethod = CertVerNone
    SslRootFile = 'RootCaCertsBundle.pem'
    SslRevocation = False
    SslReportChain = False
    SslAllowSelfSign = False
    HttpMemStrategy = HttpStratMem
    HttpDownReplace = False
    ResumeMinSize = 65535
    ProgIntSecs = 2
    ShowProgress = False
    OnHttpRestProg = HttpRest1HttpRestProg
    OnRestRequestDone = HttpRest1RestRequestDone
    Left = 60
    Top = 400
  end
  object RestOAuth1: TRestOAuth
    DebugLevel = DebugConn
    AuthType = OAuthTypeWeb
    OAOptions = []
    ProtoType = OAuthv2
    RefreshAuto = False
    RefrMinsPrior = 0
    WebSrvIP = '127.0.0.1'
    WebSrvPort = '8080'
    RefreshOffline = False
    LoginPrompt = 'consent'
    ResponseMode = 'query'
    OnOAuthAuthUrl = RestOAuth1OAuthAuthUrl
    OnOAuthProg = RestOAuth1OAuthProg
    OnOAuthNewCode = RestOAuth1OAuthNewCode
    OnOAuthNewToken = RestOAuth1OAuthNewToken
    Left = 115
    Top = 400
  end
  object OpenDirDiag: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Select Directory'
    Left = 215
    Top = 750
  end
  object DnsQueryHttps1: TDnsQueryHttps
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    OnRequestDone = DnsQueryHttps1RequestDone
    DnsSrvUrl = 'https://cloudflare-dns.com/dns-query'
    DebugLevel = DebugNone
    OnDnsProg = DnsQueryHttps1DnsProg
    Left = 160
    Top = 400
  end
  object TimerLog: TTimer
    OnTimer = TimerLogTimer
    Left = 205
    Top = 400
  end
  object IcsSMS1: TIcsSMS
    SmsProvider = SmsProvKapow
    SendDT = 43627.796545659720000000
    DebugLevel = DebugNone
    OnSmsProg = IcsSMS1SmsProg
    OnSmsDone = IcsSMS1SmsDone
    Left = 250
    Top = 400
  end
  object SslHtmlSmtpCli1: TSslHtmlSmtpCli
    Tag = 0
    ShareMode = smtpShareDenyWrite
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    Port = 'smtp'
    AuthType = smtpAuthNone
    ConfirmReceipt = False
    HdrPriority = smtpPriorityNone
    CharSet = 'windows-1252'
    ConvertToCharset = False
    WrapMsgMaxLineLen = 76
    SendMode = smtpToStream
    DefaultEncoding = smtpEnc7bit
    Allow8bitChars = True
    FoldHeaders = False
    WrapMessageText = False
    ContentType = smtpPlainText
    OwnHeaders = False
    OnBeforeOutStreamFree = SslHtmlSmtpCli1BeforeOutStreamFree
    XMailer = 'ICS SMTP Component V%VER%'
    ProxyType = smtpNoProxy
    ProxyHttpAuthType = htatDetect
    SocketFamily = sfIPv4
    SocketErrs = wsErrTech
    Timeout = 15
    SslType = smtpTlsNone
    HtmlCharSet = 'windows-1252'
    HtmlConvertToCharset = False
    Left = 295
    Top = 400
  end
  object IcsTwitter1: TIcsTwitter
    ForceLogin = False
    DebugLevel = DebugNone
    OnTwitProg = HttpRest1HttpRestProg
    OnTwitNewToken = IcsTwitter1TwitNewToken
    Left = 390
    Top = 400
  end
  object IcsRestEmail1: TIcsRestEmail
    RestEmailType = RestEmailGoogle
    ForceLogin = False
    LoginTimeout = 120
    HdrFieldList = 'to,from,subject,date'
    DebugLevel = DebugNone
    OAAuthType = OAuthTypeWeb
    OnEmailProg = HttpRest1HttpRestProg
    OnEmailNewToken = IcsRestEmail1EmailNewToken
    Left = 345
    Top = 400
  end
  object IcsInetAlive1: TIcsInetAlive
    AliveMethod = AliveMethHttp
    AliveNets = AliveNetv4
    HostIPv4 = 'www.msftconnecttest.com'
    HostIPv6 = 'ipv6.msftconnecttest.com'
    HttpPage = '/connecttest.txt'
    HttpText = 'Microsoft Connect Test'
    LocalIPv4 = '0.0.0.0'
    LocalIPv6 = '::'
    AutoStart = False
    OnlineSecs = 60
    OfflineSecs = 5
    PingHops = 15
    PingMaxSecs = 4
    PingCheckAddr = True
    FailedWaitSecs = 30
    DebugLevel = DebugNone
    OnAliveChange = IcsInetAlive1AliveChange
    OnAliveProg = HttpRest1HttpRestProg
    Left = 435
    Top = 400
  end
end
