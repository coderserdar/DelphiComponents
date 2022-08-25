object JsonDemoForm: TJsonDemoForm
  Left = 96
  Top = 180
  Caption = 
    'ICS SSL Json Object Signing Demos - http://www.overbyte.be - V8.' +
    '68 -7th Oct 2021'
  ClientHeight = 636
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object LogWin: TMemo
    Left = 0
    Top = 436
    Width = 823
    Height = 200
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 823
    Height = 436
    ActivePage = TabSheetJose
    Align = alTop
    TabOrder = 1
    object TabSheetJson: TTabSheet
      Caption = 'JSON/XML'
      ImageIndex = 1
      object JsonGrid: TListView
        Left = 0
        Top = 171
        Width = 815
        Height = 236
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
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = JsonGridDblClick
      end
      object JsonInput: TMemo
        Left = 0
        Top = 0
        Width = 815
        Height = 116
        Align = alTop
        Lines.Strings = (
          '{"LimitInfo": {"Used": 30.0,"Limit": 10000.0 },"success": true}')
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object PanelButtons: TPanel
        Left = 0
        Top = 116
        Width = 815
        Height = 55
        Align = alTop
        TabOrder = 2
        object Label33: TLabel
          Left = 511
          Top = 21
          Width = 285
          Height = 14
          Caption = 'Double click on a Json stObject or stArray item to expand.  '
        end
        object doParseJson: TButton
          Left = 30
          Top = 15
          Width = 75
          Height = 25
          Caption = 'Parse JSON'
          TabOrder = 0
          OnClick = doParseClick
        end
        object doLoadFile: TButton
          Left = 428
          Top = 15
          Width = 75
          Height = 25
          Caption = 'Load File'
          TabOrder = 1
          OnClick = doLoadFileClick
        end
        object doParseXML: TButton
          Left = 120
          Top = 15
          Width = 75
          Height = 25
          Caption = 'Parse XML'
          TabOrder = 2
          OnClick = doParseClick
        end
        object CompactXML: TCheckBox
          Left = 215
          Top = 20
          Width = 97
          Height = 17
          Caption = 'Compact XML'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object LogJson: TCheckBox
          Left = 318
          Top = 20
          Width = 97
          Height = 17
          Caption = 'Log JSON'
          TabOrder = 4
        end
      end
    end
    object TabSheetJose: TTabSheet
      Caption = 'JOSE'
      object Label25: TLabel
        Left = 5
        Top = 140
        Width = 124
        Height = 28
        Caption = 'Shared secret HMAC Key (32, 48 or 64 long)'
        WordWrap = True
      end
      object Label26: TLabel
        Left = 5
        Top = 175
        Width = 110
        Height = 28
        Caption = 'Private RSA or ECDSA Key File'
        WordWrap = True
      end
      object Label27: TLabel
        Left = 424
        Top = 255
        Width = 131
        Height = 28
        Caption = 'Key and Signing Hash Algorithm (must match key)'
        WordWrap = True
      end
      object Label1: TLabel
        Left = 10
        Top = 5
        Width = 77
        Height = 14
        Caption = 'Clear Text Lines'
      end
      object Label2: TLabel
        Left = 452
        Top = 5
        Width = 132
        Height = 14
        Caption = 'Json Web Key or Signature'
      end
      object Label3: TLabel
        Left = 5
        Top = 210
        Width = 59
        Height = 28
        Caption = 'New Private Key'
        WordWrap = True
      end
      object LabelPKey: TLabel
        Left = 5
        Top = 245
        Width = 260
        Height = 28
        AutoSize = False
        Caption = 'Private Key: '
        Color = clAppWorkSpace
        ParentColor = False
        WordWrap = True
      end
      object JoseTextLines: TMemo
        Left = 5
        Top = 21
        Width = 417
        Height = 104
        Lines.Strings = (
          '{"LimitInfo": {"Used": 30.0,"Limit": 10000.0 },"success": true}')
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = JoseTextLinesDblClick
      end
      object TestHmacKey: TEdit
        Left = 154
        Top = 140
        Width = 358
        Height = 22
        MaxLength = 64
        TabOrder = 2
        Text = 'mysecretkeyneedstobe32byteslong.'
      end
      object TestJWSAlg: TComboBox
        Left = 561
        Top = 261
        Width = 129
        Height = 22
        ItemHeight = 14
        ItemIndex = 1
        TabOrder = 11
        Text = 'jsigHmac256'
        Items.Strings = (
          'jsigNone'
          'jsigHmac256'
          'jsigHmac384'
          'jsigHmac512'
          'jsigRsa256'
          'jsigRsa384'
          'jsigRsa512'
          'jsigEcdsa256'
          'jsigEcdsa384'
          'jsigEcdsa512'
          'jsigRsaPss256'
          'jsigRsaPss384'
          'jsigRsaPss512'
          'jsigEdDSA')
      end
      object doTestSign: TButton
        Left = 154
        Top = 325
        Width = 113
        Height = 25
        Caption = 'Sign/Verify Data'
        TabOrder = 18
        OnClick = doTestSignClick
      end
      object doJWSCreate: TButton
        Left = 291
        Top = 289
        Width = 150
        Height = 25
        Caption = 'Create Json Web Signature'
        TabOrder = 14
        OnClick = doJWSCreateClick
      end
      object doSignHmac: TButton
        Left = 10
        Top = 325
        Width = 113
        Height = 25
        Caption = 'Test HMAC Digests'
        TabOrder = 17
        OnClick = doSignHmacClick
      end
      object TestPrivKeyFile: TComboBox
        Left = 154
        Top = 175
        Width = 358
        Height = 22
        ItemHeight = 14
        TabOrder = 3
        Text = 'jose-rsa-prvkey.pem'
        OnChange = TestPrivKeyFileChange
        Items.Strings = (
          'jose-rsa-prvkey.pem'
          'jose-ec-prvkey.pem'
          'jose-rsapss-prvkey.pem'
          'jose-ed25519-prvkey.pem')
      end
      object doJWKCreate: TButton
        Left = 10
        Top = 289
        Width = 121
        Height = 25
        Caption = 'Create Json Web Key'
        Enabled = False
        TabOrder = 12
        OnClick = doJWKCreateClick
      end
      object RawJWK: TMemo
        Left = 454
        Top = 24
        Width = 337
        Height = 100
        Lines.Strings = (
          'RawJWK')
        ScrollBars = ssHorizontal
        TabOrder = 1
      end
      object doJKWRead: TButton
        Left = 156
        Top = 289
        Width = 111
        Height = 25
        Caption = 'Read Json Web Key'
        TabOrder = 13
        OnClick = doJKWReadClick
      end
      object ShowRawKey: TCheckBox
        Left = 286
        Top = 240
        Width = 108
        Height = 17
        Caption = 'Show Raw Key'
        TabOrder = 9
      end
      object NewprivKey: TComboBox
        Left = 154
        Top = 212
        Width = 240
        Height = 22
        ItemHeight = 14
        TabOrder = 4
        Text = 'NewPrivKey'
      end
      object doNewPrivKey: TButton
        Left = 411
        Top = 210
        Width = 101
        Height = 25
        Caption = 'New Private Key'
        TabOrder = 7
        OnClick = doNewPrivKeyClick
      end
      object doClear: TButton
        Left = 640
        Top = 289
        Width = 101
        Height = 25
        Caption = 'Clear Screen'
        TabOrder = 16
        OnClick = doClearClick
      end
      object SelFile: TBitBtn
        Left = 524
        Top = 175
        Width = 31
        Height = 25
        TabOrder = 5
        OnClick = SelFileClick
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
      object doJWSRead: TButton
        Left = 454
        Top = 289
        Width = 150
        Height = 25
        Caption = 'Read Json Web Signature'
        TabOrder = 15
        OnClick = doJWSReadClick
      end
      object JwkPrivate: TCheckBox
        Left = 286
        Top = 265
        Width = 97
        Height = 17
        Caption = 'JWK Private Key'
        TabOrder = 10
      end
      object doLoadKeyFile: TButton
        Left = 561
        Top = 177
        Width = 101
        Height = 25
        Caption = 'Load Key File'
        TabOrder = 6
        OnClick = doLoadKeyFileClick
      end
      object doKeyParams: TButton
        Left = 535
        Top = 210
        Width = 101
        Height = 25
        Caption = 'PKey Parameters'
        TabOrder = 8
        OnClick = doKeyParamsClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Transcode'
      ImageIndex = 2
      object Label6: TLabel
        Left = 10
        Top = 5
        Width = 77
        Height = 14
        Caption = 'Clear Text Lines'
      end
      object Label7: TLabel
        Left = 8
        Top = 115
        Width = 105
        Height = 14
        Caption = 'Base64 Encoded Text'
      end
      object Label8: TLabel
        Left = 10
        Top = 155
        Width = 87
        Height = 14
        Caption = 'Hex Encoded Text'
      end
      object Label9: TLabel
        Left = 8
        Top = 195
        Width = 88
        Height = 14
        Caption = 'URL Encoded Text'
      end
      object Label4: TLabel
        Left = 30
        Top = 271
        Width = 418
        Height = 65
        Caption = 
          'This demo illustrates and tests many of the low level encoding a' +
          'nd decoding functions'#13#10'used in REST and JOSE HTTPS applications.' +
          ' Hash digests are a short string calculated '#13#10'from an inpiut, us' +
          'ed to sign an input either with a shared secret HMAC key, or wit' +
          'h a '#13#10'secret private key, where only a public key is needed to v' +
          'erify the input is not altered. '#13#10'  '
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object CodeTextLines: TMemo
        Left = 8
        Top = 20
        Width = 583
        Height = 89
        Lines.Strings = (
          '{"LimitInfo": {"Used": 30.0,"Limit": 10000.0 },"success": true}')
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = CodeTextLinesDblClick
      end
      object doBase64Enc: TButton
        Left = 609
        Top = 25
        Width = 84
        Height = 25
        Caption = 'Base64 Encode'
        TabOrder = 1
        OnClick = doBase64EncClick
      end
      object doB64URLEn: TButton
        Left = 610
        Top = 56
        Width = 84
        Height = 25
        Caption = 'B64Url Encode'
        TabOrder = 2
        OnClick = doB64URLEnClick
      end
      object doTestUrlEnc: TButton
        Left = 610
        Top = 87
        Width = 84
        Height = 25
        Caption = 'Test Base64Url'
        TabOrder = 3
        OnClick = doTestUrlEncClick
      end
      object doHexEncode: TButton
        Left = 609
        Top = 118
        Width = 84
        Height = 25
        Caption = 'Hex Encode'
        TabOrder = 4
        OnClick = doHexEncodeClick
      end
      object doEncodeURL: TButton
        Left = 610
        Top = 149
        Width = 84
        Height = 25
        Caption = 'Encode URL'
        TabOrder = 5
        OnClick = doEncodeURLClick
      end
      object doBase64Dec: TButton
        Left = 705
        Top = 25
        Width = 84
        Height = 25
        Caption = 'Base64 Decode'
        TabOrder = 6
        OnClick = doBase64DecClick
      end
      object doB64URLDec: TButton
        Left = 705
        Top = 56
        Width = 84
        Height = 25
        Caption = 'B64Url Decode'
        TabOrder = 7
        OnClick = doB64URLDecClick
      end
      object doHashDigest: TButton
        Left = 705
        Top = 87
        Width = 84
        Height = 25
        Caption = 'Hash Digests'
        TabOrder = 8
        OnClick = doHashDigestClick
      end
      object doHexDec: TButton
        Left = 705
        Top = 118
        Width = 84
        Height = 25
        Caption = 'Hex Decode'
        TabOrder = 9
        OnClick = doHexDecClick
      end
      object doDecodeURL: TButton
        Left = 705
        Top = 149
        Width = 84
        Height = 25
        Caption = 'Decode URL'
        TabOrder = 10
        OnClick = doDecodeURLClick
      end
      object HexText: TEdit
        Left = 8
        Top = 170
        Width = 583
        Height = 22
        TabOrder = 11
        Text = 
          '7b224c696d6974496e666f223a207b2255736564223a2033302e302c224c696d' +
          '6974223a2031303030302e30207d2c2273756363657373223a20747275657d'
      end
      object URLText: TEdit
        Left = 8
        Top = 210
        Width = 583
        Height = 22
        TabOrder = 12
        Text = 
          '%7B%22LimitInfo%22%3A%20%7B%22Used%22%3A%2030%2E0%2C%22Limit%22%' +
          '3A%2010000%2E0%20%7D%2C%22success%22%3A%20true%7D'
      end
      object Base64Text: TEdit
        Left = 8
        Top = 130
        Width = 583
        Height = 22
        TabOrder = 13
        Text = 
          'eyJMaW1pdEluZm8iOiB7IlVzZWQiOiAzMC4wLCJMaW1pdCI6IDEwMDAwLjAgfSwi' +
          'c3VjY2VzcyI6IHRydWV9'
      end
    end
  end
  object OpenDlg: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 300
    Top = 65
  end
end
