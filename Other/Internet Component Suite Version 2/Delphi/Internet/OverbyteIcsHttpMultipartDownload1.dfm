object MultipartHttpDownloadForm: TMultipartHttpDownloadForm
  Left = 54
  Top = 492
  Width = 600
  Height = 450
  Caption = 'Multipart HTTP Downloader - ICS - (c) 2007 Fran'#231'ois PIETTE'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 201
    Width = 584
    Height = 172
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 201
    Align = alTop
    TabOrder = 1
    object UrlLabel: TLabel
      Left = 52
      Top = 12
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object LocalFileLabel: TLabel
      Left = 8
      Top = 35
      Width = 67
      Height = 13
      Caption = 'Local filename'
    end
    object PartCountLabel: TLabel
      Left = 24
      Top = 63
      Width = 50
      Height = 13
      Caption = 'Part count'
    end
    object LocalFileEdit: TEdit
      Left = 80
      Top = 32
      Width = 329
      Height = 21
      TabOrder = 1
      Text = 'LocalFileEdit'
    end
    object UrlEdit: TEdit
      Left = 80
      Top = 8
      Width = 331
      Height = 21
      TabOrder = 0
      Text = 'UrlEdit'
    end
    object DownloadButton: TButton
      Left = 416
      Top = 8
      Width = 75
      Height = 21
      Caption = '&Download'
      TabOrder = 3
      OnClick = DownloadButtonClick
    end
    object AbortButton: TButton
      Left = 416
      Top = 32
      Width = 75
      Height = 21
      Caption = '&Abort'
      TabOrder = 4
      OnClick = AbortButtonClick
    end
    object PartCountEdit: TEdit
      Left = 80
      Top = 59
      Width = 28
      Height = 21
      TabOrder = 2
      Text = '10'
    end
    object PauseButton: TButton
      Left = 416
      Top = 96
      Width = 75
      Height = 21
      Caption = '&Pause'
      TabOrder = 6
      OnClick = PauseButtonClick
    end
    object ResumeButton: TButton
      Left = 416
      Top = 72
      Width = 75
      Height = 21
      Caption = '&Resume'
      TabOrder = 5
      OnClick = ResumeButtonClick
    end
    object AuthGroupBox: TGroupBox
      Left = 8
      Top = 88
      Width = 177
      Height = 105
      Caption = 'Authentication'
      TabOrder = 7
      object Label3: TLabel
        Left = 11
        Top = 46
        Width = 47
        Height = 13
        Caption = 'UserCode'
      end
      object Label4: TLabel
        Left = 12
        Top = 73
        Width = 46
        Height = 13
        Caption = 'Password'
      end
      object UserCodeEdit: TEdit
        Left = 62
        Top = 43
        Width = 75
        Height = 21
        TabOrder = 3
        Text = 'UserCodeEdit'
      end
      object PasswordEdit: TEdit
        Left = 62
        Top = 70
        Width = 75
        Height = 21
        TabOrder = 4
        Text = 'PasswordEdit'
      end
      object AuthNoneRadioButton: TRadioButton
        Left = 16
        Top = 16
        Width = 49
        Height = 17
        Caption = 'None'
        TabOrder = 0
        OnClick = AuthNoneRadioButtonClick
      end
      object AuthBasicRadioButton: TRadioButton
        Left = 72
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Basic'
        TabOrder = 1
        OnClick = AuthBasicRadioButtonClick
      end
      object AuthNtlmRadioButton: TRadioButton
        Left = 120
        Top = 16
        Width = 51
        Height = 17
        Caption = 'NTLM'
        TabOrder = 2
        OnClick = AuthNtlmRadioButtonClick
      end
    end
    object ProxyGroupBox: TGroupBox
      Left = 200
      Top = 88
      Width = 201
      Height = 105
      Caption = 'HTTP Proxy'
      TabOrder = 8
      object Label5: TLabel
        Left = 21
        Top = 45
        Width = 53
        Height = 13
        Caption = 'Proxy Host'
      end
      object Label6: TLabel
        Left = 23
        Top = 72
        Width = 51
        Height = 13
        Caption = 'Proxy Port'
      end
      object ProxyHostEdit: TEdit
        Left = 80
        Top = 43
        Width = 75
        Height = 21
        TabOrder = 3
        Text = 'ProxyHostEdit'
      end
      object ProxyPortEdit: TEdit
        Left = 80
        Top = 70
        Width = 75
        Height = 21
        TabOrder = 4
        Text = 'ProxyPortEdit'
      end
      object ProxyNoneRadioButton: TRadioButton
        Left = 8
        Top = 16
        Width = 49
        Height = 17
        Caption = 'None'
        TabOrder = 0
        OnClick = ProxyNoneRadioButtonClick
      end
      object ProxyHttpRadioButton: TRadioButton
        Left = 72
        Top = 16
        Width = 57
        Height = 17
        Caption = 'HTTP'
        TabOrder = 1
        OnClick = ProxyHttpRadioButtonClick
      end
      object ProxySocksRadioButton: TRadioButton
        Left = 136
        Top = 16
        Width = 57
        Height = 17
        Caption = 'SOCKS'
        TabOrder = 2
        OnClick = ProxySocksRadioButtonClick
      end
    end
    object ClearButton: TButton
      Left = 416
      Top = 136
      Width = 75
      Height = 21
      Caption = '&Clear display'
      TabOrder = 9
      OnClick = ClearButtonClick
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 373
    Width = 584
    Height = 41
    Align = alBottom
    TabOrder = 2
    object CountLabel: TLabel
      Left = 8
      Top = 4
      Width = 54
      Height = 13
      Caption = 'CountLabel'
    end
    object MPBar: TMultiProgressBar
      Left = 1
      Top = 23
      Width = 582
      Height = 17
      Align = alBottom
    end
  end
  object MPHttp: TMultipartHttpDownloader
    PartCount = 0
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    OnDisplay = MPHttpDisplay
    OnRequestDone = MPHttpRequestDone
    OnProgressAddSegment = MPHttpProgressAddSegment
    OnProgressSetPosition = MPHttpProgressSetPosition
    OnShowStats = MPHttpShowStats
    Left = 184
    Top = 240
  end
end
