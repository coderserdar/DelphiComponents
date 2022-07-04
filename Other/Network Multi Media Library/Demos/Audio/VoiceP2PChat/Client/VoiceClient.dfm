object frmMain: TfrmMain
  Left = 252
  Top = 314
  Width = 725
  Height = 382
  Caption = 'Voice Client Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 717
    Height = 109
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object TPanel
      Left = 11
      Top = 8
      Width = 94
      Height = 92
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object cbActivate: TCheckBox
        Left = 12
        Top = 9
        Width = 72
        Height = 18
        Caption = 'Active'
        TabOrder = 0
        OnClick = cbActivateClick
      end
    end
    object pcParams: TPageControl
      Left = 111
      Top = 8
      Width = 594
      Height = 93
      ActivePage = TabSheet3
      TabOrder = 1
      object TabSheet2: TTabSheet
        Caption = 'Server info'
        object Label5: TLabel
          Left = 11
          Top = 10
          Width = 54
          Height = 13
          Alignment = taRightJustify
          Caption = 'Server host'
        end
        object Label8: TLabel
          Left = 144
          Top = 10
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = 'Server port'
        end
        object Label2: TLabel
          Left = 228
          Top = 10
          Width = 96
          Height = 13
          Caption = 'Voice Frame (MSec)'
        end
        object edHost: TEdit
          Left = 10
          Top = 28
          Width = 116
          Height = 21
          TabOrder = 0
          Text = 'localhost'
        end
        object sedPort: TSpinEdit
          Left = 139
          Top = 28
          Width = 78
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object edPeriod: TEdit
          Left = 228
          Top = 28
          Width = 38
          Height = 21
          TabOrder = 2
          Text = '20'
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'User info'
        ImageIndex = 1
        object Label1: TLabel
          Left = 13
          Top = 11
          Width = 48
          Height = 13
          Alignment = taRightJustify
          Caption = 'Username'
        end
        object Label6: TLabel
          Left = 151
          Top = 9
          Width = 46
          Height = 13
          Alignment = taRightJustify
          Caption = 'Password'
        end
        object edUser: TEdit
          Left = 11
          Top = 29
          Width = 121
          Height = 21
          Color = cl3DLight
          Enabled = False
          ReadOnly = True
          TabOrder = 0
        end
        object edPassword: TEdit
          Left = 147
          Top = 28
          Width = 121
          Height = 21
          Color = cl3DLight
          Enabled = False
          ReadOnly = True
          TabOrder = 1
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Proxy info'
        ImageIndex = 2
        object Label9: TLabel
          Left = 8
          Top = 11
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Caption = 'Proxy host'
        end
        object Label10: TLabel
          Left = 8
          Top = 38
          Width = 47
          Height = 13
          Alignment = taRightJustify
          Caption = 'Proxy port'
        end
        object Label3: TLabel
          Left = 159
          Top = 11
          Width = 78
          Height = 13
          Alignment = taRightJustify
          Caption = 'Proxy user name'
        end
        object Label4: TLabel
          Left = 159
          Top = 38
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = 'Proxy password'
        end
        object Label7: TLabel
          Left = 349
          Top = 17
          Width = 95
          Height = 13
          Caption = 'Socks proxy version'
        end
        object edProxyHost: TEdit
          Left = 64
          Top = 8
          Width = 78
          Height = 21
          TabOrder = 0
        end
        object sedProxyPort: TSpinEdit
          Left = 64
          Top = 36
          Width = 78
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object edProxyUserName: TEdit
          Left = 243
          Top = 8
          Width = 78
          Height = 21
          TabOrder = 2
        end
        object edProxyPassword: TEdit
          Left = 243
          Top = 35
          Width = 78
          Height = 21
          PasswordChar = '*'
          TabOrder = 3
        end
        object cbSocksProxyVersion: TComboBox
          Left = 347
          Top = 33
          Width = 110
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 334
    Width = 717
    Height = 21
    Panels = <
      item
        Text = 'Disconnected'
        Width = 80
      end
      item
        Width = 500
      end>
  end
  object pcData: TPageControl
    Left = 0
    Top = 109
    Width = 717
    Height = 225
    ActivePage = tsPicture
    Align = alClient
    TabOrder = 2
    object tsPicture: TTabSheet
      Caption = 'Picture'
    end
    object tsNetStatistics: TTabSheet
      Caption = 'NetStatistics'
      ImageIndex = 3
      object lbDeltaBytesReceived: TLabel
        Left = 12
        Top = 9
        Width = 101
        Height = 13
        Caption = 'FrameBytesRecieved'
      end
      object lbTotalBytesReceived: TLabel
        Left = 12
        Top = 40
        Width = 96
        Height = 13
        Caption = 'TotalBytesRecieved'
      end
      object lbDeltasReceived: TLabel
        Left = 12
        Top = 25
        Width = 80
        Height = 13
        Caption = 'FramesRecieved'
      end
    end
  end
  object ImageClient: TImageClient
    ServerHost = 'localhost'
    ServerPort = 0
    ReducePeriodOnDisconnect = False
    AutoReconnectInterval = 300000
    SocksProxyPort = 0
    OnConnectionTeminated = ImageClientConnectionTeminated
    PeriodChangedEvent = ImageClientPeriodChangedEvent
    OnStatusChanged = ImageClientStatusChanged
    OnConnectionRefused = ImageClientConnectionRefused
    OnPictureWndUpdate = ImageClientPictureWndUpdate
    Left = 472
    Top = 184
  end
end
