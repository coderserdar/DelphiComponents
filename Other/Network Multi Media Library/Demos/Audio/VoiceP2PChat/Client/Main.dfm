object frmMain: TfrmMain
  Left = 194
  Top = 280
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Voice Client Demo'
  ClientHeight = 81
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 308
    Height = 60
    Align = alTop
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 60
    Width = 308
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
  object Client: TNMMVoiceClient
    KeepConnection = False
    AutoReconnectInterval = 10000
    SocksProxyPort = 0
    OnConnectionTeminated = ImageClientConnectionTeminated
    PeriodChangedEvent = ClientPeriodChangedEvent
    OnStatusChanged = ImageClientStatusChanged
    OnConnectionRefused = ImageClientConnectionRefused
    OnTextMessage = ClientTextMessage
    Left = 35
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 80
    Top = 8
    object mnuSettings: TMenuItem
      Caption = 'Settings'
      OnClick = mnuSettingsClick
    end
    object mnuStatistics: TMenuItem
      Caption = 'Statistics'
      OnClick = mnuStatisticsClick
    end
    object mnuAbout: TMenuItem
      Caption = 'About'
      OnClick = mnuAboutClick
    end
    object mnuExit: TMenuItem
      Caption = 'Exit'
      OnClick = mnuExitClick
    end
  end
end
