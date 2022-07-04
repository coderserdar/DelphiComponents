object frmMain: TfrmMain
  Left = 134
  Top = 171
  Width = 810
  Height = 532
  Caption = 'Screen Share Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 465
    Width = 802
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
  object sbPicture: TScrollBox
    Left = 0
    Top = 0
    Width = 802
    Height = 465
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    object Image: TImage
      Left = 0
      Top = 0
      Width = 798
      Height = 457
      AutoSize = True
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
      OnMouseUp = ImageMouseUp
    end
  end
  object Client: TNMMRCClient
    ServerHost = 'localhost'
    ServerPort = 80
    AutoReconnectInterval = 300000
    SocksProxyPort = 0
    OnConnectionTeminated = ClientConnectionTeminated
    PeriodChangedEvent = ClientPeriodChangedEvent
    OnStatusChanged = ClientStatusChanged
    OnConnectionRefused = ClientConnectionRefused
    Image = Image
    OnPictureWndUpdate = ClientPictureWndUpdate
    Left = 472
    Top = 184
  end
  object MainMenu: TMainMenu
    Left = 616
    Top = 144
    object mnuConnection: TMenuItem
      Caption = 'Connection'
      object mnuConnect: TMenuItem
        Action = acActive
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuOptions: TMenuItem
        Caption = 'Options'
        OnClick = mnuOptionsClick
      end
    end
    object mnuRemoteControl: TMenuItem
      Caption = 'Remote Control'
      object mnuEnabled: TMenuItem
        Action = acEnableRC
        AutoCheck = True
      end
      object mnuRunTaskManager: TMenuItem
        Action = acRunTaskManager
      end
      object mnuPaste: TMenuItem
        Action = acPaste
      end
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
  object ActionList1: TActionList
    Left = 539
    Top = 144
    object acEnableRC: TAction
      AutoCheck = True
      Caption = 'Enable Remote Control'
      Checked = True
      OnExecute = acEnableRCExecute
    end
    object acRunTaskManager: TAction
      Caption = 'Run Task Manager'
      ShortCut = 16430
      SecondaryShortCuts.Strings = (
        'Ctrl+Alt+D')
      OnExecute = acRunTaskManagerExecute
    end
    object acPaste: TAction
      Caption = 'Paste'
      ShortCut = 8237
      SecondaryShortCuts.Strings = (
        'Ctrl-V')
      OnExecute = acPasteExecute
    end
    object acActive: TAction
      Caption = 'Active'
      OnExecute = cbActivateClick
    end
  end
end
