object MonForm: TMonForm
  Left = 44
  Top = 96
  Caption = 
    'Magenta Systems Internet Packet Monitoring Components - Display ' +
    'Packets using Raw Sockets and WinPcap/Npcap - v1.4 26th November' +
    ' 2018'
  ClientHeight = 787
  ClientWidth = 1177
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object LogWin: TMemo
    Left = 0
    Top = 111
    Width = 1177
    Height = 676
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1177
    Height = 111
    Align = alTop
    TabOrder = 1
    object LabelTraffic: TLabel
      Left = 302
      Top = 78
      Width = 33
      Height = 13
      Caption = 'Traffic:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 622
      Top = 6
      Width = 122
      Height = 14
      Caption = 'Ignore Traffic to/from IPs:'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 770
      Top = 66
      Width = 227
      Height = 28
      Caption = 
        'Note: raw aockets may not capture from some '#13#10'network adaptors, ' +
        'or may be one way ony'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 5
      Top = 5
      Width = 102
      Height = 14
      Caption = 'IP Address to Monitor'
    end
    object Label4: TLabel
      Left = 115
      Top = 5
      Width = 154
      Height = 14
      Caption = 'Adapter to Monitor (xPCap only)'
    end
    object LabelAdmin: TLabel
      Left = 302
      Top = 60
      Width = 310
      Height = 13
      Caption = 'Program does not have Administrator Rights, no socket monitoring'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object MonIpList: TListBox
      Left = 8
      Top = 20
      Width = 101
      Height = 76
      ItemHeight = 14
      TabOrder = 0
    end
    object doMonitor: TButton
      Left = 302
      Top = 9
      Width = 91
      Height = 25
      Caption = 'Start Monitor'
      TabOrder = 2
      OnClick = doMonitorClick
    end
    object doExit: TButton
      Left = 922
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 11
      OnClick = doExitClick
    end
    object IgnoreLAN: TCheckBox
      Left = 754
      Top = 10
      Width = 162
      Height = 17
      Caption = 'Ignore LAN Traffic with Mask: '
      TabOrder = 9
    end
    object IgnoreData: TCheckBox
      Left = 405
      Top = 10
      Width = 81
      Height = 17
      Caption = 'Ignore Data'
      TabOrder = 3
    end
    object IpMask: TEdit
      Left = 752
      Top = 32
      Width = 97
      Height = 22
      TabOrder = 10
      Text = '255.255.255.0'
    end
    object FullData: TCheckBox
      Left = 495
      Top = 10
      Width = 101
      Height = 17
      Caption = 'Display Full Data'
      TabOrder = 4
    end
    object IgnoreIPs: TMemo
      Left = 635
      Top = 25
      Width = 111
      Height = 71
      Lines.Strings = (
        '192.168.1.4'
        '192.168.1.255')
      ScrollBars = ssVertical
      TabOrder = 8
    end
    object UseWinPCap: TCheckBox
      Left = 302
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Use xPCap Driver'
      Enabled = False
      TabOrder = 5
      OnClick = UseWinPCapClick
    end
    object AdapterList: TListBox
      Left = 115
      Top = 20
      Width = 181
      Height = 76
      ItemHeight = 14
      TabOrder = 1
      OnClick = AdapterListClick
    end
    object IgnoreNonIp: TCheckBox
      Left = 510
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Ignore Non-IP Traffic'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 7
    end
    object Promiscuous: TCheckBox
      Left = 425
      Top = 40
      Width = 81
      Height = 17
      Caption = 'Promiscuous'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 6
    end
    object doClear: TButton
      Left = 922
      Top = 35
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 12
      OnClick = doClearClick
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 64
    Top = 144
  end
end
