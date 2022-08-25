object Form1: TForm1
  Left = 117
  Top = 89
  Caption = 
    'Magenta Serial Ports and Hardware Events - Release 3.0 - 1st Feb' +
    'ruary 2022'
  ClientHeight = 672
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 763
    Height = 76
    Align = alTop
    TabOrder = 0
    object Panel1: TPanel
      Left = 10
      Top = 10
      Width = 216
      Height = 41
      TabOrder = 0
      object StatusCTS: TApdStatusLight
        Left = 10
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object StatusDSR: TApdStatusLight
        Left = 40
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object StatusDCD: TApdStatusLight
        Left = 70
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object StatusRI: TApdStatusLight
        Left = 100
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object StatusER: TApdStatusLight
        Left = 130
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object Label1: TLabel
        Left = 10
        Top = 25
        Width = 20
        Height = 14
        Caption = 'CTS'
      end
      object Label2: TLabel
        Left = 40
        Top = 25
        Width = 21
        Height = 14
        Caption = 'DSR'
      end
      object Label3: TLabel
        Left = 70
        Top = 25
        Width = 21
        Height = 14
        Caption = 'DCD'
      end
      object Label4: TLabel
        Left = 100
        Top = 25
        Width = 21
        Height = 14
        Caption = 'Ring'
      end
      object StatusRX: TApdStatusLight
        Left = 160
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object StatusTX: TApdStatusLight
        Left = 190
        Top = 5
        Width = 18
        Height = 13
        Lit = False
      end
      object Label5: TLabel
        Left = 130
        Top = 25
        Width = 24
        Height = 14
        Caption = 'Error'
      end
      object Label6: TLabel
        Left = 160
        Top = 25
        Width = 14
        Height = 14
        Caption = 'RX'
      end
      object Label7: TLabel
        Left = 190
        Top = 25
        Width = 13
        Height = 14
        Caption = 'TX'
      end
    end
    object doCom: TButton
      Left = 420
      Top = 10
      Width = 101
      Height = 25
      Caption = 'Monitor Serial Port'
      TabOrder = 3
      OnClick = doComClick
    end
    object SetRTS: TCheckBox
      Left = 325
      Top = 20
      Width = 97
      Height = 17
      Caption = 'Raise RTS'
      TabOrder = 2
      OnClick = SetRTSClick
    end
    object SetDTR: TCheckBox
      Left = 240
      Top = 20
      Width = 81
      Height = 17
      Caption = 'Raise DTR'
      TabOrder = 1
      OnClick = SetDTRClick
    end
    object doStop: TButton
      Left = 530
      Top = 10
      Width = 51
      Height = 25
      Caption = 'Stop'
      TabOrder = 4
      OnClick = doStopClick
    end
    object doExt: TButton
      Left = 530
      Top = 40
      Width = 51
      Height = 25
      Caption = 'Exit'
      TabOrder = 5
      OnClick = doExtClick
    end
    object doListPorts: TButton
      Left = 420
      Top = 40
      Width = 101
      Height = 25
      Caption = 'List Serial Ports'
      TabOrder = 6
      OnClick = doListPortsClick
    end
    object doMonDirs: TCheckBox
      Left = 600
      Top = 14
      Width = 151
      Height = 17
      Caption = 'Monitor Directory Changes'
      TabOrder = 7
      OnClick = doMonDirsClick
    end
  end
  object Log: TMemo
    Left = 0
    Top = 494
    Width = 763
    Height = 178
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object PortList: TListView
    Left = 0
    Top = 76
    Width = 763
    Height = 418
    Align = alClient
    Columns = <
      item
        Caption = 'Port'
        Width = 60
      end
      item
        Caption = 'Enabled'
      end
      item
        Caption = 'Friendly Name'
        Width = 240
      end
      item
        Caption = 'Internal Name'
        Width = 80
      end
      item
        Caption = 'Num'
        Width = 40
      end
      item
        Caption = 'Description'
        Width = 200
      end
      item
        Caption = 'Manufacturer'
        Width = 150
      end
      item
        Caption = 'Hardware Id'
        Width = 150
      end
      item
        Caption = 'Location'
        Width = 150
      end
      item
        Caption = 'Status'
        Width = 150
      end>
    GridLines = True
    ReadOnly = True
    TabOrder = 2
    ViewStyle = vsReport
    OnDblClick = PortListDblClick
  end
  object ApdComPort: TApdComPort
    ComNumber = 1
    PromptForPort = False
    AutoOpen = False
    DTR = False
    RTS = False
    TraceName = 'APRO.TRC'
    LogName = 'APRO.LOG'
    OnTriggerStatus = ApdComPortTriggerStatus
    Left = 125
    Top = 135
  end
  object ApdSLController: TApdSLController
    ComPort = ApdComPort
    RXDOffTimeout = 2
    TXDOffTimeout = 2
    RingOffTimeout = 16
    Lights.CTSLight = StatusCTS
    Lights.DSRLight = StatusDSR
    Lights.DCDLight = StatusDCD
    Lights.RINGLight = StatusRI
    Lights.TXDLight = StatusTX
    Lights.RXDLight = StatusRX
    Lights.ERRORLight = StatusER
    Left = 85
    Top = 135
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 165
    Top = 135
  end
end
