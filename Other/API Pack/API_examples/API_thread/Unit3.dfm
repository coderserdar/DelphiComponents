object Form1: TForm1
  Left = 215
  Top = 113
  Width = 582
  Height = 297
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    574
    263)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 176
    Width = 32
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Critical'
  end
  object Label2: TLabel
    Left = 88
    Top = 176
    Width = 33
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Normal'
  end
  object Label3: TLabel
    Left = 144
    Top = 176
    Width = 34
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Lowest'
  end
  object Label4: TLabel
    Left = 200
    Top = 176
    Width = 18
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Idle'
  end
  object API_progressbar1: TAPI_progressbar
    Left = 32
    Top = 16
    Width = 25
    Height = 137
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Max = 100.000000000000000000
    Position = 20.000000000000000000
    BackColor = clBlack
    StartColor = clLime
    EndColor = clRed
    ValueAsText = False
    AddToText = '%'
    Vertical = True
    BorderWidth = 1
    BorderColor = clGray
    Alignment = taCenter
    Steps = 0
  end
  object API_progressbar2: TAPI_progressbar
    Left = 88
    Top = 16
    Width = 25
    Height = 137
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Max = 100.000000000000000000
    Position = 20.000000000000000000
    BackColor = clBlack
    StartColor = clLime
    EndColor = clRed
    ValueAsText = False
    AddToText = '%'
    Vertical = True
    BorderWidth = 1
    BorderColor = clGray
    Alignment = taCenter
    Steps = 0
  end
  object API_progressbar3: TAPI_progressbar
    Left = 144
    Top = 16
    Width = 25
    Height = 137
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Max = 100.000000000000000000
    Position = 20.000000000000000000
    BackColor = clBlack
    StartColor = clLime
    EndColor = clRed
    ValueAsText = False
    AddToText = '%'
    Vertical = True
    BorderWidth = 1
    BorderColor = clGray
    Alignment = taCenter
    Steps = 0
  end
  object API_progressbar4: TAPI_progressbar
    Left = 200
    Top = 16
    Width = 25
    Height = 137
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Max = 100.000000000000000000
    Position = 20.000000000000000000
    BackColor = clBlack
    StartColor = clLime
    EndColor = clRed
    ValueAsText = False
    AddToText = '%'
    Vertical = True
    BorderWidth = 1
    BorderColor = clGray
    Alignment = taCenter
    Steps = 0
  end
  object Button1: TButton
    Left = 32
    Top = 215
    Width = 193
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start All Threads'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 256
    Top = 16
    Width = 305
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object API_thread1: TAPI_thread
    Priority = tpTimeCritical
    Synchronized = False
    Interval = 1
    OnStart = API_thread1Start
    OnExecute = API_thread1Execute
    OnStop = API_thread1Stop
    OnException = API_thread1Exception
    Left = 32
    Top = 8
  end
  object API_thread2: TAPI_thread
    Priority = tpNormal
    Synchronized = False
    Interval = 1
    OnExecute = API_thread2Execute
    Left = 88
    Top = 8
  end
  object API_thread3: TAPI_thread
    Priority = tpLowest
    Synchronized = False
    Interval = 1
    OnExecute = API_thread3Execute
    Left = 144
    Top = 8
  end
  object API_thread4: TAPI_thread
    Priority = tpIdle
    Synchronized = False
    Interval = 1
    OnExecute = API_thread4Execute
    Left = 200
    Top = 8
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 240
    Top = 104
  end
end
