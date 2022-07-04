object Form1: TForm1
  Left = 327
  Top = 308
  Width = 427
  Height = 171
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    419
    137)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 96
    Top = 8
    Width = 313
    Height = 65
    Anchors = [akLeft, akTop, akRight]
  end
  object Label1: TLabel
    Left = 104
    Top = 16
    Width = 35
    Height = 13
    Caption = 'Synced'
  end
  object Label2: TLabel
    Left = 104
    Top = 32
    Width = 48
    Height = 13
    Caption = 'UnSynced'
  end
  object Label3: TLabel
    Left = 304
    Top = 13
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 304
    Top = 33
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 200
    Top = 32
    Width = 37
    Height = 13
    Caption = 'Label5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 200
    Top = 13
    Width = 54
    Height = 13
    Caption = 'Difference:'
  end
  object Label7: TLabel
    Left = 104
    Top = 51
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label8: TLabel
    Left = 200
    Top = 51
    Width = 37
    Height = 13
    Caption = 'Label5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label10: TLabel
    Left = 304
    Top = 51
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object API_progressbar1: TAPI_progressbar
    Left = 8
    Top = 79
    Width = 401
    Height = 50
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    EndColor = 16744448
    ValueAsText = False
    AddToText = '%'
    Vertical = False
    BorderWidth = 1
    BorderColor = clGray
    Alignment = taCenter
    Steps = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run Threads'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Stop Threads'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 384
    Top = 8
  end
  object API_thread1: TAPI_thread
    Priority = tpNormal
    Synchronized = True
    Interval = 0
    OnExecute = API_thread1Execute
    Left = 72
    Top = 8
  end
  object API_thread2: TAPI_thread
    Priority = tpNormal
    Synchronized = False
    Interval = 0
    OnExecute = API_thread2Execute
    Left = 72
    Top = 40
  end
  object API_thread3: TAPI_thread
    Priority = tpLower
    Synchronized = True
    Interval = 0
    OnExecute = API_thread3Execute
    Left = 72
    Top = 72
  end
  object API_thread4: TAPI_thread
    Priority = tpNormal
    Synchronized = False
    Interval = 0
    OnExecute = API_thread4Execute
    Left = 72
    Top = 104
  end
end
