object Form1: TForm1
  Left = 191
  Top = 105
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 300
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 168
    Top = 40
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 168
    Top = 64
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 120
    Width = 289
    Height = 9
    Shape = bsFrame
  end
  object Label3: TLabel
    Left = 168
    Top = 160
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 168
    Top = 200
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 8
    Top = 272
    Width = 49
    Height = 13
    Caption = 'Thread 1 :'
    Visible = False
  end
  object Button1: TButton
    Left = 56
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 56
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 56
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 56
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 3
    OnClick = Button4Click
  end
  object ProgressBar1: TProgressBar
    Left = 64
    Top = 272
    Width = 217
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 4
    Visible = False
  end
  object ButtonStopAll: TButton
    Left = 112
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Stop all'
    Enabled = False
    TabOrder = 5
    OnClick = ButtonStopAllClick
  end
  object BMThread1: TBMThread
    UpdateEnabled = True
    ThreadGroup = BMThreadGroup1
    OnExecute = BMThread1Execute
    OnStart = BMThread1Start
    OnTerminate = BMThread1Terminate
    Left = 232
    Top = 72
  end
  object BMThreadGroup1: TBMThreadGroup
    OnUpdate = BMThreadGroup1Update
    OnStart = BMThreadGroup1Start
    OnTerminate = BMThreadGroup1Terminate
    Left = 232
    Top = 152
  end
  object BMThread2: TBMThread
    UpdateEnabled = True
    ThreadGroup = BMThreadGroup1
    UpdatePriority = 1
    OnExecute = BMThread2Execute
    OnStart = BMThread2Start
    OnTerminate = BMThread2Terminate
    Left = 232
    Top = 192
  end
end
