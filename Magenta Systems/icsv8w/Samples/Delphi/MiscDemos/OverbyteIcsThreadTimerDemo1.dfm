object IcsTimerDemoForm: TIcsTimerDemoForm
  Left = 529
  Top = 184
  Caption = 'TIcsThreadTimer demo'
  ClientHeight = 288
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 146
    Top = 60
    Width = 56
    Height = 13
    Caption = 'Timer count'
  end
  object StartButton: TButton
    Left = 12
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start Timer'
    TabOrder = 0
    OnClick = StartButtonClick
  end
  object DisplayMemo: TMemo
    Left = 12
    Top = 62
    Width = 119
    Height = 210
    TabOrder = 1
  end
  object StopButton: TButton
    Left = 92
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = StopButtonClick
  end
  object TimerCountEdit: TEdit
    Left = 146
    Top = 74
    Width = 53
    Height = 21
    TabOrder = 3
    Text = '10000'
  end
  object FreeAllButton: TButton
    Left = 172
    Top = 8
    Width = 75
    Height = 25
    Caption = 'FreeAll'
    TabOrder = 4
    OnClick = FreeAllButtonClick
  end
  object SleepButton: TButton
    Left = 146
    Top = 100
    Width = 75
    Height = 25
    Caption = 'Sleep(5000)'
    TabOrder = 5
    OnClick = SleepButtonClick
  end
  object GroupBox1: TGroupBox
    Left = 136
    Top = 130
    Width = 111
    Height = 143
    Caption = ' Global Vars '
    TabOrder = 6
    object Label2: TLabel
      Left = 10
      Top = 20
      Width = 96
      Height = 13
      Caption = 'MaxTimerPerThread'
    end
    object Label3: TLabel
      Left = 11
      Top = 62
      Width = 92
      Height = 13
      Caption = 'MinTimerResolution'
    end
    object SetGlobalsButton: TButton
      Left = 10
      Top = 106
      Width = 75
      Height = 25
      Caption = 'SetGlobals'
      TabOrder = 0
      OnClick = SetGlobalsButtonClick
    end
    object TimersPerClockEdit: TEdit
      Left = 10
      Top = 34
      Width = 53
      Height = 21
      TabOrder = 1
      Text = '100'
    end
    object MinResolutionEdit: TEdit
      Left = 10
      Top = 76
      Width = 53
      Height = 21
      TabOrder = 2
      Text = '100'
    end
  end
  object LoopCheckBox: TCheckBox
    Left = 12
    Top = 38
    Width = 131
    Height = 17
    Caption = 'Loop/FreeAll - Interval:'
    TabOrder = 7
  end
  object LoopIntervalEdit: TEdit
    Left = 146
    Top = 36
    Width = 53
    Height = 21
    TabOrder = 8
    Text = '10'
  end
end
