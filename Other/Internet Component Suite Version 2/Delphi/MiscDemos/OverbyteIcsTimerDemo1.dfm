object IcsTimerDemoForm: TIcsTimerDemoForm
  Left = 70
  Top = 47
  Caption = 'TIcsTimer demo'
  ClientHeight = 285
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
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
    Top = 43
    Width = 213
    Height = 229
    TabOrder = 1
  end
  object StopButton: TButton
    Left = 100
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = StopButtonClick
  end
end
