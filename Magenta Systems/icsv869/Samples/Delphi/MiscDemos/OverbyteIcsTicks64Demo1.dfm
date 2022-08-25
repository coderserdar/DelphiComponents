object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ICS - Demo IcsGetTickCount64'
  ClientHeight = 202
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelInfo: TLabel
    Left = 20
    Top = 8
    Width = 45
    Height = 13
    Caption = 'LabelInfo'
  end
  object Label1: TLabel
    Left = 235
    Top = 7
    Width = 99
    Height = 13
    Caption = 'Force Specific Mode:'
  end
  object doUpdate: TButton
    Left = 70
    Top = 159
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 0
    OnClick = doUpdateClick
  end
  object doExit: TButton
    Left = 180
    Top = 159
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 1
    OnClick = doExitClick
  end
  object TickModes: TListBox
    Left = 235
    Top = 23
    Width = 121
    Height = 68
    ItemHeight = 13
    Items.Strings = (
      'API 64'
      'Perf Counter'
      'API 32')
    TabOrder = 2
    OnClick = TickModesClick
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 290
    Top = 155
  end
end
