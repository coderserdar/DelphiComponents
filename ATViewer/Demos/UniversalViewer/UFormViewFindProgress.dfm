object FormViewFindProgress: TFormViewFindProgress
  Left = 337
  Top = 293
  AlphaBlend = True
  AlphaBlendValue = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 86
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labSearch: TTntLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Searching:'
  end
  object labPercent: TLabel
    Left = 8
    Top = 42
    Width = 29
    Height = 13
    Caption = '100%'
  end
  object btnCancel: TButton
    Left = 104
    Top = 56
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 24
    Width = 265
    Height = 17
    Step = 1
    TabOrder = 1
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 20
    OnTimer = Timer1Timer
    Left = 208
    Top = 48
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer2Timer
    Left = 240
    Top = 48
  end
end
