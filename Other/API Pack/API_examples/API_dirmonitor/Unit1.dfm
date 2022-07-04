object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Form1'
  ClientHeight = 84
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 96
    Height = 13
    Caption = 'Last changed stamp'
  end
  object Label2: TLabel
    Left = 205
    Top = 32
    Width = 67
    Height = 13
    Alignment = taRightJustify
    Caption = 'Thread Status'
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 265
    Height = 21
    TabOrder = 0
    OnClick = Edit1Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 32
    Width = 121
    Height = 17
    Caption = 'Enable dir monitoring'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object API_files1: TAPI_files
    ShowProgress = True
    Confirmation = False
    Left = 184
    Top = 16
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 152
    Top = 16
  end
  object API_DirMonitor1: TAPI_DirMonitor
    Active = False
    Subs = False
    Options = [fmFilename, fmAttributes]
    OnChange = API_DirMonitor1Change
    Left = 152
    Top = 48
  end
end
