object Form1: TForm1
  Left = 192
  Top = 107
  Width = 206
  Height = 75
  Caption = 'Form1'
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
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object tAPI_processlimit1: TAPI_processlimit
    Instance = 'SingleInstance'
    OnExists = tAPI_processlimit1Exists
    Left = 72
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 120
    Top = 8
  end
end
