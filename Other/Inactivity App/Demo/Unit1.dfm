object Form1: TForm1
  Left = 283
  Top = 153
  Width = 557
  Height = 399
  Caption = 'Demo del componente InactivityApp'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    549
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object lblProceso: TLabel
    Left = 24
    Top = 24
    Width = 497
    Height = 74
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -64
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblState: TLabel
    Left = 328
    Top = 118
    Width = 5
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbl1: TLabel
    Left = 16
    Top = 320
    Width = 81
    Height = 13
    Caption = 'Inactividad total: '
  end
  object lbl2: TLabel
    Left = 16
    Top = 336
    Width = 90
    Height = 13
    Caption = 'Inactividad actual: '
  end
  object Label1: TLabel
    Left = 128
    Top = 320
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label2: TLabel
    Left = 128
    Top = 336
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Button1: TButton
    Left = 24
    Top = 112
    Width = 289
    Height = 25
    Caption = 'Activar / Desactivar sensor de Inactividad'
    TabOrder = 0
    OnClick = Button1Click
  end
  object mmLog: TMemo
    Left = 8
    Top = 152
    Width = 529
    Height = 153
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object InactivityApp1: TInactivityApp
    InactivityIntervalCheck = 1000
    InactivityMin = 0
    InactivitySec = 6
    Active = False
    OnInactivityComplete = InactivityApp1InactivityComplete
    OnResetTimer = InactivityApp1ResetTimer
    OnInactivityStep = InactivityApp1InactivityStep
    Left = 16
    Top = 16
  end
end
