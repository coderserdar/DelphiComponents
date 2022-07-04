object frmThreadInfo: TfrmThreadInfo
  Left = 538
  Top = 326
  Width = 210
  Height = 177
  Caption = 'Thread Info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblClientGroupThreads: TLabel
    Left = 10
    Top = 20
    Width = 94
    Height = 13
    Caption = 'ClientGroupThreads'
  end
  object lblWriteStreamThread: TLabel
    Left = 11
    Top = 46
    Width = 97
    Height = 13
    Caption = 'WriteStreamThreads'
  end
  object lblClientHandles: TLabel
    Left = 11
    Top = 72
    Width = 65
    Height = 13
    Caption = 'ClientHandles'
  end
  object ThreadInfoTimer: TTimer
    Interval = 200
    OnTimer = ThreadInfoTimerTimer
    Left = 120
    Top = 40
  end
end
