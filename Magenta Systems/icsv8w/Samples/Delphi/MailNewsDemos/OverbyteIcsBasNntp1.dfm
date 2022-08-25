object BasicNntpForm: TBasicNntpForm
  Left = 119
  Top = 113
  Caption = 'Basic NNTP demo'
  ClientHeight = 396
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 494
    Height = 355
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 494
    Height = 41
    Align = alTop
    TabOrder = 1
    object ExecButton: TButton
      Left = 12
      Top = 8
      Width = 75
      Height = 25
      Caption = 'ExecButton'
      TabOrder = 0
      OnClick = ExecButtonClick
    end
  end
  object NntpCli1: TNntpCli
    Port = 'nntp'
    LineLimit = 65536
    OnSessionConnected = NntpCli1SessionConnected
    OnSessionClosed = NntpCli1SessionClosed
    OnRequestDone = NntpCli1RequestDone
    OnMessageLine = NntpCli1MessageLine
    Left = 68
    Top = 100
  end
end
