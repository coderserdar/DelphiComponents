object BasicNntpForm: TBasicNntpForm
  Left = 3
  Top = 84
  Caption = 'Basic NNTP demo'
  ClientHeight = 341
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 535
    Height = 41
    Align = alTop
    TabOrder = 0
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
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 535
    Height = 300
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
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
