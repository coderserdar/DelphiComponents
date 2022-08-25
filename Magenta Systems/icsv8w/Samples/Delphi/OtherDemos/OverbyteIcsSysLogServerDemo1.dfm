object SysLogServerForm: TSysLogServerForm
  Left = 70
  Top = 187
  Caption = 'SysLogServer - http://www.overbyte.be'
  ClientHeight = 220
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 41
    Align = alTop
    TabOrder = 0
    object StartButton: TButton
      Left = 16
      Top = 11
      Width = 75
      Height = 21
      Caption = 'Start'
      TabOrder = 0
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 104
      Top = 11
      Width = 75
      Height = 21
      Caption = 'Stop'
      TabOrder = 1
      OnClick = StopButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 371
    Height = 179
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
