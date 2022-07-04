object AppBaseForm: TAppBaseForm
  Left = 75
  Top = 319
  Caption = 'AppBaseForm'
  ClientHeight = 215
  ClientWidth = 462
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
    Width = 462
    Height = 101
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 100
      Top = 44
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Button1: TButton
      Left = 12
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 92
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Connect1Button: TButton
      Left = 188
      Top = 8
      Width = 75
      Height = 17
      Caption = 'Connect1'
      TabOrder = 2
      OnClick = Connect1ButtonClick
    end
    object Button4: TButton
      Left = 12
      Top = 36
      Width = 75
      Height = 25
      Caption = 'Button4'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Free1Button: TButton
      Left = 188
      Top = 32
      Width = 75
      Height = 17
      Caption = 'Free1'
      TabOrder = 4
      OnClick = Free1ButtonClick
    end
    object Release1Button: TButton
      Left = 188
      Top = 52
      Width = 75
      Height = 17
      Caption = 'Release1'
      TabOrder = 5
      OnClick = Release1ButtonClick
    end
    object HttpCliButton: TButton
      Left = 188
      Top = 76
      Width = 75
      Height = 17
      Caption = 'HttpCli'
      TabOrder = 6
      OnClick = HttpCliButtonClick
    end
    object WSocketServerButton: TButton
      Left = 348
      Top = 76
      Width = 89
      Height = 17
      Caption = 'WSocketServer'
      TabOrder = 7
      OnClick = WSocketServerButtonClick
    end
    object FtpCliButton: TButton
      Left = 268
      Top = 76
      Width = 75
      Height = 17
      Caption = 'FtpCli'
      TabOrder = 8
      OnClick = FtpCliButtonClick
    end
    object Thread1Button: TButton
      Left = 348
      Top = 8
      Width = 77
      Height = 17
      Caption = 'Thread1'
      TabOrder = 9
      OnClick = Thread1ButtonClick
    end
    object StopThread1Button: TButton
      Left = 348
      Top = 32
      Width = 75
      Height = 17
      Caption = 'StopThread1'
      TabOrder = 10
      OnClick = StopThread1ButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 101
    Width = 462
    Height = 114
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
  object Connect2Button: TButton
    Left = 268
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Connect2'
    TabOrder = 2
    OnClick = Connect2ButtonClick
  end
  object Free2Button: TButton
    Left = 268
    Top = 32
    Width = 75
    Height = 17
    Caption = 'Free1'
    TabOrder = 3
    OnClick = Free2ButtonClick
  end
end
