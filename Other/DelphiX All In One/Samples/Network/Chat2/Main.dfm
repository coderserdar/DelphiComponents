object MainForm: TMainForm
  Left = 207
  Top = 110
  ActiveControl = Edit1
  BorderStyle = bsDialog
  Caption = 'Chat'
  ClientHeight = 415
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 9
    Top = 9
    Width = 495
    Height = 365
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 9
    Top = 381
    Width = 408
    Height = 21
    TabOrder = 1
    OnKeyDown = Edit1KeyDown
  end
  object Button1: TButton
    Left = 425
    Top = 381
    Width = 81
    Height = 27
    Caption = 'Send'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object DXPlay1: TDXPlay
    Async = False
    GUID = '{913C8000-CA0A-11D1-8138-444553540000}'
    MaxPlayers = 0
    ModemSetting.Enabled = False
    TCPIPSetting.Enabled = False
    TCPIPSetting.Port = 0
    OnAddPlayer = DXPlay1AddPlayer
    OnDeletePlayer = DXPlay1DeletePlayer
    OnMessage = DXPlay1Message
    OnOpen = DXPlay1Open
    Left = 224
    Top = 144
  end
end
