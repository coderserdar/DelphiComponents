object frmBroadcastMessage: TfrmBroadcastMessage
  Left = 361
  Top = 348
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Broadcast message'
  ClientHeight = 63
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 10
    Width = 96
    Height = 13
    Caption = 'Message to all users'
  end
  object edMsg: TEdit
    Left = 10
    Top = 29
    Width = 287
    Height = 21
    TabOrder = 0
  end
  object btnSendBroadcast: TButton
    Left = 312
    Top = 27
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 1
    OnClick = btnSendBroadcastClick
  end
end
