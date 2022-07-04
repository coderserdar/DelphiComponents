object frmEditContact: TfrmEditContact
  Left = 474
  Top = 322
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit contact'
  ClientHeight = 125
  ClientWidth = 204
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
    Left = 7
    Top = 4
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 7
    Top = 45
    Width = 109
    Height = 13
    Caption = 'Phone URL (host:port):'
  end
  object edName: TEdit
    Left = 7
    Top = 18
    Width = 191
    Height = 21
    TabOrder = 0
  end
  object edURL: TEdit
    Left = 7
    Top = 60
    Width = 191
    Height = 21
    TabOrder = 1
    OnExit = edURLExit
  end
  object bOk: TBitBtn
    Left = 16
    Top = 95
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object bCancel: TBitBtn
    Left = 110
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
end
