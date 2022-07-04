object OQBDBFormNCOCI8: TOQBDBFormNCOCI8
  Left = 252
  Top = 111
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Select Database'
  ClientHeight = 91
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 248
    Height = 57
    Shape = bsFrame
  end
  object BtnOk: TButton
    Left = 119
    Top = 64
    Width = 61
    Height = 20
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 186
    Top = 64
    Width = 59
    Height = 20
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object CheckDB: TCheckBox
    Left = 13
    Top = 31
    Width = 222
    Height = 14
    Caption = 'Include System Tables'
    TabOrder = 2
  end
  object CheckView: TCheckBox
    Left = 13
    Top = 11
    Width = 222
    Height = 14
    Caption = 'Include Views'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
