object fext: Tfext
  Left = 436
  Top = 423
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = '  Run BLOB'
  ClientHeight = 90
  ClientWidth = 292
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 152
    Height = 13
    Caption = 'Open as file type (file extension).'
  end
  object Combo: TComboBox
    Left = 16
    Top = 40
    Width = 145
    Height = 21
    BevelKind = bkFlat
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboChange
    Items.Strings = (
      'jpg'
      'gif'
      'bmp'
      'doc'
      'htm'
      'rtf'
      'pdf'
      'txt'
      'mp3'
      'wav')
  end
  object Button1: TButton
    Left = 176
    Top = 40
    Width = 50
    Height = 25
    Caption = 'OK'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
  end
  object Button2: TButton
    Left = 232
    Top = 40
    Width = 50
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
