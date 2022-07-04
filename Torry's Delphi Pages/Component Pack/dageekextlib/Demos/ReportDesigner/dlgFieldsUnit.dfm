object dlgFields: TdlgFields
  Left = 241
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Fields dialog'
  ClientHeight = 234
  ClientWidth = 266
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
    Left = 16
    Top = 8
    Width = 217
    Height = 13
    Caption = 'Select field from fields list and click '#39'Ok'#39' button'
  end
  object Button1: TButton
    Left = 96
    Top = 202
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 176
    Top = 202
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 16
    Top = 32
    Width = 233
    Height = 161
    ItemHeight = 13
    TabOrder = 2
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
end
