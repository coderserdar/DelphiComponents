object dlgLinesEditor: TdlgLinesEditor
  Left = 208
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Lines editor'
  ClientHeight = 305
  ClientWidth = 386
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
  object Button1: TButton
    Left = 216
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 296
    Top = 272
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 16
    Top = 16
    Width = 353
    Height = 241
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button3: TButton
    Left = 40
    Top = 272
    Width = 121
    Height = 25
    Caption = 'Insert data field'
    TabOrder = 3
    OnClick = Button3Click
  end
end
