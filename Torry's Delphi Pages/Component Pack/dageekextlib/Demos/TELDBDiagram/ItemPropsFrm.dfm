object frmItemProps: TfrmItemProps
  Left = 269
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Item properties'
  ClientHeight = 190
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 233
    Height = 137
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 46
    Top = 158
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 126
    Top = 158
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
