object NewFieldForm: TNewFieldForm
  Left = 372
  Top = 207
  BorderStyle = bsDialog
  Caption = 'NewFieldForm'
  ClientHeight = 95
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelChoiceField: TLabel
    Left = 16
    Top = 8
    Width = 81
    Height = 13
    Caption = 'LabelChoiceField'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 248
    Height = 4
    Align = alTop
    Shape = bsTopLine
  end
  object EditField: TComboBox
    Left = 16
    Top = 24
    Width = 217
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object BitBtnOk: TBitBtn
    Left = 24
    Top = 56
    Width = 97
    Height = 25
    Caption = 'BtnYes'
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 128
    Top = 56
    Width = 99
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
