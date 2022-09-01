object AlignmentForm: TAlignmentForm
  Left = 74
  Top = 101
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'AlignmentForm'
  ClientHeight = 89
  ClientWidth = 248
  Color = clWindow
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
  object BitBtnOk: TBitBtn
    Left = 168
    Top = 16
    Width = 75
    Height = 25
    Caption = 'BitBtnOk'
    TabOrder = 0
    Kind = bkOK
  end
  object BtnCancel: TBitBtn
    Left = 168
    Top = 48
    Width = 75
    Height = 25
    Caption = 'BtnCancel'
    TabOrder = 1
    Kind = bkCancel
  end
  object EditAlignment: TRadioGroup
    Left = 8
    Top = 8
    Width = 153
    Height = 72
    Items.Strings = (
      'Left'
      'center'
      'right')
    TabOrder = 2
  end
end
