object frmLinkProps: TfrmLinkProps
  Left = 245
  Top = 164
  BorderStyle = bsDialog
  Caption = 'Link Properties'
  ClientHeight = 278
  ClientWidth = 217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 30
    Top = 246
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 110
    Top = 246
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 32
    Width = 185
    Height = 81
    Caption = 'Begin point type'
    Items.Strings = (
      'None'
      'One'
      'Infinity')
    TabOrder = 2
  end
  object RadioGroup2: TRadioGroup
    Left = 16
    Top = 152
    Width = 185
    Height = 81
    Caption = 'End point type'
    Items.Strings = (
      'None'
      'One'
      'Infinity')
    TabOrder = 3
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 8
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
  end
  object ComboBox2: TComboBox
    Left = 16
    Top = 128
    Width = 185
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
  end
end
