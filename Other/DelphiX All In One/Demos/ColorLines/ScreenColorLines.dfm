object FormColorLines: TFormColorLines
  Left = 390
  Top = 352
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Color Lines'
  ClientHeight = 299
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelRadius: TLabel
    Left = 251
    Top = 9
    Width = 33
    Height = 13
    Caption = 'Radius'
  end
  object Image: TDXPaintBox
    Left = 45
    Top = 105
    Width = 240
    Height = 180
    AutoStretch = False
    Center = False
    KeepAspect = False
    Stretch = False
    ViewWidth = 0
    ViewHeight = 0
  end
  object RadioGroupPixelGeometry: TRadioGroup
    Left = 22
    Top = 9
    Width = 100
    Height = 80
    Caption = 'Pixel Geometry'
    ItemIndex = 1
    Items.Strings = (
      'Point'
      'Cicular'
      'Rectangular')
    TabOrder = 0
    OnClick = RadioGroupPixelGeometryClick
  end
  object SpinEditRadius: TSpinEdit
    Left = 250
    Top = 26
    Width = 51
    Height = 22
    MaxValue = 100
    MinValue = 1
    TabOrder = 1
    Value = 4
    OnChange = FormCreate
  end
  object RadioGroupColorStyle: TRadioGroup
    Left = 134
    Top = 9
    Width = 100
    Height = 80
    Caption = 'Color Style'
    ItemIndex = 1
    Items.Strings = (
      'Solid'
      'Gradient'
      'Rainbow')
    TabOrder = 2
    OnClick = FormCreate
  end
end
