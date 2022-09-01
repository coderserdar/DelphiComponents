object ColorEditForm: TColorEditForm
  Left = 159
  Top = 180
  BorderStyle = bsDialog
  Caption = 'Color editor'
  ClientHeight = 110
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Editor1: TbvColorEdit
    Left = 8
    Top = 24
    Width = 145
    Height = 23
    Cursor = crHandPoint
    ColorValue = clGreen
    OnChangeColor = Editor1ChangeColor
  end
  object Label1: TLabel
    Left = 176
    Top = 24
    Width = 70
    Height = 13
    Caption = 'Current color is'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 366
    Height = 4
    Align = alTop
    Shape = bsTopLine
  end
  object Editor2: TbvColorEdit
    Left = 8
    Top = 56
    Width = 145
    Height = 23
    Cursor = crHandPoint
    Flat = True
    ColorValue = clGreen
    OnChangeColor = Editor1ChangeColor
  end
  object Shape: TShape
    Left = 184
    Top = 48
    Width = 137
    Height = 41
    Brush.Color = clGreen
    Shape = stEllipse
  end
  object bvFormSaver1: TbvFormSaver
    Enabled = True
    SavePosOnly = True
    Left = 296
    Top = 8
  end
end
