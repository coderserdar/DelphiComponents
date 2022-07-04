object FreeHandWindow: TFreeHandWindow
  Left = 505
  Top = 114
  BorderStyle = bsNone
  ClientHeight = 200
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 24
    Top = 0
    Width = 2
    Height = 201
    Brush.Style = bsClear
    Visible = False
    OnMouseDown = FormMouseDown
    OnMouseMove = FormMouseMove
    OnMouseUp = FormMouseUp
  end
  object Shape2: TShape
    Left = 0
    Top = 19
    Width = 289
    Height = 2
    Brush.Style = bsClear
    Visible = False
    OnMouseDown = FormMouseDown
    OnMouseMove = FormMouseMove
    OnMouseUp = FormMouseUp
  end
  object Shape3: TShape
    Left = 266
    Top = 0
    Width = 2
    Height = 201
    Brush.Style = bsClear
    Visible = False
    OnMouseDown = FormMouseDown
    OnMouseMove = FormMouseMove
    OnMouseUp = FormMouseUp
  end
  object Shape4: TShape
    Left = 0
    Top = 180
    Width = 289
    Height = 2
    Brush.Style = bsClear
    Visible = False
    OnMouseDown = FormMouseDown
    OnMouseMove = FormMouseMove
    OnMouseUp = FormMouseUp
  end
end
