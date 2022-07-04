object Magnifier: TMagnifier
  Left = 240
  Top = 192
  Width = 389
  Height = 246
  BorderStyle = bsSizeToolWin
  Caption = 'Magnifier'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 381
    Height = 219
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 0
    object PaintBox: TPaintBox
      Left = 0
      Top = 0
      Width = 377
      Height = 215
      Align = alClient
      OnPaint = PaintBoxPaint
    end
  end
end
