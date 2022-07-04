object Form1: TForm1
  Left = 204
  Top = 133
  Width = 432
  Height = 142
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    424
    108)
  PixelsPerInch = 96
  TextHeight = 13
  object API_stringgrid1: TAPI_stringgrid
    Left = 0
    Top = 0
    Width = 424
    Height = 108
    Align = alClient
    Color = 12443601
    DefaultRowHeight = 16
    DefaultDrawing = False
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    MultilineCells = True
    WordWrap = True
    FontFixed = clWindowText
    FontFocused = clWindowText
    FontSelected = clWindowText
    ColorFocused = clYellow
    ColorSelected = clWindow
    VerAlign = taAlignTop
    HorAlign = taLeftJustify
    RowHeights = (
      16
      16
      16
      16
      16)
  end
  object Button1: TButton
    Left = 341
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
end
