object ComparisonTestForm: TComparisonTestForm
  Left = 192
  Top = 107
  Width = 800
  Height = 600
  Caption = 'Comparison Test Results'
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
  object ComparisonTestStringGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 792
    Height = 536
    Align = alClient
    DefaultRowHeight = 44
    DefaultDrawing = False
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goRowMoving, goColMoving]
    TabOrder = 0
    OnDrawCell = ComparisonTestStringGridDrawCell
  end
  object Panel1: TPanel
    Left = 0
    Top = 536
    Width = 792
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 704
      Top = 0
      Width = 88
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object CloseButton: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Close'
        ModalResult = 2
        TabOrder = 0
      end
    end
  end
end
