object Form1: TForm1
  Left = 187
  Top = 133
  Caption = 'Form1'
  ClientHeight = 209
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    748
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 688
    Top = 6
    Width = 32
    Height = 13
    Anchors = [akRight]
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 688
    Top = 81
    Width = 32
    Height = 13
    Anchors = [akRight]
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 688
    Top = 157
    Width = 32
    Height = 13
    Anchors = [akRight]
    Caption = 'Label3'
  end
  object tAPI_linechart1: TAPI_linechart
    Left = 8
    Top = 8
    Width = 673
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clSilver
    ParentColor = False
    HistoryCount = 500
    Lines = 1
    BackgroundImage.Data = {07544269746D617000000000}
    Grid = True
    GridColor = clNavy
    GridCountH = 10
    GridCountV = 10
    ZoomAuto = True
    ShowMinMax = False
    ZoomMargin = 10.000000000000000000
    Gradient = True
    GradientStart = clBtnFace
    GradientEnd = clBlack
    GradientStyle = gsVerCenter
  end
  object Button1: TButton
    Left = 8
    Top = 176
    Width = 673
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'next'
    TabOrder = 0
    OnClick = Button1Click
  end
end
