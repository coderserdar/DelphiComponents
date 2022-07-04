object Form1: TForm1
  Left = 192
  Top = 114
  Width = 525
  Height = 233
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    517
    199)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 420
    Top = 16
    Width = 32
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 420
    Top = 32
    Width = 32
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 420
    Top = 48
    Width = 32
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 421
    Top = 64
    Width = 32
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Label4'
  end
  object API_chart1: TAPI_chart
    Left = 8
    Top = 8
    Width = 405
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    OnMouseDown = API_chart1MouseDown
    Sorted = True
    Title = 'My Chart'
    TitleColor = clBlack
    ColorBackground1 = clSilver
    ColorBackground2 = clWhite
    ChartType = ctLine
    XAxisFormat = '0.0'
    YAxisFormat = '0.0'
    XAxisShowItem = False
    YAxisShowItem = False
    XAxisShowExt = True
    YAxisShowExt = True
    MaxValues = 0
    XAxisSpacing = 5
    YAxisSpacing = 5
    ColorDefault = clRed
    ColorGrid = clBlack
  end
  object CheckBox1: TCheckBox
    Left = 420
    Top = 80
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'LIne/Bar Chart'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
end
