object Form1: TForm1
  Left = 32
  Top = 112
  Width = 706
  Height = 561
  Caption = 'Chuong trinh minh hoa su dung doi tuong TGraph2D'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 490
    Top = 0
    Width = 208
    Height = 534
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 208
      Height = 321
      Align = alTop
      Caption = 'Items'
      TabOrder = 0
      object Label1: TLabel
        Left = 10
        Top = 230
        Width = 57
        Height = 13
        Caption = 'Shape Style'
      end
      object Label4: TLabel
        Left = 10
        Top = 260
        Width = 58
        Height = 13
        Caption = 'Shape Color'
      end
      object butAdd: TSpeedButton
        Left = 0
        Top = 290
        Width = 100
        Height = 25
        Caption = 'Add'
        Flat = True
        OnClick = butAddClick
      end
      object butRemove: TSpeedButton
        Left = 100
        Top = 290
        Width = 100
        Height = 25
        Caption = 'Remove'
        Flat = True
        OnClick = butRemoveClick
      end
      object ComboStyle: TComboBox
        Left = 90
        Top = 229
        Width = 101
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'String'
          'Point'
          'Line'
          'LineDraw'
          'Ellipse'
          'Rectangle'
          'FillRectangle'
          'Arc'
          'Pie'
          'YX'
          'XY'
          'RT'
          'YXT')
      end
      object ListShape: TListBox
        Left = 2
        Top = 15
        Width = 204
        Height = 196
        Align = alTop
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = ListShapeClick
      end
      object butShapeColor: TPanel
        Left = 90
        Top = 260
        Width = 65
        Height = 21
        Color = clYellow
        TabOrder = 2
        OnClick = butShapeColorClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 321
      Width = 208
      Height = 213
      Align = alClient
      Caption = 'View'
      TabOrder = 1
      object Label2: TLabel
        Left = 20
        Top = 110
        Width = 46
        Height = 13
        Caption = 'Axis Color'
      end
      object Label3: TLabel
        Left = 20
        Top = 140
        Width = 85
        Height = 13
        Caption = 'Background Color'
      end
      object Label5: TLabel
        Left = 20
        Top = 170
        Width = 48
        Height = 13
        BiDiMode = bdLeftToRight
        Caption = 'Grid COlor'
        ParentBiDiMode = False
      end
      object butZoomIn: TSpeedButton
        Left = 0
        Top = 28
        Width = 100
        Height = 25
        Caption = 'Zoom In'
        Flat = True
        OnClick = butZoomInClick
      end
      object butZoomOut: TSpeedButton
        Left = 100
        Top = 28
        Width = 100
        Height = 25
        Caption = 'Zoom Out'
        Flat = True
        OnClick = butZoomOutClick
      end
      object chkAxis: TCheckBox
        Left = 20
        Top = 70
        Width = 97
        Height = 17
        Caption = 'Axis'
        TabOrder = 0
        OnClick = chkAxisClick
      end
      object chkGrid: TCheckBox
        Left = 120
        Top = 70
        Width = 97
        Height = 17
        Caption = 'Grid'
        TabOrder = 1
        OnClick = chkGridClick
      end
      object butAxisColor: TPanel
        Left = 110
        Top = 112
        Width = 65
        Height = 21
        TabOrder = 2
        OnClick = butAxisColorClick
      end
      object butBackColor: TPanel
        Left = 110
        Top = 142
        Width = 65
        Height = 21
        TabOrder = 3
        OnClick = butBackColorClick
      end
      object butGridColor: TPanel
        Left = 110
        Top = 172
        Width = 65
        Height = 21
        TabOrder = 4
        OnClick = butGridColorClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 534
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    object G: TGraph
      Left = 1
      Top = 1
      Width = 488
      Height = 532
      Align = alClient
      POx = 200
      POy = 200
      POP = 17
      PColorAxis = 16503172
      PColorBackground = clBlack
      PColorGrid = clWhite
      PColorOxy = clLime
      PColorText = clWhite
      PColorBorder1 = clWhite
      PColorBorder2 = clBlue
      PSizeOxy = 12
      PSizeText = 8
      PSizeLine = 5
      PLineSpace = 1
      PLineWidth = 1
      PGrid = True
      PAxisVisible = True
    end
  end
  object ColorDialog: TColorDialog
    Left = 670
    Top = 490
  end
end
