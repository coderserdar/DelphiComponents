object Form1: TForm1
  Left = 31
  Top = 108
  Width = 741
  Height = 562
  Caption = 'Chuong trinh minh hoa su dung doi tuong Graph3D'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 508
    Height = 535
    Align = alClient
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 508
    Top = 0
    Width = 225
    Height = 535
    Align = alRight
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 223
      Height = 321
      Align = alTop
      Caption = 'Items'
      TabOrder = 0
      object Label1: TLabel
        Left = 10
        Top = 190
        Width = 57
        Height = 13
        Caption = 'Shape Style'
      end
      object Label4: TLabel
        Left = 10
        Top = 220
        Width = 64
        Height = 13
        Caption = 'Shape Color1'
      end
      object butAdd: TSpeedButton
        Left = 10
        Top = 280
        Width = 91
        Height = 25
        Caption = 'Add'
        Flat = True
        OnClick = butAddClick
      end
      object butRemove: TSpeedButton
        Left = 110
        Top = 280
        Width = 101
        Height = 25
        Caption = 'Remove'
        Flat = True
        OnClick = butRemoveClick
      end
      object Label6: TLabel
        Left = 10
        Top = 250
        Width = 64
        Height = 13
        Caption = 'Shape Color2'
      end
      object ListShape: TListBox
        Left = 2
        Top = 15
        Width = 219
        Height = 156
        Align = alTop
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
      end
      object ComboStyle: TComboBox
        Left = 90
        Top = 189
        Width = 101
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 7
        TabOrder = 0
        Text = 'ZYX'
        Items.Strings = (
          'Cone'
          'Cube'
          'Cylinder'
          'Line'
          'Plane'
          'Sphere'
          'Triangle'
          'ZYX')
      end
      object butShapeColor1: TPanel
        Left = 90
        Top = 220
        Width = 65
        Height = 21
        Color = clYellow
        TabOrder = 2
        OnClick = butShapeColor1Click
      end
      object butSHapeColor2: TPanel
        Left = 90
        Top = 250
        Width = 65
        Height = 21
        Color = clPurple
        TabOrder = 3
        OnClick = butSHapeColor2Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 1
      Top = 322
      Width = 223
      Height = 212
      Align = alClient
      Caption = 'View'
      TabOrder = 1
      object Label2: TLabel
        Left = 20
        Top = 110
        Width = 55
        Height = 13
        Caption = 'Axis Color 1'
      end
      object Label3: TLabel
        Left = 20
        Top = 170
        Width = 85
        Height = 13
        Caption = 'Background Color'
      end
      object Label5: TLabel
        Left = 20
        Top = 140
        Width = 55
        Height = 13
        Caption = 'Axis Color 2'
      end
      object butZoomIn: TSpeedButton
        Left = 10
        Top = 27
        Width = 91
        Height = 25
        Caption = 'Zoom In'
        Flat = True
        OnClick = butZoomInClick
      end
      object butZoomOut: TSpeedButton
        Left = 110
        Top = 27
        Width = 101
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
      object chkWireframe: TCheckBox
        Left = 120
        Top = 70
        Width = 97
        Height = 17
        Caption = 'Wireframe'
        TabOrder = 1
        OnClick = chkWireframeClick
      end
      object butAxisColor1: TPanel
        Left = 110
        Top = 112
        Width = 65
        Height = 21
        TabOrder = 2
        OnClick = butAxisColor1Click
      end
      object butBackColor: TPanel
        Left = 110
        Top = 172
        Width = 65
        Height = 21
        TabOrder = 3
        OnClick = butBackColorClick
      end
      object butAxisColor2: TPanel
        Left = 110
        Top = 142
        Width = 65
        Height = 21
        TabOrder = 4
        OnClick = butAxisColor2Click
      end
    end
  end
  object G: TGraph3D
    PAxisLength = 5
    PColorAxis1 = clWhite
    PColorAxis2 = 16503172
    PColorBackground = clBlack
    PAxis = True
    PWireframe = False
    Panel = Panel1
    Left = 410
    Top = 430
  end
  object Timer: TTimer
    Interval = 10
    OnTimer = TimerTimer
    Left = 360
    Top = 430
  end
  object ColorDialog: TColorDialog
    Left = 310
    Top = 430
  end
end
