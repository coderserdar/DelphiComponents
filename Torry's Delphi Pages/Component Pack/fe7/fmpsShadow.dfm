object FR_SHADOW: TFR_SHADOW
  Left = 0
  Top = 0
  Width = 236
  Height = 138
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 27
    Height = 13
    Caption = 'Angle'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 24
    Height = 13
    Caption = 'Color'
  end
  object Label3: TLabel
    Left = 8
    Top = 120
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object E_VISIBLE: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Shadow visible'
    TabOrder = 0
  end
  object E_ANGLE: TpsAngleSelector
    Left = 120
    Top = 32
    Width = 50
    Height = 50
    Color = clBtnFace
    BgBrush.Color = clGray
    BgBrush.Style = bsSolid
    BgBrush.BitmapIndex = -1
    BgBrush.OffsetX = 0
    BgBrush.OffsetY = 0
    BgBrush.Handle = 1443890771
    Angle = 0
    AngleStep = 15
    PointColor = clRed
    PointSize = 3
    PointOffset = 2
    LinesVisible = True
    LinesWidth = 5
    LinesColor = clGreen
    Style = TpsAngleStylePoint
  end
  object E_COLOR: TpsColorComboBox
    Left = 88
    Top = 88
    Width = 145
    Height = 22
    TabOrder = 2
    TypeOfBox = tyColor
    SelectedColor = clBlack
    SelectedBrushImage = -1
    SelectedShape = stRectangle
    SelectedPenStyle = psSolid
    SelectedCopyMode = 0
    SelectedPenMode = pmBlack
    UserColor = clBlack
    ItemStyle = scBoth
    SelectedDrive = 'U'
    SelectedImage = 0
    ItemHeight = 16
    ItemIndex = 0
  end
  object E_WIDTH: TEdit
    Left = 88
    Top = 112
    Width = 65
    Height = 21
    TabOrder = 3
  end
end
