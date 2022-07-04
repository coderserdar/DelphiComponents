object FRAME_PSBRUSH: TFRAME_PSBRUSH
  Left = 0
  Top = 0
  Width = 275
  Height = 125
  TabOrder = 0
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 91
    Height = 13
    Caption = 'Predefined brushes'
  end
  object Label4: TLabel
    Left = 8
    Top = 32
    Width = 24
    Height = 13
    Caption = 'Color'
  end
  object Label5: TLabel
    Left = 8
    Top = 56
    Width = 23
    Height = 13
    Caption = 'Style'
  end
  object Label6: TLabel
    Left = 8
    Top = 80
    Width = 38
    Height = 13
    Caption = 'Offset X'
  end
  object Label7: TLabel
    Left = 8
    Top = 104
    Width = 38
    Height = 13
    Caption = 'Offset Y'
  end
  object CB_BrushColor: TpsColorComboBox
    Left = 152
    Top = 24
    Width = 121
    Height = 22
    TabOrder = 1
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
  object CB_BrushStyle: TpsColorComboBox
    Left = 120
    Top = 48
    Width = 153
    Height = 22
    TabOrder = 2
    TypeOfBox = tyBrush
    SelectedColor = clBlack
    SelectedBrushImage = -1
    SelectedShape = stRectangle
    SelectedPenStyle = psSolid
    SelectedCopyMode = 0
    SelectedPenMode = pmBlack
    UserColor = clBlack
    ItemStyle = scBoth
    SelectedDrive = 'S'
    SelectedImage = 0
    ItemHeight = 16
    ItemIndex = 0
  end
  object CB_BrushOffsetX: TEdit
    Left = 120
    Top = 72
    Width = 41
    Height = 21
    TabOrder = 3
  end
  object CB_BrushOffsetY: TEdit
    Left = 120
    Top = 96
    Width = 41
    Height = 21
    TabOrder = 4
  end
  object CB_BITMAPINDEX: TpsColorComboBox
    Left = 152
    Top = 0
    Width = 121
    Height = 22
    TabOrder = 0
    TypeOfBox = tyUserBrush
    SelectedColor = clBlack
    SelectedBrushImage = -1
    SelectedShape = stRectangle
    SelectedPenStyle = psSolid
    SelectedCopyMode = 66
    SelectedPenMode = pmBlack
    UserColor = clBlack
    ItemStyle = scGlyph
    SelectedDrive = ' '
    SelectedImage = -1
    ItemHeight = 16
    ItemIndex = -1
  end
  object RG_BRUSH_COLOR: TRadioGroup
    Left = 119
    Top = -3
    Width = 29
    Height = 49
    Items.Strings = (
      ''
      '')
    TabOrder = 5
  end
end
