object FRAME_PEN: TFRAME_PEN
  Left = 0
  Top = 0
  Width = 270
  Height = 325
  TabOrder = 0
  object Label3: TLabel
    Left = 0
    Top = 64
    Width = 91
    Height = 13
    Caption = 'Predefined brushes'
  end
  object Label4: TLabel
    Left = 0
    Top = 88
    Width = 24
    Height = 13
    Caption = 'Color'
  end
  object Label5: TLabel
    Left = 0
    Top = 112
    Width = 51
    Height = 13
    Caption = 'Brush style'
  end
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 47
    Height = 13
    Caption = 'Pen width'
  end
  object lbl1: TLabel
    Left = 0
    Top = 24
    Width = 43
    Height = 13
    Caption = 'Pen style'
  end
  object Label2: TLabel
    Left = 0
    Top = 144
    Width = 39
    Height = 13
    Caption = 'Line join'
  end
  object Label6: TLabel
    Left = 0
    Top = 168
    Width = 40
    Height = 13
    Caption = 'End cap'
  end
  object Label7: TLabel
    Left = 0
    Top = 192
    Width = 48
    Height = 13
    Caption = 'Pen mode'
  end
  object CB_BrushColor: TpsColorComboBox
    Left = 144
    Top = 80
    Width = 121
    Height = 22
    TabOrder = 0
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
    Left = 112
    Top = 104
    Width = 153
    Height = 22
    TabOrder = 1
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
  object CB_BITMAPINDEX: TpsColorComboBox
    Left = 144
    Top = 56
    Width = 121
    Height = 22
    TabOrder = 2
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
    Left = 111
    Top = 53
    Width = 29
    Height = 49
    Items.Strings = (
      ''
      '')
    TabOrder = 3
  end
  object E_WIDTH: TEdit
    Left = 112
    Top = 0
    Width = 49
    Height = 21
    TabOrder = 4
  end
  object E_LINEJOIN: TComboBox
    Left = 112
    Top = 136
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      'Join Bevel'
      'Join Miter'
      'Join Round')
  end
  object E_STYLE: TpsColorComboBox
    Left = 112
    Top = 24
    Width = 153
    Height = 22
    TabOrder = 6
    TypeOfBox = tyPenStyle
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
  object E_ENDCAP: TComboBox
    Left = 112
    Top = 160
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      'Round'
      'Square'
      'Flat')
  end
  object E_PENMODE: TpsColorComboBox
    Left = 112
    Top = 184
    Width = 153
    Height = 22
    TabOrder = 8
    TypeOfBox = tyPenMode
    SelectedColor = clBlack
    SelectedBrushImage = -1
    SelectedShape = stRectangle
    SelectedPenStyle = psSolid
    SelectedCopyMode = 0
    SelectedPenMode = pmBlack
    UserColor = clBlack
    ItemStyle = scBoth
    SelectedDrive = 'p'
    SelectedImage = 0
    ItemHeight = 16
    ItemIndex = 0
  end
  object E_USEUSERSTYLE: TCheckBox
    Left = 0
    Top = 224
    Width = 97
    Height = 17
    Caption = 'User line style'
    TabOrder = 9
  end
  object E_USERSTYLE: TMemo
    Left = 112
    Top = 224
    Width = 153
    Height = 89
    TabOrder = 10
  end
end
