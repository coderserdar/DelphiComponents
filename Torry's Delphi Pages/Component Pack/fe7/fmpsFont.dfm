object FRPSFONT: TFRPSFONT
  Left = 0
  Top = 0
  Width = 359
  Height = 304
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 56
    Width = 36
    Height = 13
    Caption = 'Charset'
  end
  object Label2: TLabel
    Left = 0
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label3: TLabel
    Left = 0
    Top = 32
    Width = 24
    Height = 13
    Caption = 'Color'
  end
  object Label4: TLabel
    Left = 0
    Top = 208
    Width = 59
    Height = 13
    Caption = 'Escapement'
  end
  object Label5: TLabel
    Left = 0
    Top = 232
    Width = 51
    Height = 13
    Caption = 'Orientation'
  end
  object Pitch: TLabel
    Left = 0
    Top = 80
    Width = 24
    Height = 13
    Caption = 'Pitch'
  end
  object Label7: TLabel
    Left = 0
    Top = 144
    Width = 31
    Height = 13
    Caption = 'Height'
  end
  object Label6: TLabel
    Left = 0
    Top = 168
    Width = 92
    Height = 13
    Caption = 'Average char width'
  end
  object Label8: TLabel
    Left = 0
    Top = 256
    Width = 79
    Height = 13
    Caption = 'Text extra space'
  end
  object Label9: TLabel
    Left = 0
    Top = 104
    Width = 29
    Height = 13
    Caption = 'Family'
  end
  object Label10: TLabel
    Left = 0
    Top = 280
    Width = 60
    Height = 13
    Caption = 'Line spacing'
  end
  object FONT_NAME: TpsColorComboBox
    Left = 112
    Top = 0
    Width = 242
    Height = 22
    TabOrder = 0
    TypeOfBox = tyFontSamples
    SelectedColor = clBlack
    SelectedBrushImage = -1
    SelectedShape = stRectangle
    SelectedPenStyle = psSolid
    SelectedCopyMode = 0
    SelectedPenMode = pmBlack
    UserColor = clBlack
    ItemStyle = scBoth
    SelectedDrive = 'A'
    SelectedImage = 0
    ItemHeight = 16
    ItemIndex = 0
  end
  object FONT_COLOR: TpsColorComboBox
    Left = 112
    Top = 24
    Width = 242
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
  object CB_CHARSET: TComboBox
    Left = 112
    Top = 48
    Width = 242
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object CB_PITCH: TComboBox
    Left = 112
    Top = 72
    Width = 242
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Default'
      'Variable'
      'Fixed')
  end
  object CB_HEIGHT: TComboBox
    Left = 112
    Top = 136
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 5
    Text = '12'
  end
  object CB_CharWidth: TComboBox
    Left = 112
    Top = 160
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 6
    Text = '20'
  end
  object E_ESCAPEMENT: TEdit
    Left = 112
    Top = 200
    Width = 65
    Height = 21
    TabOrder = 7
    Text = '0'
  end
  object E_ORIENTATION: TEdit
    Left = 112
    Top = 224
    Width = 65
    Height = 21
    TabOrder = 8
    Text = '0'
  end
  object GroupBox1: TGroupBox
    Left = 184
    Top = 120
    Width = 169
    Height = 177
    Caption = 'Style'
    TabOrder = 11
    object CB_BOLD: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Bold'
      TabOrder = 0
    end
    object CB_ITALIC: TCheckBox
      Left = 16
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Italic'
      TabOrder = 1
    end
    object CB_UNDERLINE: TCheckBox
      Left = 16
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Underline'
      TabOrder = 2
    end
    object CB_STRIKEOUT: TCheckBox
      Left = 16
      Top = 72
      Width = 97
      Height = 17
      Caption = 'StrikeOut'
      TabOrder = 3
    end
    object CB_AUTOWIDTH: TCheckBox
      Left = 16
      Top = 120
      Width = 97
      Height = 17
      Caption = 'Auto width'
      TabOrder = 4
    end
    object CB_AUTOWIDTHALL: TCheckBox
      Left = 16
      Top = 136
      Width = 147
      Height = 17
      Caption = 'Auto width for all lines'
      TabOrder = 5
    end
    object CB_AUTOHEIGHT: TCheckBox
      Left = 16
      Top = 152
      Width = 97
      Height = 17
      Caption = 'Auto height'
      TabOrder = 6
    end
  end
  object E_EXTRASPACE: TEdit
    Left = 112
    Top = 248
    Width = 65
    Height = 21
    TabOrder = 9
    Text = '0'
  end
  object CB_FAMILY: TComboBox
    Left = 112
    Top = 96
    Width = 242
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Dontcare'
      'Roman'
      'Swiss'
      'Modern'
      'Script'
      'Decorative')
  end
  object E_LINESPACING: TEdit
    Left = 112
    Top = 272
    Width = 65
    Height = 21
    TabOrder = 10
    Text = '0'
  end
end
