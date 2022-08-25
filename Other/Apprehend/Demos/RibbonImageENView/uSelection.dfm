object FormSelection: TFormSelection
  Left = 164
  Top = 22
  BorderStyle = bsToolWindow
  Caption = 'Selection and Object Grip Properties'
  ClientHeight = 266
  ClientWidth = 387
  Color = clWindow
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnActivate = FormActivate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 261
    Height = 195
    Caption = 'Selection Grip'
    TabOrder = 0
    object Label2: TLabel
      Left = 24
      Top = 17
      Width = 20
      Height = 15
      Caption = 'Size'
    end
    object Label3: TLabel
      Left = 20
      Top = 66
      Width = 38
      Height = 15
      Caption = 'Color 1'
    end
    object Label4: TLabel
      Left = 139
      Top = 65
      Width = 38
      Height = 15
      Caption = 'Color 2'
    end
    object Label9: TLabel
      Left = 24
      Top = 112
      Width = 30
      Height = 15
      Caption = 'Brush'
    end
    object Edit1: TEdit
      Left = 22
      Top = 32
      Width = 45
      Height = 23
      NumbersOnly = True
      TabOrder = 0
      Text = '5'
      OnChange = Edit1Change
    end
    object UpDownSize: TUpDown
      Left = 67
      Top = 32
      Width = 16
      Height = 23
      Associate = Edit1
      Position = 5
      TabOrder = 1
    end
    object ColorBoxColor1: TColorBox
      Left = 23
      Top = 80
      Width = 110
      Height = 22
      NoneColorColor = clWhite
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = ColorBoxColor1Change
    end
    object ColorBoxColor2: TColorBox
      Left = 139
      Top = 80
      Width = 110
      Height = 22
      NoneColorColor = clNone
      Selected = 11075583
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      TabOrder = 3
      OnChange = ColorBoxColor2Change
    end
    object CheckBoxExtendedSelectionDrawing: TCheckBox
      Left = 3
      Top = 171
      Width = 179
      Height = 17
      Caption = 'Extended Selection Drawing'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBoxExtendedSelectionDrawingClick
    end
    object cbxBrushName1: TComboBox
      Left = 24
      Top = 127
      Width = 99
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'Solid'
      OnChange = cbxBrushName1Change
      Items.Strings = (
        'Solid'
        'Clear'
        'Horizontal'
        'Vertical'
        'BDiagonal'
        'FDiagonal'
        'Cross'
        'DiagCross')
    end
    object GripShape1: TRadioGroup
      Left = 168
      Top = 108
      Width = 81
      Height = 57
      Caption = 'Shape'
      ItemIndex = 1
      Items.Strings = (
        'Box'
        'Circle')
      TabOrder = 6
      OnClick = GripShape1Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 277
    Top = 4
    Width = 103
    Height = 219
    Caption = 'Selection Options'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 14
      Top = 18
      Width = 75
      Height = 17
      Hint = 'The selection will be animated'
      Caption = 'Animated'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 14
      Top = 45
      Width = 75
      Height = 17
      Hint = 'Can resize the selection (display grips)'
      Caption = 'Sizeable'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 14
      Top = 73
      Width = 75
      Height = 17
      Hint = 'Can move the selection (display grips)'
      Caption = 'Moveable'
      TabOrder = 2
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 14
      Top = 101
      Width = 75
      Height = 17
      Hint = 'The selection is filled with inverted image'
      Caption = 'Filled'
      TabOrder = 3
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 14
      Top = 128
      Width = 75
      Height = 17
      Hint = 
        'Can drag the selection out of borders (cut the selection borders' +
        ')'
      Caption = 'CutBorders'
      TabOrder = 4
      OnClick = CheckBox5Click
    end
    object CheckBox6: TCheckBox
      Left = 14
      Top = 156
      Width = 75
      Height = 17
      Hint = 'Makes the unselected area (the area out of selection) grayed'
      Caption = 'MarkOuter'
      TabOrder = 5
      OnClick = CheckBox6Click
    end
    object CheckBox7: TCheckBox
      Left = 14
      Top = 184
      Width = 75
      Height = 17
      Hint = 
        'Enables auto-scrolling of the image when selecting out of visibl' +
        'e area'
      Caption = 'CanScroll'
      TabOrder = 6
      OnClick = CheckBox7Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 230
    Width = 387
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    object CancelBtn: TButton
      Left = 89
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object OKBtn: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end
