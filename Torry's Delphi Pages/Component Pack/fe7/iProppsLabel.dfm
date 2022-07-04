inherited psLabelFmt: TpsLabelFmt
  Left = 16
  Top = 17
  Caption = 'TpsLabel component editor'
  ClientHeight = 370
  ClientWidth = 435
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNL: TPanel
    Top = 339
    Width = 435
  end
  object ZAL: TPageControl
    Left = 0
    Top = 0
    Width = 435
    Height = 339
    ActivePage = SH_FONT
    Align = alClient
    TabOrder = 1
    object SH_MAIN: TTabSheet
      Caption = 'Main'
      object lblCaption: TLabel
        Left = 24
        Top = 16
        Width = 36
        Height = 13
        Caption = 'Caption'
      end
      object Label3: TLabel
        Left = 24
        Top = 168
        Width = 23
        Height = 13
        Caption = 'Align'
      end
      object Label4: TLabel
        Left = 24
        Top = 192
        Width = 94
        Height = 13
        Caption = 'Alignment horizontal'
      end
      object Label5: TLabel
        Left = 24
        Top = 216
        Width = 83
        Height = 13
        Caption = 'Alignment vertical'
      end
      object lbl: TLabel
        Left = 24
        Top = 248
        Width = 83
        Height = 13
        Caption = 'Auto caption type'
      end
      object Label2: TLabel
        Left = 24
        Top = 272
        Width = 92
        Height = 13
        Caption = 'Auto caption format'
      end
      object Label6: TLabel
        Left = 24
        Top = 120
        Width = 23
        Height = 13
        Caption = 'Style'
      end
      object Shape1: TShape
        Left = 24
        Top = 139
        Width = 401
        Height = 3
      end
      object E_CAPTION: TMemo
        Left = 160
        Top = 8
        Width = 265
        Height = 97
        TabOrder = 0
      end
      object CB_ALIGN: TComboBox
        Left = 160
        Top = 160
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'None'
          'Top'
          'Bottom'
          'Left'
          'Right'
          'Client')
      end
      object CB_ALIGNMENT_H: TComboBox
        Left = 160
        Top = 184
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          'Left'
          'Right'
          'Center')
      end
      object CB_ALIGNMENT_V: TComboBox
        Left = 160
        Top = 208
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        Items.Strings = (
          'Top'
          'Center'
          'Bottom')
      end
      object CB_AutoCaption: TComboBox
        Left = 160
        Top = 240
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'None'
          'DateTime'
          'PriceUp'
          'PriceDown'
          'CompanyName'
          'UserName'
          'ComputerName')
      end
      object CB_AutoCaptionFormat: TComboBox
        Left = 160
        Top = 264
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 5
        Text = 'dd.mm.yyyy'
        Items.Strings = (
          'dd.mm.yy'
          'dd.mm.yyyy'
          'dd.mmm.yy'
          'dd.mmm.yyyy'
          'mm.dd.yy'
          'mm.dd.yyyy'
          'mmm.dd.yy'
          'mmm.dd.yyyy'
          'dd/mm/yy'
          'dd/mm/yyyy'
          'dd/mmm/yy'
          'dd/mmm/yyyy'
          'mm/dd/yy'
          'mm/dd/yyyy'
          'mmm/dd/yy'
          'mmm/dd/yyyy'
          'dd-mm-yy'
          'dd-mm-yyyy'
          'dd-mmm-yy'
          'dd-mmm-yyyy'
          'mm-dd-yy'
          'mm-dd-yyyy'
          'mmm-dd-yy'
          'mmm-dd-yyyy'
          'd'
          'dd'
          'm'
          'mm'
          'mmm'
          '')
      end
      object CB_STYLE: TComboBox
        Left = 160
        Top = 112
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        Items.Strings = (
          'Brush filled'
          'Outlined'
          'Brush filled & Outlined')
      end
    end
    object SH_FONT: TTabSheet
      Caption = 'Font'
      object Label1: TLabel
        Left = 0
        Top = 56
        Width = 36
        Height = 13
        Caption = 'Charset'
      end
      object Label7: TLabel
        Left = 0
        Top = 8
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label8: TLabel
        Left = 0
        Top = 32
        Width = 24
        Height = 13
        Caption = 'Color'
      end
      object Label9: TLabel
        Left = 0
        Top = 208
        Width = 59
        Height = 13
        Caption = 'Escapement'
      end
      object Label10: TLabel
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
      object Label14: TLabel
        Left = 0
        Top = 168
        Width = 31
        Height = 13
        Caption = 'Height'
      end
      object Label15: TLabel
        Left = 0
        Top = 144
        Width = 92
        Height = 13
        Caption = 'Average char width'
      end
      object Label16: TLabel
        Left = 0
        Top = 256
        Width = 79
        Height = 13
        Caption = 'Text extra space'
      end
      object Label17: TLabel
        Left = 0
        Top = 104
        Width = 29
        Height = 13
        Caption = 'Family'
      end
      object Label18: TLabel
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
        Top = 160
        Width = 65
        Height = 21
        ItemHeight = 13
        TabOrder = 6
        Text = '12'
      end
      object CB_CharWidth: TComboBox
        Left = 112
        Top = 136
        Width = 65
        Height = 21
        ItemHeight = 13
        TabOrder = 5
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
    object SH_BRUSH: TTabSheet
      Caption = 'Brushes'
      object E_TRANSPARENT: TCheckBox
        Left = 16
        Top = 144
        Width = 97
        Height = 17
        Caption = 'Transparent'
        TabOrder = 0
      end
      object PNL_BRUSH: TGroupBox
        Left = 0
        Top = 0
        Width = 425
        Height = 137
        Caption = 'Label brush'
        TabOrder = 1
        object Label19: TLabel
          Left = 8
          Top = 24
          Width = 91
          Height = 13
          Caption = 'Predefined brushes'
        end
        object Label20: TLabel
          Left = 8
          Top = 48
          Width = 24
          Height = 13
          Caption = 'Color'
        end
        object Label21: TLabel
          Left = 8
          Top = 72
          Width = 23
          Height = 13
          Caption = 'Style'
        end
        object Label22: TLabel
          Left = 8
          Top = 96
          Width = 38
          Height = 13
          Caption = 'Offset X'
        end
        object Label23: TLabel
          Left = 8
          Top = 120
          Width = 38
          Height = 13
          Caption = 'Offset Y'
        end
        object CB_BrushColor: TpsColorComboBox
          Left = 152
          Top = 40
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
          Left = 120
          Top = 64
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
        object CB_BrushOffsetX: TEdit
          Left = 120
          Top = 88
          Width = 41
          Height = 21
          TabOrder = 2
        end
        object CB_BrushOffsetY: TEdit
          Left = 120
          Top = 112
          Width = 41
          Height = 21
          TabOrder = 3
        end
        object CB_BITMAPINDEX: TpsColorComboBox
          Left = 152
          Top = 16
          Width = 121
          Height = 22
          TabOrder = 4
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
          Top = 13
          Width = 29
          Height = 49
          Items.Strings = (
            ''
            '')
          TabOrder = 5
        end
      end
      object PNL_BG_BRUSH: TGroupBox
        Left = 2
        Top = 168
        Width = 425
        Height = 137
        Caption = 'Background brush'
        TabOrder = 2
        object Label24: TLabel
          Left = 8
          Top = 24
          Width = 91
          Height = 13
          Caption = 'Predefined brushes'
        end
        object Label25: TLabel
          Left = 8
          Top = 48
          Width = 24
          Height = 13
          Caption = 'Color'
        end
        object Label26: TLabel
          Left = 8
          Top = 72
          Width = 23
          Height = 13
          Caption = 'Style'
        end
        object Label27: TLabel
          Left = 8
          Top = 96
          Width = 38
          Height = 13
          Caption = 'Offset X'
        end
        object Label28: TLabel
          Left = 8
          Top = 120
          Width = 38
          Height = 13
          Caption = 'Offset Y'
        end
        object CB_BRUSHCOLOR_BG: TpsColorComboBox
          Left = 152
          Top = 40
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
        object CB_BRUSHSTYLE_BG: TpsColorComboBox
          Left = 120
          Top = 64
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
        object CB_BRUSHOFFSETX_BG: TEdit
          Left = 120
          Top = 88
          Width = 41
          Height = 21
          TabOrder = 2
        end
        object CB_BRUSHOFFSETY_BG: TEdit
          Left = 120
          Top = 112
          Width = 41
          Height = 21
          TabOrder = 3
        end
        object CB_BITMAPINDEX_BG: TpsColorComboBox
          Left = 152
          Top = 16
          Width = 121
          Height = 22
          TabOrder = 4
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
        object RG_BRUSH_COLOR_BG: TRadioGroup
          Left = 119
          Top = 13
          Width = 29
          Height = 49
          Items.Strings = (
            ''
            '')
          TabOrder = 5
        end
      end
    end
    object SH_PEN: TTabSheet
      Caption = 'Pen'
      object Label29: TLabel
        Left = 8
        Top = 72
        Width = 91
        Height = 13
        Caption = 'Predefined brushes'
      end
      object Label30: TLabel
        Left = 8
        Top = 96
        Width = 24
        Height = 13
        Caption = 'Color'
      end
      object Label31: TLabel
        Left = 8
        Top = 120
        Width = 51
        Height = 13
        Caption = 'Brush style'
      end
      object Label32: TLabel
        Left = 8
        Top = 16
        Width = 47
        Height = 13
        Caption = 'Pen width'
      end
      object lbl1: TLabel
        Left = 8
        Top = 40
        Width = 43
        Height = 13
        Caption = 'Pen style'
      end
      object Label33: TLabel
        Left = 8
        Top = 152
        Width = 39
        Height = 13
        Caption = 'Line join'
      end
      object Label34: TLabel
        Left = 8
        Top = 176
        Width = 40
        Height = 13
        Caption = 'End cap'
      end
      object Label35: TLabel
        Left = 8
        Top = 200
        Width = 48
        Height = 13
        Caption = 'Pen mode'
      end
      object PEN_BrushColor: TpsColorComboBox
        Left = 152
        Top = 88
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
      object PEN_BRUSHSTYLE: TpsColorComboBox
        Left = 120
        Top = 112
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
      object PEN_BitmapIndex: TpsColorComboBox
        Left = 152
        Top = 64
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
      object RG_PEN: TRadioGroup
        Left = 119
        Top = 61
        Width = 29
        Height = 49
        Items.Strings = (
          ''
          '')
        TabOrder = 3
      end
      object PEN_WIDTH: TEdit
        Left = 120
        Top = 8
        Width = 49
        Height = 21
        TabOrder = 4
      end
      object PEN_LINEJOIN: TComboBox
        Left = 120
        Top = 144
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
      object PEN_STYLE: TpsColorComboBox
        Left = 120
        Top = 32
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
      object PEN_ENDCAP: TComboBox
        Left = 120
        Top = 168
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
      object PEN_MODE: TpsColorComboBox
        Left = 120
        Top = 192
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
        Left = 8
        Top = 232
        Width = 97
        Height = 17
        Caption = 'User line style'
        TabOrder = 9
      end
      object E_USERSTYLE: TMemo
        Left = 120
        Top = 224
        Width = 153
        Height = 57
        TabOrder = 10
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Shadow'
      ImageIndex = 7
      object SHADOW_VISIBLE: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Shadow visible'
        TabOrder = 0
      end
      object PNL_SHADOW: TGroupBox
        Left = 8
        Top = 48
        Width = 265
        Height = 145
        Caption = 'Shadow propeties'
        TabOrder = 1
        object Label36: TLabel
          Left = 8
          Top = 32
          Width = 27
          Height = 13
          Caption = 'Angle'
        end
        object Label37: TLabel
          Left = 8
          Top = 96
          Width = 24
          Height = 13
          Caption = 'Color'
        end
        object Label38: TLabel
          Left = 8
          Top = 120
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object SHADOW_ANGLE: TpsAngleSelector
          Left = 184
          Top = 32
          Width = 50
          Height = 50
          Color = clBtnFace
          BgBrush.Color = clGray
          BgBrush.Style = bsSolid
          BgBrush.BitmapIndex = -1
          BgBrush.OffsetX = 0
          BgBrush.OffsetY = 0
          BgBrush.Handle = 1024459594
          Angle = 90
          AngleStep = 15
          PointColor = clRed
          PointSize = 3
          PointOffset = 2
          LinesVisible = True
          LinesWidth = 5
          LinesColor = clGreen
          Style = TpsAngleStylePoint
          OnAngleChange = SHADOW_ANGLEAngleChange
        end
        object SHADOW_COLOR: TpsColorComboBox
          Left = 88
          Top = 88
          Width = 145
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
        object SHADOW_WIDTH: TEdit
          Left = 88
          Top = 112
          Width = 65
          Height = 21
          TabOrder = 2
        end
        object SHADOW_ANGLE_E: TEdit
          Left = 88
          Top = 32
          Width = 49
          Height = 21
          TabOrder = 3
        end
      end
    end
    object SH_EFFECT: TTabSheet
      Caption = 'Effects'
      object Label11: TLabel
        Left = 216
        Top = 240
        Width = 41
        Height = 13
        Caption = 'Fill mode'
      end
      object E_FlipEffectH: TCheckBox
        Left = 216
        Top = 16
        Width = 200
        Height = 17
        Caption = 'Flip effect horizontal'
        TabOrder = 0
      end
      object E_FLIPEFFECTV: TCheckBox
        Left = 216
        Top = 32
        Width = 200
        Height = 17
        Caption = 'Flip effect vertical'
        TabOrder = 1
      end
      object E_FLIPHORIZONTAL: TCheckBox
        Left = 216
        Top = 48
        Width = 200
        Height = 17
        Caption = 'Flip text horizontal'
        TabOrder = 2
      end
      object E_FLIPVERTICAL: TCheckBox
        Left = 216
        Top = 64
        Width = 200
        Height = 17
        Caption = 'Flip text vertical'
        TabOrder = 3
      end
      object E_LabelEffect: TRadioGroup
        Left = 16
        Top = 8
        Width = 185
        Height = 273
        Caption = 'Label effect'
        Items.Strings = (
          'Without effect'
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9')
        TabOrder = 4
      end
      object E_PAR1: TpsSlider
        Left = 216
        Top = 104
        Width = 200
        Height = 20
        Color = clBtnFace
        Transparent = True
        LineHeight = 3
        Pen.Color = clBlack
        Pen.Style = psSolid
        Pen.Mode = pmBlack
        Pen.Width = 1
        Pen.StyleEx = psGeometric
        Pen.EndCap = psEndCapRound
        Pen.LineJoin = psJoinBevel
        Pen.UserStyleEnabled = False
        Pen.UserStyle.Strings = (
          '30'
          '5'
          '20'
          '5'
          '10'
          '5')
        Pen.Brush_Style = bsSolid
        Pen.Brush_BitmapIndex = -1
        Pen.Handle = -1404041623
        Brush.Color = clGray
        Brush.Style = bsSolid
        Brush.BitmapIndex = -1
        Brush.OffsetX = 0
        Brush.OffsetY = 0
        Brush.Handle = 923796132
        PointerBrush.Bitmap.Data = {
          96000000424D9600000000000000760000002800000008000000080000000100
          0400000000002000000000000000000000001000000010000000040204008482
          8400C4C2C400FCFEFC0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000211003322110
          0332211003322110033221100332211003322110033221100332}
        PointerBrush.Color = clTeal
        PointerBrush.Style = bsSolid
        PointerBrush.BitmapIndex = 9
        PointerBrush.OffsetX = 4
        PointerBrush.OffsetY = 0
        PointerBrush.Handle = 286262493
        Value = 50
        MinValue = 0
        MaxValue = 100
        PointerWidth = 10
        PointerHeight = 16
        PointerShape = psRoundRect
        Orientation = soLeftRight
        ChangeCursor = False
        HintOnChange = True
        HintWindowColor = clYellow
        HintWindowFont.Charset = DEFAULT_CHARSET
        HintWindowFont.Color = clWindowText
        HintWindowFont.Height = -11
        HintWindowFont.Name = 'MS Sans Serif'
        HintWindowFont.Style = []
        OnChange = E_PAR1Change
        Space = 10
        MarkStep = 10
        MarkHeight = 10
        MarkRightBottom = False
        MarkVisible = False
        MarkText = False
        MarkFont.Charset = DEFAULT_CHARSET
        MarkFont.Color = clWindowText
        MarkFont.Height = -9
        MarkFont.Name = 'Arial'
        MarkFont.Style = []
      end
      object E_PAR2: TpsSlider
        Left = 216
        Top = 136
        Width = 200
        Height = 20
        Color = clBtnFace
        Transparent = True
        LineHeight = 3
        Pen.Color = clBlack
        Pen.Style = psSolid
        Pen.Mode = pmBlack
        Pen.Width = 1
        Pen.StyleEx = psGeometric
        Pen.EndCap = psEndCapRound
        Pen.LineJoin = psJoinBevel
        Pen.UserStyleEnabled = False
        Pen.UserStyle.Strings = (
          '30'
          '5'
          '20'
          '5'
          '10'
          '5')
        Pen.Brush_Style = bsSolid
        Pen.Brush_BitmapIndex = -1
        Pen.Handle = 1464861518
        Brush.Color = clGray
        Brush.Style = bsSolid
        Brush.BitmapIndex = -1
        Brush.OffsetX = 0
        Brush.OffsetY = 0
        Brush.Handle = -972028199
        PointerBrush.Bitmap.Data = {
          96000000424D9600000000000000760000002800000008000000080000000100
          0400000000002000000000000000000000001000000010000000040204008482
          8400C4C2C400FCFEFC0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000211003322110
          0332211003322110033221100332211003322110033221100332}
        PointerBrush.Color = clTeal
        PointerBrush.Style = bsSolid
        PointerBrush.BitmapIndex = 9
        PointerBrush.OffsetX = 4
        PointerBrush.OffsetY = 0
        PointerBrush.Handle = -1223687549
        Value = 50
        MinValue = 0
        MaxValue = 100
        PointerWidth = 10
        PointerHeight = 16
        PointerShape = psRoundRect
        Orientation = soLeftRight
        ChangeCursor = False
        HintOnChange = True
        HintWindowColor = clYellow
        HintWindowFont.Charset = DEFAULT_CHARSET
        HintWindowFont.Color = clWindowText
        HintWindowFont.Height = -11
        HintWindowFont.Name = 'MS Sans Serif'
        HintWindowFont.Style = []
        OnChange = E_PAR1Change
        Space = 10
        MarkStep = 10
        MarkHeight = 10
        MarkRightBottom = False
        MarkVisible = False
        MarkText = False
        MarkFont.Charset = DEFAULT_CHARSET
        MarkFont.Color = clWindowText
        MarkFont.Height = -9
        MarkFont.Name = 'Arial'
        MarkFont.Style = []
      end
      object E_PAR3: TpsSlider
        Left = 216
        Top = 168
        Width = 200
        Height = 20
        Color = clBtnFace
        Transparent = True
        LineHeight = 3
        Pen.Color = clBlack
        Pen.Style = psSolid
        Pen.Mode = pmBlack
        Pen.Width = 1
        Pen.StyleEx = psGeometric
        Pen.EndCap = psEndCapRound
        Pen.LineJoin = psJoinBevel
        Pen.UserStyleEnabled = False
        Pen.UserStyle.Strings = (
          '30'
          '5'
          '20'
          '5'
          '10'
          '5')
        Pen.Brush_Style = bsSolid
        Pen.Brush_BitmapIndex = -1
        Pen.Handle = 240125651
        Brush.Color = clGray
        Brush.Style = bsSolid
        Brush.BitmapIndex = -1
        Brush.OffsetX = 0
        Brush.OffsetY = 0
        Brush.Handle = -1106246777
        PointerBrush.Bitmap.Data = {
          96000000424D9600000000000000760000002800000008000000080000000100
          0400000000002000000000000000000000001000000010000000040204008482
          8400C4C2C400FCFEFC0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000211003322110
          0332211003322110033221100332211003322110033221100332}
        PointerBrush.Color = clTeal
        PointerBrush.Style = bsSolid
        PointerBrush.BitmapIndex = 9
        PointerBrush.OffsetX = 4
        PointerBrush.OffsetY = 0
        PointerBrush.Handle = -1945106972
        Value = 50
        MinValue = 0
        MaxValue = 100
        PointerWidth = 10
        PointerHeight = 16
        PointerShape = psRoundRect
        Orientation = soLeftRight
        ChangeCursor = False
        HintOnChange = True
        HintWindowColor = clYellow
        HintWindowFont.Charset = DEFAULT_CHARSET
        HintWindowFont.Color = clWindowText
        HintWindowFont.Height = -11
        HintWindowFont.Name = 'MS Sans Serif'
        HintWindowFont.Style = []
        OnChange = E_PAR1Change
        Space = 10
        MarkStep = 10
        MarkHeight = 10
        MarkRightBottom = False
        MarkVisible = False
        MarkText = False
        MarkFont.Charset = DEFAULT_CHARSET
        MarkFont.Color = clWindowText
        MarkFont.Height = -9
        MarkFont.Name = 'Arial'
        MarkFont.Style = []
      end
      object E_FILLMODE: TComboBox
        Left = 216
        Top = 256
        Width = 193
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 8
        Items.Strings = (
          'Alternate'
          'Windiwng')
      end
    end
    object SH_ABOUT: TTabSheet
      Caption = 'About ...'
      object psLabel2: TpsLabel
        Left = 8
        Top = 8
        Width = 409
        Height = 249
        Pen.Color = clRed
        Pen.Style = psSolid
        Pen.Mode = pmBlack
        Pen.Width = 1
        Pen.StyleEx = psGeometric
        Pen.EndCap = psEndCapRound
        Pen.LineJoin = psJoinBevel
        Pen.UserStyleEnabled = False
        Pen.UserStyle.Strings = (
          '30'
          '5'
          '20'
          '5'
          '10'
          '5')
        Pen.Brush_Style = bsSolid
        Pen.Brush_BitmapIndex = -1
        Pen.Handle = 542114651
        Brush.Bitmap.Data = {
          96000000424D9600000000000000760000002800000008000000080000000100
          04000000000020000000C40E0000EB0E00001000000000000000040204008482
          8400C4C2C400FCFEFC0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000321032100321
          0321103210322103210332103210032103211032103221032103}
        Brush.Color = clYellow
        Brush.Style = bsSolid
        Brush.BitmapIndex = 14
        Brush.OffsetX = 0
        Brush.OffsetY = 0
        Brush.Handle = -653261948
        BgBrush.Bitmap.Data = {
          96000000424D9600000000000000760000002800000008000000080000000100
          04000000000020000000C40E0000EB0E00001000000000000000040204008482
          8400C4C2C400FCFEFC0000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000321032100321
          0321103210322103210332103210032103211032103221032103}
        BgBrush.Color = clBlue
        BgBrush.Style = bsSolid
        BgBrush.BitmapIndex = 14
        BgBrush.OffsetX = 0
        BgBrush.OffsetY = 0
        BgBrush.Handle = 1963984346
        BgGradient.Enabled = False
        BgGradient.ColorStart = clWhite
        BgGradient.ColorEnd = clTeal
        BgGradient.Style = gsLine
        BgGradient.Angle = 0
        BgGradient.NumColors = 256
        Gradient.Enabled = False
        Gradient.ColorStart = clWhite
        Gradient.ColorEnd = clTeal
        Gradient.Style = gsLine
        Gradient.Angle = 0
        Gradient.NumColors = 256
        BgBitmapMode = bmStretch
        psFont.CharSet = DEFAULT_CHARSET
        psFont.Name = 'GeoSlab703 MdCn BT'
        psFont.Size = -113
        psFont.Color = clMaroon
        psFont.Height = 150
        psFont.Pitch = fpDefault
        psFont.Style = [fsBold, fsStrikeOut]
        psFont.Escapement = 0
        psFont.Orientation = 0
        psFont.AverageCharWidth = 60
        psFont.TextExtraSpace = 10
        psFont.AutoWidth = True
        psFont.AutoHeight = True
        psFont.AutoWidthMultiLine = True
        psFont.AutoHeightPerc = 20
        psFont.Family = ffDECORATIVE
        psFont.Weight = fwDONTCARE
        psFont.LineSpacing = 0
        Alignment = taCenter
        Transparent = True
        AlignmentVertical = tlCenter
        Angle = 0
        Shadow.Width = 10
        Shadow.Color = clAqua
        Shadow.Angle = 0
        Shadow.Enabled = True
        Style = loBrushFilled
        Par1 = 86
        Par2 = 52
        Par3 = 50
        LabelEffect = cs9
        Caption = 'PSOFT'
        SplineDetails = 10
        FlipHorizontal = False
        FlipVertical = False
        FlipEffectH = True
        FlipEffectV = False
        AutoCaption = acNone
        FillMode = fmAlternate
      end
      object Label12: TLabel
        Left = 32
        Top = 264
        Width = 95
        Height = 13
        Caption = 'http://www.psoft.sk'
      end
      object Label13: TLabel
        Left = 32
        Top = 280
        Width = 85
        Height = 13
        Caption = 'formedit@psoft.sk'
      end
    end
  end
end
