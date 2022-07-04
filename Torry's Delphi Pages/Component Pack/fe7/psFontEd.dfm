object psFontEditor: TpsFontEditor
  Left = 109
  Top = 30
  Width = 333
  Height = 430
  Caption = 'PSOFT font editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 36
    Height = 13
    Caption = 'Charset'
  end
  object Label4: TLabel
    Left = 8
    Top = 88
    Width = 24
    Height = 13
    Caption = 'Color'
  end
  object Label8: TLabel
    Left = 8
    Top = 64
    Width = 24
    Height = 13
    Caption = 'Pitch'
  end
  object Bevel2: TBevel
    Left = 8
    Top = 368
    Width = 313
    Height = 3
  end
  object SpeedButton1: TSpeedButton
    Left = 297
    Top = 80
    Width = 23
    Height = 22
    Flat = True
    OnClick = SpeedButton1Click
  end
  object FONT_NAME: TComboBox
    Left = 120
    Top = 8
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = FONT_NAMEChange
  end
  object FONT_CHARSET: TComboBox
    Left = 120
    Top = 32
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = FONT_NAMEChange
  end
  object FONT_PITCH: TComboBox
    Left = 120
    Top = 56
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = FONT_NAMEChange
    Items.Strings = (
      'DEFAULT_PITCH'
      'FIXED_PITCH'
      'VARIABLE_PITCH'
      ' ')
  end
  object FONT_COLOR: TComboBox
    Left = 120
    Top = 80
    Width = 177
    Height = 19
    Style = csOwnerDrawFixed
    ItemHeight = 13
    TabOrder = 3
    OnChange = FONT_NAMEChange
    OnDrawItem = FONT_COLORDrawItem
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 376
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = BitBtn1Click
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 168
    Top = 376
    Width = 75
    Height = 25
    TabOrder = 6
    Kind = bkCancel
  end
  object BitBtn4: TBitBtn
    Left = 248
    Top = 376
    Width = 75
    Height = 25
    TabOrder = 7
    Kind = bkHelp
  end
  object SAMPLE: TPanel
    Left = 8
    Top = 296
    Width = 313
    Height = 65
    BevelInner = bvLowered
    TabOrder = 8
    object SMP: TPaintBox
      Left = 2
      Top = 2
      Width = 309
      Height = 61
      Align = alClient
      OnPaint = SMPPaint
    end
  end
  object ZAL: TPageControl
    Left = 8
    Top = 107
    Width = 313
    Height = 185
    ActivePage = P1
    TabOrder = 9
    object P1: TTabSheet
      Caption = 'Page1'
      object Label2: TLabel
        Left = 8
        Top = 64
        Width = 92
        Height = 13
        Caption = 'Average char width'
      end
      object Label6: TLabel
        Left = 8
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Height'
      end
      object Label9: TLabel
        Left = 8
        Top = 40
        Width = 20
        Height = 13
        Caption = 'Size'
      end
      object Label10: TLabel
        Left = 8
        Top = 88
        Width = 79
        Height = 13
        Caption = 'Text extra space'
      end
      object FONT_HEIGHT: TEdit
        Left = 112
        Top = 8
        Width = 41
        Height = 21
        TabOrder = 0
        Text = 'FONT_HEIGHT'
        OnChange = FONT_NAMEChange
      end
      object FONT_SIZE: TEdit
        Left = 112
        Top = 32
        Width = 41
        Height = 21
        TabOrder = 1
        Text = 'FONT_SIZE'
        OnChange = FONT_NAMEChange
      end
      object FONT_WIDTH: TEdit
        Left = 112
        Top = 56
        Width = 41
        Height = 21
        TabOrder = 2
        Text = 'FONT_WIDTH'
        OnChange = FONT_NAMEChange
      end
      object FONT_SPACE: TEdit
        Left = 112
        Top = 80
        Width = 41
        Height = 21
        TabOrder = 3
        Text = 'FONT_SPACE'
        OnChange = FONT_NAMEChange
      end
      object Style: TGroupBox
        Left = 160
        Top = 8
        Width = 137
        Height = 145
        Caption = 'Style'
        TabOrder = 4
        object Label11: TLabel
          Left = 120
          Top = 107
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Bevel3: TBevel
          Left = 16
          Top = 89
          Width = 98
          Height = 3
        end
        object FONT_BOLD: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Bold'
          TabOrder = 0
          OnClick = FONT_NAMEChange
        end
        object FONT_ITALIC: TCheckBox
          Left = 16
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Italic'
          TabOrder = 1
          OnClick = FONT_NAMEChange
        end
        object FONT_UNDERLINE: TCheckBox
          Left = 16
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Underline'
          TabOrder = 2
          OnClick = FONT_NAMEChange
        end
        object FONT_STRIKEOUT: TCheckBox
          Left = 16
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Strikeout'
          TabOrder = 3
          OnClick = FONT_NAMEChange
        end
        object FONT_AUTOSIZE: TCheckBox
          Left = 16
          Top = 104
          Width = 97
          Height = 17
          Caption = 'Auto size'
          TabOrder = 4
          OnClick = FONT_NAMEChange
        end
        object FONT_AUTOWIDTH: TCheckBox
          Left = 16
          Top = 120
          Width = 97
          Height = 17
          Caption = 'Auto width'
          TabOrder = 5
          OnClick = FONT_NAMEChange
        end
        object FONT_AUTOSIZEPERC: TEdit
          Left = 96
          Top = 104
          Width = 25
          Height = 21
          TabOrder = 6
          Text = 'FONT_AUTOSIZEPERC'
          OnChange = FONT_NAMEChange
        end
      end
    end
    object P2: TTabSheet
      Caption = 'Page2'
      ImageIndex = 1
      object Label5: TLabel
        Left = 8
        Top = 16
        Width = 59
        Height = 13
        Caption = 'Escapement'
      end
      object Label7: TLabel
        Left = 8
        Top = 40
        Width = 51
        Height = 13
        Caption = 'Orientation'
      end
      object Label12: TLabel
        Left = 8
        Top = 64
        Width = 34
        Height = 13
        Caption = 'Weight'
      end
      object Label13: TLabel
        Left = 8
        Top = 88
        Width = 32
        Height = 13
        Caption = 'Quality'
      end
      object FONT_ESCAPEMENT: TEdit
        Left = 112
        Top = 8
        Width = 41
        Height = 21
        TabOrder = 0
        Text = 'FONT_ESCAPEMENT'
        OnChange = FONT_NAMEChange
      end
      object FONT_ORIENTATION: TEdit
        Left = 112
        Top = 32
        Width = 41
        Height = 21
        TabOrder = 1
        Text = 'Edit1'
        OnChange = FONT_NAMEChange
      end
      object FONT_WEIGHT: TComboBox
        Left = 112
        Top = 56
        Width = 177
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
        OnChange = FONT_NAMEChange
        Items.Strings = (
          'FW_DONTCARE'
          'FW_THIN'
          'FW_ULTRALIGHT'
          'FW_LIGHT'
          'FW_NORMAL'
          'FW_MEDIUM'
          'FW_SEMIBOLD'
          'FW_BOLD'
          'FW_ULTRABOLD'
          'FW_HEAVY'
          ' ')
      end
      object ComboBox2: TComboBox
        Left = 112
        Top = 80
        Width = 177
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 3
        OnChange = FONT_NAMEChange
      end
    end
  end
  object CD: TColorDialog
    Ctl3D = True
    Left = 288
    Top = 112
  end
end
