object psc_frm_setup_colors: Tpsc_frm_setup_colors
  Left = 277
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Setup colors'
  ClientHeight = 396
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_Colors: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 396
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label_Item: TLabel
      Left = 16
      Top = 8
      Width = 23
      Height = 13
      Caption = 'Item:'
    end
    object Label_Font: TLabel
      Left = 16
      Top = 62
      Width = 24
      Height = 13
      Caption = 'Font:'
    end
    object PSCColorButton_Color: TPSCColorButton
      Left = 192
      Top = 26
      Width = 36
      Height = 21
      ButtonStyle = bstBrush
      Flat = False
      HighlightColor = clBlack
      OnColorSelected = PSCColorButton_ColorColorSelected
    end
    object PSCColorButton_Font: TPSCColorButton
      Left = 248
      Top = 80
      Width = 36
      Height = 21
      Enabled = False
      Flat = False
      HighlightColor = clBlack
      OnColorSelected = PSCColorButton_FontColorSelected
    end
    object SpeedButton_Bold: TSpeedButton
      Left = 304
      Top = 80
      Width = 23
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'B'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton_BoldClick
    end
    object SpeedButton_Italic: TSpeedButton
      Left = 344
      Top = 80
      Width = 23
      Height = 21
      AllowAllUp = True
      GroupIndex = 2
      Caption = 'I'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsItalic]
      ParentFont = False
      OnClick = SpeedButton_ItalicClick
    end
    object Label_Size: TLabel
      Left = 192
      Top = 62
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object Label_FontColor: TLabel
      Left = 248
      Top = 62
      Width = 27
      Height = 13
      Caption = 'Color:'
    end
    object Label_Color: TLabel
      Left = 192
      Top = 8
      Width = 27
      Height = 13
      Caption = 'Color:'
    end
    object ComboBox_PSCCalendarColors: TComboBox
      Left = 16
      Top = 26
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox_PSCCalendarColorsChange
    end
    object PSCFontBar1: TPSCFontEdit
      Left = 16
      Top = 80
      Width = 161
      Height = 21
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      FontName = 'MS Sans Serif'
      OnChange = PSCFontBar1Change
    end
    object ComboBox_FontSize: TComboBox
      Left = 192
      Top = 80
      Width = 41
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
      OnChange = ComboBox_FontSizeChange
    end
  end
  object ColorDialog1: TColorDialog
    Left = 416
    Top = 16
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 384
    Top = 16
  end
end
