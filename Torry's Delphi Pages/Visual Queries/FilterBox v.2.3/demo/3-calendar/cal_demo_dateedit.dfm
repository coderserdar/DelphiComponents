object cal_demo_frm_datetime: Tcal_demo_frm_datetime
  Left = 311
  Top = 112
  Width = 540
  Height = 545
  Caption = 'Date Edit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel_DateTime_Main: TPanel
    Left = 0
    Top = 0
    Width = 532
    Height = 511
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel_Buttons: TPanel
      Left = 0
      Top = 0
      Width = 532
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Bevel1: TBevel
        Left = 0
        Top = 30
        Width = 532
        Height = 2
        Align = alBottom
        Shape = bsBottomLine
      end
    end
    object Panel_Manager: TPanel
      Left = 333
      Top = 32
      Width = 199
      Height = 479
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Bevel2: TBevel
        Left = 0
        Top = 0
        Width = 2
        Height = 479
        Align = alLeft
        Shape = bsLeftLine
      end
      object Label_ThemeName: TLabel
        Left = 8
        Top = 12
        Width = 36
        Height = 13
        Caption = 'Theme:'
      end
      object Label_Font: TLabel
        Left = 8
        Top = 41
        Width = 24
        Height = 13
        Caption = 'Font:'
      end
      object SpeedButton_Bold: TSpeedButton
        Left = 92
        Top = 65
        Width = 23
        Height = 21
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'B'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButton_BoldClick
      end
      object SpeedButton_Italic: TSpeedButton
        Left = 124
        Top = 65
        Width = 23
        Height = 21
        AllowAllUp = True
        GroupIndex = 2
        Caption = 'I'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsItalic]
        ParentFont = False
        OnClick = SpeedButton_ItalicClick
      end
      object CheckBox_ButtonsVisible: TCheckBox
        Left = 8
        Top = 95
        Width = 180
        Height = 17
        Caption = 'Button Visible'
        TabOrder = 0
        OnClick = CheckBox_ButtonsVisibleClick
      end
      object CheckBox_Enabled: TCheckBox
        Left = 8
        Top = 119
        Width = 180
        Height = 17
        Caption = 'Enabled'
        TabOrder = 1
        OnClick = CheckBox_EnabledClick
      end
      object CheckBox_ShowDateEdit: TCheckBox
        Left = 8
        Top = 143
        Width = 180
        Height = 17
        Caption = 'Date Edit in Popup'
        TabOrder = 2
        OnClick = CheckBox_ShowDateEditClick
      end
      object CheckBox_MultiSelect: TCheckBox
        Left = 8
        Top = 167
        Width = 180
        Height = 17
        Caption = 'Allow Multi Select'
        TabOrder = 3
        OnClick = CheckBox_MultiSelectClick
      end
      object ComboBox_EditThemeNames: TComboBox
        Left = 48
        Top = 8
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = ComboBox_EditThemeNamesChange
      end
      object PSCFontBar_DateEdit: TPSCFontEdit
        Left = 46
        Top = 37
        Width = 147
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        FontName = 'MS Sans Serif'
        OnChange = PSCFontBar_DateEditChange
        DrawFonts = False
      end
      object ComboBox_FontSize: TComboBox
        Left = 46
        Top = 66
        Width = 41
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        OnChange = ComboBox_FontSizeChange
      end
    end
    object Panel7: TPanel
      Left = 0
      Top = 32
      Width = 333
      Height = 479
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 333
        Height = 319
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel_ControlsCenter: TPanel
          Left = 0
          Top = 160
          Width = 333
          Height = 159
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label_Date: TLabel
            Left = 54
            Top = 49
            Width = 56
            Height = 13
            Caption = 'Date Editor:'
          end
          object Bevel4: TBevel
            Left = 0
            Top = 157
            Width = 333
            Height = 2
            Align = alBottom
            Shape = bsBottomLine
          end
          object PSCDateEdit_Date: TPSCDateEdit
            Left = 54
            Top = 69
            Width = 225
            Height = 21
            Date = 37417.000000000000000000
            Kind = cpkDate
            SelStartEdit = PSCDateEdit_DateTime
            TabOrder = 0
          end
        end
        object Panel_ControlsTop: TPanel
          Left = 0
          Top = 0
          Width = 333
          Height = 160
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Label_DateTime: TLabel
            Left = 54
            Top = 49
            Width = 82
            Height = 13
            Caption = 'Date-Time Editor:'
          end
          object Bevel3: TBevel
            Left = 0
            Top = 158
            Width = 333
            Height = 2
            Align = alBottom
            Shape = bsBottomLine
          end
          object PSCDateEdit_DateTime: TPSCDateEdit
            Left = 54
            Top = 69
            Width = 225
            Height = 21
            Time = 0.733084953702928000
            Date = 37417.000000000000000000
            SelEndEdit = PSCDateEdit_Date
            TabOrder = 0
          end
        end
      end
      object Panel_ControlsBottom: TPanel
        Left = 0
        Top = 319
        Width = 333
        Height = 160
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
end
