object cal_demo_frm_pscdateupdown: Tcal_demo_frm_pscdateupdown
  Left = 316
  Top = 123
  Width = 540
  Height = 545
  Caption = 'Date Up/Down'
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
  object Panel_PSCDateUpDown_Main: TPanel
    Left = 0
    Top = 0
    Width = 532
    Height = 511
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel_Manager: TPanel
      Left = 333
      Top = 32
      Width = 199
      Height = 479
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 199
        Height = 479
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
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
          Left = 90
          Top = 66
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
          Left = 122
          Top = 66
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
          Visible = False
          OnClick = SpeedButton_ItalicClick
        end
        object Bevel3: TBevel
          Left = 8
          Top = 245
          Width = 177
          Height = 2
          Shape = bsBottomLine
        end
        object Label_Parts: TLabel
          Left = 8
          Top = 261
          Width = 22
          Height = 13
          Caption = 'Part:'
        end
        object Bevel5: TBevel
          Left = 0
          Top = 0
          Width = 2
          Height = 479
          Align = alLeft
          Shape = bsLeftLine
        end
        object Bevel1: TBevel
          Left = 8
          Top = 357
          Width = 177
          Height = 2
          Shape = bsBottomLine
        end
        object CheckBox_ButtonsVisible: TCheckBox
          Left = 8
          Top = 119
          Width = 185
          Height = 17
          Caption = 'Button Visible'
          TabOrder = 0
          OnClick = CheckBox_ButtonsVisibleClick
        end
        object ComboBox_EditThemeNames: TComboBox
          Left = 48
          Top = 8
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = ComboBox_EditThemeNamesChange
        end
        object CheckBox_Enabled: TCheckBox
          Left = 8
          Top = 95
          Width = 185
          Height = 17
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = CheckBox_EnabledClick
        end
        object PSCFontBar_Edit: TPSCFontEdit
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
          TabOrder = 3
          FontName = 'MS Sans Serif'
          OnChange = PSCFontBar_EditChange
          DrawFonts = False
        end
        object ComboBox_FontSize: TComboBox
          Left = 46
          Top = 66
          Width = 41
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
          OnChange = ComboBox_FontSizeChange
        end
        object CheckBox_FlatCheckBoxes: TCheckBox
          Left = 8
          Top = 167
          Width = 185
          Height = 17
          Caption = 'Flat Check Boxes'
          TabOrder = 5
          OnClick = CheckBox_FlatCheckBoxesClick
        end
        object CheckBox_ShowCheckBoxes: TCheckBox
          Left = 8
          Top = 143
          Width = 185
          Height = 17
          Caption = 'Show Check Boxes'
          TabOrder = 6
          OnClick = CheckBox_ShowCheckBoxesClick
        end
        object ComboBox_Parts: TComboBox
          Left = 56
          Top = 258
          Width = 129
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 7
          OnChange = ComboBox_PartsChange
        end
        object CheckBox_PartEnabled: TCheckBox
          Left = 9
          Top = 331
          Width = 72
          Height = 17
          Caption = 'Enabled'
          TabOrder = 8
          OnClick = CheckBox_PartEnabledClick
        end
        object CheckBox_PartChecked: TCheckBox
          Left = 9
          Top = 309
          Width = 72
          Height = 17
          Caption = 'Checked'
          TabOrder = 9
          OnClick = CheckBox_PartCheckedClick
        end
        object CheckBox_LongDateFormat: TCheckBox
          Left = 8
          Top = 191
          Width = 185
          Height = 17
          Caption = 'Use Long Date Format'
          TabOrder = 10
          OnClick = CheckBox_LongDateFormatClick
        end
        object CheckBox_LongTimeFormat: TCheckBox
          Left = 8
          Top = 215
          Width = 185
          Height = 17
          Caption = 'Use Long Time Format'
          TabOrder = 11
          OnClick = CheckBox_LongTimeFormatClick
        end
        object CheckBox_PartVisible: TCheckBox
          Left = 9
          Top = 287
          Width = 97
          Height = 17
          Caption = 'Check Visible'
          TabOrder = 12
          OnClick = CheckBox_PartVisibleClick
        end
      end
    end
    object Panel1: TPanel
      Left = 0
      Top = 32
      Width = 333
      Height = 479
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Panel_Controls: TPanel
        Left = 0
        Top = 0
        Width = 333
        Height = 319
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Panel_ControlsTop: TPanel
          Left = 0
          Top = 0
          Width = 333
          Height = 160
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Bevel7: TBevel
            Left = 0
            Top = 158
            Width = 333
            Height = 2
            Align = alBottom
            Shape = bsBottomLine
          end
          object Label_CurrentSelDateTime: TLabel
            Left = 2
            Top = 97
            Width = 3
            Height = 13
          end
          object Label_DateTime: TLabel
            Left = 38
            Top = 49
            Width = 82
            Height = 13
            Caption = 'Date-Time Editor:'
          end
          object PSCDateTimeUpDown_DateTime: TPSCDateTimeUpDown
            Left = 42
            Top = 69
            Width = 250
            Height = 21
            DateTime = 37566.700639976850000000
            TabOrder = 0
            OnChange = PSCDateTimeUpDown_DateTimeChange
          end
        end
        object Panel_ControlsCenter: TPanel
          Left = 0
          Top = 160
          Width = 333
          Height = 159
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object Bevel8: TBevel
            Left = 0
            Top = 157
            Width = 333
            Height = 2
            Align = alBottom
            Shape = bsBottomLine
          end
          object Label_CurrentSelDate: TLabel
            Left = 2
            Top = 97
            Width = 3
            Height = 13
          end
          object Label_Date: TLabel
            Left = 38
            Top = 49
            Width = 56
            Height = 13
            Caption = 'Date Editor:'
          end
          object PSCDateTimeUpDown_Date: TPSCDateTimeUpDown
            Left = 42
            Top = 69
            Width = 250
            Height = 21
            DateTime = 37566.700639976850000000
            Kind = cpkDate
            TabOrder = 0
            OnChange = PSCDateTimeUpDown_DateChange
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
        object Label_CurrentSelTime: TLabel
          Left = 2
          Top = 97
          Width = 3
          Height = 13
        end
        object Label1: TLabel
          Left = 38
          Top = 49
          Width = 56
          Height = 13
          Caption = 'Time Editor:'
        end
        object PSCDateTimeUpDown_Time: TPSCDateTimeUpDown
          Left = 42
          Top = 69
          Width = 250
          Height = 21
          DateTime = 37566.700640092590000000
          Kind = cpkTime
          FlatCheckBoxes = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = PSCDateTimeUpDown_TimeChange
        end
      end
    end
    object Panel_Buttons: TPanel
      Left = 0
      Top = 0
      Width = 532
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Bevel4: TBevel
        Left = 0
        Top = 30
        Width = 532
        Height = 2
        Align = alBottom
        Shape = bsBottomLine
      end
    end
  end
end
