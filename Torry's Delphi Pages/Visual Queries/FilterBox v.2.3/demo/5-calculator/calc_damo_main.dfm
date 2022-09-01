object psc_frm_setup_main: Tpsc_frm_setup_main
  Left = 275
  Top = 114
  Caption = 'Calculator Demo'
  ClientHeight = 403
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl_Main: TPageControl
    Left = 0
    Top = 0
    Width = 362
    Height = 403
    ActivePage = TabSheet_Calculator
    Align = alClient
    TabOrder = 0
    OnChange = PageControl_MainChange
    object TabSheet_Calculator: TTabSheet
      Caption = 'Calculator'
      object PSCCalculator1: TPSCCalculator
        Left = 24
        Top = 24
        Width = 230
        Height = 184
        TabOrder = 0
      end
    end
    object TabSheet_CalcEdit: TTabSheet
      Caption = 'Calc Edit'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PSCCalcEdit1: TPSCCalcEdit
        Left = 8
        Top = 24
        Width = 121
        Height = 21
        NullValue = 888.000000000000000000
        UseNullValue = True
        TabOrder = 0
      end
    end
  end
  object Panel_Manager: TPanel
    Left = 362
    Top = 0
    Width = 199
    Height = 403
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Label_ButtonDist: TLabel
      Left = 8
      Top = 12
      Width = 55
      Height = 13
      Caption = 'Button Dist:'
    end
    object Label_ButtonLongDist: TLabel
      Left = 8
      Top = 41
      Width = 48
      Height = 13
      Caption = 'Long Dist:'
    end
    object Label_Precision: TLabel
      Left = 8
      Top = 70
      Width = 46
      Height = 13
      Caption = 'Precision:'
    end
    object Label_Colors: TLabel
      Left = 72
      Top = 157
      Width = 32
      Height = 13
      Caption = 'Colors:'
    end
    object Label_Format: TLabel
      Left = 8
      Top = 128
      Width = 35
      Height = 13
      Caption = 'Format:'
    end
    object Label_Digits: TLabel
      Left = 8
      Top = 99
      Width = 29
      Height = 13
      Caption = 'Digits:'
    end
    object PSCEdit_ButtonLongDist: TPSCEdit
      Left = 72
      Top = 37
      Width = 121
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      ReadOnly = True
      TabOrder = 1
      Text = ''
      OnButtonClick = PSCEdit_ButtonLongDistButtonClick
    end
    object PSCEdit_Precision: TPSCEdit
      Left = 72
      Top = 66
      Width = 121
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      ReadOnly = True
      TabOrder = 2
      Text = ''
      OnButtonClick = PSCEdit_PrecisionButtonClick
    end
    object PSCEdit_ButtonDist: TPSCEdit
      Left = 72
      Top = 8
      Width = 121
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      ReadOnly = True
      TabOrder = 0
      Text = ''
      OnButtonClick = PSCEdit_ButtonDistButtonClick
    end
    object CheckBox_Enabled: TCheckBox
      Left = 8
      Top = 214
      Width = 97
      Height = 17
      Caption = 'Enabled'
      TabOrder = 7
      OnClick = CheckBox_EnabledClick
    end
    object CheckBox_Flat: TCheckBox
      Left = 8
      Top = 238
      Width = 97
      Height = 17
      Caption = 'Flat'
      TabOrder = 8
      OnClick = CheckBox_FlatClick
    end
    object CheckBox_ShowDisplay: TCheckBox
      Left = 8
      Top = 262
      Width = 97
      Height = 17
      Caption = 'Show Display'
      TabOrder = 9
      OnClick = CheckBox_ShowDisplayClick
    end
    object PSCColorEdit1: TPSCColorEdit
      Left = 72
      Top = 186
      Width = 121
      Height = 21
      OnChange = PSCColorEdit1Change
      Options = [cboShowStdColors, cboShowWinColors, cboShowFontColors, cboShowMoreColors]
      TabOrder = 6
    end
    object ComboBox_Colors: TComboBox
      Left = 112
      Top = 153
      Width = 81
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox_ColorsChange
    end
    object ComboBox_Format: TComboBox
      Left = 72
      Top = 124
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnChange = ComboBox_FormatChange
    end
    object PSCEdit_Digits: TPSCEdit
      Left = 72
      Top = 95
      Width = 121
      Height = 21
      BtnKind = bkUpDown
      ButtonsVisible = True
      ReadOnly = True
      TabOrder = 3
      Text = ''
      OnButtonClick = PSCEdit_DigitsButtonClick
    end
    object Panel_CalcEdit: TPanel
      Left = 0
      Top = 291
      Width = 199
      Height = 112
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 10
      Visible = False
      object Label_NullValue: TLabel
        Left = 8
        Top = 37
        Width = 51
        Height = 13
        Caption = 'Null Value:'
      end
      object Label_ThemeName: TLabel
        Left = 8
        Top = 66
        Width = 36
        Height = 13
        Caption = 'Theme:'
      end
      object Bevel2: TBevel
        Left = 0
        Top = 0
        Width = 199
        Height = 2
        Align = alTop
      end
      object ComboBox_EditThemeNames: TComboBox
        Left = 72
        Top = 62
        Width = 121
        Height = 21
        Style = csDropDownList
        TabOrder = 2
        OnChange = ComboBox_EditThemeNamesChange
      end
      object PSCEdit_NullValue: TPSCEdit
        Left = 72
        Top = 33
        Width = 121
        Height = 21
        BtnKind = bkUpDown
        ButtonsVisible = True
        ReadOnly = True
        TabOrder = 1
        Text = ''
        OnButtonClick = PSCEdit_NullValueButtonClick
      end
      object CheckBox_UseNullValue: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Use Null Value'
        TabOrder = 0
        OnClick = CheckBox_UseNullValueClick
      end
    end
  end
end
