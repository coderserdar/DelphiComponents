object psc_frm_setup_holidays: Tpsc_frm_setup_holidays
  Left = 277
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Setup holidays'
  ClientHeight = 392
  ClientWidth = 446
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
  object Panel_Holidays: TPanel
    Left = 0
    Top = 0
    Width = 446
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object CheckBox_DefaultHolidays: TCheckBox
      Left = 8
      Top = 32
      Width = 185
      Height = 17
      Caption = 'Show Default Country Holidays'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBox_DefaultHolidaysClick
    end
    object CheckBox_ShowHolidays: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Show Holidays'
      TabOrder = 0
      OnClick = CheckBox_ShowHolidaysClick
    end
    object CheckBox_ShowHints: TCheckBox
      Left = 208
      Top = 32
      Width = 188
      Height = 17
      Caption = 'Show Holiday as a Hint'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox_ShowHintsClick
    end
    object GroupBox_CountryHolidays: TGroupBox
      Left = 8
      Top = 56
      Width = 426
      Height = 177
      Caption = ' Country Holidays '
      TabOrder = 3
      object Label_SelectLocations: TLabel
        Left = 16
        Top = 18
        Width = 393
        Height = 13
        AutoSize = False
        Caption = 
          'Select the locations whose holidays you would like copied to you' +
          'r calendar'
        WordWrap = True
      end
      object CheckListBox_Countries: TCheckListBox
        Left = 16
        Top = 44
        Width = 297
        Height = 113
        OnClickCheck = CheckListBox_CountriesClickCheck
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
      end
      object Button_AddCountry: TButton
        Left = 331
        Top = 44
        Width = 75
        Height = 25
        Caption = 'Add'
        Enabled = False
        TabOrder = 1
        OnClick = Button_AddClick
      end
      object Button_DeleteCountry: TButton
        Left = 331
        Top = 84
        Width = 75
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 2
        OnClick = Button_DeleteClick
      end
      object Button_EditCountry: TButton
        Left = 331
        Top = 124
        Width = 75
        Height = 25
        Caption = 'Edit'
        Enabled = False
        TabOrder = 3
        OnClick = Button_EditClick
      end
    end
    object GroupBox_CustomHolidays: TGroupBox
      Left = 8
      Top = 246
      Width = 426
      Height = 131
      Caption = ' Custom Holidays '
      TabOrder = 4
      object Button_Add: TButton
        Left = 331
        Top = 22
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 0
        OnClick = Button_AddClick
      end
      object Button_Delete: TButton
        Left = 331
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 1
        OnClick = Button_DeleteClick
      end
      object Button_Edit: TButton
        Left = 331
        Top = 90
        Width = 75
        Height = 25
        Caption = 'Edit'
        TabOrder = 2
        OnClick = Button_EditClick
      end
      object ListBox_CustomHolidays: TListBox
        Left = 16
        Top = 22
        Width = 297
        Height = 93
        ItemHeight = 13
        TabOrder = 3
      end
    end
  end
end
