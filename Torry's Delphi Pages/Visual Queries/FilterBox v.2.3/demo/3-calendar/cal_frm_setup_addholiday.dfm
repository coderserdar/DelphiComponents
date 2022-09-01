object psc_frm_addholiday: Tpsc_frm_addholiday
  Left = 277
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Add Holiday'
  ClientHeight = 152
  ClientWidth = 444
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
  object Label_Name: TLabel
    Left = 16
    Top = 17
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label1: TLabel
    Left = 292
    Top = 80
    Width = 9
    Height = 13
    Caption = 'of'
  end
  object Edit_Name: TEdit
    Left = 72
    Top = 14
    Width = 361
    Height = 21
    TabOrder = 0
    OnChange = Edit_NameChange
  end
  object Button_AddOK: TButton
    Left = 270
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object Button_AddCancel: TButton
    Left = 358
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ComboBox_Day: TComboBox
    Left = 72
    Top = 78
    Width = 100
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    Items.Strings = (
      'First'
      'Second'
      'Third'
      'Forth'
      'Last')
  end
  object RadioButton_The: TRadioButton
    Left = 16
    Top = 80
    Width = 49
    Height = 17
    Caption = 'The'
    TabOrder = 4
  end
  object RadioButton_Every: TRadioButton
    Left = 16
    Top = 48
    Width = 49
    Height = 17
    Caption = 'Every'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
  object Edit_Day: TEdit
    Left = 200
    Top = 45
    Width = 41
    Height = 21
    TabOrder = 6
  end
  object ComboBox_EveryMonth: TComboBox
    Left = 72
    Top = 45
    Width = 121
    Height = 21
    Style = csDropDownList
    TabOrder = 7
  end
  object ComboBox_WeekDay: TComboBox
    Left = 176
    Top = 78
    Width = 111
    Height = 21
    Style = csDropDownList
    TabOrder = 8
  end
  object ComboBox_Month: TComboBox
    Left = 312
    Top = 78
    Width = 121
    Height = 21
    Style = csDropDownList
    TabOrder = 9
  end
end
