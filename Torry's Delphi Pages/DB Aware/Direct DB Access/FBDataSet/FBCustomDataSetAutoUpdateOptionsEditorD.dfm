object FBCustomDataSetAutoUpdateOptionsEditorForm: TFBCustomDataSetAutoUpdateOptionsEditorForm
  Left = 351
  Top = 368
  BorderStyle = bsDialog
  Caption = 'Auto update options'
  ClientHeight = 120
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    520
    120)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 63
    Height = 13
    Caption = 'Updated field'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 76
    Height = 13
    Caption = 'Generator name'
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 61
    Height = 13
    Caption = 'Increment by'
  end
  object ComboBox1: TComboBox
    Left = 96
    Top = 8
    Width = 225
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 96
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object ComboBox2: TComboBox
    Left = 96
    Top = 32
    Width = 225
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object Button1: TButton
    Left = 362
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 442
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object RadioGroup1: TRadioGroup
    Left = 328
    Top = 8
    Width = 185
    Height = 73
    Caption = 'Update action'
    Items.Strings = (
      'wgNever'
      'wgOnNewRecord'
      'wgBeforePost')
    TabOrder = 5
  end
  object JvUIBQuery1: TJvUIBQuery
    SQL.Strings = (
      'SELECT'
      '  RDB$GENERATOR_NAME'
      'FROM'
      '  RDB$GENERATORS'
      'WHERE'
      '    (RDB$SYSTEM_FLAG = 0)'
      '  OR'
      '    (RDB$SYSTEM_FLAG is NULL)'
      'order by'
      '  RDB$GENERATOR_NAME')
    Left = 8
    Top = 8
  end
end
