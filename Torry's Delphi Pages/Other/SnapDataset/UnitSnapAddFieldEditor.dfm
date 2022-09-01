object FormAddField: TFormAddField
  Left = 244
  Top = 223
  ActiveControl = edName
  BorderStyle = bsDialog
  Caption = 'Add Field ...'
  ClientHeight = 257
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LookupGroup: TGroupBox
    Left = 8
    Top = 136
    Width = 401
    Height = 81
    Caption = 'Lookup definition'
    TabOrder = 0
    object DatasetLabel: TLabel
      Left = 208
      Top = 19
      Width = 40
      Height = 13
      Caption = 'D&ataset:'
      Enabled = False
      FocusControl = cbDataSet
    end
    object KeyFieldsLabel: TLabel
      Left = 8
      Top = 19
      Width = 51
      Height = 13
      Caption = '&Key Fields:'
      Enabled = False
      FocusControl = cbKeyField
    end
    object LookupKeysLabel: TLabel
      Left = 8
      Top = 51
      Width = 65
      Height = 13
      Caption = 'Look&up Keys:'
      Enabled = False
      FocusControl = cbLookupField
    end
    object ResultFieldLabel: TLabel
      Left = 208
      Top = 51
      Width = 58
      Height = 13
      Caption = '&Result Field:'
      Enabled = False
      FocusControl = cbResultField
    end
    object cbDataSet: TComboBox
      Left = 272
      Top = 16
      Width = 121
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
      OnExit = cbDataSetExit
    end
    object cbKeyField: TComboBox
      Left = 80
      Top = 16
      Width = 121
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
    end
    object cbLookupField: TComboBox
      Left = 80
      Top = 48
      Width = 121
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
    end
    object cbResultField: TComboBox
      Left = 272
      Top = 48
      Width = 121
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
    end
  end
  object btnOk: TButton
    Left = 254
    Top = 226
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 334
    Top = 226
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object FieldGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 401
    Height = 81
    Caption = 'Field properties'
    TabOrder = 3
    object ComponentNameLabel: TLabel
      Left = 208
      Top = 19
      Width = 57
      Height = 13
      Caption = 'C&omponent:'
      FocusControl = edComponent
    end
    object FieldNameLabel: TLabel
      Left = 11
      Top = 19
      Width = 31
      Height = 13
      Caption = '&Name:'
      FocusControl = edName
    end
    object FieldTypeLabel: TLabel
      Left = 8
      Top = 51
      Width = 27
      Height = 13
      Caption = '&Type:'
      FocusControl = cbFieldType
    end
    object SizeEditLabel: TLabel
      Left = 208
      Top = 51
      Width = 23
      Height = 13
      Caption = '&Size:'
      FocusControl = edSize
    end
    object edComponent: TEdit
      Left = 272
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 1
      OnChange = edComponentChange
    end
    object edName: TEdit
      Left = 64
      Top = 16
      Width = 137
      Height = 21
      TabOrder = 0
      OnChange = edNameChange
    end
    object cbFieldType: TComboBox
      Left = 64
      Top = 48
      Width = 137
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      OnChange = cbFieldTypeChange
    end
    object edSize: TEdit
      Left = 272
      Top = 48
      Width = 57
      Height = 21
      MaxLength = 5
      TabOrder = 3
    end
  end
  object gbFieldtype: TRadioGroup
    Left = 8
    Top = 92
    Width = 401
    Height = 41
    Caption = 'Field type'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      '&Data'
      '&Calculated'
      '&Lookup')
    TabOrder = 4
    OnClick = gbFieldtypeClick
  end
end
