inherited AddFieldForm: TAddFieldForm
  BorderStyle = bsDialog
  Caption = 'Add Field'
  ClientHeight = 228
  ClientWidth = 280
  Font.Height = -12
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 264
    Height = 179
    TabOrder = 0
    object FieldNameLabel: TLabel
      Left = 12
      Top = 24
      Width = 58
      Height = 14
      Caption = 'Field name'
    end
    object FieldTypeLabel: TLabel
      Left = 12
      Top = 50
      Width = 53
      Height = 14
      Caption = 'Field type'
    end
    object FieldSizeLabel: TLabel
      Left = 12
      Top = 75
      Width = 47
      Height = 14
      Caption = 'Field size'
    end
    object DataProcessLabel: TLabel
      Left = 12
      Top = 150
      Width = 86
      Height = 14
      Caption = 'Data processing'
    end
    object Bevel1: TBevel
      Left = 12
      Top = 136
      Width = 239
      Height = 7
      Shape = bsTopLine
    end
    object FieldNameEdit: TEdit
      Left = 76
      Top = 19
      Width = 175
      Height = 22
      TabOrder = 0
    end
    object DataSizeEdit: TEdit
      Left = 76
      Top = 71
      Width = 175
      Height = 22
      TabOrder = 2
    end
    object FieldTypeComboBox: TComboBox
      Left = 76
      Top = 45
      Width = 175
      Height = 22
      Style = csDropDownList
      DropDownCount = 17
      ItemHeight = 14
      TabOrder = 1
      OnClick = FieldTypeComboBoxClick
    end
    object Panel1: TPanel
      Left = 13
      Top = 97
      Width = 238
      Height = 31
      BevelOuter = bvLowered
      BorderWidth = 3
      Color = 14474460
      TabOrder = 4
      object CommentsLabel: TLabel
        Left = 4
        Top = 4
        Width = 230
        Height = 23
        Align = alClient
      end
    end
    object DPModeComboBox: TComboBox
      Left = 104
      Top = 145
      Width = 147
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 3
      OnClick = FieldTypeComboBoxClick
      Items.Strings = (
        'fdDefault'
        'fdOriginal')
    end
  end
  object OkButton: TButton
    Left = 104
    Top = 193
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CloseButton: TButton
    Left = 196
    Top = 193
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = CloseButtonClick
  end
end
