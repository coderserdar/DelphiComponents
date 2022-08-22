inherited AddIndexForm: TAddIndexForm
  BorderStyle = bsDialog
  Caption = 'Add Index'
  ClientHeight = 256
  ClientWidth = 350
  Font.Height = -12
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 333
    Height = 208
    TabOrder = 0
    object IndexNameLabel: TLabel
      Left = 12
      Top = 18
      Width = 65
      Height = 14
      Caption = 'Index name'
    end
    object IndexFieldsLabel: TLabel
      Left = 12
      Top = 62
      Width = 180
      Height = 14
      Caption = 'Index fields (use "," to separate)'
    end
    object FieldsLabel: TLabel
      Left = 12
      Top = 107
      Width = 29
      Height = 14
      Caption = 'Fields'
    end
    object IndexNameEdit: TEdit
      Left = 12
      Top = 35
      Width = 178
      Height = 22
      TabOrder = 0
    end
    object IndexFieldEdit: TEdit
      Left = 12
      Top = 79
      Width = 178
      Height = 22
      TabOrder = 1
    end
    object FieldsListBox: TListBox
      Left = 12
      Top = 124
      Width = 178
      Height = 70
      ItemHeight = 14
      TabOrder = 2
      OnMouseDown = FieldsListBoxMouseDown
    end
    object SortRadioGroup: TRadioGroup
      Left = 205
      Top = 132
      Width = 113
      Height = 62
      Caption = 'Sort mode'
      ItemIndex = 0
      Items.Strings = (
        'Asc'
        'Desc')
      TabOrder = 4
    end
    object GroupBox2: TGroupBox
      Left = 205
      Top = 29
      Width = 113
      Height = 92
      TabOrder = 3
      object PrimaryCheckBox: TCheckBox
        Left = 13
        Top = 17
        Width = 87
        Height = 17
        Caption = 'Primary'
        TabOrder = 0
      end
      object UniqueCheckBox: TCheckBox
        Left = 13
        Top = 41
        Width = 87
        Height = 17
        Caption = 'Unique'
        TabOrder = 1
      end
      object CaseCheckBox: TCheckBox
        Left = 13
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Case sensitive'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object OkButton: TButton
    Left = 175
    Top = 222
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CloseButton: TButton
    Left = 266
    Top = 222
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = CloseButtonClick
  end
end
