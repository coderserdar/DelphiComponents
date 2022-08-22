object FilterForm: TFilterForm
  Left = 192
  Top = 174
  BorderStyle = bsDialog
  Caption = 'Filter'
  ClientHeight = 179
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object ConGroupBox: TGroupBox
    Left = 7
    Top = 6
    Width = 359
    Height = 132
    Caption = 'Filter'
    TabOrder = 0
    object FieldComboBox: TComboBox
      Left = 11
      Top = 19
      Width = 103
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 3
    end
    object ValueEdit: TEdit
      Left = 183
      Top = 19
      Width = 93
      Height = 22
      TabOrder = 0
    end
    object AddButton: TButton
      Left = 286
      Top = 19
      Width = 61
      Height = 22
      Caption = 'Add'
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object OprComboBox: TComboBox
      Left = 122
      Top = 19
      Width = 54
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 4
      Items.Strings = (
        '='
        '>'
        '<'
        '>='
        '<='
        '<>'
        'LIKE')
    end
    object ConMemo: TMemo
      Left = 11
      Top = 48
      Width = 206
      Height = 71
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object GroupBox2: TGroupBox
      Left = 226
      Top = 43
      Width = 122
      Height = 76
      TabOrder = 5
      object CaseInsCheckBox: TCheckBox
        Left = 10
        Top = 19
        Width = 108
        Height = 17
        Caption = 'Case insensitive'
        TabOrder = 0
      end
      object NoPartCheckBox: TCheckBox
        Left = 10
        Top = 47
        Width = 107
        Height = 17
        Caption = 'No partial compare'
        TabOrder = 1
      end
    end
  end
  object OkButton: TButton
    Left = 208
    Top = 146
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 291
    Top = 146
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object CancelFilterButton: TButton
    Left = 8
    Top = 146
    Width = 101
    Height = 25
    Caption = 'Cancel Filter'
    TabOrder = 3
    OnClick = CancelFilterButtonClick
  end
end
