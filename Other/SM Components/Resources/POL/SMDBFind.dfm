object frmFind: TfrmFind
  Left = 410
  Top = 332
  BorderStyle = bsDialog
  Caption = 'Wyszukaj rekord'
  ClientHeight = 189
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblValue: TLabel
    Left = 5
    Top = 11
    Width = 48
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Wartoœæ:'
    FocusControl = edValue
  end
  object bvlButton: TBevel
    Left = 312
    Top = 5
    Width = 5
    Height = 190
    Shape = bsLeftLine
  end
  object btnOk: TButton
    Left = 320
    Top = 8
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 320
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Anuluj'
    ModalResult = 2
    TabOrder = 5
  end
  object gbOptions: TGroupBox
    Left = 8
    Top = 107
    Width = 161
    Height = 73
    Caption = ' Opcje wyszukiwania '
    TabOrder = 2
    object cbCaseSensitive: TCheckBox
      Left = 8
      Top = 20
      Width = 142
      Height = 17
      Caption = '&Uwzglêdniaj wielkoœæ liter'
      TabOrder = 0
    end
    object cbMode: TComboBox
      Left = 7
      Top = 44
      Width = 146
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      Items.Strings = (
        'Tylko od pocz¹tku'
        'Ca³e wyrazy'
        'Na dowolnej pozycji')
    end
  end
  object rgPosition: TRadioGroup
    Left = 176
    Top = 107
    Width = 129
    Height = 73
    Caption = ' Rozpocznij od: '
    ItemIndex = 0
    Items.Strings = (
      'pierwszy rekord'
      'bie¿¹cy rekord')
    TabOrder = 3
  end
  object edValue: TEdit
    Left = 60
    Top = 8
    Width = 245
    Height = 21
    TabOrder = 0
  end
  object gbFields: TGroupBox
    Left = 8
    Top = 35
    Width = 297
    Height = 64
    Caption = ' Szukaj '
    TabOrder = 1
    object cbFields: TComboBox
      Left = 64
      Top = 34
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
      OnClick = cbFieldsClick
    end
    object rbOneField: TRadioButton
      Left = 8
      Top = 34
      Width = 57
      Height = 20
      Caption = 'w polu'
      TabOrder = 1
    end
    object rbAllFields: TRadioButton
      Left = 8
      Top = 14
      Width = 257
      Height = 20
      Caption = 'we wszystkich polach'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
  end
end
