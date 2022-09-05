object FormCombineImage: TFormCombineImage
  Left = 321
  Top = 112
  BorderStyle = bsDialog
  Caption = 'Combine images'
  ClientHeight = 202
  ClientWidth = 336
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 8
    Top = 168
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 96
    Top = 168
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbCombine: TGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 153
    Caption = 'Combine to RGB'
    TabOrder = 2
    object lChannel1: TLabel
      Left = 16
      Top = 28
      Width = 47
      Height = 13
      Caption = 'lChannel1'
      FocusControl = cbChannel1
    end
    object lChannel2: TLabel
      Left = 16
      Top = 60
      Width = 47
      Height = 13
      Caption = 'lChannel2'
      FocusControl = cbChannel2
    end
    object lChannel3: TLabel
      Left = 16
      Top = 92
      Width = 47
      Height = 13
      Caption = 'lChannel3'
      FocusControl = cbChannel3
    end
    object lChannel4: TLabel
      Left = 16
      Top = 124
      Width = 47
      Height = 13
      Caption = 'lChannel4'
      FocusControl = cbChannel4
    end
    object cbChannel1: TComboBox
      Left = 80
      Top = 24
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object cbChannel2: TComboBox
      Left = 80
      Top = 56
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object cbChannel3: TComboBox
      Left = 80
      Top = 88
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object cbChannel4: TComboBox
      Left = 80
      Top = 120
      Width = 225
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
  end
end
