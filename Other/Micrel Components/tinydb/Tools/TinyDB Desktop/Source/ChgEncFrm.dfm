inherited ChgEncForm: TChgEncForm
  Left = 240
  Top = 142
  BorderStyle = bsDialog
  Caption = 'Change Encrypt Algorithm'
  ClientHeight = 174
  ClientWidth = 307
  Font.Height = -12
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object OkButton: TButton
    Left = 133
    Top = 140
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 224
    Top = 140
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 11
    Width = 290
    Height = 117
    TabOrder = 2
    object EncAlgoLabel: TLabel
      Left = 12
      Top = 28
      Width = 52
      Height = 14
      Caption = 'Algorithm'
    end
    object PasswordLabel: TLabel
      Left = 12
      Top = 56
      Width = 51
      Height = 14
      Caption = 'Password'
    end
    object Password2Label: TLabel
      Left = 12
      Top = 84
      Width = 60
      Height = 14
      Caption = 'Type again'
    end
    object EncryptCheckBox: TCheckBox
      Left = 12
      Top = -2
      Width = 80
      Height = 17
      Caption = 'Encrypt'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = EncryptCheckBoxClick
    end
    object EncAlgoComboBox: TComboBox
      Left = 83
      Top = 24
      Width = 192
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
    end
    object PasswordEdit: TEdit
      Left = 83
      Top = 52
      Width = 192
      Height = 22
      PasswordChar = '*'
      TabOrder = 2
    end
    object Password2Edit: TEdit
      Left = 83
      Top = 80
      Width = 192
      Height = 22
      PasswordChar = '*'
      TabOrder = 3
    end
  end
end
