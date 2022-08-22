object ChgPwdForm: TChgPwdForm
  Left = 262
  Top = 220
  BorderStyle = bsDialog
  Caption = 'Change Password'
  ClientHeight = 137
  ClientWidth = 292
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
  object GroupBox1: TGroupBox
    Left = 9
    Top = 10
    Width = 274
    Height = 82
    TabOrder = 2
    object PasswordLabel: TLabel
      Left = 13
      Top = 25
      Width = 51
      Height = 14
      Caption = 'Password'
    end
    object Password2Label: TLabel
      Left = 13
      Top = 51
      Width = 60
      Height = 14
      Caption = 'Type again'
    end
    object PasswordEdit: TEdit
      Left = 100
      Top = 21
      Width = 160
      Height = 22
      PasswordChar = '*'
      TabOrder = 0
    end
    object CheckPwdCheckBox: TCheckBox
      Left = 13
      Top = -2
      Width = 143
      Height = 17
      Caption = 'Check password'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckPwdCheckBoxClick
    end
    object Password2Edit: TEdit
      Left = 100
      Top = 47
      Width = 160
      Height = 22
      PasswordChar = '*'
      TabOrder = 1
    end
  end
  object OkButton: TButton
    Left = 117
    Top = 102
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 208
    Top = 102
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
end
