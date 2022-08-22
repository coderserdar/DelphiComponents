object ChgPwdForm: TChgPwdForm
  Left = 262
  Top = 220
  BorderStyle = bsDialog
  Caption = 'Change Password'
  ClientHeight = 137
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 9
    Top = 10
    Width = 274
    Height = 82
    TabOrder = 0
    object PromptLabel: TLabel
      Left = 13
      Top = 25
      Width = 74
      Height = 13
      Caption = 'New password:'
    end
    object Label1: TLabel
      Left = 13
      Top = 51
      Width = 57
      Height = 13
      Caption = 'Type again:'
    end
    object PasswordEdit: TEdit
      Left = 100
      Top = 21
      Width = 160
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
    end
    object CheckPwdCheckBox: TCheckBox
      Left = 13
      Top = -2
      Width = 111
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
      Height = 21
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
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 208
    Top = 102
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
