object PasswdForm: TPasswdForm
  Left = 387
  Top = 236
  BorderStyle = bsToolWindow
  Caption = 'Change my password'
  ClientHeight = 232
  ClientWidth = 173
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 161
    Height = 41
    AutoSize = False
    Caption = 
      'You have to be online. Doesn'#39't affect on Password property (you ' +
      'have to change it manualy).'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 102
    Height = 13
    Caption = 'Type your password:'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 109
    Height = 13
    Caption = 'Type a new password:'
  end
  object Label4: TLabel
    Left = 8
    Top = 152
    Width = 130
    Height = 13
    Caption = 'Retype the new password:'
  end
  object PasswordEdit1: TPasswordEdit
    Left = 8
    Top = 72
    Width = 153
    Height = 21
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    TabOrder = 0
    OnChange = PasswordEdit1Change
  end
  object PasswordEdit2: TPasswordEdit
    Left = 8
    Top = 120
    Width = 153
    Height = 21
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    TabOrder = 1
  end
  object PasswordEdit3: TPasswordEdit
    Left = 8
    Top = 168
    Width = 153
    Height = 21
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 9
    ParentFont = False
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 88
    Top = 200
    Width = 75
    Height = 25
    Caption = '&OK'
    Enabled = False
    TabOrder = 3
    OnClick = btnOKClick
  end
end
