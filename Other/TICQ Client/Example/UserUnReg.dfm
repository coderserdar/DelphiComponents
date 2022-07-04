object UserUnRegForm: TUserUnRegForm
  Left = 396
  Top = 296
  Width = 178
  Height = 118
  BorderStyle = bsSizeToolWin
  Caption = 'Unregister UIN...'
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
    Width = 107
    Height = 13
    Caption = 'Insert your password:'
  end
  object PasswordEdit: TPasswordEdit
    Left = 8
    Top = 24
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
  end
  object btnContinue: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Continue'
    TabOrder = 1
    OnClick = btnContinueClick
  end
end
