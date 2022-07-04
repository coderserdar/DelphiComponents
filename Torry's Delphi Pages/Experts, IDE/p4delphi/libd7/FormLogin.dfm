object FrmLogin: TFrmLogin
  Left = 376
  Top = 217
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 107
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    297
    107)
  PixelsPerInch = 96
  TextHeight = 13
  object LblPassword: TLabel
    Left = 8
    Top = 8
    Width = 60
    Height = 13
    Caption = 'LblPassword'
  end
  object EdtPassword: TEdit
    Left = 8
    Top = 27
    Width = 281
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 135
    Top = 79
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    ExplicitTop = 93
  end
  object btnCancel: TButton
    Left = 218
    Top = 79
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitTop = 93
  end
  object ChkRemember: TCheckBox
    Left = 8
    Top = 54
    Width = 145
    Height = 17
    Caption = 'Remember my password'
    TabOrder = 1
  end
end
