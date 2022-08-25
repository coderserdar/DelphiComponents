object AuthForm: TAuthForm
  Left = 470
  Top = 309
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'AuthForm'
  ClientHeight = 271
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 15
    Top = 156
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label1: TLabel
    Left = 15
    Top = 110
    Width = 51
    Height = 13
    Caption = 'User name'
  end
  object LabelMethod: TLabel
    Left = 13
    Top = 3
    Width = 306
    Height = 13
    AutoSize = False
    Caption = 'Authentication Method(s), select one'
    WordWrap = True
  end
  object LabelPageURL: TLabel
    Left = 15
    Top = 90
    Width = 421
    Height = 13
    AutoSize = False
    Caption = 'Page requesting login'
    WordWrap = True
  end
  object AuthUsername: TEdit
    Left = 15
    Top = 129
    Width = 231
    Height = 21
    TabOrder = 0
  end
  object AuthPassword: TEdit
    Left = 15
    Top = 175
    Width = 231
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 60
    Top = 210
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = OKBtnClick
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 181
    Top = 210
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object ListMethods: TListBox
    Left = 15
    Top = 22
    Width = 426
    Height = 59
    ItemHeight = 13
    Items.Strings = (
      'Basic Authentication (clear) - Realm? ')
    TabOrder = 4
  end
end
