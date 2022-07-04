object UserRegNewForm: TUserRegNewForm
  Left = 329
  Top = 240
  BorderStyle = bsToolWindow
  Caption = 'Registration...'
  ClientHeight = 129
  ClientWidth = 177
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
    Caption = 'Password of new UIN:'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 88
    Height = 13
    Caption = 'Retype password:'
  end
  object Button1: TButton
    Left = 48
    Top = 96
    Width = 123
    Height = 25
    Caption = '&Continue Registration'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit1: TPasswordEdit
    Left = 8
    Top = 24
    Width = 161
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
  object Edit2: TPasswordEdit
    Left = 8
    Top = 64
    Width = 161
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
end
