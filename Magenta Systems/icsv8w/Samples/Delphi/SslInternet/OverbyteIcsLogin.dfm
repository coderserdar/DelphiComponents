object FormLogin: TFormLogin
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'ICS Authentication Login'
  ClientHeight = 224
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 124
    Width = 51
    Height = 13
    Caption = 'User name'
  end
  object LabelMethod: TLabel
    Left = 8
    Top = 8
    Width = 306
    Height = 13
    AutoSize = False
    Caption = 'Authentication Method(s), select one'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 10
    Top = 159
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object LabelPageURL: TLabel
    Left = 10
    Top = 95
    Width = 421
    Height = 13
    AutoSize = False
    Caption = 'Page requesting login'
    WordWrap = True
  end
  object AuthUsername: TEdit
    Left = 89
    Top = 119
    Width = 297
    Height = 21
    TabOrder = 0
  end
  object AuthPassword: TEdit
    Left = 89
    Top = 156
    Width = 297
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object doOK: TBitBtn
    Left = 119
    Top = 191
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = doOKClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object doCancel: TBitBtn
    Left = 250
    Top = 191
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object ListMethods: TListBox
    Left = 10
    Top = 27
    Width = 426
    Height = 59
    ItemHeight = 13
    Items.Strings = (
      'Basic Authentication (clear) - Realm? ')
    TabOrder = 4
  end
end
