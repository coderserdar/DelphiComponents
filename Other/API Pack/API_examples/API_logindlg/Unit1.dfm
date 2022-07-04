object Form1: TForm1
  Left = 192
  Top = 107
  Width = 221
  Height = 183
  AlphaBlend = True
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    213
    149)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 183
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Login'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 16
    Top = 40
    Width = 183
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    ItemHeight = 13
    TabOrder = 1
  end
  object tAPI_logindlg1: TAPI_logindlg
    Caption = 'Login dialog'
    Fade = True
    ClearOnExecute = True
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 64
    Top = 48
  end
  object API_usermanager1: TAPI_usermanager
    MaxUserCount = 0
    Username = 'Anonymous'
    Left = 64
    Top = 80
  end
end
