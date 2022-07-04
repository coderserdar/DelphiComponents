object Form1: TForm1
  Left = 192
  Top = 107
  Width = 185
  Height = 188
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 136
    Width = 161
    Height = 17
    Caption = 'Show message'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 161
    Height = 97
    Lines.Strings = (
      'Page not found')
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 161
    Height = 21
    TabOrder = 2
    Text = 'Error #404'
  end
  object tAPI_msgdlg1: tAPI_msgdlg
    Caption = 'Message'
    Fade = True
    Left = 104
    Top = 48
  end
end
