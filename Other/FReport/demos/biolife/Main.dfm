object FormMain: TFormMain
  Left = 307
  Top = 268
  Width = 280
  Height = 114
  HorzScrollBar.Range = 259
  VertScrollBar.Range = 81
  ActiveControl = Button2
  AutoScroll = False
  Caption = 'FishFact'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button2: TButton
    Left = 60
    Top = 48
    Width = 65
    Height = 22
    Caption = 'Preview'
    TabOrder = 0
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 148
    Top = 48
    Width = 65
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
end
