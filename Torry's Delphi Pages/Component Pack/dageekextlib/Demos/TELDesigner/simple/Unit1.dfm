object frmMain: TfrmMain
  Left = 260
  Top = 216
  Width = 154
  Height = 79
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 12
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ELDesigner1: TELDesigner
    ClipboardFormat = 'Extension Library designer components'
    OnChangeSelection = ELDesigner1ChangeSelection
    Left = 99
    Top = 12
  end
end
