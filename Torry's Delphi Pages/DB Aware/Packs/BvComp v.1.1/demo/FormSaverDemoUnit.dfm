object FormSaverDemo: TFormSaverDemo
  Left = 126
  Top = 119
  Width = 285
  Height = 178
  Caption = 'Demo of FormSaver'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 277
    Height = 4
    Align = alTop
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 0
    Top = 4
    Width = 277
    Height = 147
    Align = alClient
    Alignment = taCenter
    Caption = 
      'Change sizes of the form, move and close. After that again open.' +
      #13#10'The form will restore of the last setup.'
    Layout = tlCenter
    WordWrap = True
  end
  object bvFormSaver1: TbvFormSaver
    Enabled = True
    SavePosOnly = False
    Left = 152
    Top = 64
  end
end
