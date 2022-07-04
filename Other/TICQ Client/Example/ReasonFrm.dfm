object ReasonForm: TReasonForm
  Left = 469
  Top = 268
  Width = 225
  Height = 139
  BorderIcons = []
  BorderStyle = bsSizeToolWin
  Caption = 'Enter a reason for denial'
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object btnReasonOK: TButton
    Left = 72
    Top = 80
    Width = 75
    Height = 23
    Caption = '&Send'
    TabOrder = 1
    OnClick = btnReasonOKClick
  end
  object memoReason: TMemo
    Left = 8
    Top = 8
    Width = 201
    Height = 65
    TabOrder = 0
  end
end
