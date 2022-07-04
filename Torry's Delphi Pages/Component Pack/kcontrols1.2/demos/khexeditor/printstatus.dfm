object PrintStatusForm: TPrintStatusForm
  Left = 433
  Top = 373
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Print status'
  ClientHeight = 80
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LBPage: TLabel
    Left = 8
    Top = 16
    Width = 35
    Height = 13
    Caption = 'LBPage'
  end
  object BUAbort: TButton
    Left = 88
    Top = 48
    Width = 75
    Height = 25
    Caption = '&Abort'
    TabOrder = 0
    OnClick = BUAbortClick
  end
end
