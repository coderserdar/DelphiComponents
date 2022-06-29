object FormProgress: TFormProgress
  Left = 343
  Top = 358
  ActiveControl = btnCancel
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Search progress'
  ClientHeight = 79
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labCaption: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Searching:'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 24
    Width = 281
    Height = 17
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 111
    Top = 48
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
