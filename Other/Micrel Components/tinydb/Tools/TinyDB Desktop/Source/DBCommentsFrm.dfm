object DBCommentsForm: TDBCommentsForm
  Left = 264
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Database Comments'
  ClientHeight = 209
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 7
    Top = 3
    Width = 325
    Height = 165
    TabOrder = 0
    object Memo: TMemo
      Left = 10
      Top = 16
      Width = 304
      Height = 138
      MaxLength = 256
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object OkButton: TButton
    Left = 168
    Top = 176
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 256
    Top = 176
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
