object InputForm: TInputForm
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Input'
  ClientHeight = 103
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 6
    Top = 2
    Width = 274
    Height = 58
    TabOrder = 0
    object PromptLabel: TLabel
      Left = 12
      Top = 25
      Width = 8
      Height = 13
      Caption = '#'
    end
    object ValueEdit: TEdit
      Left = 104
      Top = 21
      Width = 155
      Height = 21
      TabOrder = 0
    end
  end
  object OkButton: TButton
    Left = 112
    Top = 69
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 204
    Top = 69
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
