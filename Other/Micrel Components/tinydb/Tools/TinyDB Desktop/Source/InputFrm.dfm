object InputForm: TInputForm
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Input'
  ClientHeight = 102
  ClientWidth = 320
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
    Left = 6
    Top = 2
    Width = 307
    Height = 58
    TabOrder = 0
    object PromptLabel: TLabel
      Left = 12
      Top = 25
      Width = 9
      Height = 14
      Caption = '#'
    end
    object ValueEdit: TEdit
      Left = 120
      Top = 21
      Width = 173
      Height = 22
      TabOrder = 0
    end
  end
  object OkButton: TButton
    Left = 146
    Top = 69
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 238
    Top = 69
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
