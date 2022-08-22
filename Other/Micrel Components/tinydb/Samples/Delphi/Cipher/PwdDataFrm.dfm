object PwdDataForm: TPwdDataForm
  Left = 241
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Password Info'
  ClientHeight = 178
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 8
    Top = 64
    Width = 216
    Height = 105
    Caption = 'Note'
    TabOrder = 1
    object NoteMemo: TMemo
      Left = 12
      Top = 19
      Width = 192
      Height = 74
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 4
    Width = 216
    Height = 54
    Caption = 'Password'
    TabOrder = 0
    object PasswordEdit: TEdit
      Left = 12
      Top = 19
      Width = 192
      Height = 22
      TabOrder = 0
    end
  end
  object OkButton: TButton
    Left = 234
    Top = 10
    Width = 80
    Height = 27
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 234
    Top = 46
    Width = 80
    Height = 27
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = CancelButtonClick
  end
end
