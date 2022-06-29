object FormViewFilter: TFormViewFilter
  Left = 197
  Top = 146
  BorderStyle = bsDialog
  Caption = 'Filter'
  ClientHeight = 166
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 96
    Width = 297
    Height = 9
    Shape = bsBottomLine
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 219
    Height = 13
    Caption = 'During folder browsing, show these files only:'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 112
    Width = 289
    Height = 17
    Caption = 'Don'#39't show this dialog again'
    TabOrder = 0
  end
  object RadioButton1: TRadioButton
    Left = 16
    Top = 32
    Width = 280
    Height = 17
    Caption = 'Show all files'
    TabOrder = 1
  end
  object RadioButton2: TRadioButton
    Left = 16
    Top = 48
    Width = 280
    Height = 17
    Caption = 'Multimedia clips'
    TabOrder = 2
  end
  object RadioButton3: TRadioButton
    Left = 16
    Top = 64
    Width = 280
    Height = 17
    Caption = 'Images'
    TabOrder = 3
  end
  object RadioButton4: TRadioButton
    Left = 16
    Top = 80
    Width = 280
    Height = 17
    Caption = 'Web documents'
    TabOrder = 4
  end
  object Button1: TButton
    Left = 120
    Top = 136
    Width = 81
    Height = 23
    Caption = 'OK'
    TabOrder = 5
  end
end
