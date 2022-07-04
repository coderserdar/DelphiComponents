object rmFrmEditBinding: TrmFrmEditBinding
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Editing Key Binding'
  ClientHeight = 96
  ClientWidth = 297
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
  object GroupBox1: TGroupBox
    Left = 3
    Top = 3
    Width = 290
    Height = 59
    TabOrder = 0
    object cbAlt: TCheckBox
      Left = 18
      Top = 24
      Width = 50
      Height = 17
      Caption = 'ALT +'
      TabOrder = 0
    end
    object cbCTRL: TCheckBox
      Left = 72
      Top = 24
      Width = 58
      Height = 17
      Caption = 'CTRL +'
      TabOrder = 1
    end
    object cbShift: TCheckBox
      Left = 134
      Top = 24
      Width = 51
      Height = 17
      Caption = 'Shift +'
      TabOrder = 2
    end
    object cbxKey: TComboBox
      Left = 189
      Top = 22
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        '(None)'
        'A'
        'B'
        'C'
        'D'
        'E'
        'F'
        'G'
        'H'
        'I'
        'J'
        'K'
        'L'
        'M'
        'N'
        'O'
        'P'
        'Q'
        'R'
        'S'
        'T'
        'U'
        'V'
        'W'
        'X'
        'Y'
        'Z'
        '0'
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9'
        '0'
        'F1'
        'F2'
        'F3'
        'F4'
        'F5'
        'F6'
        'F7'
        'F8'
        'F9'
        'F10'
        'F11'
        'F12'
        'Home'
        'End'
        'PgUp'
        'PgDn'
        'Ins'
        'Del'
        'Left'
        'Right'
        'Up'
        'Down'
        'BkSp')
    end
  end
  object Button1: TButton
    Left = 140
    Top = 66
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 217
    Top = 66
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
