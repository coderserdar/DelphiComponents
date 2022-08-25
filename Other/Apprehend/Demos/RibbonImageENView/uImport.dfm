object FormImport: TFormImport
  Left = 360
  Top = 167
  BorderStyle = bsDialog
  Caption = 'Import'
  ClientHeight = 277
  ClientWidth = 380
  Color = clWindow
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 159
    Height = 108
    Caption = 'Color Depth'
    ItemIndex = 0
    Items.Strings = (
      'Win XP'
      'True Color'
      '256 Color'
      '16 Color'
      'Monochrome')
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 193
    Top = 8
    Width = 178
    Height = 225
    Caption = 'Size'
    TabOrder = 1
    object RzLabel1: TLabel
      Left = 95
      Top = 198
      Width = 7
      Height = 15
      Caption = 'X'
    end
    object RadioButton16: TRadioButton
      Left = 18
      Top = 18
      Width = 115
      Height = 17
      Caption = '16 x 16'
      TabOrder = 0
      OnClick = RadioButton16Click
    end
    object RadioButton32: TRadioButton
      Left = 18
      Top = 37
      Width = 115
      Height = 17
      Caption = '32 x 32'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RadioButton32Click
    end
    object RadioButton48: TRadioButton
      Left = 18
      Top = 57
      Width = 115
      Height = 17
      Caption = '48 x 48'
      TabOrder = 2
      OnClick = RadioButton48Click
    end
    object RadioButton64: TRadioButton
      Left = 18
      Top = 77
      Width = 115
      Height = 17
      Caption = '64 x 64'
      TabOrder = 3
      OnClick = RadioButton64Click
    end
    object RadioButton72: TRadioButton
      Left = 18
      Top = 97
      Width = 115
      Height = 17
      Caption = '72 x 72'
      TabOrder = 4
      OnClick = RadioButton72Click
    end
    object RadioButton128: TRadioButton
      Left = 18
      Top = 136
      Width = 115
      Height = 17
      Caption = '128 x 128'
      TabOrder = 5
      OnClick = RadioButton128Click
    end
    object RadioButton7: TRadioButton
      Left = 18
      Top = 176
      Width = 115
      Height = 17
      Caption = 'Other'
      TabOrder = 6
      OnClick = RadioButton7Click
    end
    object SpinEdit1: TSpinEdit
      Left = 36
      Top = 194
      Width = 52
      Height = 24
      MaxValue = 0
      MinValue = 0
      TabOrder = 7
      Value = 0
    end
    object SpinEdit2: TSpinEdit
      Left = 108
      Top = 194
      Width = 52
      Height = 24
      MaxValue = 0
      MinValue = 0
      TabOrder = 8
      Value = 0
    end
    object RadioButton256: TRadioButton
      Left = 18
      Top = 156
      Width = 115
      Height = 17
      Caption = '256 x 256'
      TabOrder = 9
      OnClick = RadioButton256Click
    end
    object RadioButton96: TRadioButton
      Left = 18
      Top = 116
      Width = 115
      Height = 17
      Caption = '96 x 96'
      TabOrder = 10
      OnClick = RadioButton196Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 128
    Width = 177
    Height = 67
    Caption = 'Filter'
    TabOrder = 2
    object Label3: TLabel
      Left = 10
      Top = 19
      Width = 26
      Height = 15
      Caption = 'Filter'
    end
    object ComboBox1: TComboBox
      Left = 9
      Top = 33
      Width = 160
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'None'
      Items.Strings = (
        'None'
        'Triangle'
        'Hermite'
        'Bell'
        'BSpline'
        'Lanczos3'
        'Mitchell')
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 241
    Width = 380
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 3
    object Button1: TButton
      Left = 8
      Top = 5
      Width = 67
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 81
      Top = 5
      Width = 67
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
end
