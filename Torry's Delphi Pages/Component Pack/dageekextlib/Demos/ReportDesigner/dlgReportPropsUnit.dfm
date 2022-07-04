object dlgReportProps: TdlgReportProps
  Left = 182
  Top = 124
  BorderStyle = bsDialog
  Caption = 'Report porperties'
  ClientHeight = 311
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 272
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 352
    Top = 280
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 425
    Height = 49
    Caption = 'Paper'
    TabOrder = 2
    object Label1: TLabel
      Left = 120
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object Label2: TLabel
      Left = 218
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Height'
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 16
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox1Change
      Items.Strings = (
        'Default'
        'A3'
        'A4'
        'A4 Small'
        'A5'
        'B4'
        'B5'
        'Custom')
    end
    object Edit1: TEdit
      Left = 152
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 1
      Text = 'Edit1'
      OnKeyPress = Edit3KeyPress
    end
    object Edit2: TEdit
      Left = 256
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 2
      Text = 'Edit2'
      OnKeyPress = Edit3KeyPress
    end
    object ComboBox2: TComboBox
      Left = 328
      Top = 16
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'Portait'
        'Landscape')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 64
    Width = 425
    Height = 81
    Caption = 'Margins'
    TabOrder = 3
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 18
      Height = 13
      Caption = 'Left'
    end
    object Label4: TLabel
      Left = 8
      Top = 56
      Width = 25
      Height = 13
      Caption = 'Right'
    end
    object Label5: TLabel
      Left = 120
      Top = 24
      Width = 19
      Height = 13
      Caption = 'Top'
    end
    object Label6: TLabel
      Left = 120
      Top = 56
      Width = 33
      Height = 13
      Caption = 'Bottom'
    end
    object Label7: TLabel
      Left = 256
      Top = 24
      Width = 67
      Height = 13
      Caption = 'Column space'
    end
    object Label8: TLabel
      Left = 256
      Top = 56
      Width = 73
      Height = 13
      Caption = 'Column number'
    end
    object Edit3: TEdit
      Left = 40
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 0
      Text = 'Edit3'
      OnKeyPress = Edit3KeyPress
    end
    object Edit4: TEdit
      Left = 40
      Top = 48
      Width = 57
      Height = 21
      TabOrder = 1
      Text = 'Edit3'
      OnKeyPress = Edit3KeyPress
    end
    object Edit5: TEdit
      Left = 160
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 2
      Text = 'Edit3'
      OnKeyPress = Edit3KeyPress
    end
    object Edit6: TEdit
      Left = 160
      Top = 48
      Width = 57
      Height = 21
      TabOrder = 3
      Text = 'Edit3'
      OnKeyPress = Edit3KeyPress
    end
    object Edit7: TEdit
      Left = 352
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 4
      Text = 'Edit3'
      OnKeyPress = Edit3KeyPress
    end
    object Edit8: TEdit
      Left = 352
      Top = 48
      Width = 41
      Height = 21
      ReadOnly = True
      TabOrder = 5
      Text = '1'
    end
    object UpDown1: TUpDown
      Left = 393
      Top = 48
      Width = 15
      Height = 21
      Associate = Edit8
      Min = 1
      Max = 20
      Position = 1
      TabOrder = 6
      Wrap = False
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 152
    Width = 425
    Height = 121
    Caption = 'Other'
    TabOrder = 4
    object Label9: TLabel
      Left = 264
      Top = 32
      Width = 24
      Height = 13
      Caption = 'Units'
    end
    object ComboBox3: TComboBox
      Left = 304
      Top = 24
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'MM'
        'Inches'
        'Pixels'
        'Characters'
        'Native')
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 16
      Width = 185
      Height = 97
      Caption = 'Font'
      TabOrder = 1
      object Panel1: TPanel
        Left = 8
        Top = 48
        Width = 169
        Height = 41
        Caption = 'Aa Bb Cc Dd Ee'
        TabOrder = 0
      end
      object Button3: TButton
        Left = 8
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Change'
        TabOrder = 1
        OnClick = Button3Click
      end
    end
    object CheckBox1: TCheckBox
      Left = 264
      Top = 64
      Width = 145
      Height = 17
      Caption = 'Print first page header'
      TabOrder = 2
    end
    object CheckBox2: TCheckBox
      Left = 264
      Top = 88
      Width = 145
      Height = 17
      Caption = 'Print last page footer'
      TabOrder = 3
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 32
    Top = 280
  end
end
