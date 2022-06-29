object Form2: TForm2
  Left = 490
  Top = 108
  Width = 307
  Height = 462
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 120
    Top = 344
    Width = 33
    Height = 33
  end
  object Panel1: TPanel
    Left = 16
    Top = 48
    Width = 249
    Height = 105
    Caption = 'Panel1'
    TabOrder = 0
    object Image2: TImage
      Left = 168
      Top = 56
      Width = 33
      Height = 33
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 80
      Width = 113
      Height = 17
      Caption = 'RadioButton1'
      TabOrder = 0
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 1
    end
    object Button3: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 2
    end
    object Edit1: TEdit
      Left = 104
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'Edit1'
    end
  end
  object StringGrid1: TStringGrid
    Left = 16
    Top = 160
    Width = 257
    Height = 89
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing]
    TabOrder = 1
    ColWidths = (
      64
      52
      64
      64
      64)
    RowHeights = (
      24
      25
      24
      24
      24)
  end
  object PageControl1: TPageControl
    Left = 16
    Top = 256
    Width = 225
    Height = 65
    ActivePage = TabSheet3
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
    end
  end
  object Button2: TButton
    Left = 24
    Top = 344
    Width = 73
    Height = 25
    Caption = '0'
    TabOrder = 3
  end
  object UpDown1: TUpDown
    Left = 97
    Top = 344
    Width = 15
    Height = 25
    Associate = Button2
    Min = 0
    Position = 0
    TabOrder = 4
    Wrap = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 299
    Height = 33
    ButtonHeight = 24
    Caption = 'ToolBar1'
    TabOrder = 5
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Caption = 'ToolButton1'
      ImageIndex = 0
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 2
      Caption = 'ToolButton2'
      ImageIndex = 1
    end
    object ToolButton3: TToolButton
      Left = 46
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 54
      Top = 2
      Caption = 'ToolButton4'
      ImageIndex = 2
    end
  end
end
