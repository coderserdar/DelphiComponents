object FormConfigureMagnifier: TFormConfigureMagnifier
  Left = 607
  Top = 283
  Hint = 'Configure Magnifier'
  BorderStyle = bsToolWindow
  Caption = 'Configure Magnifier'
  ClientHeight = 183
  ClientWidth = 237
  Color = clWindow
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 225
    Height = 135
    Caption = ' Options '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 90
      Width = 32
      Height = 15
      Caption = 'Shape'
    end
    object Label2: TLabel
      Left = 68
      Top = 50
      Width = 23
      Height = 15
      Caption = 'Rate'
    end
    object Label3: TLabel
      Left = 126
      Top = 48
      Width = 71
      Height = 15
      Caption = 'Transparency'
    end
    object Label4: TLabel
      Left = 8
      Top = 48
      Width = 20
      Height = 15
      Caption = 'Size'
    end
    object MagniferBorders1: TCheckBox
      Left = 8
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Show borders'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = MagniferBorders1Click
    end
    object ComboBoxMagnifierStyle1: TComboBox
      Left = 8
      Top = 106
      Width = 129
      Height = 23
      Hint = 'Magnifier Shape'
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 1
      Text = 'Ellipse'
      OnChange = ComboBoxMagnifierStyle1Change
      Items.Strings = (
        'Rectangle'
        'Ellipse')
    end
    object EditMagnifierSize1: TEdit
      Left = 8
      Top = 64
      Width = 35
      Height = 23
      Hint = 'Magnifier Size'
      TabOrder = 2
      Text = '100'
    end
    object UpDownMagnifierSize1: TUpDown
      Left = 43
      Top = 64
      Width = 16
      Height = 23
      Hint = 'Magnifier Size'
      Associate = EditMagnifierSize1
      Min = 25
      Max = 500
      Increment = 25
      Position = 100
      TabOrder = 3
      OnChanging = UpDownMagnifierSize1Changing
    end
    object EditMagnifierRate1: TEdit
      Left = 68
      Top = 64
      Width = 35
      Height = 23
      Hint = 'Magnifier Rate'
      TabOrder = 4
      Text = '20'
    end
    object EditMagnifierTransparency1: TEdit
      Left = 126
      Top = 64
      Width = 35
      Height = 23
      Hint = 'MagnifierTransparency'
      TabOrder = 5
      Text = '255'
    end
    object UpDownMagnifierRate1: TUpDown
      Left = 103
      Top = 64
      Width = 16
      Height = 23
      Hint = 'Magnifier Rate'
      Associate = EditMagnifierRate1
      Min = 20
      Increment = 5
      Position = 20
      TabOrder = 6
      OnChanging = UpDownMagnifierRate1Changing
    end
    object UpDownMagnifierTransparency1: TUpDown
      Left = 161
      Top = 64
      Width = 16
      Height = 23
      Hint = 'MagnifierTransparency'
      Associate = EditMagnifierTransparency1
      Max = 255
      Increment = 10
      Position = 255
      TabOrder = 7
      OnChanging = UpDownMagnifierTransparency1Changing
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 147
    Width = 237
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
  end
end
