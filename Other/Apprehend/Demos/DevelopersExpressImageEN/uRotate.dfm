object FormRotate: TFormRotate
  Left = 539
  Top = 139
  BorderIcons = [biSystemMenu]
  Caption = 'Rotate Preview'
  ClientHeight = 339
  ClientWidth = 421
  Color = clWindow
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 98
    Top = 274
    Width = 96
    Height = 15
    Caption = 'Background Color'
  end
  object CheckBox1: TCheckBox
    Left = 108
    Top = 193
    Width = 86
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Antialiasing'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object ImageEnView1: TImageEnView
    Left = 8
    Top = 13
    Width = 185
    Height = 139
    Background = clWhite
    ParentCtl3D = False
    BackgroundStyle = iebsChessboard
    AutoShrink = True
    EnableInteractionHints = True
    TabOrder = 1
  end
  object LockPreview1: TCheckBox
    Left = 108
    Top = 216
    Width = 86
    Height = 16
    Alignment = taLeftJustify
    Caption = 'Lock Preview'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = LockPreview1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 303
    Width = 421
    Height = 36
    Align = alBottom
    ParentBackground = False
    TabOrder = 3
    object Button1: TButton
      Left = 8
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 89
      Top = 5
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Preview1: TButton
      Left = 170
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 2
      OnClick = Preview1Click
    end
    object Undo1: TButton
      Left = 251
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Undo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = Undo1Click
    end
    object Reset1: TButton
      Left = 332
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 4
      OnClick = Reset1Click
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 202
    Top = 13
    Width = 211
    Height = 139
    Caption = 'Angle'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '0 degrees'
      '45 degrees'
      '90 degrees'
      '135 degrees'
      '180 degrees'
      '225 degrees'
      '270 degrees'
      '-45 degrees'
      '-90 degrees'
      '-135 degrees'
      '-180 degrees'
      '-225 degrees'
      '-270 degrees')
    TabOrder = 4
    OnClick = RadioGroup1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 202
    Top = 188
    Width = 203
    Height = 75
    Caption = 'Antialias Mode'
    ItemIndex = 2
    Items.Strings = (
      '(ierFast'
      'erBilinear'
      'ierBicubic')
    TabOrder = 5
    OnClick = RadioGroup2Click
  end
  object cxColorComboBox1: TcxColorComboBox
    Left = 202
    Top = 270
    ColorValue = clBlack
    Properties.CustomColors = <>
    Properties.ImmediatePost = True
    Properties.NamingConvention = cxncX11
    TabOrder = 6
    Width = 197
  end
  object Angle1: TcxSpinEdit
    Left = 202
    Top = 162
    Properties.ImmediatePost = True
    Properties.MaxValue = 360.000000000000000000
    Properties.OnChange = cxSpinEdit1PropertiesChange
    TabOrder = 7
    Width = 121
  end
  object cxLabel1: TcxLabel
    Left = 147
    Top = 163
    Caption = 'Degrees'
  end
end
