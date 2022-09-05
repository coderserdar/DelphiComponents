object ConfigForm: TConfigForm
  Left = 272
  Top = 191
  BorderStyle = bsDialog
  Caption = 'Config'
  ClientHeight = 275
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '‚l‚r ‚oƒSƒVƒbƒN'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Bevel1: TBevel
    Left = 11
    Top = 45
    Width = 460
    Height = 180
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 32
    Top = 128
    Width = 76
    Height = 16
    Caption = 'Display mode'
  end
  object Label2: TLabel
    Left = 32
    Top = 64
    Width = 77
    Height = 16
    Caption = 'Display Driver'
  end
  object Bevel2: TBevel
    Left = 224
    Top = 192
    Width = 3
    Height = 27
  end
  object AutomaticButton: TRadioButton
    Left = 21
    Top = 11
    Width = 98
    Height = 22
    Caption = 'Automatic'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object ManualButton: TRadioButton
    Left = 21
    Top = 32
    Width = 87
    Height = 33
    Caption = 'Manual'
    TabOrder = 1
  end
  object DriverBox: TComboBox
    Left = 32
    Top = 85
    Width = 417
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    TabOrder = 2
    OnClick = DriverBoxClick
  end
  object DisplayModeBox: TComboBox
    Left = 32
    Top = 149
    Width = 417
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    TabOrder = 3
    OnClick = DisplayModeBoxClick
  end
  object HardwareCheckBox: TCheckBox
    Left = 235
    Top = 192
    Width = 129
    Height = 23
    Caption = 'Hardware'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = DriverBoxClick
  end
  object Button1: TButton
    Left = 267
    Top = 235
    Width = 100
    Height = 33
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 373
    Top = 235
    Width = 100
    Height = 33
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = Button2Click
  end
  object FullScreenButton: TCheckBox
    Left = 32
    Top = 192
    Width = 129
    Height = 23
    Caption = 'FullScreen'
    TabOrder = 7
    OnClick = DriverBoxClick
  end
end
