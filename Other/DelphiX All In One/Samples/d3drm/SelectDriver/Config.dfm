object ConfigForm: TConfigForm
  Left = 272
  Top = 191
  BorderStyle = bsDialog
  Caption = 'Config'
  ClientHeight = 223
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 9
    Top = 37
    Width = 373
    Height = 146
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 26
    Top = 104
    Width = 63
    Height = 13
    Caption = 'Display mode'
  end
  object Label2: TLabel
    Left = 26
    Top = 52
    Width = 65
    Height = 13
    Caption = 'Display Driver'
  end
  object Bevel2: TBevel
    Left = 182
    Top = 156
    Width = 2
    Height = 22
  end
  object AutomaticButton: TRadioButton
    Left = 17
    Top = 9
    Width = 79
    Height = 18
    Caption = 'Automatic'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object ManualButton: TRadioButton
    Left = 17
    Top = 26
    Width = 71
    Height = 27
    Caption = 'Manual'
    TabOrder = 1
  end
  object DriverBox: TComboBox
    Left = 26
    Top = 69
    Width = 339
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnClick = DriverBoxClick
  end
  object DisplayModeBox: TComboBox
    Left = 26
    Top = 121
    Width = 339
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnClick = DisplayModeBoxClick
  end
  object HardwareCheckBox: TCheckBox
    Left = 191
    Top = 156
    Width = 105
    Height = 18
    Caption = 'Hardware'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = DriverBoxClick
  end
  object Button1: TButton
    Left = 217
    Top = 191
    Width = 81
    Height = 27
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 303
    Top = 191
    Width = 82
    Height = 27
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 6
    OnClick = Button2Click
  end
  object FullScreenButton: TCheckBox
    Left = 26
    Top = 156
    Width = 105
    Height = 18
    Caption = 'FullScreen'
    TabOrder = 7
    OnClick = DriverBoxClick
  end
end
