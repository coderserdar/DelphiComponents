object ChangeDeviceForm: TChangeDeviceForm
  Left = 350
  Top = 480
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Change Device'
  ClientHeight = 152
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 80
    Width = 161
    Height = 65
    Caption = 'Fullscreeen &modes'
    TabOrder = 0
    object ModeCombo: TComboBox
      Left = 8
      Top = 16
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'ModeCombo'
      OnChange = ModeComboChange
    end
    object cStereo: TCheckBox
      Left = 8
      Top = 40
      Width = 129
      Height = 17
      Caption = '&Steroscopic viewing'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cStereoClick
    end
  end
  object bOK: TButton
    Left = 184
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 184
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 161
    Height = 65
    Caption = '&Device Selection'
    TabOrder = 3
    object DeviceCombo: TComboBox
      Left = 8
      Top = 16
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'DeviceCombo'
      OnChange = DeviceComboChange
    end
    object cDesktop: TCheckBox
      Left = 8
      Top = 40
      Width = 129
      Height = 17
      Caption = 'Use desktop &window'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cDesktopClick
    end
  end
end
