object Form1: TForm1
  Left = 499
  Top = 326
  Width = 511
  Height = 495
  Caption = 'GT Delphi Components  - Setting Controls Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 51
    Height = 13
    Caption = 'SettingEdit'
  end
  object Label2: TLabel
    Left = 24
    Top = 80
    Width = 43
    Height = 13
    Caption = 'TableList'
  end
  object Label3: TLabel
    Left = 176
    Top = 56
    Width = 42
    Height = 13
    Caption = 'FreeText'
  end
  object FileNamePath: TgtCSCEdit
    Left = 24
    Top = 32
    Width = 121
    Height = 21
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = ''
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object btnSave: TButton
    Left = 160
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = btnSaveClick
  end
  object btnLoad: TButton
    Left = 248
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 2
    OnClick = btnLoadClick
  end
  object btnClear: TButton
    Left = 208
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = btnClearClick
  end
  object IsActive: TgtCSCCheckBox
    Left = 24
    Top = 56
    Width = 121
    Height = 21
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = False
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Caption = 'Is Active'
  end
  object DataBaseType: TgtCSCComboBox
    Left = 176
    Top = 32
    Width = 121
    Height = 21
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = -1
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5')
    SelectedIndex = -1
  end
  object TableList: TgtCSCListBox
    Left = 16
    Top = 96
    Width = 100
    Height = 120
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = 'Table1,Table2,Table3'
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      'Table1'
      'Table2'
      'Table3')
    SelectedIndex = -1
  end
  object FreeText: TgtCSCMemo
    Left = 176
    Top = 72
    Width = 289
    Height = 49
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = ''
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object gtCSCDateTimePicker1: TgtCSCDateTimePicker
    Left = 176
    Top = 136
    Width = 121
    Height = 21
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = 39836.6614283102d
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    DateTimeKind = dtkDate
    DateFormat = dfShort
    ShowCheckBox = False
    Checked = True
  end
  object gtCSCTrackBar1: TgtCSCTrackBar
    Left = 176
    Top = 168
    Width = 121
    Height = 121
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = 0
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Orientation = trVertical
  end
  object gtCSCRadioGroup1: TgtCSCRadioGroup
    Left = 312
    Top = 136
    Width = 121
    Height = 65
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = -1
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Items.Strings = (
      '1'
      '2'
      '3')
    SelectedIndex = -1
  end
  object gtCSCColorBox1: TgtCSCColorBox
    Left = 312
    Top = 208
    Width = 121
    Height = 21
    SettingsManager = gtSettingsManager1
    GroupID = 0
    IniSection = 'General'
    SettingValue = 44
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Selected = clScrollBar
  end
  object gtCSCLabeledEdit1: TgtCSCLabeledEdit
    Left = 32
    Top = 384
    Width = 145
    Height = 21
    GroupID = 0
    SettingValue = ''
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    LabelPosition = lpAbove
    Caption = 'Test'
  end
  object gtSettingsManager1: TgtSettingsManager
    SettingsType = stIni
    StorageOptions.IniFileName = 'C:\test.ini'
    Left = 32
    Top = 288
  end
end
