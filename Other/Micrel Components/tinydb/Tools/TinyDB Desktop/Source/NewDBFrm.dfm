object NewDBForm: TNewDBForm
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'New Database'
  ClientHeight = 261
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object DatabaseNameLabel: TLabel
    Left = 10
    Top = 14
    Width = 69
    Height = 14
    Caption = 'Database file'
  end
  object OkButton: TButton
    Left = 310
    Top = 10
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 4
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 310
    Top = 47
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 5
    OnClick = CancelButtonClick
  end
  object HelpButton: TButton
    Left = 310
    Top = 84
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 6
    OnClick = HelpButtonClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 41
    Width = 290
    Height = 85
    TabOrder = 2
    object CompLevelLabel: TLabel
      Left = 12
      Top = 53
      Width = 28
      Height = 14
      Caption = 'Level'
    end
    object CompAlgoLabel: TLabel
      Left = 12
      Top = 25
      Width = 52
      Height = 14
      Caption = 'Algorithm'
    end
    object CompressCheckBox: TCheckBox
      Left = 12
      Top = -2
      Width = 81
      Height = 17
      Caption = 'Compress'
      TabOrder = 0
      OnClick = CompressCheckBoxClick
    end
    object CompLevelComboBox: TComboBox
      Left = 83
      Top = 49
      Width = 192
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 2
      Items.Strings = (
        'clMaximum'
        'clNormal'
        'clFast'
        'clSuperFast')
    end
    object CompAlgoComboBox: TComboBox
      Left = 83
      Top = 21
      Width = 192
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 136
    Width = 290
    Height = 116
    TabOrder = 3
    object EncryptAlgoLabel: TLabel
      Left = 12
      Top = 28
      Width = 52
      Height = 14
      Caption = 'Algorithm'
    end
    object PasswordLabel: TLabel
      Left = 12
      Top = 56
      Width = 51
      Height = 14
      Caption = 'Password'
    end
    object Password2Label: TLabel
      Left = 12
      Top = 84
      Width = 41
      Height = 14
      Caption = 'Confirm'
    end
    object EncryptCheckBox: TCheckBox
      Left = 12
      Top = -2
      Width = 80
      Height = 17
      Caption = 'Encrypt'
      TabOrder = 0
      OnClick = EncryptCheckBoxClick
    end
    object EncAlgoComboBox: TComboBox
      Left = 83
      Top = 24
      Width = 192
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
    end
    object PasswordEdit: TEdit
      Left = 83
      Top = 52
      Width = 192
      Height = 22
      PasswordChar = '*'
      TabOrder = 2
    end
    object Password2Edit: TEdit
      Left = 83
      Top = 80
      Width = 192
      Height = 22
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object FileNameEdit: TEdit
    Left = 92
    Top = 10
    Width = 179
    Height = 22
    TabOrder = 0
  end
  object BrowseButton: TButton
    Left = 275
    Top = 10
    Width = 23
    Height = 20
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = BrowseButtonClick
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.tdb'
    Filter = 'TinyDB Files(*.tdb)|*.tdb|All Files(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 312
    Top = 168
  end
end
