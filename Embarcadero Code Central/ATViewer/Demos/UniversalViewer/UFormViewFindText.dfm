object FormViewFindText: TFormViewFindText
  Left = 257
  Top = 211
  ActiveControl = edText
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 234
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labFind: TLabel
    Left = 8
    Top = 12
    Width = 60
    Height = 13
    Caption = 'Text to find:'
    FocusControl = edText
  end
  object btnOk: TButton
    Left = 28
    Top = 204
    Width = 80
    Height = 23
    Caption = 'Find'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 204
    Top = 204
    Width = 80
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object edText: TTntComboBox
    Left = 80
    Top = 8
    Width = 225
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object btnHelp: TButton
    Left = 116
    Top = 204
    Width = 80
    Height = 23
    Caption = 'Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object boxOptions: TGroupBox
    Left = 8
    Top = 32
    Width = 297
    Height = 105
    Caption = 'Options'
    TabOrder = 1
    object chkWords: TCheckBox
      Left = 8
      Top = 16
      Width = 284
      Height = 17
      Caption = '&Whole words only'
      TabOrder = 0
    end
    object chkCase: TCheckBox
      Left = 8
      Top = 32
      Width = 284
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 1
    end
    object chkHex: TCheckBox
      Left = 8
      Top = 48
      Width = 284
      Height = 17
      Caption = 'Hex string'
      TabOrder = 2
      OnClick = chkHexClick
    end
    object chkRegex: TCheckBox
      Left = 8
      Top = 64
      Width = 284
      Height = 17
      Caption = 'RegEx'
      TabOrder = 3
      OnClick = chkRegexClick
    end
    object chkMLine: TCheckBox
      Left = 8
      Top = 80
      Width = 281
      Height = 17
      Caption = 'Multiline regex'
      TabOrder = 4
    end
  end
  object boxDirection: TGroupBox
    Left = 8
    Top = 140
    Width = 137
    Height = 56
    Caption = 'Direction'
    TabOrder = 2
    object chkDirForward: TRadioButton
      Left = 8
      Top = 16
      Width = 125
      Height = 17
      Caption = 'Forward'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object chkDirBackward: TRadioButton
      Left = 8
      Top = 32
      Width = 125
      Height = 17
      Caption = 'Backward'
      TabOrder = 1
    end
  end
  object boxOrigin: TGroupBox
    Left = 152
    Top = 140
    Width = 153
    Height = 56
    Caption = 'Origin'
    TabOrder = 3
    object chkOriginCursor: TRadioButton
      Left = 8
      Top = 16
      Width = 142
      Height = 17
      Caption = 'From cursor'
      TabOrder = 0
    end
    object chkOriginEntire: TRadioButton
      Left = 8
      Top = 32
      Width = 142
      Height = 17
      Caption = 'Entire file'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
end
