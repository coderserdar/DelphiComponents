object SearchForm: TSearchForm
  Left = 335
  Top = 330
  BorderStyle = bsDialog
  Caption = 'Find Text'
  ClientHeight = 204
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LBFindText: TLabel
    Left = 8
    Top = 12
    Width = 60
    Height = 13
    Caption = '&Text to find:'
    FocusControl = CBTextToFind
  end
  object CBTextToFind: TComboBox
    Left = 72
    Top = 8
    Width = 249
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnChange = CBTextToFindChange
  end
  object GBOptions: TGroupBox
    Left = 8
    Top = 36
    Width = 173
    Height = 61
    Caption = 'Options'
    TabOrder = 1
    object CBMatchCase: TCheckBox
      Left = 8
      Top = 16
      Width = 153
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object CBHexaSearch: TCheckBox
      Left = 8
      Top = 36
      Width = 157
      Height = 17
      Caption = 'As he&xadecimal digits'
      TabOrder = 1
    end
  end
  object BUFind: TButton
    Left = 164
    Top = 172
    Width = 75
    Height = 25
    Caption = '&Find'
    Default = True
    ModalResult = 6
    TabOrder = 2
    OnClick = BUFindClick
  end
  object BUCancel: TButton
    Left = 248
    Top = 172
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object GBDirection: TGroupBox
    Left = 192
    Top = 36
    Width = 129
    Height = 61
    Caption = 'Direction'
    TabOrder = 4
    object RBForward: TRadioButton
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = 'Forwar&d'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RBBackward: TRadioButton
      Left = 8
      Top = 36
      Width = 105
      Height = 17
      Caption = '&Backward'
      TabOrder = 1
    end
  end
  object GBScope: TGroupBox
    Left = 8
    Top = 104
    Width = 173
    Height = 61
    Caption = 'Scope'
    TabOrder = 5
    object RBGlobal: TRadioButton
      Left = 8
      Top = 16
      Width = 157
      Height = 17
      Caption = '&Global'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RBSelectedOnly: TRadioButton
      Left = 8
      Top = 36
      Width = 153
      Height = 17
      Caption = '&Selected only'
      TabOrder = 1
    end
  end
  object GBOrigin: TGroupBox
    Left = 192
    Top = 104
    Width = 129
    Height = 61
    Caption = 'Origin'
    TabOrder = 6
    object RBFromCursor: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Fro&m cursor'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RBEntireScope: TRadioButton
      Left = 8
      Top = 36
      Width = 113
      Height = 17
      Caption = '&Entire scope'
      TabOrder = 1
    end
  end
end
