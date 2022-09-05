object FormPageMargin: TFormPageMargin
  Left = 381
  Top = 172
  ActiveControl = btnOK
  BorderStyle = bsDialog
  Caption = 'Page margin'
  ClientHeight = 215
  ClientWidth = 168
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 88
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbMargin: TGroupBox
    Left = 8
    Top = 8
    Width = 153
    Height = 169
    Caption = 'Margin'
    TabOrder = 2
    object lLeft: TLabel
      Left = 16
      Top = 21
      Width = 21
      Height = 13
      Caption = '&Left:'
      FocusControl = rsLeft
    end
    object lTop: TLabel
      Left = 16
      Top = 53
      Width = 22
      Height = 13
      Caption = '&Top:'
      FocusControl = rsTop
    end
    object lRight: TLabel
      Left = 16
      Top = 85
      Width = 28
      Height = 13
      Caption = '&Right:'
      FocusControl = rsRight
    end
    object lBottom: TLabel
      Left = 16
      Top = 117
      Width = 36
      Height = 13
      Caption = '&Bottom:'
      FocusControl = rsBottom
    end
    object rsLeft: TmcmRealSpin
      Left = 80
      Top = 16
      Width = 57
      Height = 22
      TabOrder = 0
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object rsTop: TmcmRealSpin
      Left = 80
      Top = 48
      Width = 57
      Height = 22
      TabOrder = 1
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object rsRight: TmcmRealSpin
      Left = 80
      Top = 80
      Width = 57
      Height = 22
      TabOrder = 2
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object rsBottom: TmcmRealSpin
      Left = 80
      Top = 112
      Width = 57
      Height = 22
      TabOrder = 3
      Value = 1.000000000000000000
      Decimals = 2
      Increment = 1.000000000000000000
    end
    object cbForceMargin: TCheckBox
      Left = 16
      Top = 144
      Width = 97
      Height = 17
      Caption = '&Force margin'
      TabOrder = 4
    end
  end
end
