object FormMath: TFormMath
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Image compute'
  ClientHeight = 315
  ClientWidth = 538
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 8
    Top = 280
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 88
    Top = 280
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbSource1: TGroupBox
    Left = 8
    Top = 8
    Width = 169
    Height = 249
    Caption = 'Source 1'
    TabOrder = 2
    object icSource1: TmcmImageCtrl
      Left = 8
      Top = 56
      Width = 153
      Height = 153
      BorderStyle = BS_SINGLE
      Center = True
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleToFit = False
    end
    object lBlendFactor1: TLabel
      Left = 8
      Top = 224
      Width = 60
      Height = 13
      Caption = 'Blend factor:'
    end
    object cbImage1: TComboBox
      Left = 8
      Top = 24
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbImage1Change
    end
    object rsBlendFactor1: TmcmRealSpin
      Left = 104
      Top = 216
      Width = 57
      Height = 22
      TabOrder = 1
      OnChange = rsBlendFactorChange
      Value = 50.000000000000000000
      MaxValue = 255.000000000000000000
      Decimals = 1
      Increment = 1.000000000000000000
    end
  end
  object gbSource2: TGroupBox
    Left = 184
    Top = 8
    Width = 169
    Height = 249
    Caption = 'Source 2'
    TabOrder = 3
    object icSource2: TmcmImageCtrl
      Left = 8
      Top = 56
      Width = 153
      Height = 153
      BorderStyle = BS_SINGLE
      Center = True
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleToFit = False
    end
    object lBlendFactor2: TLabel
      Left = 8
      Top = 224
      Width = 60
      Height = 13
      Caption = 'Blend factor:'
    end
    object cbImage2: TComboBox
      Left = 8
      Top = 24
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbImage2Change
    end
    object rsBlendFactor2: TmcmRealSpin
      Left = 104
      Top = 216
      Width = 57
      Height = 22
      TabOrder = 1
      OnChange = rsBlendFactorChange
      Value = 50.000000000000000000
      MaxValue = 255.000000000000000000
      Decimals = 1
      Increment = 1.000000000000000000
    end
  end
  object gbResult: TGroupBox
    Left = 360
    Top = 8
    Width = 169
    Height = 249
    Caption = 'Result'
    TabOrder = 4
    object icResult: TmcmImageCtrl
      Left = 8
      Top = 56
      Width = 153
      Height = 153
      BorderStyle = BS_SINGLE
      Center = True
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleToFit = False
    end
    object cbMethod: TComboBox
      Left = 8
      Top = 24
      Width = 153
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbMethodChange
    end
  end
end
