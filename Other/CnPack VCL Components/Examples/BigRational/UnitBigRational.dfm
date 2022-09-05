object FormRational: TFormRational
  Left = 192
  Top = 107
  Width = 1142
  Height = 656
  Caption = 'Big Rational Number Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblFloat: TLabel
    Left = 16
    Top = 16
    Width = 48
    Height = 13
    Caption = 'Extended:'
  end
  object grpBR1: TGroupBox
    Left = 16
    Top = 48
    Width = 465
    Height = 145
    Caption = 'Big Rational Number 1:'
    TabOrder = 0
    object bvl1: TBevel
      Left = 24
      Top = 48
      Width = 273
      Height = 18
      Shape = bsBottomLine
    end
    object edtBR1N: TEdit
      Left = 24
      Top = 24
      Width = 273
      Height = 21
      TabOrder = 0
      Text = '10000'
    end
    object edtBR1D: TEdit
      Left = 24
      Top = 88
      Width = 273
      Height = 21
      TabOrder = 1
      Text = '200'
    end
    object btnSet1: TButton
      Left = 312
      Top = 24
      Width = 75
      Height = 21
      Caption = 'Set String'
      TabOrder = 2
      OnClick = btnSet1Click
    end
    object btnSet2: TButton
      Left = 312
      Top = 56
      Width = 75
      Height = 21
      Caption = 'Set Value'
      TabOrder = 3
      OnClick = btnSet2Click
    end
    object btnSet3: TButton
      Left = 312
      Top = 88
      Width = 75
      Height = 21
      Caption = 'Set Int'
      TabOrder = 4
      OnClick = btnSet3Click
    end
    object btnReduce: TButton
      Left = 400
      Top = 24
      Width = 57
      Height = 84
      Caption = 'Reduce'
      TabOrder = 5
      OnClick = btnReduceClick
    end
  end
  object grpRN2: TGroupBox
    Left = 624
    Top = 48
    Width = 481
    Height = 145
    Caption = 'Big Rational Number 2:'
    TabOrder = 1
    object bvlRN2: TBevel
      Left = 24
      Top = 48
      Width = 273
      Height = 18
      Shape = bsBottomLine
    end
    object edtBR2N: TEdit
      Left = 24
      Top = 24
      Width = 273
      Height = 21
      TabOrder = 0
      Text = '20000'
    end
    object edtBR2D: TEdit
      Left = 24
      Top = 88
      Width = 273
      Height = 21
      TabOrder = 1
      Text = '250'
    end
    object btnRN2SetValue: TButton
      Left = 312
      Top = 56
      Width = 75
      Height = 21
      Caption = 'Set Value'
      TabOrder = 2
      OnClick = btnRN2SetValueClick
    end
    object btnToDec: TButton
      Left = 312
      Top = 88
      Width = 75
      Height = 21
      Caption = 'ToDec'
      TabOrder = 3
      OnClick = btnToDecClick
    end
  end
  object btnAdd: TButton
    Left = 536
    Top = 56
    Width = 25
    Height = 25
    Caption = '+'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnSub: TButton
    Left = 536
    Top = 93
    Width = 25
    Height = 25
    Caption = '-'
    TabOrder = 3
    OnClick = btnSubClick
  end
  object btnMul: TButton
    Left = 536
    Top = 131
    Width = 25
    Height = 25
    Caption = '*'
    TabOrder = 4
    OnClick = btnMulClick
  end
  object btnDiv: TButton
    Left = 536
    Top = 168
    Width = 25
    Height = 25
    Caption = '/'
    TabOrder = 5
    OnClick = btnDivClick
  end
  object edtExtended: TEdit
    Left = 80
    Top = 16
    Width = 233
    Height = 21
    TabOrder = 6
    Text = '-123321.847820398924'
  end
  object btnSetExtended: TButton
    Left = 328
    Top = 16
    Width = 75
    Height = 21
    Caption = 'Set Extended'
    TabOrder = 7
    OnClick = btnSetExtendedClick
  end
  object btnSetString: TButton
    Left = 408
    Top = 16
    Width = 75
    Height = 21
    Caption = 'Set String'
    TabOrder = 8
    OnClick = btnSetStringClick
  end
  object btnAddInt: TButton
    Left = 40
    Top = 208
    Width = 25
    Height = 25
    Caption = '+'
    TabOrder = 9
    OnClick = btnAddIntClick
  end
  object btnSubInt: TButton
    Left = 40
    Top = 245
    Width = 25
    Height = 25
    Caption = '-'
    TabOrder = 10
    OnClick = btnSubIntClick
  end
  object btnMulInt: TButton
    Left = 40
    Top = 283
    Width = 25
    Height = 25
    Caption = '*'
    TabOrder = 11
    OnClick = btnMulIntClick
  end
  object btnDivInt: TButton
    Left = 40
    Top = 320
    Width = 25
    Height = 25
    Caption = '/'
    TabOrder = 12
    OnClick = btnDivIntClick
  end
  object edtInt: TEdit
    Left = 104
    Top = 264
    Width = 177
    Height = 21
    TabOrder = 13
    Text = '-3893472'
  end
  object btnBNAdd: TButton
    Left = 312
    Top = 208
    Width = 25
    Height = 25
    Caption = '+'
    TabOrder = 14
    OnClick = btnBNAddClick
  end
  object btnBNSub: TButton
    Left = 312
    Top = 245
    Width = 25
    Height = 25
    Caption = '-'
    TabOrder = 15
    OnClick = btnBNSubClick
  end
  object btnBNMul: TButton
    Left = 312
    Top = 283
    Width = 25
    Height = 25
    Caption = '*'
    TabOrder = 16
    OnClick = btnBNMulClick
  end
  object btnBNDiv: TButton
    Left = 312
    Top = 320
    Width = 25
    Height = 25
    Caption = '/'
    TabOrder = 17
    OnClick = btnBNDivClick
  end
  object btnCompare: TButton
    Left = 480
    Top = 208
    Width = 145
    Height = 25
    Caption = 'Compare Rational Number'
    TabOrder = 18
    OnClick = btnCompareClick
  end
  object btnCompareInt: TButton
    Left = 480
    Top = 248
    Width = 145
    Height = 25
    Caption = 'Compare Rational Number'
    TabOrder = 19
    OnClick = btnCompareIntClick
  end
  object btnCompareBN: TButton
    Left = 480
    Top = 288
    Width = 145
    Height = 25
    Caption = 'Compare BigNumber'
    TabOrder = 20
    OnClick = btnCompareBNClick
  end
end
