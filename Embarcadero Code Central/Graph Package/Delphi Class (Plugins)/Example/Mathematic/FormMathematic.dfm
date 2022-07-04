object Form1: TForm1
  Left = 85
  Top = 97
  Width = 666
  Height = 482
  Caption = 'Chuong trinh minh hoa su dung doi tuong TMathematic'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 20
    Top = 20
    Width = 300
    Height = 201
    Caption = 'Tinh bieu thuc'
    TabOrder = 0
    object Label7: TLabel
      Left = 20
      Top = 40
      Width = 45
      Height = 13
      Caption = 'Bieu thuc'
    end
    object Label8: TLabel
      Left = 20
      Top = 80
      Width = 30
      Height = 13
      Caption = 'Bien 1'
    end
    object Label9: TLabel
      Left = 20
      Top = 150
      Width = 37
      Height = 13
      Caption = 'Ket qua'
    end
    object Label10: TLabel
      Left = 140
      Top = 80
      Width = 59
      Height = 13
      Caption = 'Gia tri bien 1'
    end
    object EvalExp: TEdit
      Left = 90
      Top = 40
      Width = 181
      Height = 21
      TabOrder = 0
      Text = '5*x+sin(0)+cos(0)'
    end
    object EvalVar1: TEdit
      Left = 70
      Top = 78
      Width = 31
      Height = 21
      TabOrder = 1
      Text = 'x'
    end
    object EvalResult: TEdit
      Left = 90
      Top = 150
      Width = 181
      Height = 21
      TabOrder = 2
    end
    object EvalCal: TButton
      Left = 200
      Top = 110
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Tinh'
      TabOrder = 3
      OnClick = EvalCalClick
    end
    object EvalVal1: TEdit
      Left = 210
      Top = 78
      Width = 61
      Height = 21
      TabOrder = 4
      Text = '1'
    end
  end
  object GroupBox2: TGroupBox
    Left = 350
    Top = 20
    Width = 300
    Height = 201
    Caption = 'Tinh dao ham'
    TabOrder = 1
    object Label1: TLabel
      Left = 20
      Top = 40
      Width = 45
      Height = 13
      Caption = 'Bieu thuc'
    end
    object Label2: TLabel
      Left = 20
      Top = 80
      Width = 48
      Height = 13
      Caption = 'Theo bien'
    end
    object Label3: TLabel
      Left = 20
      Top = 150
      Width = 37
      Height = 13
      Caption = 'Ket qua'
    end
    object DiffExpress: TEdit
      Left = 90
      Top = 40
      Width = 181
      Height = 21
      TabOrder = 0
      Text = 'x^x'
    end
    object DiffVariable: TEdit
      Left = 90
      Top = 80
      Width = 181
      Height = 21
      TabOrder = 1
      Text = 'x'
    end
    object DiffResult: TEdit
      Left = 90
      Top = 150
      Width = 181
      Height = 21
      TabOrder = 2
    end
    object EvalDiff: TButton
      Left = 200
      Top = 110
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Tinh'
      TabOrder = 3
      OnClick = EvalDiffClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 20
    Top = 240
    Width = 300
    Height = 201
    Caption = 'Tinh tong'
    TabOrder = 2
    object Label11: TLabel
      Left = 20
      Top = 40
      Width = 45
      Height = 13
      Caption = 'Bieu thuc'
    end
    object Label12: TLabel
      Left = 20
      Top = 80
      Width = 30
      Height = 13
      Caption = 'Bien 1'
    end
    object Label13: TLabel
      Left = 120
      Top = 80
      Width = 13
      Height = 13
      Caption = 'Tu'
    end
    object Label14: TLabel
      Left = 20
      Top = 150
      Width = 37
      Height = 13
      Caption = 'Ket qua'
    end
    object Label15: TLabel
      Left = 200
      Top = 80
      Width = 20
      Height = 13
      Caption = 'Den'
    end
    object SumExp: TEdit
      Left = 90
      Top = 40
      Width = 181
      Height = 21
      TabOrder = 0
      Text = 'i*i'
    end
    object SumVar: TEdit
      Left = 70
      Top = 78
      Width = 31
      Height = 21
      TabOrder = 1
      Text = 'i'
    end
    object SumFrom: TEdit
      Left = 150
      Top = 78
      Width = 31
      Height = 21
      TabOrder = 2
      Text = '1'
    end
    object SumResult: TEdit
      Left = 90
      Top = 150
      Width = 181
      Height = 21
      TabOrder = 3
    end
    object SumCal: TButton
      Left = 200
      Top = 110
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Tinh'
      TabOrder = 4
      OnClick = SumCalClick
    end
    object SumTo: TEdit
      Left = 240
      Top = 78
      Width = 31
      Height = 21
      TabOrder = 5
      Text = '10'
    end
  end
  object GroupBox4: TGroupBox
    Left = 350
    Top = 240
    Width = 300
    Height = 201
    Caption = 'Tinh tich phan'
    TabOrder = 3
    object Label4: TLabel
      Left = 20
      Top = 40
      Width = 45
      Height = 13
      Caption = 'Bieu thuc'
    end
    object Label5: TLabel
      Left = 20
      Top = 80
      Width = 48
      Height = 13
      Caption = 'Theo bien'
    end
    object Label6: TLabel
      Left = 20
      Top = 150
      Width = 37
      Height = 13
      Caption = 'Ket qua'
    end
    object Edit1: TEdit
      Left = 90
      Top = 40
      Width = 180
      Height = 21
      TabOrder = 0
      Text = 'ln(x)'
    end
    object Edit2: TEdit
      Left = 90
      Top = 80
      Width = 180
      Height = 21
      TabOrder = 1
      Text = 'x'
    end
    object Edit3: TEdit
      Left = 90
      Top = 150
      Width = 180
      Height = 21
      TabOrder = 2
    end
    object Button2: TButton
      Left = 200
      Top = 110
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Tinh'
      Enabled = False
      TabOrder = 3
      OnClick = EvalDiffClick
    end
  end
  object Math: TMathematic
    Left = 590
    Top = 400
  end
end
