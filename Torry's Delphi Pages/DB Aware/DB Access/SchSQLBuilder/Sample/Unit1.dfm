object Form1: TForm1
  Left = 298
  Top = 174
  Width = 696
  Height = 513
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object l1: TLabel
    Left = 8
    Top = 16
    Width = 30
    Height = 13
    Caption = 'Fields:'
  end
  object l2: TLabel
    Left = 8
    Top = 112
    Width = 35
    Height = 13
    Caption = 'Tables:'
  end
  object l3: TLabel
    Left = 8
    Top = 280
    Width = 35
    Height = 13
    Caption = 'Where:'
  end
  object l4: TLabel
    Left = 272
    Top = 96
    Width = 96
    Height = 13
    Caption = 'SQL SELECT script:'
  end
  object l5: TLabel
    Left = 8
    Top = 376
    Width = 41
    Height = 13
    Caption = 'OrderBy:'
  end
  object l6: TLabel
    Left = 8
    Top = 184
    Width = 27
    Height = 13
    Caption = 'Joins:'
  end
  object mFields: TMemo
    Left = 56
    Top = 16
    Width = 209
    Height = 89
    Lines.Strings = (
      'orders.ordercode'
      'items.itemname'
      'customers.custcode'
      'customers.custname')
    TabOrder = 0
  end
  object mTables: TMemo
    Left = 56
    Top = 112
    Width = 209
    Height = 65
    Lines.Strings = (
      'items'
      'customers')
    TabOrder = 1
  end
  object mWhere: TMemo
    Left = 56
    Top = 280
    Width = 209
    Height = 89
    Lines.Strings = (
      'customers.custname like '#39'A%'#39
      'AND orders.value>1000')
    TabOrder = 2
  end
  object btn1: TButton
    Left = 272
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 3
    OnClick = btn1Click
  end
  object mResults: TMemo
    Left = 272
    Top = 112
    Width = 409
    Height = 353
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object mOrderBy: TMemo
    Left = 56
    Top = 376
    Width = 209
    Height = 89
    Lines.Strings = (
      'customers.custname'
      'items.itemname')
    TabOrder = 5
  end
  object cbDistinct: TCheckBox
    Left = 400
    Top = 8
    Width = 97
    Height = 17
    Caption = 'cbDistinct'
    TabOrder = 6
    OnClick = cbDistinctClick
  end
  object cbUseTableAlias: TCheckBox
    Left = 400
    Top = 32
    Width = 97
    Height = 17
    Caption = 'cbUseTableAlias'
    TabOrder = 7
    OnClick = cbUseTableAliasClick
  end
  object cbUseFieldAlias: TCheckBox
    Left = 400
    Top = 56
    Width = 97
    Height = 17
    Caption = 'cbUseFieldAlias'
    TabOrder = 8
    OnClick = cbUseFieldAliasClick
  end
  object cbUseGenFieldAlias: TCheckBox
    Left = 400
    Top = 80
    Width = 97
    Height = 17
    Caption = 'cbUseGenFieldAlias'
    TabOrder = 9
    OnClick = cbUseGenFieldAliasClick
  end
  object mJoin: TMemo
    Left = 56
    Top = 184
    Width = 209
    Height = 89
    Lines.Strings = (
      'orders.custcode=customers.custcode'
      'items.itemcode=orders.itemcode')
    TabOrder = 10
  end
  object rg1: TRadioGroup
    Left = 520
    Top = 8
    Width = 137
    Height = 73
    Caption = 'Language'
    ItemIndex = 0
    Items.Strings = (
      'English'
      'Hungarian')
    TabOrder = 11
    OnClick = rg1Click
  end
  object SchSQLBuilder1: TSchSQLBuilder
    OnError = SchSQLBuilder1Error
    UseGenFieldAlias = False
    About = 'Version 1.1, 1996-2005'#174' Snuki'
    Left = 272
    Top = 16
  end
end
