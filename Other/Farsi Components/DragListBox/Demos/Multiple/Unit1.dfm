object Form1: TForm1
  Left = 266
  Top = 126
  Width = 443
  Height = 452
  Caption = 'TDragListBox demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 12
    Width = 137
    Height = 361
  end
  object Bevel2: TBevel
    Left = 152
    Top = 12
    Width = 273
    Height = 361
  end
  object Bevel3: TBevel
    Left = 288
    Top = 12
    Width = 17
    Height = 361
    Shape = bsLeftLine
  end
  object Bevel6: TBevel
    Left = 152
    Top = 280
    Width = 273
    Height = 33
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 156
    Top = 20
    Width = 39
    Height = 13
    Caption = 'Group A'
  end
  object Label2: TLabel
    Left = 156
    Top = 108
    Width = 39
    Height = 13
    Caption = 'Group B'
  end
  object Label3: TLabel
    Left = 156
    Top = 196
    Width = 39
    Height = 13
    Caption = 'Group C'
  end
  object Label4: TLabel
    Left = 156
    Top = 284
    Width = 40
    Height = 13
    Caption = 'Group D'
  end
  object Label5: TLabel
    Left = 296
    Top = 20
    Width = 39
    Height = 13
    Caption = 'Group E'
  end
  object Label6: TLabel
    Left = 296
    Top = 108
    Width = 38
    Height = 13
    Caption = 'Group F'
  end
  object Label7: TLabel
    Left = 296
    Top = 196
    Width = 40
    Height = 13
    Caption = 'Group G'
  end
  object Label8: TLabel
    Left = 296
    Top = 284
    Width = 40
    Height = 13
    Caption = 'Group H'
  end
  object Label9: TLabel
    Left = 16
    Top = 20
    Width = 35
    Height = 13
    Caption = 'Teams:'
  end
  object Bevel4: TBevel
    Left = 152
    Top = 104
    Width = 273
    Height = 33
    Shape = bsTopLine
  end
  object Bevel5: TBevel
    Left = 152
    Top = 192
    Width = 273
    Height = 33
    Shape = bsTopLine
  end
  object DragListBox1: TDragListBox
    Left = 16
    Top = 36
    Width = 121
    Height = 325
    ItemHeight = 13
    Items.Strings = (
      'Argentina'
      'Belgium'
      'Brazil'
      'Cameroon'
      'China PR'
      'Costa Rica'
      'Croatia'
      'Denmark'
      'Ecuador'
      'England'
      'France'
      'Germany'
      'Italy'
      'Japan'
      'Korea Republic'
      'Mexico'
      'Nigeria'
      'Paraguay'
      'Poland'
      'Portugal'
      'Republic of Ireland'
      'Russia'
      'Saudi Arabia'
      'Senegal'
      'Slovenia'
      'South Africa'
      'Spain'
      'Sweden'
      'Tunisia'
      'Turkey'
      'Uruguay'
      'USA')
    Sorted = True
    TabOrder = 0
  end
  object DragListBox2: TDragListBox
    Left = 156
    Top = 36
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 1
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox3: TDragListBox
    Left = 156
    Top = 124
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 2
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox4: TDragListBox
    Left = 156
    Top = 212
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 3
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox5: TDragListBox
    Left = 156
    Top = 300
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 4
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox6: TDragListBox
    Left = 296
    Top = 36
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 5
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox7: TDragListBox
    Left = 296
    Top = 124
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 6
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox8: TDragListBox
    Left = 296
    Top = 212
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 7
    OnDragOver = DragListBox2DragOver
  end
  object DragListBox9: TDragListBox
    Left = 296
    Top = 300
    Width = 121
    Height = 61
    ItemHeight = 13
    TabOrder = 8
    OnDragOver = DragListBox2DragOver
  end
  object Button1: TButton
    Left = 348
    Top = 384
    Width = 75
    Height = 25
    Caption = 'Kick Off'
    TabOrder = 9
    OnClick = Button1Click
  end
end
